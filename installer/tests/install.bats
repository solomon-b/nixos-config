#!/usr/bin/env bats
# Tests for install_truenas_vm in installer/install.sh.
# Run: nix run nixpkgs#bats -- installer/tests/install.bats

setup() {
  INSTALLER_LIB_DIR="$BATS_TEST_DIRNAME/../lib"
  # shellcheck source=../install.sh
  source "$BATS_TEST_DIRNAME/../install.sh"

  CALLS="$BATS_TEST_TMPDIR/calls"
  INPUTS="$BATS_TEST_TMPDIR/inputs"
  : > "$CALLS"
  : > "$INPUTS"
  GUM_CONFIRM=0
  SSH_OK=0
  TRUENAS_SSH_TIMEOUT=2
  TRUENAS_SSH_INTERVAL=1
  SERVER_KIND="virtual machine"
  INSTALLER_ISO="$BATS_TEST_TMPDIR/0123456789abcdfghijklmnpqrsvwxyz-nixos-iso"
  mkdir -p "$INSTALLER_ISO/iso" && touch "$INSTALLER_ISO/iso/NixOS.iso"

  # Fakes for the tools and the TrueNAS library.
  gum() {
    case "$1" in
      confirm) return "$GUM_CONFIRM" ;;
      input) echo "gum $*" >> "$CALLS"; head -n1 "$INPUTS"; sed -i 1d "$INPUTS" ;;
      log) shift; echo "$*" >&2 ;;
    esac
  }
  pass() { echo "pass $*" >> "$CALLS"; cat > /dev/null; }
  ssh() { echo "ssh $*" >> "$CALLS"; return "$SSH_OK"; }
  truenas_load_key() { :; }
  truenas_zvol_exists() { return 1; }
  ssh-keygen() { :; }
  truenas_check_vm() { echo "$CHECK_RESULT"; }
  truenas_ensure_iso() { echo "ensure_iso $1" >> "$CALLS"; echo /mnt/virtual-machines/iso/x.iso; }
  truenas_create_vm() { echo "create_vm $*" >> "$CALLS"; echo 12; }
  truenas_vm_state() { echo "${VM_STATE:-STOPPED}"; }
  truenas_start_vm() { echo "start_vm $1" >> "$CALLS"; }
}

@test "a foreign VM stops the run with no create call" {
  CHECK_RESULT=foreign
  run install_truenas_vm apollyon
  [ "$status" -eq 1 ]
  [[ "$output" == *"the installer did not make it"* ]]
  ! grep -q create_vm "$CALLS"
}

@test "saying no to TrueNAS creation returns 2" {
  CHECK_RESULT=absent
  GUM_CONFIRM=1
  run install_truenas_vm caleuche
  [ "$status" -eq 2 ]
  ! grep -q create_vm "$CALLS"
}

@test "a new VM is created with the specs, then started, and the IP is set" {
  CHECK_RESULT=absent
  printf '%s\n' 2 4 50 192.168.5.106 > "$INPUTS"
  install_truenas_vm caleuche 2> "$BATS_TEST_TMPDIR/err"
  [ "$IP" = "192.168.5.106" ]
  [ "$TRUENAS_VM_ID" = "12" ]
  grep -q '^pass insert -m -f machine/caleuche/vnc-password$' "$CALLS"
  grep -q "^ensure_iso $INSTALLER_ISO/iso/NixOS.iso$" "$CALLS"
  grep -qE '^create_vm caleuche 2 4 50 00:a0:98:[0-9a-f:]{8} /mnt/virtual-machines/iso/x.iso [A-Za-z0-9]{8}$' "$CALLS"
  grep -q '^start_vm 12$' "$CALLS"
  grep -q 'Make a fixed IP reservation for this VM in UniFi:' "$BATS_TEST_TMPDIR/err"
  grep -qE 'MAC address: 00:a0:98:' "$BATS_TEST_TMPDIR/err"
}

@test "bad specs are asked for again" {
  CHECK_RESULT=absent
  printf '%s\n' 3 4 50 2 4 50 192.168.5.106 > "$INPUTS"
  install_truenas_vm caleuche 2> "$BATS_TEST_TMPDIR/err"
  grep -q 'even number' "$BATS_TEST_TMPDIR/err"
  grep -q '^create_vm caleuche 2 4 50 ' "$CALLS"
}

@test "a VM from an earlier run is reused and not started twice" {
  CHECK_RESULT='ours {"id":9,"devices":[{"dtype":"NIC","attributes":{"mac":"00:a0:98:01:02:03"}}]}'
  VM_STATE=RUNNING
  printf '%s\n' 192.168.5.106 > "$INPUTS"
  install_truenas_vm caleuche 2> "$BATS_TEST_TMPDIR/err"
  [ "$TRUENAS_VM_ID" = "9" ]
  grep -q 'Found the VM caleuche from an earlier run' "$BATS_TEST_TMPDIR/err"
  grep -q 'MAC address: 00:a0:98:01:02:03' "$BATS_TEST_TMPDIR/err"
  ! grep -qE 'create_vm|start_vm|ensure_iso' "$CALLS"
}

@test "no SSH answer before the timeout stops the run with a hint" {
  CHECK_RESULT=absent
  SSH_OK=1
  printf '%s\n' 2 4 50 192.168.5.106 > "$INPUTS"
  run install_truenas_vm caleuche
  [ "$status" -eq 1 ]
  [[ "$output" == *"did not answer on SSH at 192.168.5.106"* ]]
  [[ "$output" == *"Run the installer again"* ]]
}

@test "the ISO host key is trusted for this IP only, after old keys are removed" {
  CHECK_RESULT=absent
  printf '%s\n' 2 4 50 192.168.5.106 > "$INPUTS"
  ssh-keygen() { echo "ssh-keygen $*" >> "$CALLS"; }
  install_truenas_vm caleuche 2> /dev/null
  grep -q '^ssh-keygen -R 192.168.5.106$' "$CALLS"
  grep -q '^ssh .*StrictHostKeyChecking=accept-new.*root@192.168.5.106' "$CALLS"
  ! grep -q 'UserKnownHostsFile=/dev/null' "$CALLS"
}

@test "an existing zvol stops the run before any write" {
  CHECK_RESULT=absent
  truenas_zvol_exists() { return 0; }
  printf '%s\n' 2 4 50 192.168.5.106 > "$INPUTS"
  run install_truenas_vm caleuche
  [ "$status" -eq 1 ]
  [[ "$output" == *"virtual-machines/caleuche"* ]]
  ! grep -qE '^pass|ensure_iso|create_vm' "$CALLS"
}

@test "a failed ISO build stops the run before the upload" {
  CHECK_RESULT=absent
  INSTALLER_ISO=""
  nix() { return 1; }
  find() { echo "find $*" >> "$CALLS"; }
  printf '%s\n' 2 4 50 192.168.5.106 > "$INPUTS"
  run install_truenas_vm caleuche
  [ "$status" -eq 1 ]
  ! grep -qE '^find|ensure_iso|create_vm' "$CALLS"
}

@test "a failed pass insert stops the run before the VM is made" {
  CHECK_RESULT=absent
  pass() { cat > /dev/null; return 1; }
  printf '%s\n' 2 4 50 192.168.5.106 > "$INPUTS"
  run install_truenas_vm caleuche
  [ "$status" -eq 1 ]
  ! grep -qE 'ensure_iso|create_vm' "$CALLS"
}

@test "a failed ISO detach is a warning, not the end of the run" {
  truenas_detach_iso() { return 1; }
  run detach_installer_iso 12
  [ "$status" -eq 0 ]
  [[ "$output" == *"Remove the CD-ROM device by hand"* ]]
}

@test "each spec prompt has a header, because a pre-filled field hides its placeholder" {
  CHECK_RESULT=absent
  printf '%s\n' 2 4 50 192.168.5.106 > "$INPUTS"
  run install_truenas_vm caleuche
  [ "$status" -eq 0 ]
  grep -q -- '--header Guest CPUs (even number, 2 to 16)' "$CALLS"
  grep -q -- '--header RAM in GiB' "$CALLS"
  grep -q -- '--header Disk size in GiB' "$CALLS"
}

@test "a VNC password left by an earlier failed run does not block a re-run" {
  CHECK_RESULT=absent
  printf '%s\n' 2 4 50 192.168.5.106 > "$INPUTS"
  # Like pass with an existing entry: insert without -f fails.
  pass() { echo "pass $*" >> "$CALLS"; cat > /dev/null; [[ " $* " == *" -f "* ]]; }
  run install_truenas_vm caleuche
  [ "$status" -eq 0 ]
  grep -q create_vm "$CALLS"
}

@test "find_installer_iso returns the ISO file when the store directory name ends in .iso" {
  INSTALLER_ISO="$BATS_TEST_TMPDIR/0123456789abcdfghijklmnpqrsvwxyz-nixos-x86_64-linux.iso"
  mkdir -p "$INSTALLER_ISO/iso" && echo data > "$INSTALLER_ISO/iso/nixos.iso"
  run find_installer_iso
  [ "$status" -eq 0 ]
  [ "$output" = "$INSTALLER_ISO/iso/nixos.iso" ]
}

@test "install.sh never runs the install command, because the package's own bin/install comes first in PATH" {
  ! grep -nE '(^|[;&|(]|\s)install\s' "$BATS_TEST_DIRNAME/../install.sh" "$BATS_TEST_DIRNAME/../lib/truenas.sh"
}
