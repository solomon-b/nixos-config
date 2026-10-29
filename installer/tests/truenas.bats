#!/usr/bin/env bats
# Tests for installer/lib/truenas.sh.
# Run: nix run nixpkgs#bats -- installer/tests/truenas.bats

setup() {
  PATH="$BATS_TEST_DIRNAME/stubs:$PATH"
  STUB_LOG="$BATS_TEST_TMPDIR/curl.log"
  export STUB_LOG
  : > "$STUB_LOG"
  TRUENAS_API_KEY=test-key
  # shellcheck source=../lib/truenas.sh
  source "$BATS_TEST_DIRNAME/../lib/truenas.sh"
}

@test "truenas_vm_name changes hyphens to underscores" {
  [ "$(truenas_vm_name my-vm-1)" = "my_vm_1" ]
  [ "$(truenas_vm_name caleuche)" = "caleuche" ]
}

@test "truenas_gen_mac uses the TrueNAS prefix" {
  [[ "$(truenas_gen_mac)" =~ ^00:a0:98:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}$ ]]
}

@test "truenas_gen_vnc_password is 8 alphanumeric characters" {
  [[ "$(truenas_gen_vnc_password)" =~ ^[A-Za-z0-9]{8}$ ]]
}

@test "truenas_validate_specs accepts the defaults" {
  run truenas_validate_specs 2 4 50
  [ "$status" -eq 0 ]
}

@test "truenas_validate_specs rejects bad input" {
  for args in "3 4 50" "0 4 50" "abc 4 50" "2 0 50" "2 4 x" "'' 4 50"; do
    eval "run truenas_validate_specs $args"
    [ "$status" -eq 1 ]
    [ -n "$output" ]
  done
}

@test "truenas_api returns the body on HTTP 200" {
  STUB_STATUS=200 STUB_BODY='[]' run truenas_api GET /vm
  [ "$status" -eq 0 ]
  [ "$output" = "[]" ]
}

@test "truenas_api fails with the status on HTTP 401" {
  STUB_STATUS=401 STUB_BODY='denied' run truenas_api GET /vm
  [ "$status" -eq 1 ]
  [[ "$output" == *"HTTP 401"* ]]
}

@test "truenas_api keeps the key off the command line" {
  STUB_STATUS=200 STUB_BODY='[]' truenas_api GET /vm
  grep -q 'STDIN.*test-key' "$STUB_LOG"
  ! grep '^ARGS' "$STUB_LOG" | grep -q 'test-key'
}

@test "truenas_check_vm reports absent" {
  STUB_STATUS=200 STUB_BODY='[]' run truenas_check_vm caleuche
  [ "$output" = "absent" ]
}

@test "truenas_check_vm reports a VM the installer made" {
  STUB_STATUS=200 STUB_BODY='[{"id":9,"name":"caleuche","description":"created by nixos-config installer","devices":[{"dtype":"NIC","attributes":{"mac":"00:a0:98:01:02:03"}}]}]' \
    run truenas_check_vm caleuche
  [[ "$output" == ours* ]]
  [ "$(truenas_vm_mac "${output#ours }")" = "00:a0:98:01:02:03" ]
}

@test "truenas_check_vm reports a foreign VM and makes no write call" {
  STUB_STATUS=200 STUB_BODY='[{"id":3,"name":"apollyon","description":"qbittorrent","devices":[]}]' \
    run truenas_check_vm apollyon
  [ "$output" = "foreign" ]
  ! grep -qE 'POST|PUT|DELETE' "$STUB_LOG"
}

# A fake truenas_api for the create, ISO, and device tests. Each test
# fills RESPONSES with "METHOD PATH" -> "ok:<body>". Any other call
# fails like an HTTP 404. Every call goes to the file $CALLS.
use_fake_api() {
  CALLS="$BATS_TEST_TMPDIR/calls"
  : > "$CALLS"
  declare -gA RESPONSES=()
  truenas_api() {
    echo "$1 $2" >> "$CALLS"
    # "KEY#N" answers only the Nth call to KEY. Plain "KEY" answers the rest.
    local n; n=$(grep -cxF "$1 $2" "$CALLS")
    local r="${RESPONSES["$1 $2#$n"]-${RESPONSES["$1 $2"]-fail}}"
    if [[ "$r" == ok:* ]]; then
      printf '%s\n' "${r#ok:}"
    else
      echo "TrueNAS API $1 $2 failed: HTTP 404" >&2
      return 1
    fi
  }
}

@test "truenas_zvol_payload is thick, 16K, sized in bytes" {
  p=$(truenas_zvol_payload caleuche 50)
  [ "$(jq -r .name <<<"$p")" = "virtual-machines/caleuche" ]
  [ "$(jq -r .type <<<"$p")" = "VOLUME" ]
  [ "$(jq -r .volsize <<<"$p")" = "53687091200" ]
  [ "$(jq -r .volblocksize <<<"$p")" = "16K" ]
  [ "$(jq -r .sparse <<<"$p")" = "false" ]
}

@test "truenas_vm_payload matches the existing VMs" {
  p=$(truenas_vm_payload my-vm 4 8 00:a0:98:01:02:03 /mnt/virtual-machines/iso/x.iso abcd1234)
  [ "$(jq -r .name <<<"$p")" = "my_vm" ]
  [ "$(jq -r .description <<<"$p")" = "created by nixos-config installer" ]
  [ "$(jq -c '[.vcpus,.cores,.threads,.memory]' <<<"$p")" = "[1,2,2,8192]" ]
  [ "$(jq -r '[.bootloader,.time,.autostart,.shutdown_timeout] | map(tostring) | join(",")' <<<"$p")" = "UEFI,LOCAL,true,90" ]
  [ "$(jq -r '.devices[] | select(.dtype=="DISK") | .attributes | [.path,.type] | join(",")' <<<"$p")" = "/dev/zvol/virtual-machines/my-vm,VIRTIO" ]
  [ "$(jq -r '.devices[] | select(.dtype=="CDROM") | .attributes.path' <<<"$p")" = "/mnt/virtual-machines/iso/x.iso" ]
  [ "$(jq -r '.devices[] | select(.dtype=="NIC") | .attributes | [.type,.nic_attach,.mac] | join(",")' <<<"$p")" = "VIRTIO,igb0,00:a0:98:01:02:03" ]
  [ "$(jq -r '.devices[] | select(.dtype=="VNC") | .attributes | [.vnc_password,.vnc_bind,(.vnc_web|tostring),.vnc_resolution,(.wait|tostring)] | join(",")' <<<"$p")" = "abcd1234,192.168.5.6,true,1024x768,false" ]
  [ "$(jq -c '[.devices[] | {(.dtype): .order}] | add' <<<"$p")" = '{"DISK":1000,"CDROM":1001,"NIC":1002,"VNC":1003}' ]
}

@test "truenas_create_vm stops when the zvol exists, with no write call" {
  use_fake_api
  RESPONSES["GET /pool/dataset/id/virtual-machines%2Fcaleuche"]='ok:{"id":"virtual-machines/caleuche"}'
  run truenas_create_vm caleuche 2 4 50 00:a0:98:01:02:03 /x.iso abcd1234
  [ "$status" -eq 1 ]
  [[ "$output" == *"virtual-machines/caleuche"* ]]
  ! grep -qE '^(POST|PUT|DELETE)' "$CALLS"
}

@test "truenas_create_vm creates the zvol before the VM and prints the id" {
  use_fake_api
  RESPONSES["POST /pool/dataset"]='ok:{}'
  RESPONSES["POST /vm"]='ok:{"id":12}'
  run truenas_create_vm caleuche 2 4 50 00:a0:98:01:02:03 /x.iso abcd1234
  [ "$status" -eq 0 ]
  [ "${lines[-1]}" = "12" ]
  [ "$(grep -n '^POST /pool/dataset$' "$CALLS" | cut -d: -f1)" -lt "$(grep -n '^POST /vm$' "$CALLS" | cut -d: -f1)" ]
}

@test "truenas_create_vm explains a partial failure" {
  use_fake_api
  RESPONSES["POST /pool/dataset"]='ok:{}'
  run truenas_create_vm caleuche 2 4 50 00:a0:98:01:02:03 /x.iso abcd1234
  [ "$status" -eq 1 ]
  [[ "$output" == *"Created zvol virtual-machines/caleuche, but the VM create failed."* ]]
  [[ "$output" == *"Delete zvol"* ]]
}

@test "truenas_ensure_iso skips the upload when the ISO is there" {
  use_fake_api
  RESPONSES["POST /filesystem/mkdir"]='ok:{}'
  RESPONSES["POST /filesystem/stat"]='ok:{"size":5}'
  iso="$BATS_TEST_TMPDIR/0123456789abcdfghijklmnpqrsvwxyz-nixos-iso/iso/NixOS.iso"
  mkdir -p "$(dirname "$iso")" && echo data > "$iso"
  run truenas_ensure_iso "$iso"
  [ "$status" -eq 0 ]
  [ "${lines[-1]}" = "/mnt/virtual-machines/iso/nixos-installer-0123456789abcdfghijklmnpqrsvwxyz.iso" ]
  ! grep -q '_upload' "$STUB_LOG"
}

@test "truenas_ensure_iso uploads when the ISO is absent and waits for the job" {
  use_fake_api
  RESPONSES["POST /filesystem/mkdir"]='ok:{}'
  RESPONSES["GET /core/get_jobs?id=5"]='ok:[{"id":5,"state":"SUCCESS"}]'
  RESPONSES["POST /filesystem/stat#2"]='ok:{"size":5}'
  iso="$BATS_TEST_TMPDIR/0123456789abcdfghijklmnpqrsvwxyz-nixos-iso/iso/NixOS.iso"
  mkdir -p "$(dirname "$iso")" && echo data > "$iso"
  STUB_STATUS=200 STUB_BODY='{"job_id":5}' run truenas_ensure_iso "$iso"
  [ "$status" -eq 0 ]
  [ "${lines[-1]}" = "/mnt/virtual-machines/iso/nixos-installer-0123456789abcdfghijklmnpqrsvwxyz.iso" ]
  grep -q '^ARGS.*https://sandra-voi.home.arpa/_upload' "$STUB_LOG"
  ! grep '^ARGS' "$STUB_LOG" | grep -q 'test-key'
}

@test "truenas_ensure_iso uploads again when the ISO on the NAS has the wrong size" {
  use_fake_api
  RESPONSES["POST /filesystem/mkdir"]='ok:{}'
  RESPONSES["POST /filesystem/stat#1"]='ok:{"size":0}'
  RESPONSES["POST /filesystem/stat#2"]='ok:{"size":5}'
  RESPONSES["GET /core/get_jobs?id=5"]='ok:[{"id":5,"state":"SUCCESS"}]'
  iso="$BATS_TEST_TMPDIR/0123456789abcdfghijklmnpqrsvwxyz-nixos-iso/iso/NixOS.iso"
  mkdir -p "$(dirname "$iso")" && echo data > "$iso"
  STUB_STATUS=200 STUB_BODY='{"job_id":5}' run truenas_ensure_iso "$iso"
  [ "$status" -eq 0 ]
  grep -q '^ARGS.*https://sandra-voi.home.arpa/_upload' "$STUB_LOG"
}

@test "truenas_ensure_iso fails when the uploaded ISO has the wrong size" {
  use_fake_api
  RESPONSES["POST /filesystem/mkdir"]='ok:{}'
  RESPONSES["POST /filesystem/stat#2"]='ok:{"size":0}'
  RESPONSES["GET /core/get_jobs?id=5"]='ok:[{"id":5,"state":"SUCCESS"}]'
  iso="$BATS_TEST_TMPDIR/0123456789abcdfghijklmnpqrsvwxyz-nixos-iso/iso/NixOS.iso"
  mkdir -p "$(dirname "$iso")" && echo data > "$iso"
  STUB_STATUS=200 STUB_BODY='{"job_id":5}' run truenas_ensure_iso "$iso"
  [ "$status" -eq 1 ]
  [[ "$output" == *"The ISO on the NAS has 0 bytes, but the local ISO has 5."* ]]
}

@test "truenas_start_vm and truenas_vm_state use the VM id" {
  use_fake_api
  RESPONSES["POST /vm/id/12/start"]='ok:null'
  RESPONSES["GET /vm/id/12"]='ok:{"id":12,"status":{"state":"STOPPED"}}'
  truenas_start_vm 12
  [ "$(truenas_vm_state 12)" = "STOPPED" ]
  grep -q '^POST /vm/id/12/start$' "$CALLS"
}

@test "truenas_detach_iso deletes only CDROM devices" {
  use_fake_api
  RESPONSES["GET /vm/id/12"]='ok:{"id":12,"devices":[{"id":1,"dtype":"DISK"},{"id":2,"dtype":"CDROM"},{"id":3,"dtype":"NIC"}]}'
  RESPONSES["DELETE /vm/device/id/2"]='ok:true'
  truenas_detach_iso 12
  grep -q '^DELETE /vm/device/id/2$' "$CALLS"
  ! grep -qE '^DELETE /vm/device/id/(1|3)$' "$CALLS"
}

@test "truenas_validate_specs rejects leading zeros and more than 16 CPUs without a crash" {
  for args in "08 4 50" "010 4 50" "18 4 50" "32 4 50"; do
    eval "run truenas_validate_specs $args"
    [ "$status" -eq 1 ]
  done
  run truenas_validate_specs 16 4 50
  [ "$status" -eq 0 ]
}

@test "truenas_zvol_exists is true only when the zvol GET succeeds" {
  use_fake_api
  RESPONSES["GET /pool/dataset/id/virtual-machines%2Fcaleuche"]='ok:{"id":"virtual-machines/caleuche"}'
  truenas_zvol_exists caleuche
  ! truenas_zvol_exists other
}

@test "truenas_wait_job fails when the API call fails" {
  use_fake_api
  run truenas_wait_job 5
  [ "$status" -eq 1 ]
}

@test "truenas_wait_job fails on an unknown job" {
  use_fake_api
  RESPONSES["GET /core/get_jobs?id=5"]='ok:[]'
  run truenas_wait_job 5
  [ "$status" -eq 1 ]
}

@test "truenas_wait_job stops at the timeout" {
  use_fake_api
  RESPONSES["GET /core/get_jobs?id=5"]='ok:[{"id":5,"state":"RUNNING"}]'
  TRUENAS_JOB_TIMEOUT=2 TRUENAS_JOB_INTERVAL=1 run truenas_wait_job 5
  [ "$status" -eq 1 ]
}

@test "truenas_detach_iso fails when the VM GET fails" {
  use_fake_api
  run truenas_detach_iso 12
  [ "$status" -eq 1 ]
}
