# shellcheck shell=bash
#
# TrueNAS CORE helpers for the installer. Source this file. It defines
# constants and functions only.
#
# The installer uses these functions to make bhyve VMs that match the
# existing VMs on the TrueNAS box. Messages go to stderr. Data goes to
# stdout.

readonly TRUENAS_HOST="sandra-voi.home.arpa"
readonly TRUENAS_API="https://${TRUENAS_HOST}/api/v2.0"
readonly TRUENAS_POOL="virtual-machines"
readonly TRUENAS_NIC="igb0"
# The TrueNAS LAN address. VNC consoles listen only here.
readonly TRUENAS_VNC_BIND="192.168.5.6"
readonly TRUENAS_MARKER="created by nixos-config installer"
readonly TRUENAS_ISO_DIR="/mnt/virtual-machines/iso"
readonly TRUENAS_KEY_PASS_PATH="service/truenas/api/nixos-installer"

# Read the API key from pass into TRUENAS_API_KEY.
truenas_load_key() {
    TRUENAS_API_KEY=$(pass show "${TRUENAS_KEY_PASS_PATH}" 2>/dev/null | head -n1) || true
    if [ -z "${TRUENAS_API_KEY}" ]; then
        echo "Cannot read the TrueNAS API key from pass at ${TRUENAS_KEY_PASS_PATH}." >&2
        return 1
    fi
}

# Write the curl config with the API key. curl reads it on stdin, so the
# key does not show in the process list.
truenas_curl_config() {
    printf 'header = "Authorization: Bearer %s"\n' "${TRUENAS_API_KEY}"
}

# truenas_api METHOD PATH [JSON-BODY]
# Print the response body. Return 1 if the HTTP status is not 2xx.
truenas_api() {
    local method="$1" path="$2" body="${3-}"
    local response status
    local args=(-sk -K - -X "${method}" -H "Content-Type: application/json" -w '\n%{http_code}')

    if [ -n "${body}" ]; then
        response=$(truenas_curl_config | curl "${args[@]}" --data-binary @<(printf '%s' "${body}") "${TRUENAS_API}${path}") || {
            echo "TrueNAS API ${method} ${path} failed: curl could not connect" >&2
            return 1
        }
    else
        response=$(truenas_curl_config | curl "${args[@]}" "${TRUENAS_API}${path}") || {
            echo "TrueNAS API ${method} ${path} failed: curl could not connect" >&2
            return 1
        }
    fi

    status="${response##*$'\n'}"
    response="${response%$'\n'*}"
    if [[ ! "${status}" =~ ^2[0-9][0-9]$ ]]; then
        echo "TrueNAS API ${method} ${path} failed: HTTP ${status}" >&2
        echo "${response}" >&2
        return 1
    fi
    printf '%s\n' "${response}"
}

# URL-encode one value.
truenas_uri() {
    jq -rn --arg v "$1" '$v | @uri'
}

# TrueNAS VM names allow no hyphens.
truenas_vm_name() {
    echo "${1//-/_}"
}

truenas_gen_mac() {
    local octets
    read -r -a octets < <(od -An -N3 -tx1 /dev/urandom)
    printf '00:a0:98:%s:%s:%s\n' "${octets[@]}"
}

# VNC passwords on bhyve are at most 8 characters.
truenas_gen_vnc_password() {
    head -c 512 /dev/urandom | LC_ALL=C tr -dc 'A-Za-z0-9' | cut -c1-8
}

# truenas_validate_specs CPUS RAM_GIB DISK_GIB
truenas_validate_specs() {
    local cpus="$1" ram="$2" disk="$3"

    # bhyve on FreeBSD 13 allows at most 16 vCPUs.
    if [[ ! "${cpus}" =~ ^[1-9][0-9]?$ ]] || [ "${cpus}" -lt 2 ] || [ "${cpus}" -gt 16 ] || [ $((cpus % 2)) -ne 0 ]; then
        echo "The CPU count must be an even number from 2 to 16." >&2
        return 1
    fi
    if [[ ! "${ram}" =~ ^[1-9][0-9]{0,3}$ ]]; then
        echo "The RAM must be a whole number of GiB, 1 or more." >&2
        return 1
    fi
    if [[ ! "${disk}" =~ ^[1-9][0-9]{0,4}$ ]]; then
        echo "The disk size must be a whole number of GiB, 1 or more." >&2
        return 1
    fi
}

# truenas_check_vm VM_NAME
# Print "absent", "ours <vm-json>", or "foreign".
truenas_check_vm() {
    local vms
    vms=$(truenas_api GET "/vm?name=$(truenas_uri "$1")") || return 1

    if [ "$(jq 'length' <<<"${vms}")" -eq 0 ]; then
        echo "absent"
    elif [ "$(jq -r '.[0].description' <<<"${vms}")" = "${TRUENAS_MARKER}" ]; then
        echo "ours $(jq -c '.[0]' <<<"${vms}")"
    else
        echo "foreign"
    fi
}

# truenas_vm_mac VM_JSON
truenas_vm_mac() {
    jq -r '[.devices[] | select(.dtype == "NIC") | .attributes.mac][0]' <<<"$1"
}

# truenas_zvol_payload MACHINE DISK_GIB
# A thick zvol, like the zvols of the existing VMs.
truenas_zvol_payload() {
    jq -n \
        --arg name "${TRUENAS_POOL}/$1" \
        --argjson volsize "$(($2 * 1024 * 1024 * 1024))" \
        '{name: $name, type: "VOLUME", volsize: $volsize, volblocksize: "16K", sparse: false}'
}

# truenas_vm_payload MACHINE CPUS RAM_GIB MAC ISO_PATH VNC_PASSWORD
# The disk comes before the CD-ROM in the boot order. On the first boot
# the disk is empty, so the VM boots the ISO. After the install, the VM
# boots the disk.
truenas_vm_payload() {
    local machine="$1" cpus="$2" ram="$3" mac="$4" iso="$5" vnc_password="$6"
    jq -n \
        --arg name "$(truenas_vm_name "${machine}")" \
        --arg description "${TRUENAS_MARKER}" \
        --argjson cores "$((cpus / 2))" \
        --argjson memory "$((ram * 1024))" \
        --arg disk "/dev/zvol/${TRUENAS_POOL}/${machine}" \
        --arg iso "${iso}" \
        --arg nic "${TRUENAS_NIC}" \
        --arg mac "${mac}" \
        --arg vnc_password "${vnc_password}" \
        --arg vnc_bind "${TRUENAS_VNC_BIND}" \
        '{
            name: $name,
            description: $description,
            vcpus: 1,
            cores: $cores,
            threads: 2,
            memory: $memory,
            bootloader: "UEFI",
            time: "UTC",
            autostart: true,
            shutdown_timeout: 90,
            devices: [
                {dtype: "DISK", order: 1000, attributes: {path: $disk, type: "VIRTIO"}},
                {dtype: "CDROM", order: 1001, attributes: {path: $iso}},
                {dtype: "NIC", order: 1002, attributes: {type: "VIRTIO", nic_attach: $nic, mac: $mac}},
                {dtype: "VNC", order: 1003, attributes: {
                    wait: false,
                    vnc_bind: $vnc_bind,
                    vnc_web: true,
                    vnc_resolution: "1024x768",
                    vnc_password: $vnc_password
                }}
            ]
        }'
}

# truenas_zvol_exists MACHINE
truenas_zvol_exists() {
    truenas_api GET "/pool/dataset/id/$(truenas_uri "${TRUENAS_POOL}/$1")" >/dev/null 2>&1
}

# truenas_create_vm MACHINE CPUS RAM_GIB DISK_GIB MAC ISO_PATH VNC_PASSWORD
# Make the zvol, then the VM. Print the VM id.
truenas_create_vm() {
    local machine="$1" cpus="$2" ram="$3" disk="$4" mac="$5" iso="$6" vnc_password="$7"
    local zvol="${TRUENAS_POOL}/${machine}"
    local vm

    if truenas_zvol_exists "${machine}"; then
        echo "The zvol ${zvol} exists already. The installer will not change it." >&2
        return 1
    fi

    truenas_api POST /pool/dataset "$(truenas_zvol_payload "${machine}" "${disk}")" >/dev/null || return 1

    if ! vm=$(truenas_api POST /vm "$(truenas_vm_payload "${machine}" "${cpus}" "${ram}" "${mac}" "${iso}" "${vnc_password}")"); then
        echo "Created zvol ${zvol}, but the VM create failed." >&2
        echo "To remove it: Storage > Pools > ${TRUENAS_POOL} > ${machine} > Delete zvol, in the TrueNAS web UI." >&2
        return 1
    fi
    jq -r '.id' <<<"${vm}"
}

# Wait for a TrueNAS job to finish. Return 1 if it fails, if the job is
# unknown, or after TRUENAS_JOB_TIMEOUT seconds.
truenas_wait_job() {
    local id="$1" jobs state waited=0
    local timeout="${TRUENAS_JOB_TIMEOUT:-600}" interval="${TRUENAS_JOB_INTERVAL:-2}"
    while true; do
        jobs=$(truenas_api GET "/core/get_jobs?id=${id}") || return 1
        state=$(jq -r '.[0].state // empty' <<<"${jobs}")
        case "${state}" in
            SUCCESS) return 0 ;;
            "")
                echo "TrueNAS job ${id} is unknown." >&2
                return 1
                ;;
            FAILED | ABORTED)
                echo "TrueNAS job ${id} ended with state ${state}." >&2
                return 1
                ;;
        esac
        if [ "${waited}" -ge "${timeout}" ]; then
            echo "TrueNAS job ${id} did not finish in ${timeout} seconds." >&2
            return 1
        fi
        sleep "${interval}"
        waited=$((waited + interval))
    done
}

# truenas_remote_size PATH
# Print the size in bytes of a file on the NAS. Print nothing if it is absent.
truenas_remote_size() {
    truenas_api POST /filesystem/stat "$(jq -n --arg p "$1" '$p')" 2>/dev/null | jq -r '.size // empty'
}

# truenas_ensure_iso LOCAL_ISO_PATH
# Copy the installer ISO to the NAS if it is not there. Print the path
# on the NAS. The name holds the Nix store hash, so a new ISO build gets
# a new name.
truenas_ensure_iso() {
    local local_iso="$1" hash remote response status job size remote_size

    if [[ ! "${local_iso}" =~ /([0-9a-z]{32})-[^/]*/ ]]; then
        echo "Cannot find a Nix store hash in ${local_iso}." >&2
        return 1
    fi
    hash="${BASH_REMATCH[1]}"
    remote="${TRUENAS_ISO_DIR}/nixos-installer-${hash}.iso"

    # The directory can exist already. A real error shows on the upload.
    truenas_api POST /filesystem/mkdir "$(jq -n --arg p "${TRUENAS_ISO_DIR}" '$p')" >/dev/null 2>&1 || true

    size=$(stat -c %s "${local_iso}") || return 1
    # A short file from an earlier failed upload has the same name, so
    # compare the size too.
    if [ "$(truenas_remote_size "${remote}")" = "${size}" ]; then
        echo "The installer ISO is on the NAS already." >&2
        echo "${remote}"
        return 0
    fi

    echo "Copying the installer ISO to the NAS..." >&2
    response=$(truenas_curl_config | curl -sk -K - -w '\n%{http_code}' \
        -F "data=$(jq -cn --arg p "${remote}" '{method: "filesystem.put", params: [$p, {mode: 420}]}')" \
        -F "file=@${local_iso}" \
        "https://${TRUENAS_HOST}/_upload") || {
        echo "The ISO upload failed: curl could not connect." >&2
        return 1
    }
    status="${response##*$'\n'}"
    response="${response%$'\n'*}"
    if [[ ! "${status}" =~ ^2[0-9][0-9]$ ]]; then
        echo "The ISO upload failed: HTTP ${status}" >&2
        echo "${response}" >&2
        return 1
    fi

    job=$(jq -r '.job_id' <<<"${response}")
    truenas_wait_job "${job}" || return 1
    remote_size=$(truenas_remote_size "${remote}")
    if [ "${remote_size}" != "${size}" ]; then
        echo "The ISO on the NAS has ${remote_size:-no} bytes, but the local ISO has ${size}." >&2
        return 1
    fi
    echo "${remote}"
}

truenas_start_vm() {
    truenas_api POST "/vm/id/$1/start" '{"overcommit": false}' >/dev/null
}

truenas_vm_state() {
    local vm
    vm=$(truenas_api GET "/vm/id/$1") || return 1
    jq -r '.status.state' <<<"${vm}"
}

# Remove every CD-ROM device from the VM.
truenas_detach_iso() {
    local vm device
    vm=$(truenas_api GET "/vm/id/$1") || return 1
    for device in $(jq -r '.devices[] | select(.dtype == "CDROM") | .id' <<<"${vm}"); do
        truenas_api DELETE "/vm/device/id/${device}" >/dev/null || return 1
    done
}
