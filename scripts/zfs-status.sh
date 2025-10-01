#!/usr/bin/env bash

# ZFS Dataset Status
# Monitors ZFS pools, datasets, snapshots, and syncoid synchronization status

set -euo pipefail

# Initialize from environment variables (can be overridden by command line args)
VERBOSE=${ZFS_STATUS_VERBOSE:-false}
SELECTED_POOLS=${ZFS_STATUS_POOLS:-""}
JSON_OUTPUT=${ZFS_STATUS_JSON:-false}

# Convert string env vars to boolean
[[ "$VERBOSE" =~ ^(true|1|yes)$ ]] && VERBOSE=true || VERBOSE=false
[[ "$JSON_OUTPUT" =~ ^(true|1|yes)$ ]] && JSON_OUTPUT=true || JSON_OUTPUT=false

# Parse command line arguments (override env vars)
while [[ $# -gt 0 ]]; do
    case $1 in
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -p|--pools)
            SELECTED_POOLS="$2"
            shift 2
            ;;
        -j|--json)
            JSON_OUTPUT=true
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  -v, --verbose           Show detailed logs and output"
            echo "  -p, --pools POOLS       Comma-separated list of pools to scan (default: all pools)"
            echo "  -j, --json              Output in JSON format"
            echo "  -h, --help              Show this help message"
            echo ""
            echo "Environment Variables (command line args take precedence):"
            echo "  ZFS_STATUS_VERBOSE      Set to 'true', '1', or 'yes' to enable verbose mode"
            echo "  ZFS_STATUS_POOLS        Comma-separated list of pools to scan"
            echo "  ZFS_STATUS_JSON         Set to 'true', '1', or 'yes' to enable JSON output"
            echo ""
            echo "Examples:"
            echo "  $0                                              # Show ZFS status with syncoid info"
            echo "  $0 -p tank                                      # Scan only 'tank' pool"
            echo "  $0 -j                                           # JSON output"
            echo "  $0 -v                                           # Verbose output with scrub status"
            echo "  ZFS_STATUS_JSON=true $0                         # Using environment variables"
            echo ""
            echo "Note: Remote syncoid sync status is automatically detected from syncoid service configurations."
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use -h or --help for usage information"
            exit 1
            ;;
    esac
done

# Configuration - will discover all pools automatically

# Get hostname for destination path
HOSTNAME=$(hostname)

# Create unique SSH control socket path
SSH_CONTROL_PATH="/tmp/syncoid-ssh-$$-$(date +%s)"

# Display functions
print_header() {
    if [[ "$JSON_OUTPUT" != true ]]; then
        gum style --foreground 212 --bold "ZFS Status - $HOSTNAME"
    fi
}

print_section() {
    local title="$1"
    if [[ "$JSON_OUTPUT" != true ]]; then
        gum style --foreground 75 --bold "$title"
    fi
}

print_info() {
    local message="$1"
    if [[ "$JSON_OUTPUT" != true ]]; then
        gum style --foreground 159 "  $message"
    fi
}

print_subinfo() {
    local message="$1"
    if [[ "$JSON_OUTPUT" != true ]]; then
        gum style --foreground 159 "    $message"
    fi
}

print_subheading() {
    local title="$1"
    if [[ "$JSON_OUTPUT" != true ]]; then
        gum style --foreground 117 --bold "  $title"
    fi
}

print_status_summary() {
    local type="$1"
    local message="$2"
    
    if [[ "$JSON_OUTPUT" != true ]]; then
        case $type in
            "success")
                gum style --foreground 46 --bold "$message"
                ;;
            "error")
                gum style --foreground 196 --bold "$message"
                ;;
            "warning")
                gum style --foreground 214 --bold "$message"
                ;;
        esac
    fi
}

# JSON helper functions
json_escape() {
    local input="$1"
    # Escape quotes and backslashes for JSON
    echo "$input" | sed 's/\\/\\\\/g' | sed 's/"/\\"/g'
}

# Parse syncoid service configuration
# Returns: remote_user|remote_host|remote_dataset
parse_syncoid_service() {
    local service_name="$1"

    # Get the ExecStart line from the service
    local exec_start
    exec_start=$(systemctl show "$service_name" --property=ExecStart --value 2>/dev/null || echo "")

    if [[ -z "$exec_start" ]]; then
        return 1
    fi

    # Extract the syncoid command arguments from argv[]
    # Format: argv[]=/path/to/syncoid [options] source user@host:destination
    # The destination pattern is: user@host:path (not at end of line due to metadata)
    local destination
    destination=$(echo "$exec_start" | grep -oE '[^[:space:]]+@[^[:space:]]+:[^[:space:];]+' || echo "")

    if [[ -z "$destination" ]]; then
        return 1
    fi

    # Parse destination: user@host:path
    local remote_user
    local host_and_path
    local remote_host
    local remote_dataset
    remote_user=$(echo "$destination" | cut -d'@' -f1)
    host_and_path=$(echo "$destination" | cut -d'@' -f2)
    remote_host=$(echo "$host_and_path" | cut -d':' -f1)
    remote_dataset=$(echo "$host_and_path" | cut -d':' -f2-)

    echo "$remote_user|$remote_host|$remote_dataset"
    return 0
}

# JSON data collectors
declare -a JSON_POOLS
declare -a JSON_DATASETS

print_header

# Check ZFS pools status
print_section "ZFS Pools"

# Discover zpools (either selected ones or all)
if [[ -n "$SELECTED_POOLS" ]]; then
    # Convert comma-separated list to space-separated and validate pools exist
    ZPOOLS=""
    IFS=',' read -ra POOL_ARRAY <<< "$SELECTED_POOLS"
    for pool in "${POOL_ARRAY[@]}"; do
        pool=$(echo "$pool" | xargs)  # trim whitespace
        if zpool list "$pool" >/dev/null 2>&1; then
            ZPOOLS="$ZPOOLS $pool"
        else
            echo "Warning: Pool '$pool' not found, skipping..." >&2
        fi
    done
    ZPOOLS=$(echo "$ZPOOLS" | xargs)  # trim leading/trailing spaces
else
    # Discover all zpools
    ZPOOLS=$(zpool list -H -o name 2>/dev/null || echo "")
fi

for pool in $ZPOOLS; do
    if [[ -n "$pool" ]]; then
        # Get pool status information
        pool_status=$(zpool status "$pool" 2>/dev/null)
        health=$(echo "$pool_status" | grep "state:" | awk '{print $2}' || echo "unknown")
        
        # Get pool properties
        pool_props=$(zpool get size,allocated,free,capacity,health -H -o property,value "$pool" 2>/dev/null)
        size=$(echo "$pool_props" | grep "^size" | awk '{print $2}' || echo "unknown")
        allocated=$(echo "$pool_props" | grep "^allocated" | awk '{print $2}' || echo "unknown")
        free=$(echo "$pool_props" | grep "^free" | awk '{print $2}' || echo "unknown")
        capacity=$(echo "$pool_props" | grep "^capacity" | awk '{print $2}' || echo "unknown")
        
        # Check for errors
        errors_info=$(echo "$pool_status" | grep -E "^\s*$pool\s+" | head -1)
        read_errors="0"
        write_errors="0"
        cksum_errors="0"
        if [[ -n "$errors_info" ]]; then
            read_errors=$(echo "$errors_info" | awk '{print $3}' | tr -d '\n' || echo "0")
            write_errors=$(echo "$errors_info" | awk '{print $4}' | tr -d '\n' || echo "0")
            cksum_errors=$(echo "$errors_info" | awk '{print $5}' | tr -d '\n' || echo "0")
        fi
        
        # Get scrub status if verbose
        scrub_status=""
        if [[ $VERBOSE == true ]]; then
            scrub_status=$(echo "$pool_status" | grep -A1 "scan:" | tail -1 | sed 's/^[[:space:]]*//' || echo "")
        fi
        
        if [[ "$JSON_OUTPUT" == true ]]; then
            # Collect JSON data
            pool_json="{\"name\":\"$(json_escape "$pool")\",\"health\":\"$(json_escape "$health")\",\"size\":\"$(json_escape "$size")\",\"allocated\":\"$(json_escape "$allocated")\",\"free\":\"$(json_escape "$free")\",\"capacity\":\"$(json_escape "$capacity")\",\"read_errors\":\"$read_errors\",\"write_errors\":\"$write_errors\",\"checksum_errors\":\"$cksum_errors\""
            if [[ $VERBOSE == true && -n "$scrub_status" ]]; then
                pool_json="${pool_json},\"scrub_status\":\"$(json_escape "$scrub_status")\""
            fi
            pool_json="${pool_json}}"
            JSON_POOLS+=("$pool_json")
        else
            # Terminal output
            case $health in
                "ONLINE")
                    gum style --foreground 46 "🏊 $pool"
                    ;;
                "DEGRADED")
                    gum style --foreground 214 "⚠️  $pool"
                    ;;
                "FAULTED"|"OFFLINE"|"UNAVAIL")
                    gum style --foreground 196 "❌ $pool"
                    ;;
                *)
                    gum style --foreground 159 "🏊 $pool"
                    ;;
            esac
            
            print_info "Health: $health | Size: $size | Used: $allocated | Free: $free | Capacity: $capacity"
            
            if [[ "$read_errors" != "0" || "$write_errors" != "0" || "$cksum_errors" != "0" ]]; then
                print_info "Errors: ${read_errors} read, ${write_errors} write, ${cksum_errors} checksum"
            fi
            
            if [[ $VERBOSE == true && -n "$scrub_status" ]]; then
                print_info "Scrub: $scrub_status"
            fi
            
            echo ""
        fi
    fi
done

# Check datasets and their sync status
print_section "ZFS Datasets"

# Discover datasets from all pools
DATASETS=""
for pool in $ZPOOLS; do
    pool_datasets=$(zfs list -H -o name -t filesystem -r "$pool" 2>/dev/null | grep -v "^$pool$" || echo "")
    DATASETS="$DATASETS $pool_datasets"
done
DATASETS=$(echo "$DATASETS" | tr ' ' '\n' | grep -v '^$' | sort)

# Discover which datasets have syncoid services configured (handle case where none exist)
SYNCOID_DATASETS=$(systemctl list-unit-files --type=service 2>/dev/null | grep -E "^syncoid-.*\.service" | sed 's/syncoid-//' | sed 's/\.service.*//' | tr '\n' ' ' || true)

for dataset in $DATASETS; do
    # Check if this dataset has a syncoid service configured
    # Convert dataset path to service name format (replace / with -)
    dataset_service_name=$(echo "$dataset" | tr '/' '-')
    has_syncoid_service=false
    remote_user=""
    remote_host=""
    remote_dataset=""

    for syncoid_dataset in $SYNCOID_DATASETS; do
        if [[ "$dataset_service_name" == "$syncoid_dataset" ]]; then
            has_syncoid_service=true
            # Parse the syncoid service to get actual remote configuration
            service_config=$(parse_syncoid_service "syncoid-${syncoid_dataset}.service")
            if [[ $? -eq 0 && -n "$service_config" ]]; then
                remote_user=$(echo "$service_config" | cut -d'|' -f1)
                remote_host=$(echo "$service_config" | cut -d'|' -f2)
                remote_dataset=$(echo "$service_config" | cut -d'|' -f3)
            fi
            break
        fi
    done

    # Check if local dataset exists
    if zfs list "$dataset" >/dev/null 2>&1; then
        local_snapshot_name=$(zfs list -t snapshot -H -o name "$dataset" 2>/dev/null | tail -1)
        [[ -z "$local_snapshot_name" ]] && local_snapshot_name="none"
        
        # Get dataset health & usage info with single ZFS call (needed for both snapshot and no-snapshot cases)
        dataset_props=$(zfs get used,available,compressratio -H -o property,value "$dataset" 2>/dev/null)
        used=$(echo "$dataset_props" | grep "^used" | awk '{print $2}' || echo "unknown")
        available=$(echo "$dataset_props" | grep "^available" | awk '{print $2}' || echo "unknown")
        compressratio=$(echo "$dataset_props" | grep "^compressratio" | awk '{print $2}' | sed 's/x$//' || echo "unknown")

        # Get performance & health info (needed for both snapshot and no-snapshot cases)
        pool_name=$(echo "$dataset" | cut -d'/' -f1)
        errors_line=$(zpool status "$pool_name" 2>/dev/null | grep -E "^\s*$pool_name\s+" | head -1)
        read_errors=$(echo "$errors_line" | awk '{print $3}' | tr -d '\n' || echo "0")
        write_errors=$(echo "$errors_line" | awk '{print $4}' | tr -d '\n' || echo "0")

        if [[ "$local_snapshot_name" != "none" ]]; then
            # Get creation time for just this snapshot
            local_snapshot_time=$(zfs get creation -H -o value "$local_snapshot_name" 2>/dev/null || echo "unknown")

            # Only check remote status if this dataset has syncoid configured and we have remote config
            remote_latest=""
            remote_snap_time=""
            if [[ "$has_syncoid_service" == true && -n "$remote_host" && -n "$remote_user" && -n "$remote_dataset" ]]; then
                # Get remote snapshot info with single SSH call
                remote_script="
                    if zfs list '$remote_dataset' >/dev/null 2>&1; then
                        latest=\$(zfs list -t snapshot -H -o name '$remote_dataset' 2>/dev/null | tail -1)
                        if [ -n \"\$latest\" ]; then
                            time=\$(zfs get creation \"\$latest\" 2>/dev/null | tail -1 | awk '{print \$3, \$4, \$5, \$6, \$7}')
                            echo \"\$latest|\$time\"
                        fi
                    fi
                "
                remote_info=$(ssh -o ConnectTimeout=5 -o BatchMode=yes -o ControlMaster=auto -o ControlPath="$SSH_CONTROL_PATH" -o ControlPersist=10s "$remote_user@$remote_host" "$remote_script" 2>/dev/null || echo "")

                if [[ -n "$remote_info" ]]; then
                    remote_latest=$(echo "$remote_info" | cut -d'|' -f1)
                    remote_snap_time=$(echo "$remote_info" | cut -d'|' -f2)
                fi
            fi

            # Determine sync status and display
            local_snap_name=$(basename "$local_snapshot_name")
            remote_snap_name=$(basename "$remote_latest")

            # Calculate sync status
            sync_status="not_configured"
            lag_hours=0
            lag_minutes=0
            if [[ "$has_syncoid_service" == true ]]; then
                if [[ -n "$remote_host" && -n "$remote_user" && -n "$remote_dataset" ]]; then
                    if [[ -n "$remote_latest" ]]; then
                        if [[ "$local_snap_name" == "$remote_snap_name" ]]; then
                            sync_status="in_sync"
                        else
                            sync_status="out_of_sync"
                            # Calculate lag
                            local_epoch=$(date -d "$local_snapshot_time" +%s 2>/dev/null || echo 0)
                            remote_epoch=$(date -d "$remote_snap_time" +%s 2>/dev/null || echo 0)
                            if [[ $local_epoch -gt 0 && $remote_epoch -gt 0 ]]; then
                                lag_seconds=$((local_epoch - remote_epoch))
                                if [[ $lag_seconds -gt 0 ]]; then
                                    lag_hours=$((lag_seconds / 3600))
                                    lag_minutes=$(((lag_seconds % 3600) / 60))
                                fi
                            fi
                        fi
                    else
                        sync_status="remote_unavailable"
                    fi
                else
                    sync_status="config_parse_error"
                fi
            fi

            if [[ "$JSON_OUTPUT" == true ]]; then
                # Collect JSON data
                if [[ "$local_snapshot_name" == "none" ]]; then
                    json_snapshot_name="null"
                    json_snapshot_time="null"
                else
                    json_snapshot_name="\"$(json_escape "$local_snapshot_name")\""
                    json_snapshot_time="\"$(json_escape "$local_snapshot_time")\""
                fi
                
                dataset_json="{\"name\":\"$(json_escape "$dataset")\",\"used\":\"$(json_escape "$used")\",\"available\":\"$(json_escape "$available")\",\"compression_ratio\":\"$(json_escape "$compressratio")\",\"read_errors\":\"$read_errors\",\"write_errors\":\"$write_errors\",\"local_latest_snapshot\":$json_snapshot_name,\"local_snapshot_time\":$json_snapshot_time,\"has_syncoid_service\":$([[ "$has_syncoid_service" == true ]] && echo "true" || echo "false"),\"sync_status\":\"$sync_status\""
                
                if [[ "$has_syncoid_service" == true && -n "$remote_latest" ]]; then
                    dataset_json="${dataset_json},\"remote_latest_snapshot\":\"$(json_escape "$remote_latest")\",\"remote_snapshot_time\":\"$(json_escape "$remote_snap_time")\""
                fi
                
                if [[ "$sync_status" == "out_of_sync" && ($lag_hours -gt 0 || $lag_minutes -gt 0) ]]; then
                    dataset_json="${dataset_json},\"lag_hours\":$lag_hours,\"lag_minutes\":$lag_minutes"
                fi
                
                dataset_json="${dataset_json}}"
                JSON_DATASETS+=("$dataset_json")
            else
                # Terminal output
                gum style --foreground 46 "💾 $dataset"

                # Display dataset info
                print_info "Used: $used | Available: $available | Compression: ${compressratio}x"
                if [[ "$read_errors" != "0" || "$write_errors" != "0" ]]; then
                    print_info "I/O errors: ${read_errors} read, ${write_errors} write"
                fi

                print_subheading "Snapshots"
                if [[ "$local_snapshot_name" != "none" ]]; then
                    print_subinfo "Local latest:  $local_snapshot_name ($local_snapshot_time)"
                else
                    print_subinfo "Local latest:  None"
                fi
                
                # Show remote status based on configuration
                if [[ "$has_syncoid_service" == true ]]; then
                    if [[ -n "$remote_host" && -n "$remote_user" && -n "$remote_dataset" ]]; then
                        if [[ -n "$remote_latest" ]]; then
                            print_subinfo "Remote latest: $remote_latest ($remote_snap_time)"
                            print_subinfo "Remote: $remote_user@$remote_host:$remote_dataset"

                            if [[ "$local_snap_name" == "$remote_snap_name" ]]; then
                                gum style --foreground 46 "      ✓ In sync"
                            else
                                gum style --foreground 214 "      ⚠ Out of sync"

                                if [[ $lag_hours -gt 0 || $lag_minutes -gt 0 ]]; then
                                    print_subinfo "Lag: ${lag_hours}h ${lag_minutes}m behind"
                                fi
                            fi
                        else
                            print_subinfo "Remote: $remote_user@$remote_host:$remote_dataset"
                            print_subinfo "Remote latest: Cannot connect or no snapshots found"
                        fi
                    else
                        print_subinfo "Remote sync: Configured but unable to parse service"
                    fi
                else
                    print_subinfo "Remote sync: Not configured"
                fi
            fi
        else
            # Dataset with no snapshots - still show the structure
            if [[ "$JSON_OUTPUT" == true ]]; then
                # Collect JSON data for dataset with no snapshots
                json_snapshot_name="null"
                json_snapshot_time="null"
                
                dataset_json="{\"name\":\"$(json_escape "$dataset")\",\"used\":\"$(json_escape "$used")\",\"available\":\"$(json_escape "$available")\",\"compression_ratio\":\"$(json_escape "$compressratio")\",\"read_errors\":\"$read_errors\",\"write_errors\":\"$write_errors\",\"local_latest_snapshot\":$json_snapshot_name,\"local_snapshot_time\":$json_snapshot_time,\"has_syncoid_service\":$([[ "$has_syncoid_service" == true ]] && echo "true" || echo "false"),\"sync_status\":\"not_configured\"}"
                JSON_DATASETS+=("$dataset_json")
            else
                # Terminal output for dataset with no snapshots
                gum style --foreground 46 "💾 $dataset"

                # Display dataset info
                print_info "Used: $used | Available: $available | Compression: ${compressratio}x"
                if [[ "$read_errors" != "0" || "$write_errors" != "0" ]]; then
                    print_info "I/O errors: ${read_errors} read, ${write_errors} write"
                fi

                print_subheading "Snapshots"
                print_subinfo "Local latest:  None"

                if [[ "$has_syncoid_service" == true ]]; then
                    if [[ -n "$remote_host" && -n "$remote_user" && -n "$remote_dataset" ]]; then
                        print_subinfo "Remote: $remote_user@$remote_host:$remote_dataset"
                        print_subinfo "Remote sync: Configured (no snapshots to check)"
                    else
                        print_subinfo "Remote sync: Configured but unable to parse service"
                    fi
                else
                    print_subinfo "Remote sync: Not configured"
                fi
            fi
        fi
    else
        if [[ "$JSON_OUTPUT" == true ]]; then
            # Dataset not found
            dataset_json="{\"name\":\"$(json_escape "$dataset")\",\"status\":\"not_found\"}"
            JSON_DATASETS+=("$dataset_json")
        else
            gum style --foreground 196 "💾 $dataset (not found)"
        fi
    fi
    
    if [[ "$JSON_OUTPUT" != true ]]; then
        echo ""
    fi
done

# Output JSON if requested
if [[ "$JSON_OUTPUT" == true ]]; then
    echo "{"
    echo "  \"hostname\": \"$(json_escape "$HOSTNAME")\","
    echo "  \"timestamp\": \"$(date -Iseconds)\","
    echo "  \"pools\": ["
    
    # Output pools
    first_pool=true
    for pool_json in "${JSON_POOLS[@]}"; do
        if [[ "$first_pool" == true ]]; then
            first_pool=false
        else
            echo ","
        fi
        echo "    $pool_json"
    done
    
    echo "  ],"
    echo "  \"datasets\": ["
    
    # Output datasets
    first_dataset=true
    for dataset_json in "${JSON_DATASETS[@]}"; do
        if [[ "$first_dataset" == true ]]; then
            first_dataset=false
        else
            echo ","
        fi
        echo "    $dataset_json"
    done
    
    echo "  ]"
    echo "}"
fi
