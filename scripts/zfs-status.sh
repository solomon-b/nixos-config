#!/usr/bin/env bash

# ZFS Dataset Status
# Monitors ZFS dataset synchronization between local machine and sandra-voi

set -euo pipefail

# Initialize from environment variables (can be overridden by command line args)
VERBOSE=${ZFS_STATUS_VERBOSE:-false}
SELECTED_POOLS=${ZFS_STATUS_POOLS:-""}
JSON_OUTPUT=${ZFS_STATUS_JSON:-false}
DESTINATION_HOST=${ZFS_STATUS_DEST_HOST:-""}
DESTINATION_USER=${ZFS_STATUS_DEST_USER:-""}
DESTINATION_BASE=${ZFS_STATUS_DEST_BASE:-""}

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
        --dest-host)
            DESTINATION_HOST="$2"
            shift 2
            ;;
        --dest-user)
            DESTINATION_USER="$2"
            shift 2
            ;;
        --dest-base)
            DESTINATION_BASE="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  -v, --verbose           Show detailed logs and output"
            echo "  -p, --pools POOLS       Comma-separated list of pools to scan (default: all pools)"
            echo "  -j, --json              Output in JSON format"
            echo "  --dest-host HOST        Remote host for syncoid sync checking"
            echo "  --dest-user USER        Remote user for syncoid sync checking"
            echo "  --dest-base PATH        Remote base path for syncoid datasets"
            echo "  -h, --help              Show this help message"
            echo ""
            echo "Environment Variables (command line args take precedence):"
            echo "  ZFS_STATUS_VERBOSE      Set to 'true', '1', or 'yes' to enable verbose mode"
            echo "  ZFS_STATUS_POOLS        Comma-separated list of pools to scan"
            echo "  ZFS_STATUS_JSON         Set to 'true', '1', or 'yes' to enable JSON output"
            echo "  ZFS_STATUS_DEST_HOST    Remote host for syncoid sync checking"
            echo "  ZFS_STATUS_DEST_USER    Remote user for syncoid sync checking"
            echo "  ZFS_STATUS_DEST_BASE    Remote base path for syncoid datasets"
            echo ""
            echo "Examples:"
            echo "  $0                                              # Local ZFS status only"
            echo "  $0 -p tank                                      # Scan only 'tank' pool"
            echo "  $0 -j                                           # JSON output, local only"
            echo "  $0 --dest-host backup.example.com --dest-user root --dest-base tank/backups"
            echo "                                                  # Include remote sync checking"
            echo "  ZFS_STATUS_JSON=true ZFS_STATUS_DEST_HOST=sandra-voi.home.arpa $0"
            echo "                                                  # Using environment variables"
            echo "  $0 -p tank,backup -j --dest-host sandra-voi.home.arpa --dest-user solomon --dest-base tank/system-snapshots"
            echo "                                                  # Full featured scan with JSON"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use -h or --help for usage information"
            exit 1
            ;;
    esac
done

# Determine if remote checking should be enabled
REMOTE_CHECKING=false
if [[ -n "$DESTINATION_HOST" && -n "$DESTINATION_USER" && -n "$DESTINATION_BASE" ]]; then
    REMOTE_CHECKING=true
elif [[ -n "$DESTINATION_HOST" || -n "$DESTINATION_USER" || -n "$DESTINATION_BASE" ]]; then
    echo "Error: If using remote checking, all three options must be provided: --dest-host, --dest-user, --dest-base" >&2
    exit 1
fi

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

# Establish SSH master connection for reuse (only if remote checking is enabled)
if [[ "$REMOTE_CHECKING" == true ]]; then
    ssh -o ConnectTimeout=5 -o BatchMode=yes -o ControlMaster=yes -o ControlPath="$SSH_CONTROL_PATH" -o ControlPersist=10s "$DESTINATION_USER@$DESTINATION_HOST" "true" 2>/dev/null &
fi

# Discover datasets from all pools
DATASETS=""
for pool in $ZPOOLS; do
    pool_datasets=$(zfs list -H -o name -t filesystem -r "$pool" 2>/dev/null | grep -v "^$pool$" || echo "")
    DATASETS="$DATASETS $pool_datasets"
done
DATASETS=$(echo "$DATASETS" | tr ' ' '\n' | grep -v '^$' | sort)

# Discover which datasets have syncoid services configured
SYNCOID_DATASETS=$(systemctl list-unit-files --type=service 2>/dev/null | grep -E "^syncoid-.*\.service" | sed 's/syncoid-//' | sed 's/\.service.*//' | tr '\n' ' ')

for dataset in $DATASETS; do
    dataset_name=$(basename "$dataset")
    remote_dataset="$DESTINATION_BASE/$HOSTNAME/$dataset_name"
    
    # Check if this dataset has a syncoid service configured
    # Convert dataset path to service name format (replace / with -)
    dataset_service_name=$(echo "$dataset" | tr '/' '-')
    has_syncoid_service=false
    for syncoid_dataset in $SYNCOID_DATASETS; do
        if [[ "$dataset_service_name" == "$syncoid_dataset" ]]; then
            has_syncoid_service=true
            break
        fi
    done
    
    # Check if local dataset exists
    if zfs list "$dataset" >/dev/null 2>&1; then
        local_snapshot_name=$(zfs list -t snapshot -H -o name "$dataset" 2>/dev/null | tail -1 || echo "none")
        
        if [[ "$local_snapshot_name" != "none" ]]; then
            # Get creation time for just this snapshot
            local_snapshot_time=$(zfs get creation -H -o value "$local_snapshot_name" 2>/dev/null || echo "unknown")

            # Only check remote status if this dataset has syncoid configured AND remote checking is enabled
            remote_latest=""
            remote_snap_time=""
            if [[ "$has_syncoid_service" == true && "$REMOTE_CHECKING" == true ]]; then
                # Get remote snapshot info with single SSH call using master connection
                remote_script="
                    if zfs list '$remote_dataset' >/dev/null 2>&1; then
                        latest=\$(zfs list -t snapshot -H -o name '$remote_dataset' 2>/dev/null | tail -1)
                        if [ -n \"\$latest\" ]; then
                            time=\$(zfs get creation \"\$latest\" 2>/dev/null | tail -1 | awk '{print \$3, \$4, \$5, \$6, \$7}')
                            echo \"\$latest|\$time\"
                        fi
                    fi
                "
                remote_info=$(ssh -o ControlPath="$SSH_CONTROL_PATH" "$DESTINATION_USER@$DESTINATION_HOST" "$remote_script" 2>/dev/null || echo "")

                if [[ -n "$remote_info" ]]; then
                    remote_latest=$(echo "$remote_info" | cut -d'|' -f1)
                    remote_snap_time=$(echo "$remote_info" | cut -d'|' -f2)
                fi
            fi
            
            # Determine sync status and display
            local_snap_name=$(basename "$local_snapshot_name")
            remote_snap_name=$(basename "$remote_latest")

            # Get dataset health & usage info with single ZFS call
            dataset_props=$(zfs get used,available,compressratio -H -o property,value "$dataset" 2>/dev/null)
            used=$(echo "$dataset_props" | grep "^used" | awk '{print $2}' || echo "unknown")
            available=$(echo "$dataset_props" | grep "^available" | awk '{print $2}' || echo "unknown")
            compressratio=$(echo "$dataset_props" | grep "^compressratio" | awk '{print $2}' | sed 's/x$//' || echo "unknown")

            # Get performance & health info
            pool_name=$(echo "$dataset" | cut -d'/' -f1)
            errors_line=$(zpool status "$pool_name" 2>/dev/null | grep -E "^\s*$pool_name\s+" | head -1)
            read_errors=$(echo "$errors_line" | awk '{print $3}' | tr -d '\n' || echo "0")
            write_errors=$(echo "$errors_line" | awk '{print $4}' | tr -d '\n' || echo "0")

            # Calculate sync status
            sync_status="not_configured"
            lag_hours=0
            lag_minutes=0
            if [[ "$has_syncoid_service" == true ]]; then
                if [[ "$REMOTE_CHECKING" == true ]]; then
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
                    sync_status="remote_not_checked"
                fi
            fi

            if [[ "$JSON_OUTPUT" == true ]]; then
                # Collect JSON data
                dataset_json="{\"name\":\"$(json_escape "$dataset")\",\"used\":\"$(json_escape "$used")\",\"available\":\"$(json_escape "$available")\",\"compression_ratio\":\"$(json_escape "$compressratio")\",\"read_errors\":\"$read_errors\",\"write_errors\":\"$write_errors\",\"local_latest_snapshot\":\"$(json_escape "$local_snapshot_name")\",\"local_snapshot_time\":\"$(json_escape "$local_snapshot_time")\",\"has_syncoid_service\":$([[ "$has_syncoid_service" == true ]] && echo "true" || echo "false"),\"sync_status\":\"$sync_status\""
                
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

                print_info "Local latest:  $local_snapshot_name ($local_snapshot_time)"
                
                # Show remote status based on configuration
                if [[ "$has_syncoid_service" == true ]]; then
                    if [[ "$REMOTE_CHECKING" == true ]]; then
                        if [[ -n "$remote_latest" ]]; then
                            print_info "Remote latest: $remote_latest ($remote_snap_time)"

                            if [[ "$local_snap_name" == "$remote_snap_name" ]]; then
                                gum style --foreground 46 "  ✓ In sync"
                            else
                                gum style --foreground 214 "  ⚠ Out of sync"

                                if [[ $lag_hours -gt 0 || $lag_minutes -gt 0 ]]; then
                                    print_info "Lag: ${lag_hours}h ${lag_minutes}m behind"
                                fi
                            fi
                        else
                            print_info "Remote latest: Cannot connect or no snapshots found"
                        fi
                    else
                        print_info "Remote sync: Configured but not checked (use --dest-* flags)"
                    fi
                else
                    print_info "Remote sync: Not configured"
                fi
            fi
        else
            if [[ "$JSON_OUTPUT" == true ]]; then
                # Dataset with no snapshots
                dataset_json="{\"name\":\"$(json_escape "$dataset")\",\"status\":\"no_snapshots\",\"has_syncoid_service\":$([[ "$has_syncoid_service" == true ]] && echo "true" || echo "false")}"
                JSON_DATASETS+=("$dataset_json")
            else
                gum style --foreground 214 "💾 $dataset (no local snapshots found)"
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

# Clean up SSH master connection (only if remote checking was enabled)
if [[ "$REMOTE_CHECKING" == true ]]; then
    ssh -o ControlPath="$SSH_CONTROL_PATH" -O exit "$DESTINATION_USER@$DESTINATION_HOST" 2>/dev/null || true
fi

# Output JSON if requested
if [[ "$JSON_OUTPUT" == true ]]; then
    echo "{"
    echo "  \"hostname\": \"$(json_escape "$HOSTNAME")\","
    echo "  \"timestamp\": \"$(date -Iseconds)\","
    echo "  \"remote_checking_enabled\": $([[ "$REMOTE_CHECKING" == true ]] && echo "true" || echo "false"),"
    if [[ "$REMOTE_CHECKING" == true ]]; then
        echo "  \"remote_destination\": {"
        echo "    \"host\": \"$(json_escape "$DESTINATION_HOST")\","
        echo "    \"user\": \"$(json_escape "$DESTINATION_USER")\","
        echo "    \"base_path\": \"$(json_escape "$DESTINATION_BASE")\""
        echo "  },"
    fi
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
