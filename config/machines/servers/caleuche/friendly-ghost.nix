{ config, ... }:

{
  # friendly-ghost runs on this host because Hermes reads findings.json from
  # the local disk. It sends no mail and uses no LLM, so it needs no secrets.
  services.friendly-ghost = {
    enable = true;
    source = "loki";

    loki = {
      url = "http://accompaniment-of-shadows:3101";
      auth = "none";
      query = ''{service_name!=""}'';
    };

    filter = {
      units = [ ''.*\.service'' ];
      priority = "warning";
      ignorePatterns = [
        # alloy and loki are the monitoring substrate itself; their own warnings
        # about pushing/querying logs would create a feedback loop.
        "alloy"
        "loki"
        # HA bluetooth subsystem runs a permission check every restart even
        # though no integration uses it; the warning is benign.
        "Missing required permissions for Bluetooth"
        # HA emits this for every custom integration on every startup.
        "custom integration .* which has not been tested by Home Assistant"
        # irrigation_unlimited compares switches against Zigbee devices that
        # report "unknown" briefly during HA boot before re-reporting state.
        "irrigation_unlimited.*SYNCHRONISATION.*found: unknown"
        # Some container's locale init writes to stderr, which Docker's
        # journald driver mislabels as priority=err regardless of content.
        "^Language set to .+$"
      ];
    };

    structured = {
      enable = true;
      readerGroup = config.services.hermes-agent.group;
    };
  };
}
