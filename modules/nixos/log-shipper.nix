{ config, lib, ... }:

let
  cfg = config.services.log-shipper;
in
{
  options.services.log-shipper = {
    enable = lib.mkEnableOption "shipping the systemd journal to Loki via Grafana Alloy";

    lokiUrl = lib.mkOption {
      type = lib.types.str;
      example = "http://192.168.5.105:3101";
      description = "Loki base URL. The /loki/api/v1/push suffix is appended automatically.";
    };

    hostLabel = lib.mkOption {
      type = lib.types.str;
      default = config.networking.hostName;
      defaultText = lib.literalExpression "config.networking.hostName";
      description = "Value of the `host` label attached to every log entry.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.alloy.enable = true;

    environment.etc."alloy/config.alloy".text = ''
      loki.relabel "journal" {
        forward_to = []
        rule {
          source_labels = ["__journal__systemd_unit"]
          target_label  = "service_name"
        }
        rule {
          source_labels = ["__journal_priority_keyword"]
          target_label  = "level"
        }
        rule {
          source_labels = ["__journal_container_name"]
          target_label  = "container_name"
        }
      }

      loki.source.journal "system" {
        max_age       = "12h"
        labels        = { host = "${cfg.hostLabel}" }
        relabel_rules = loki.relabel.journal.rules
        forward_to    = [loki.process.container_level.receiver]
      }

      // Docker's journald driver maps stderr -> priority=err, which mislabels
      // any container that logs everything to stderr (Caddy, most Go apps, etc).
      // Re-derive `level` from the message body for entries with a container_name.
      loki.process "container_level" {
        forward_to = [loki.write.default.receiver]

        stage.match {
          selector = "{container_name=~\".+\"}"

          // Structured logs: pull "level" from JSON.
          stage.json {
            expressions = { extracted_level = "level" }
          }

          // Plain-text fallback. No leading/trailing anchors -- log lines often
          // wrap level tokens with ANSI escape codes (zerolog/Caddy), and \b on
          // a letter-letter boundary won't match between an ANSI 'm' and a level
          // letter. Both uppercase (zerolog/Caddy) and lowercase (most loggers)
          // forms are listed explicitly.
          stage.regex {
            expression = "(?P<extracted_level>TRC|DBG|INF|WRN|ERR|FTL|PNC|TRACE|DEBUG|INFO|WARNING|WARN|ERROR|FATAL|NOTICE|trace|debug|info|warning|warn|error|fatal|notice)"
          }

          // Normalize abbreviations + casing; default to "unknown" if neither parser hit.
          stage.template {
            source   = "extracted_level"
            template = "{{- ''$l := ToLower .Value -}}{{- if eq ''$l \"inf\" -}}info{{- else if eq ''$l \"wrn\" -}}warn{{- else if eq ''$l \"warning\" -}}warn{{- else if eq ''$l \"err\" -}}error{{- else if eq ''$l \"dbg\" -}}debug{{- else if ''$l -}}{{ ''$l }}{{- else -}}unknown{{- end -}}"
          }

          stage.labels {
            values = { level = "extracted_level" }
          }
        }
      }

      loki.write "default" {
        endpoint {
          url = "${cfg.lokiUrl}/loki/api/v1/push"
        }
      }
    '';
  };
}
