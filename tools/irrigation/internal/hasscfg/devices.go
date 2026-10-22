package hasscfg

import (
	"context"
	"fmt"
	"os/exec"
	"strings"

	"gopkg.in/yaml.v3"
)

// DeviceSource locates zigbee2mqtt's devices.yaml, which maps each radio
// address to the friendly name it was given.
type DeviceSource struct {
	Host string
	Path string
}

// LoadAliases returns slug -> IEEE address.
//
// Home Assistant fixes an entity_id at discovery and never rewrites it when the
// zigbee2mqtt friendly name changes later. Valves named after discovery
// therefore keep address-based entity ids for their older attributes while
// newer ones use the slug. Both forms address one physical valve, so the tool
// has to look under each.
func (d DeviceSource) LoadAliases(ctx context.Context) (map[string]string, error) {
	cmd := exec.CommandContext(ctx, "ssh",
		"-o", "BatchMode=yes", "-o", "ConnectTimeout=5",
		d.Host, "cat", d.Path)
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("ssh %s cat %s: %w", d.Host, d.Path, err)
	}
	return ParseAliases(string(out))
}

func ParseAliases(src string) (map[string]string, error) {
	var raw map[string]struct {
		FriendlyName string `yaml:"friendly_name"`
	}
	if err := yaml.Unmarshal([]byte(src), &raw); err != nil {
		return nil, fmt.Errorf("parse devices.yaml: %w", err)
	}
	out := map[string]string{}
	for addr, d := range raw {
		if d.FriendlyName == "" {
			continue
		}
		out[Slugify(d.FriendlyName)] = addr
	}
	return out, nil
}

// Slugify mirrors how Home Assistant turns a friendly name into an entity id.
func Slugify(name string) string {
	var b strings.Builder
	prevUnderscore := false
	for _, r := range strings.ToLower(strings.TrimSpace(name)) {
		switch {
		case r >= 'a' && r <= 'z', r >= '0' && r <= '9':
			b.WriteRune(r)
			prevUnderscore = false
		default:
			if !prevUnderscore && b.Len() > 0 {
				b.WriteByte('_')
				prevUnderscore = true
			}
		}
	}
	return strings.Trim(b.String(), "_")
}
