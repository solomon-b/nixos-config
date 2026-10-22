package hasscfg

import "testing"

// Copied from the real /mnt/zigbee2mqtt/devices.yaml.
const devicesFixture = `
'0x84ba20fffe6e6d0d':
  friendly_name: Green House Repeater
'0x44e2f8fffe0c8dfe':
  friendly_name: Cactus Fence Irrigation Controller
'0x842712fffe80d231':
  friendly_name: Manifold 11
'0x842712fffe809c50':
  friendly_name: Manifold 12
`

func TestParseAliasesMapsSlugToAddress(t *testing.T) {
	got, err := ParseAliases(devicesFixture)
	if err != nil {
		t.Fatalf("ParseAliases: %v", err)
	}
	want := map[string]string{
		"green_house_repeater":               "0x84ba20fffe6e6d0d",
		"cactus_fence_irrigation_controller": "0x44e2f8fffe0c8dfe",
		"manifold_11":                        "0x842712fffe80d231",
		"manifold_12":                        "0x842712fffe809c50",
	}
	for slug, addr := range want {
		if got[slug] != addr {
			t.Errorf("alias[%q] = %q, want %q", slug, got[slug], addr)
		}
	}
}

func TestSlugify(t *testing.T) {
	cases := map[string]string{
		"Manifold 11":                        "manifold_11",
		"Cactus Fence Irrigation Controller": "cactus_fence_irrigation_controller",
		"  Spaced  Out  ":                    "spaced_out",
		"Weird-Name/Here":                    "weird_name_here",
	}
	for in, want := range cases {
		if got := Slugify(in); got != want {
			t.Errorf("Slugify(%q) = %q, want %q", in, got, want)
		}
	}
}
