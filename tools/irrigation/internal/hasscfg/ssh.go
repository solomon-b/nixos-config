package hasscfg

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"time"
)

type Source struct {
	Host string
	Path string
}

type Result struct {
	Config    Config
	FromCache bool
	CachedAt  time.Time
	Err       error
}

// Load fetches configuration.yaml over SSH. On failure it falls back to the
// last good copy on disk, so an unreachable config host does not stop the tool.
func (s Source) Load(ctx context.Context) Result {
	raw, err := s.fetch(ctx)
	if err == nil {
		cfg, perr := Parse(raw)
		if perr == nil {
			_ = writeCache(raw)
			return Result{Config: cfg}
		}
		err = perr
	}

	cached, at, cerr := readCache()
	if cerr != nil {
		return Result{Err: fmt.Errorf("fetch failed (%v) and no cache available: %w", err, cerr)}
	}
	cfg, perr := Parse(cached)
	if perr != nil {
		return Result{Err: fmt.Errorf("fetch failed (%v) and cache unparseable: %w", err, perr)}
	}
	return Result{Config: cfg, FromCache: true, CachedAt: at, Err: err}
}

// fetch shells out to ssh so the operator's existing SSH config, agent, and
// known_hosts apply. Reimplementing that with x/crypto/ssh would mean
// duplicating config parsing and host key policy for no gain.
func (s Source) fetch(ctx context.Context) (string, error) {
	cmd := exec.CommandContext(ctx, "ssh",
		"-o", "BatchMode=yes",
		"-o", "ConnectTimeout=5",
		s.Host, "cat", s.Path)
	out, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("ssh %s cat %s: %w", s.Host, s.Path, err)
	}
	return string(out), nil
}

func cachePath() (string, error) {
	base := os.Getenv("XDG_CACHE_HOME")
	if base == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return "", err
		}
		base = filepath.Join(home, ".cache")
	}
	return filepath.Join(base, "irrigation", "configuration.yaml"), nil
}

// writeCache stores the config with owner-only permissions. It holds the
// recorder password, so it must not be world readable.
func writeCache(raw string) error {
	p, err := cachePath()
	if err != nil {
		return err
	}
	if err := os.MkdirAll(filepath.Dir(p), 0o700); err != nil {
		return err
	}
	return os.WriteFile(p, []byte(raw), 0o600)
}

func readCache() (string, time.Time, error) {
	p, err := cachePath()
	if err != nil {
		return "", time.Time{}, err
	}
	b, err := os.ReadFile(p)
	if err != nil {
		return "", time.Time{}, err
	}
	fi, err := os.Stat(p)
	if err != nil {
		return "", time.Time{}, err
	}
	return string(b), fi.ModTime(), nil
}
