// Package store reads irrigation history from the Home Assistant recorder.
package store

import (
	"context"
	"fmt"
	"strconv"
	"strings"

	"github.com/jackc/pgx/v5/pgxpool"
)

type Store struct {
	pool *pgxpool.Pool
}

func Open(ctx context.Context, dsn string) (*Store, error) {
	pool, err := pgxpool.New(ctx, dsn)
	if err != nil {
		return nil, fmt.Errorf("connect: %w", err)
	}
	if err := pool.Ping(ctx); err != nil {
		pool.Close()
		return nil, fmt.Errorf("ping: %w", err)
	}
	return &Store{pool: pool}, nil
}

func (s *Store) Close() { s.pool.Close() }

// parseIntPrefix tolerates Zigbee sensors that report "87" in one message and
// "87.0" in the next. Truncate at the decimal point rather than failing.
func parseIntPrefix(s string) (int, error) {
	if i := strings.IndexByte(s, '.'); i >= 0 {
		s = s[:i]
	}
	return strconv.Atoi(s)
}
