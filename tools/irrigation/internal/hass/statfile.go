package hass

import "os"

// statFile reports whether the path exists, and whether it is a directory.
func statFile(p string) (isDir bool, err error) {
	fi, err := os.Stat(p)
	if err != nil {
		return false, err
	}
	return fi.IsDir(), nil
}
