package main

import (
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"syscall"
)

type discovery struct {
	Version  string `json:"version"`
	URL      string `json:"url"`
	Password string `json:"password"`
}

// The lock lives beside the atomically replaced discovery file, never inside it.
func lockState(path string) (*os.File, error) {
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0700); err != nil {
		return nil, err
	}
	info, err := os.Lstat(dir)
	if err != nil {
		return nil, err
	}
	if !info.IsDir() || info.Mode()&os.ModeSymlink != 0 {
		return nil, errors.New("invalid state directory")
	}
	owner, ok := info.Sys().(*syscall.Stat_t)
	if !ok || int(owner.Uid) != os.Getuid() {
		return nil, errors.New("state directory is not owned by current user")
	}
	if err = os.Chmod(dir, 0700); err != nil {
		return nil, err
	}
	file, err := os.OpenFile(path+".lock", os.O_CREATE|os.O_RDWR|syscall.O_NOFOLLOW, 0600)
	if err != nil {
		return nil, err
	}
	if err = syscall.Flock(int(file.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		file.Close()
		return nil, err
	}
	return file, nil
}
func writeState(path string, value discovery) error {
	file, err := os.CreateTemp(filepath.Dir(path), ".endpoint-")
	if err != nil {
		return err
	}
	defer os.Remove(file.Name())
	if err = json.NewEncoder(file).Encode(value); err != nil {
		file.Close()
		return err
	}
	if err = file.Close(); err != nil {
		return err
	}
	return os.Rename(file.Name(), path)
}
