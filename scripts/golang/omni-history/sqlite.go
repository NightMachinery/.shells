package main

/*
#cgo LDFLAGS: -lsqlite3
#include <sqlite3.h>
#include <stdlib.h>
*/
import "C"

import (
	"errors"
	"unsafe"
)

// Open only the disposable snapshot. SQLite may recover its rollback/WAL journal.
func sqliteQuery(path, query string) ([][]string, error) {
	name := C.CString(path)
	defer C.free(unsafe.Pointer(name))
	var db *C.sqlite3
	if C.sqlite3_open_v2(name, &db, C.SQLITE_OPEN_READWRITE|C.SQLITE_OPEN_NOMUTEX, nil) != C.SQLITE_OK {
		if db != nil {
			C.sqlite3_close(db)
		}
		return nil, errors.New("snapshot open failed")
	}
	defer C.sqlite3_close(db)
	C.sqlite3_busy_timeout(db, 150)
	sql := C.CString(query)
	defer C.free(unsafe.Pointer(sql))
	var stmt *C.sqlite3_stmt
	if C.sqlite3_prepare_v2(db, sql, -1, &stmt, nil) != C.SQLITE_OK {
		return nil, errors.New("snapshot query failed")
	}
	defer C.sqlite3_finalize(stmt)
	rows := [][]string{}
	for {
		code := C.sqlite3_step(stmt)
		if code == C.SQLITE_DONE {
			return rows, nil
		}
		if code != C.SQLITE_ROW {
			return nil, errors.New("snapshot read failed")
		}
		row := make([]string, int(C.sqlite3_column_count(stmt)))
		for i := range row {
			value := C.sqlite3_column_text(stmt, C.int(i))
			if value != nil {
				row[i] = C.GoString((*C.char)(unsafe.Pointer(value)))
			}
		}
		rows = append(rows, row)
	}
}
