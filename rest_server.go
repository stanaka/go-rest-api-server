package main

import (
	"database/sql"
	"encoding/json"
	"flag"
	"fmt"
	"github.com/bradfitz/gomemcache/memcache"
	_ "github.com/go-sql-driver/mysql"
	"github.com/ugorji/go/codec"
	"log"
	"net/http"
	"strconv"
	"strings"
)

var addr = flag.String("addr", ":9000", "http service address")
var db *sql.DB
var mc = memcache.New("localhost:11211")
var mh codec.MsgpackHandle

func main() {
	var err error
	// Edit socket path
	db, err = sql.Open("mysql", "root@unix(/var/run/mysqld/mysqld.sock)/test")
	if err != nil {
		panic(err.Error())
	}
	db.SetMaxIdleConns(16)
	defer db.Close()

	err = db.Ping()
	if err != nil {
		panic(err.Error())
	}

	mh.RawToString = true

	flag.Parse()
	http.HandleFunc("/", messagepack_handler)
	http.HandleFunc("/json", json_handler)
	http.HandleFunc("/mem", messagepack_memcached_handler)
	http.HandleFunc("/mem.json", json_memcached_handler)
	err = http.ListenAndServe(*addr, nil)
	if err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}

var id = 0

func fetch_mysql() map[string]interface{} {
	id++
	if id > 10000 {
		id = 0
	}

	var name string
	var mail string
	query := fmt.Sprintf("SELECT name,mail FROM user WHERE id=%d", id)
	err := db.QueryRow(query).Scan(&name, &mail)
	switch {
	case err == sql.ErrNoRows:
		log.Printf("No user with that ID.")
	case err != nil:
		log.Fatal(err)
	}
	return map[string]interface{}{"name": name, "mail": mail}
}

func fetch_memcached() map[string]interface{} {
	id++
	if id > 10000 {
		id = 0
	}
	res, err := mc.Get(strconv.Itoa(id))
	if err != nil {
		log.Fatal(err)
	}
	z := strings.SplitN(string(res.Value), "|", 2)
	return map[string]interface{}{"name": z[0], "mail": z[1]}
}

func messagepack_handler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/x-msgpack")
	res := fetch_mysql()
	enc := codec.NewEncoder(w, &mh)
	enc.Encode(res)
}

func messagepack_memcached_handler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/x-msgpack")
	res := fetch_memcached()
	enc := codec.NewEncoder(w, &mh)
	enc.Encode(res)
}

func json_handler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	res := fetch_mysql()
	enc := json.NewEncoder(w)
	enc.Encode(res)
}

func json_memcached_handler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	res := fetch_memcached()
	enc := json.NewEncoder(w)
	enc.Encode(res)
}
