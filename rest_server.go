package main

import (
	"database/sql"
	"encoding/json"
	"flag"
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
var err error
var mc = memcache.New("localhost:11211")
var mh codec.MsgpackHandle

type User struct {
	name string
	mail string
}

func main() {

	db, err = sql.Open("mysql", "root@/test")
	if err != nil {
		panic(err.Error())
	}
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

func fetch_mysql() *map[string]interface{} {
	id++
	if id > 10000 {
		id = 0
	}
	var user User
	re := make(map[string]interface{})

	err = db.QueryRow("SELECT name,mail FROM user WHERE id=?", id).Scan(&user.name, &user.mail)
	re["name"] = &user.name
	re["mail"] = &user.mail
	switch {
	case err == sql.ErrNoRows:
		log.Printf("No user with that ID.")
	case err != nil:
		log.Fatal(err)
	}
	return &re
}

func fetch_memcached() *map[string]interface{} {
	id++
	if id > 10000 {
		id = 0
	}
	re := make(map[string]interface{})
	res, err := mc.Get(strconv.Itoa(id))
	switch {
	case err != nil:
		log.Fatal(err)
	}
	z := strings.SplitN(string(res.Value), "|", 2)
	re["name"] = z[0]
	re["mail"] = z[1]
	return &re
}

func messagepack_handler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/x-msgpack")
	res := fetch_mysql()
	enc := codec.NewEncoder(w, &mh)
	err = enc.Encode(res)
}

func messagepack_memcached_handler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/x-msgpack")
	res := fetch_memcached()
	enc := codec.NewEncoder(w, &mh)
	err = enc.Encode(res)
}

func json_handler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	res := fetch_mysql()
	enc := json.NewEncoder(w)
	err = enc.Encode(res)
}

func json_memcached_handler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	res := fetch_mysql()
	enc := json.NewEncoder(w)
	err = enc.Encode(res)
}
