Lightweight REST API server by Go/Perl/Ruby
----

Here're the presentation slides based on this benchmark scripts.

https://speakerdeck.com/stanaka/lightweight-rest-api-server-by-go-and-performance-compalison-with-implementations-of-other-languages


## setup

To setup a test database
```
ruby setup_db.rb
```

To setup a record for innodb memcached plugin. You should read the official document, http://dev.mysql.com/doc/refman/5.6/en/innodb-memcached.html .

```
use innodb_memcache;
delete from containers where name='aaa';
INSERT INTO `containers` VALUES ('ccc','test','user','id','name|mail','c3','c4','c5','PRIMARY');
install plugin daemon_memcached soname "libmemcached.so";
```
