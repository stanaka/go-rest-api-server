#!/usr/bin/env python
import string
import random
import MySQLdb

con = MySQLdb.connect('localhost', 'root', db='test')
cur = con.cursor()
cur.execute("DROP TABLE IF EXISTS `user`")
cur.execute("""\
CREATE TABLE `user` (
  `id` varchar(32) NOT NULL,
  `name` varchar(256) NOT NULL,
  `mail` varchar(1024) NOT NULL,
  `c3` int(11) DEFAULT NULL,
  `c4` bigint(20) unsigned DEFAULT NULL,
  `c5` int(11) DEFAULT NULL,
   PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8""")

chars = string.ascii_lowercase + string.digits

for i in range(10001):
    name = ''.join(random.sample(chars, 10))
    mail = ''.join(random.sample(chars, 10)) + "@example.com"
    cur.execute("INSERT INTO `user` VALUES (%s, %s, %s, 0, 0, 0)", (i, name, mail))
con.commit()
