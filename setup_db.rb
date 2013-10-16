#!/usr/bin/env ruby
require 'mysql2'

client = Mysql2::Client.new(:host => "localhost", :username => "root", :database => "test")

results = client.query("DROP TABLE IF EXISTS `user`")
results = client.query(<<EOS);
CREATE TABLE `user` (
  `id` varchar(32) NOT NULL,
  `name` varchar(256) NOT NULL,
  `mail` varchar(1024) NOT NULL,
  `c3` int(11) DEFAULT NULL,
  `c4` bigint(20) unsigned DEFAULT NULL,
  `c5` int(11) DEFAULT NULL,
   PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8
EOS

(0..10000).each do |i|
  name = ((0..9).to_a + ("a".."z").to_a).sample(10).join
  mail = ((0..9).to_a + ("a".."z").to_a).sample(10).join + "@example.com"
  results = client.query("insert into `user` values (#{i},\"#{name}\",\"#{mail}\",0,0,0)");
end
