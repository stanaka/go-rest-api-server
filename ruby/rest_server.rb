# ruby rest_server.rb -s Puma
require "sinatra"
require "mysql2"
require "msgpack"
require "oj"
require "dalli"

set :server, :puma
set :logging, false

configure :production do
  set :server, :puma
  set :logging, false
end

$count = 0

# $client = Mysql2::Client.new(:host => "localhost", :username => "root", :database => "test")
def fetch_mysql
  $count += 1
  client = Thread.current.thread_variable_get("client")
  unless client
    Thread.current.thread_variable_set("client", Mysql2::Client.new(:host => "localhost", :username => "root", :database => "test"))
    client = Thread.current.thread_variable_get("client")
  end
  results = client.query("SELECT name,mail FROM user WHERE id=#{$count}")
  # results = $client.query("SELECT name,mail FROM user WHERE id=#{$count}")
  row = results.first
  return row
end

get '/' do
  content_type 'application/x-msgpack'
  MessagePack.pack(fetch_mysql())
end

get '/json' do
  content_type 'application/json'
  Oj.dump(fetch_mysql())
end

# $dalli = Dalli::Client.new(["localhost:11211"])
def fetch_memcached
  client = Thread.current.thread_variable_get("client")
  unless client
    Thread.current.thread_variable_set("client", Dalli::Client.new(["localhost:11211"]))
    client = Thread.current.thread_variable_get("client")
  end
  $count += 1
  row = {}
  (row["name"],row["mail"]) = client.get($count).split('|',2)
  return row
end

get '/mem' do
  content_type 'application/x-msgpack'
  MessagePack.pack(fetch_memcached())
end

get '/mem.json' do
  content_type 'application/json'
  Oj.dump(fetch_memcached())
end

