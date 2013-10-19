#!/usr/bin/env python
import meinheld, MySQLdb, msgpack, ujson
from MySQLdb.cursors import DictCursor

packer = msgpack.Packer()

con = MySQLdb.connect(user='root', host='localhost', db='test')
con.autocommit(1)
count = 0

def fetch_mysql():
    global count
    count += 1
    if count > 1000:
        count = 0
    cur = con.cursor(DictCursor)
    cur.execute("SELECT name,mail FROM user WHERE id=%d" % count)
    return cur.fetchone()


def top_handler(env, start_response):
    response = packer.pack(fetch_mysql())
    start_response("200 OK",
                   [('Content-Type', 'application/x-msgpack'),
                    ('Content-Length', str(len(response)))])
    return [response]

def json_handler(env, start_response):
    response = ujson.dumps(fetch_mysql())
    start_response("200 OK",
                   [('Content-Type', 'application/json'),
                    ('Content-Length', str(len(response)))])
    return [response]

handlers = {
    '/': top_handler,
    '/json': json_handler,
}


def application(env, start_response):
    handler = handlers[env['PATH_INFO']]
    return handler(env, start_response)

meinheld.server.listen(('0.0.0.0', 9000))
meinheld.server.set_access_logger(None)
meinheld.server.set_keepalive(100)
meinheld.server.run(application)
