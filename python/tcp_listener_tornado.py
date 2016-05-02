#!/usr/bin/python
import argparse
from tornado.tcpserver import TCPServer
from tornado.ioloop import IOLoop

PORT = 35000

class ControlServer(TCPServer):
  '''Subclass TCPServer so
      we can override handle_stream
  '''
  def __init__(self, io_loop=None, ssl_options=None, max_buffer_size=None):
    TCPServer.__init__(self, io_loop=io_loop, ssl_options=ssl_options,
      max_buffer_size=max_buffer_size)

  def handle_stream(self, stream, address):
    stream.read_until_close(callback=self.eat_message)

  def eat_message(self, msg):
    print 'Received message [%s]' % msg

server = ControlServer()
server.listen(PORT, '0.0.0.0')
IOLoop.current().start()
