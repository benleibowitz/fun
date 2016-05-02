#!/usr/bin/python
import socket

clientsocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
clientsocket.connect(('localhost', 35000))
clientsocket.send('Hello from Python client')
