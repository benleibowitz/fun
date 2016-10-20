#!/usr/bin/python
import heartbeat_config
import socket
import time

out_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
in_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
in_socket.bind(("", heartbeat_config.CLI_PORT))

while True:
  data, addr = in_socket.recvfrom(256)
  out_socket.sendto('1', (addr[0], heartbeat_config.SERV_PORT))



