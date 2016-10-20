#!/usr/bin/python
from __future__ import print_function
import heartbeat_config
import socket
import time

ips = {ip:0 for ip in heartbeat_config.ips}
dead = set()

out_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
in_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
in_socket.bind(("", heartbeat_config.SERV_PORT))
in_socket.settimeout(heartbeat_config.TIMEOUT)

'''Remove any dead IP addresses from the IP dict
'''
def clean_ips(unacknowledged):
  removable = []

  for ip in unacknowledged:
    if ips[ip] > heartbeat_config.UNACKNOWLEDGED_THRESHOLD:
      print('Dead client: %s' % ip)

      dead.add(ip)
      removable.append(ip)

  for ip in removable:
    del ips[ip]

def loop():
  while True:
    unacknowledged = set([ip for ip in ips])

    for ip in ips:
      out_socket.sendto('1', (ip, heartbeat_config.CLI_PORT))
    for _ in ips:
      try:
        data, addr = in_socket.recvfrom(256)
        unacknowledged.remove(addr[0])
        ips[addr[0]] = 0
      except:
        # Timeout waiting for response
        pass

    for ip in unacknowledged:
      ips[ip] += 1

    if len(unacknowledged) == 0:
      print('All clients acknowledged heartbeat')
    else:
      clean_ips(unacknowledged)

    print('IPs alive:', [x for x in ips])
    time.sleep(heartbeat_config.SLEEP)

if __name__ == '__main__':
  loop()
