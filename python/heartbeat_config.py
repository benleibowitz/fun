ips = [
	'192.168.2.100',
	'192.168.2.101',
	'192.168.2.102',
	'192.168.2.103',
	'192.168.2.104'
      ] 

SERV_PORT = 35010
CLI_PORT = 35000

# How many unacknowledged heartbeats before a process is considered dead
UNACKNOWLEDGED_THRESHOLD = 5

# Timeout on heartbeat request
TIMEOUT = 1

# Sleep at end of each heartbeat round
SLEEP = 3
