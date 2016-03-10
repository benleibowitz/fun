#!/usr/bin/python
# ignores newline, tab, and space. not a true code golf count, but fun nonetheless

import sys

verbose = False

if len(sys.argv) > 2 and sys.argv[2] == '--v':
    verbose = True

with open(sys.argv[1], 'r') as golf:
    count = 0
    for line in golf:
        line = line.replace('\r', '').replace('\n', '').replace(' ', '').replace('\t', '')
        for char in line:
            count += 1
            if verbose:
                print('[' + char + ']')

    print('Characters: %d' % count)
