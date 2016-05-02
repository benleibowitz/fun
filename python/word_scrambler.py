#!/usr/bin/python
# Meant to take in a document as command
# line argument, and scramble each word.
# (I read an article once that said something
# about how even if words were scrambled,
# if you keep the first and last letter
# in tact, people can still read it).

import sys
import random

with open(sys.argv[1], 'r') as f:
  text = []
  for line in f:
    ar = line.split()
    for word in ar:
      if len(word) <= 2:
        text.append(word)
      else:
        first = word[0]
        last = word[len(word) - 1]
        word = word[1:-1]
        text.append(first + ''.join(random.sample(word, len(word))) + last)
print ' '.join(text)
