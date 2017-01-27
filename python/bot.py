#!/usr/bin/python
'''Bot finds comments like "I'm dead"
   and replies "RIP /u/username"

    Command line arguments:
     --live
     --subreddit=<subreddit_name>


   Bot runs by default in non-live mode, meaning
   it will not post responses to Reddit, it will just
   find the matching comments.

   If run with argument --live, it will post comment replies to Reddit

   Bot depends on module praw, and needs praw.ini file.
   Info found here:
     https://praw.readthedocs.io/en/v4.0.0/getting_started/configuration/prawini.html
'''
from __future__ import print_function
import argparse
import praw
import re

def main():
    # parse arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('--live', action='store_true',
        help='If run with --live, post replies to Reddit. If not live, just print the matching comments to console')
    parser.add_argument('-s', '--subreddit', required=True, help='Desired subreddit to pull from')
    args = parser.parse_args()
    live = args.live
    sub = args.subreddit
    print('SUBREDDIT=%s' % sub)

    if live:
        print('RUNNING IN LIVE MODE')

    reddit = praw.Reddit('mybot')
    subreddit = reddit.subreddit(sub)

    regex = re.compile(r"(i['?]m dead)|(i['?]m dying)", re.IGNORECASE)

    for comment in subreddit.stream.comments():
        if regex.search(comment.body) is not None:
            print("FOUND on r/%s: %s" % (comment.subreddit, comment.body))
            if live:
                reply = "RIP /u/%s" % comment.author
                comment.reply(reply)
                print("REPLIED: %s" % reply)

if __name__ == '__main__':
    main()
