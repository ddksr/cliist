#!/bin/env python3

from optparse import OptionParser

from lib import process

def main():
    parser = OptionParser(usage="usage: %prog [options] task_content|search_string")

    parser.add_option('-d', '--date',
                      dest='date',
                      default=None,
                      help='Todoist due date')

    parser.add_option('-e', '--edit',
                      dest='edit_id',
                      default=None,
                      help='Todoist edit specified task content')

    parser.add_option('-q', '--query',
                      dest='query',
                      action='store_true',
                      default=False,
                      help='Todoist query')

    parser.add_option('-c', '--complete',
                      dest='complete',
                      action='store_true',
                      default=False,
                      help='Mark tasks completed')

    parser.add_option('-a', '--add',
                      dest='add_task',
                      action='store_true',
                      default=False,
                      help='Todoist add task')

    parser.add_option('-l', '--labels',
                      dest='labels',
                      action='store_true',
                      default=False,
                      help='Todoist list labels')

    parser.add_option('-p', '--projects',
                      dest='projects',
                      action='store_true',
                      default=False,
                      help='Todoist list projects')

    options, args = parser.parse_args()
    process.command(args, options)
    
if __name__ == '__main__':
    main()
