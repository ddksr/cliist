#!/bin/env python3

from optparse import OptionParser

from lib import process

USAGE = "usage: %prog [options] task_content|search_string|task_id"
DESC = """Simple Todoist console client.
If no options and arguments specified, all uncompleted tasks for today and overdue are listed.
Note: because ! is a special bash character, you can write %% instead of !!"""

def main():
    parser = OptionParser(usage=USAGE,
                          description=DESC)

    parser.add_option('-d', '--date',
                      dest='date',
                      default=None,
                      help='Todoist due date formatted in Todoist date format. Available when no other options specified and when adding or editing tasks.')
    
    parser.add_option('-s', '--sort',
                      dest='order',
                      default=None,
                      help='Sort todoist tasks by content (c), priority (p) or due date (d). Available every time a list of tasks is listed.')
    
    parser.add_option('-r', '--reverse',
                      dest='reverse',
                      action='store_true',
                      default=False,
                      help='Reverse the list. Available every time tasks, projects or labels are listed.')
    
    parser.add_option('-e', '--edit',
                      dest='edit_id',
                      default=None,
                      help='Edit specified task content. Specify content with arguments.')

    parser.add_option('-q', '--query',
                      dest='query',
                      default=None,
                      help='Query tasks using Todoist search queries as arguments.')

    parser.add_option('-c', '--complete',
                      dest='complete',
                      action='store_true',
                      default=None,
                      help='Mark tasks completed (arguments are task ids).')

    parser.add_option('-a', '--add',
                      dest='add_task',
                      action='store_true',
                      default=False,
                      help='Todoist add task where content as arguments.')

    parser.add_option('-L', '--labels',
                      dest='labels',
                      action='store_true',
                      default=False,
                      help='List Todoist labels.')

    parser.add_option('-P', '--projects',
                      dest='projects',
                      action='store_true',
                      default=False,
                      help='List Todoist projects.')

    parser.add_option('-p', '--project-tasks',
                      dest='project_name',
                      default=False,
                      help='List Todoist project tasks.')
    
    parser.add_option('-A', '--all',
                      dest='all',
                      action='store_true',
                      default=False,
                      help='List all uncompleted todoist tasks.')

    parser.add_option('--gte',
                      dest='gte_date',
                      default=None,
                      help='List tasks with due date greater or equal than GTE_DATE')

    parser.add_option('--lte',
                      dest='lte_date',
                      default=None,
                      help='List tasks with due date less or equal to LTE_DATE')

    parser.add_option('--gt',
                      dest='gt_date',
                      default=None,
                      help='List tasks with due date greater than GT_DATE')

    parser.add_option('--lt',
                      dest='lt_date',
                      default=None,
                      help='List tasks with due date less than LT_DATE')

    parser.add_option('--eqaul',
                      dest='eq_date',
                      default=None,
                      help='List tasks with due date equal to EQ_DATE')

    parser.add_option('--not-equal',
                      dest='neq_date',
                      default=None,
                      help='List tasks with due date not equal to NEQ_DATE')

    options, args = parser.parse_args()
    process.command(args, options)
    
if __name__ == '__main__':
    main()
