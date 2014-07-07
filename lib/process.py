from datetime import date
import re
from lib import todoist

EU_DATE = re.compile('\d{1,2}\.\d{1,2}\.\d{2}(\d{2})?')
ORDER_OPTIONS = {
    'c': 'content',
    'p': 'priority',
    'd': 'sort_date',
}

def is_word_label(word):
    return word and word[0] == '@'

def is_word_project(word):
    return word and word[0] == '#'

def is_word_priority(word):
    return len(word) > 2 and word[:2] == '!!' and word[2:].isdigit()


def str_date(date_str):
    y, m, d = 2000, 1, 1
    if '-' in date_str:
        y, m, d = date_str.split('-')
    if '.' in date_str:
        d, m, y = date_str.split('.')
    return date(int(y), int(m), int(d))
    
def todoist_date(date_str):
    if EU_DATE.match(date_str):
        return date_str.replace('.', '/')
    return date_str
    
def content_info(content_raw):
    content = []
    mapper = lambda w: w.replace('%%', '!!').split(' ')
    for words in map(mapper, content_raw):
        content.extend(words)   
    
    raw_words = []
    raw_labels = []
    project = None
    priority = None
    for word in content:
        if not word:
            continue
        if is_word_label(word):
            raw_labels.append(word[1:])
        elif is_word_project(word) and not project:
            project = word[1:]
        elif is_word_priority(word) and not priority:
            priority = word[2:]
        else:
            raw_words.append(word)
    return {
        'content': ' '.join(raw_words),
        'merged': ' '.join(content),
        'raw': content_raw,
        'labels': raw_labels,
        'project': project,
        'priority': priority,
    }

def get_filters(options):
    filters = {}
    if options.gte_date:
        filters['gte'] = str_date(options.gte_date)
    if options.lte_date:
        filters['lte'] = str_date(options.lte_date)
    if options.lt_date:
        filters['lt'] = str_date(options.lt_date)
    if options.gt_date:
        filters['gt'] = str_date(options.gt_date)
    if options.eq_date:
        filters['eq'] = str_date(options.eq_date)
    if options.neq_date:
        filters['neq'] = str_date(options.neq_date)
    return filters
    
def command(args, options):
    cinfo = args and content_info(args) or {}
    
    list_opts = {
        'filters': get_filters(options),
        'reverse': options.reverse,
        'order': ORDER_OPTIONS.get(options.order),
        'search': cinfo.get('merged'),
    }
    due_date = options.date and todoist_date(options.date) or None
    if options.query:
        todoist.query(cinfo, options.query, **list_opts)
    elif options.all:
        todoist.query(cinfo, 'view all', **list_opts)
    elif options.complete:
        todoist.complete_tasks(cinfo)
    elif options.add_task:
        todoist.add_task(cinfo, due_date)
    elif options.labels:
        todoist.list_labels(cinfo, reverse=options.reverse)
    elif options.projects:
        todoist.list_projects(cinfo, reverse=options.reverse)
    elif options.edit_id:
        todoist.edit_task(cinfo, options.edit_id, due_date)
    elif options.project_name:
        todoist.project_tasks(cinfo, options.project_name, **list_opts)
    else:
        todoist.list_tasks(cinfo, due_date, **list_opts)
        
