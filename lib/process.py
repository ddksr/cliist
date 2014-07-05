import re

from lib import todoist
import settings

EU_DATE = re.compile('\d{1,2}\.\d{1,2}\.\d{2}(\d{2})?')

def is_word_label(word):
    return word and word[0] == '@'

def is_word_project(word):
    return word and word[0] == '#'

def is_word_priority(word):
    return len(word) > 2 and word[:2] == '!!' and word[2:].isdigit()
    
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
        'priority': priority
    }

def date(date_str):
    if EU_DATE.math(date_str):
        return date_str.replace('.', '/')
    return date_str

def command(args, options):
    cinfo = args and content_info(args) or {}
    due_date = options.date and date(options.date) or None
    if options.query:
        todoist.query(cinfo)
    elif options.complete:
        todoist.complete_tasks(cinfo)
    elif options.add_task:
        todoist.add_task(cinfo, due_date)
    elif options.labels:
        todoist.list_labels(cinfo)
    elif options.projects:
        todoist.list_projects(cinfo)
    elif options.edit_id:
        todoist.edit_task(cinfo, options.edit_id, due_date)
    else:
        todoist.list_tasks(cinfo, due_date)
        
