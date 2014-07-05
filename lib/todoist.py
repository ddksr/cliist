import urllib.parse
import urllib.request
import json
import re

from settings import colors, API_TOKEN
from lib import models

QUERY_DELIMITER = re.compile(', *')
API_URL = 'https://api.todoist.com/API'
TASK_FORMAT = '{c0}{indent} - {taskid:10} {priority}{c1}{content} {c2}{due}'
def ulist(l):
    return json.dumps(l).replace(' ', '')

def api_call(method, **options):
    options['token'] = API_TOKEN
    query_string = urllib.parse.urlencode(options,
                                          safe='',
                                          errors=None,
                                          encoding=None)
    url = "{apiurl}/{method}?{query}".format(apiurl=API_URL,
                                             method=method,
                                             query=query_string)
    try:
        req = urllib.request.urlopen(url)
        content = req.read().decode('utf-8')
        return json.loads(content)
        
    except Exception as ex:
        print(ex)

def print_task(task, search=None, date=None):
    indent = '  ' * (int(task.get('indent', '1')) - 1)
    due_date = task.get('due_date', '')
    if not due_date:
        due_date = ''
    content = task.get('content', '')
    taskid = task.get('id')
    priority = task.get('priority', '')
    if priority and priority != 1:
        priority = '{}!!{} '.format(colors.PRIORITY,
                                        priority)
    else:
        priority = ''
            
    print(TASK_FORMAT.format(c0=colors.ENDC,
                             c1=colors.CONTENT,
                             c2=colors.DATE,
                             indent=indent,
                             priority=priority,
                             due=due_date,
                             content=content,
                             taskid=taskid))
            
        
def print_tasks_list(result):
    for project_or_task in result[0]['data']:
        if project_or_task.get('content'):
            print_task(project_or_task)
        else:
            project_name = project_or_task.get('project_name')
            print('{}#{}'.format(colors.PROJECT,
                                 project_name))
            for task in project_or_task.get('uncompleted', []):
                print_task(task)


def query(info, stdout=True):
    queries = ['view all']
    if info:
        queries = ulist(QUERY_DELIMITER.split(info['merged']))
    
    result = api_call('query', queries=queries)
    result_set = models.ResultSet(result, info['merged'])
    if stdout:
        result_set.pprint()
    return result_set


def complete_tasks(cinfo):
    api_call('completeItems', ids=[
        int(taskid) for taskid in cinfo['raw']
    ])

def add_task(cinfo, due_date):
    if not cinfo:
        return None
    labels, project = [], None
    if cinfo.get('labels'):
        all_labels = list_labels(cinfo, stdout=False,
                             do_search=False)
        for label in cinfo['labels']:
            if label not in all_labels:
                continue
            labels.append(all_labels[label])
    if cinfo.get('project'):
        all_projects = list_projects(cinfo, stdout=False,
                                do_search=False)
        for proj in all_projects:
            if cinfo.get('project') == proj['name']:
                project = proj
                break
    args = {
        'content': cinfo['content']
    }
    if project:
        args['project_id'] = project['id']
    if labels:
        args['labels'] = [label['id'] for label in labels]
    if cinfo['priority']:
        args['priority'] = int(cinfo['priority'])
    else:
        args['priority'] = 1
    api_call('addItem', **args)

def list_labels(cinfo, stdout=True, do_search=True):
    result = api_call('getLabels')
    search = do_search and cinfo.get('merged')
    for label in result:
        if search and search.lower() not in label.lower():
            continue
        if stdout:
            print('@' + label)
    return result
    
def list_projects(cinfo, stdout=True, do_search=True):
    result = api_call('getProjects')
    search = do_search and cinfo.get('merged')
    for project in result:
        name = project['name']
        if search and search.lower() not in name.lower():
            continue
        indent = '  ' * (int(project.get('indent', '1')) - 1)
        if stdout:
            print(indent + '#' + name)
    return result

def list_tasks(cinfo, date, stdout=True):
    result = api_call('query', queries=ulist([
        'view all' + (date and '& {}'.format(date) or '')
    ]))
    result_set = models.ResultSet(result)
    if stdout:
        result_set.pprint()
    return result_set
