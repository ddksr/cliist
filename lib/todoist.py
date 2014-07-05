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

def prepare_task_info(cinfo):
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
    args = {}
    if cinfo['content'].strip():
        args['content'] = cinfo['content']
    if project:
        args['project_id'] = project['id']
    if labels:
        args['labels'] = [label['id'] for label in labels]
    if cinfo['priority']:
        args['priority'] = int(cinfo['priority'])
    return labels, project, args

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

def add_task(cinfo, due_date=None):
    if not cinfo:
        return None
    labels, project, api_args = prepare_task_info(cinfo)
    if 'priority' not in api_args:
        api_args['priority'] = 1
    if 'content' not in api_args:
        api_args['content'] = '...'
    api_call('addItem', **api_args)

def edit_task(cinfo, edit_id, due_date=None):
    if not cinfo:
        return None
    labels, project, api_args = prepare_task_info(cinfo)
    api_args['id'] = edit_id
    api_call('updateItem', **api_args)
    

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
