import urllib.parse
import urllib.request
import json
import re

from settings import API_TOKEN
from . import models, output
from .utils import CliistException

QUERY_DELIMITER = re.compile(', *')
API_URL = 'https://api.todoist.com/API'
TASK_FORMAT = '{c0}{indent} - {taskid:10} {priority}{c1}{content} {c2}{due}'

def ulist(l):
    return json.dumps(l).replace(', ', ',')


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

def prepare_task_info(cinfo, due_date=None):
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
    content = cinfo.get('content', '')
    if content.strip():
        args['content'] = content
    if project:
        args['project_id'] = project['id']
    if labels:
        args['labels'] = [label['id'] for label in labels]
    if cinfo.get('priority'):
        args['priority'] = int(cinfo['priority'])
    if due_date:
        args['date_string'] = due_date
    return labels, project, args

def get_taks(cinfo, task=None):
    cached = models.ResultSet.load()
    ids_recurring, ids_normal = [], []
    for task_raw in [task] if task else cinfo['raw']:
        if task_raw.isdigit():
            task_id = int(task_raw)
            if cached:
                task = cached.lookup_one(task_raw)
                if task is not None and task.is_recurring:
                    ids_recurring.append(task_id)
                    continue
                
            ids_normal.append(task_id)
        elif cached is not None:
            result = cached.lookup(task_raw)
            if len(result) > 1:
                raise CliistException('Too many cached results for {}.\nNo tasks were marked completed'.format(task_raw))

            elif len(result) < 1:
                raise CliistException('No cached results for "{}".\nNo tasks were marked completed'.format(task_raw))
            else:
                task = result[0]
                task_id = task.get('id')
                if task.is_recurring:
                    ids_recurring.append(task_id)
                else:
                    ids_normal.append(task_id)
        else:
            raise CliistException('No chached results. Please list your tasks with cliist to enable task lookup.\nNo tasks were marked completed')
    return ids_normal + ids_normal, ids_normal, ids_recurring


def list_cache(output_engine=output.Plain):
    cached = models.ResultSet.load()
    if cached is None:
        raise CliistException('Cache is empty')
    cached.pprint(output_engine=output_engine)

def project_tasks(cinfo, project_name, stdout=True,
                  output_engine=output.Plain, **options):
    all_projects = list_projects(cinfo, stdout=False,
                                 do_search=False)
    project_id = None
    for proj in all_projects:
        if project_name == proj['name']:
            project_id = proj.get('id')
            break
    if not project_id:
        return
    result = api_call('getUncompletedItems', project_id=project_id)
    result_set = models.ResultSet(result, project_name or 'view all', **options)
    if stdout:
        result_set.pprint(output_engine=output_engine)
    return result_set

def archive(cinfo, limit, date=None, project_name=None, stdout=True, 
            output_engine=output.Plain, **options):
    result = None
    all_projects = list_projects(cinfo, stdout=False, do_search=False)
    kwargs = {'limit': limit}
    if date:
        kwargs['from_date'] = date
    for proj in all_projects:
        if project_name == proj['name']:
            kwargs['project_id'] = proj.get('id')
            break
    result = api_call('getAllCompletedItems', **kwargs)['items']
    result_set = models.ResultSet(result,
                                  'Completed: ' + (project_name or 'all'),
                                  **options)
    if stdout:
        result_set.pprint(output_engine=output_engine)
    return result_set
    
def query(info, query, stdout=True, output_engine=output.Plain, **options):
    queries = QUERY_DELIMITER.split(query)
    result = api_call('query', queries=ulist(queries))
    result_set = models.ResultSet(result, query or 'view all', **options)
    if stdout:
        result_set.pprint(output_engine=output_engine)
    return result_set

def complete_tasks(cinfo):
    ids, ids_normal, ids_recurring = get_taks(cinfo)

    if ids_normal:
        api_call('completeItems', ids=ids_normal)
    if ids_recurring:
        api_call('updateRecurringDate', ids=ids_recurring)

def add_task(cinfo, due_date=None):
    if not cinfo:
        raise CliistException('Task has no content!')
    labels, project, api_args = prepare_task_info(cinfo, due_date)
    if 'content' not in api_args:
        raise CliistException('Task has no content!')
    if 'priority' not in api_args:
        api_args['priority'] = 1
    api_call('addItem', **api_args)

def edit_task(cinfo, edit_id, due_date=None):
    if not cinfo:
        raise CliistException('No task content')
    # TODO: could use lookup
    labels, project, api_args = prepare_task_info(cinfo, due_date)
    api_args['id'] = edit_id
    api_call('updateItem', **api_args)
    

def list_labels(cinfo, stdout=True, info=False,
                do_search=True, reverse=False):
    result = api_call('getLabels')
    search = do_search and cinfo.get('merged')
    for label_name in reverse and result.keys()[::-1] or result.keys():
        label = result[label_name]
        if search and search.lower() not in label.lower():
            continue
        if stdout:
            out_str = '@' + label_name
            if info:
                out_str += ' ' + str(label['id'])
            print(out_str)
    return result
    
def list_projects(cinfo, stdout=True, do_search=True, reverse=False):
    result = api_call('getProjects')
    search = do_search and cinfo.get('merged')
    for project in reverse and result[::-1] or result:
        name = project['name']
        if search and search.lower() not in name.lower():
            continue
        indent = '  ' * (int(project.get('indent', '1')) - 1)
        if stdout:
            print(indent + '#' + name)
    return result

def list_tasks(cinfo, due_date, stdout=True, output_engine=output.Plain, **options):
    result = api_call('query', queries=ulist(['overdue','today']))
    if cinfo:
        options['search'] = cinfo.get('merged')
    result_set = models.ResultSet(result, name='Overdue and today', **options)
    if stdout:
        result_set.pprint(output_engine=output_engine)
    return result_set
