from datetime import datetime
import json
import os.path

from . import output

from settings import colors, OUTPUT_DATE_FORMAT

try:
    from settings import CACHE_ENABLED, CACHE
except:
    CACHE_ENABLED, CACHE = False, ''


class Task(dict):
    def __init__(self, task_raw):
        for key, val in task_raw.items():
            self[key] = val

        self.due_date = None
        if task_raw.get('due_date'):
            due_date = task_raw['due_date']
            if '+' in due_date:
                self.due_date = datetime.strptime(due_date,
                                                  '%a %d %b %Y %H:%M:%S %z')
            else:
                self.due_date = datetime.strptime(due_date,
                                                  '%a %d %b %Y %H:%M:%S')
        self.sort_date = (self.due_date or datetime(1500, 1, 1)).replace(tzinfo=None)
        
        self.project = task_raw.get('project')
        self.priority = int(task_raw.get('priority', '1'))
        self.labels = task_raw.get('labels', [])
        self.content = task_raw.get('content', '')
        self.raw = task_raw
        self.date_string = task_raw.get('date_string', '')
        self.is_recurring = any([
            'every ' in (self.date_string or ''),
            'ev ' in (self.date_string or ''),
        ])

    def serialize(self):
        return json.dumps(self)

    def get_date(self):
        if self.due_date:
            return self.due_date.strftime(OUTPUT_DATE_FORMAT)
        return ''

    def get_key(self, order):
        key = getattr(self, order)
        if type(key) == str:
            return key.lower()
        return key

    def __hash__(self):
        return self.get('id')
        
    def pprint(self, output_engine=output.Plain):
        output_engine.task(self)
        

class TaskSet(list):
    FILTERS = {
        'gte': lambda val: (lambda item: item.sort_date.date() >= val),
        'lte': lambda val: (lambda item: item.sort_date.date() <= val),
        'gt': lambda val: (lambda item: item.sort_date.date() > val),
        'lt': lambda val: (lambda item: item.sort_date.date() < val),
        'eq': lambda val: (lambda item: item.sort_date.date() == val),
        'neq': lambda val: (lambda item: item.sort_date.date() != val),
        'search': lambda val: (lambda item: val.lower() in item['content'].lower()),
    }
    def __init__(self, result = {}, set_type='unknown'):
        if 'project_id' in result:
            self.set_type = 'project'
        else:
            self.set_type = set_type

        for task in result.get('uncompleted', []):
            self.append(Task(task))
        self.raw = result

    def serialize(self):
        return json.dumps(self)

    def copy(self):
        copied = TaskSet(set_type=self.set_type)
        copied.set_type = self.set_type
        copied.extend(self)
        copied.raw = self.raw
        return copied

    def select(self, order=None, reverse=False, search=None, filters={}):
        if search:
            filters['search'] = search
        filtered = self.copy()
        for filtername, filterval in filters.items():
            filtered = filter(TaskSet.FILTERS[filtername](filterval), filtered)
        if order:
            filtered = sorted(filtered, key=lambda task: task.get_key(order))
        filtered=list(filtered)
        selected = TaskSet(set_type=self.set_type)
        selected.raw = self.raw
        for item in (reverse and filtered[::-1] or filtered):
            selected.append(item)
        return selected
        
    def pprint(self, output_engine=output.Plain):
        output_engine.task_set(self)

    def lookup(self, task_info):
        results = set()
        for task in self:
            if task_info.isdigit():
                task_id = int(task_info)
                if task_id and task_id == int(task.get('id', 0)):
                    results.add(task)
            elif task_info.lower() in task.get('content').lower():
                results.add(task)
        return results
        
        
class ResultSet:
    def __init__(self, result, name=None, no_save=False, **options):
        self.task_sets = []
        self.tasks = TaskSet()
        self.name = name
        self.raw = result
        for resultset in result or []:
            if resultset.get('content'):
                self.tasks.append(Task(resultset))
                continue
            for item in resultset['data']:
                if item.get('content'):
                    self.tasks.append(Task(item))
                else:
                    self.task_sets.append(TaskSet(item).select(**options))
        if options:
            self.tasks = self.tasks.select(**options)

        if not no_save:
            self.save()

    def pprint(self, output_engine=output.Plain):
        output_engine.result_set(self)

    def select(self, **options):
        return ResultSet(self.raw, name=self.name, **options)

    def serialize(self):
        dump = { 'name': self.name, 'raw': self.raw, }
        return json.dumps(dump)

    def save(self):
        if not CACHE_ENABLED:
            return None
        with open(CACHE, 'w') as fd:
            fd.write(self.serialize())

    def lookup(self, task_info):
        sets = [self.tasks] + self.task_sets
        tasks = set()
        for task_set in sets:
            for task_subset in map(lambda s: s.lookup(task_info), sets):
                for task in task_subset:
                    tasks.add(task)
        return list(filter(lambda task: task is not None, tasks))

    def lookup_one(self, task_info):
        tasks = self.lookup(task_info)
        if len(tasks) == 1:
            return tasks[0]
        return None

    @staticmethod
    def load():
        if not CACHE_ENABLED:
            return None
        if not os.path.exists(CACHE):
            return None
        with open(CACHE, 'r') as fd:
            return ResultSet.deserialize(fd.read())

    @staticmethod
    def deserialize(dumped_str):
        dump = json.loads(dumped_str)
        return ResultSet(dump['raw'],
                         name=dump['name'], no_save=True)
