from settings import colors
from datetime import datetime, date, timedelta


class Task(dict):
    FORMAT = '{c0}{indent} - {taskid:10} {priority}{c2}{due}{c1}{content}\n'
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
 
        self.project = task_raw.get('project')
        self.priority = int(task_raw.get('priority', '1'))
        self.labels = task_raw.get('labels', [])

    def get_date(self):
        if self.due_date:
            return self.due_date.strftime('%d.%m.%Y @ %H:%M:%S')
        return ''
        
    def pprint(self):
        indent = '  ' * (int(self.get('indent', '1')) - 1)
        priority = ''
        if self.priority and self.priority != 1:
            priority = '{}{}{} '.format(colors.PRIORITY,
                                        self.priority,
                                        colors.ENDC)
        due = self.get_date()
        if due:
            due += ' '
        print(Task.FORMAT.format(c0=colors.ENDC,
                                  c1=colors.CONTENT,
                                  c2=colors.DATE,
                                  indent=indent,
                                  priority=priority,
                                  content=self.get('content'),
                                  due=due,
                                  taskid=self.get('id')), end='')

class TaskSet(list):
    COLORS = {
        'project': colors.PROJECT,
        'unknown': colors.ENDC,
        'set': colors.ENDC
    }
    FORMAT = {
        'project': '{color}#{project_name}\n',
        'unknown': '',
    }
    FILTERS = {
        'gte': lambda val: (lambda item: item.due_date >= val),
        'lte': lambda val: (lambda item: item.due_date <= val),
        'gt': lambda val: (lambda item: item.due_date > val),
        'lt': lambda val: (lambda item: item.due_date < val),
        'eq': lambda val: (lambda item: item.due_date == val),
        'neq': lambda val: (lambda item: item.due_date != val),
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

    def select(self, order=None, reverse=False, search=None, **filters):
        if search:
            filters['search'] = search
        filtered = self
        for filtername, filterval in filters.items():
            filtered = filter(TaskSet.FILTERS[filtername](filterval), filtered)
        if order:
            return list(sorted(filtered, key=lambda task: task[order],
                               reverse=reverse))
        return filtered
        
    def pprint(self, **options):
        color = TaskSet.COLORS[self.set_type]
        print(TaskSet.FORMAT[self.set_type].format(color=color,
                                                   **self.raw), end='')
        for task in self.select(**options):
            task.pprint()
        
        
class ResultSet:
    def __init__(self, result, name=None):
        self.task_sets = []
        self.tasks = TaskSet()
        self.name = name
        for resultset in result:
            for item in resultset['data']:
                if item.get('content'):
                    self.tasks.append(Task(item))
                else:
                    self.task_sets.append(TaskSet(item))
                    
    def pprint(self, search=None, filters=None, order=None):
        if self.name:
            print('{}{}\n{}{}\n'.format(colors.FILTER, self.name,
                                    ''.join('=' for _ in self.name),
                                    colors.ENDC))
        for task_set in self.task_sets:
            task_set.pprint()
        if self.tasks:
            self.tasks.pprint()
