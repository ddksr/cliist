import datetime

from settings import colors, OUTPUT_DATE_FORMAT

class Plain:
    COLORS = {
        'project': colors.PROJECT,
        'unknown': colors.ENDC,
        'set': colors.ENDC
    }
    FORMAT = {
        'task': '{c0}{indent}  -{taskid:>9} {c2}{due}{priority}{c1}{content}{c0}\n',
        'project': '{color} #{project_name}\n',
        'unknown': '',
    }
    
    @staticmethod
    def task(obj):
        indent = '  ' * (int(obj.get('indent', '1')) - 1)
        priority = '  '
        if obj.priority and obj.priority != 1:
            priority = '{}{}{} '.format(colors.PRIORITY,
                                        (5 - obj.priority),
                                        colors.ENDC)
        due = obj.get_date()
        if due:
            due += ' '
        print(Plain.FORMAT['task'].format(c0=colors.ENDC,
                                          c1=colors.CONTENT,
                                          c2=colors.DATE,
                                          indent=indent,
                                          priority=priority,
                                          content=obj.get('content'),
                                          due=due,
                                          taskid=obj.get('id')), end='')

    @staticmethod
    def task_set(obj):
        color = Plain.COLORS[obj.set_type]
        print(Plain.FORMAT[obj.set_type].format(color=color,
                                                **obj.raw), end='')
        for task in obj:
            Plain.task(task)

    @staticmethod
    def result_set(obj):
        if obj.name:
            print('{}{}\n{}{}'.format(colors.FILTER, obj.name,
                                      ''.join('=' for _ in obj.name or ''),
                                      colors.ENDC))
        for task_set in obj.task_sets:
            Plain.task_set(task_set)
        if obj.tasks:
            Plain.task_set(obj.tasks)

class Org:
    PRIORITY = { 1: '', 2: 'C', 3: 'B', 4: 'A' }
    DATE = 'DEADLINE: <{} {}>'
    NAMES = {
        'project': '{project_name}',
        'unknown': '',
    }
    
    @staticmethod
    def task(obj, level=2):
        stars = ('*' * (level - 1)) + ('*' * (int(obj.get('indent', '1'))))
        indent = ' ' * (len(stars) + 1)
        priority = Org.PRIORITY[obj.priority or 1]
        due = obj.due_date and Org.DATE.format(obj.due_date.date().isoformat(),
                                               obj.due_date.strftime("%A")[:3])
        props = {
            'TaskID': obj.get('id'),
            'Recurring': obj.is_recurring and 'yes' or 'no',
        }
        if obj.labels:
            props['Labels'] = ', '.join(map(str, obj.labels))
        if obj.project:
            props['Project'] = obj.project
        if obj.date_string:
            props['DateString'] = obj.date_string

        print('{} {} {}{}'.format(stars,
                                  'DONE' if obj.checked else 'TODO',
                                  '[#{}] '.format(priority) if priority else '',
                                  obj.content))
        if due:
            print(indent + due)
        print(indent + ':PROPERTIES:')
        prop_len = max(len(val) for val in props.keys()) + 3
        for prop, value in props.items():
            prop_value = ('{:<' + str(prop_len) + '}{}').format(':{}:'.format(prop),
                                                                value)
            print(indent + prop_value)
        print(indent + ':END:')

    @staticmethod
    def task_set(obj, level=1):
        name = Org.NAMES[obj.set_type].format(**obj.raw)
        if name:
            print('{} {}'.format('*' * level, name))
        for task in obj:
            Org.task(task, level=(level+1) if name else level)

    @staticmethod
    def result_set(obj):
        level = 1
        if obj.name:
            level = 2
            print('* ' + obj.name)
        for task_set in obj.task_sets:
            Org.task_set(task_set, level=level)
        for task in obj.tasks:
            Org.task(task, level=level)
            
formaters = {
    'plain': Plain,
    'org': Org
}
