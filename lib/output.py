import datetime

from settings import colors
import xml.etree.ElementTree as ET

class Plain:
    COLORS = {
        'project': colors.PROJECT,
        'unknown': colors.ENDC,
        'set': colors.ENDC
    }
    FORMAT = {
        'task': '{c0}{indent}{c5}{priority:>3.3} {c1}{content}{c0}\n        {c3}{project_name:26.26}{c4} {label_names:26.26} {c2}Due: {due:12.12}\n{c0}',
        'project': '\n{color}#{project_name}\n',
        'unknown': '',
    }
    
    @staticmethod
    def task(obj):
        indent = '  ' * (int(obj.get('indent', '1')) - 1)
        priority = '  '
        if obj.priority and obj.priority != 1:
            priority = '!' * (obj.priority - 1)
        due = obj.get_date()
        if due:
            due += ' '
        print(Plain.FORMAT['task'].format(c0=colors.ENDC,
                                          c1=colors.CONTENT,
                                          c2=colors.DATE,
                                          c3=colors.PROJECT,
                                          c4=colors.LABEL,
                                          c5=colors.PRIORITY,
					  indent=indent,
                                          priority=priority,
                                          content=obj.get('content'),
                                          project_name=obj.get('project_name'),
                                          label_names=obj.get('label_names'),
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

class Alfred:
    @staticmethod
    def task(items, obj):
        item = ET.SubElement(items, 'item')
        item.set('uid', str(obj.get('id')))
        item.set('arg', str(obj.get('id')))

        title = ET.SubElement(item, 'title')
        title.text = obj.content

    @staticmethod
    def task_set(items, obj):
        for task in obj:
            Alfred.task(items, task)

    @staticmethod
    def result_set(obj):
         items = ET.Element('items')
         for task_set in obj.task_sets:
             Alfred.task_set(items, task_set)
         for task in obj.tasks:
             Alfred.task(items, task)
         ET.dump(items)

formaters = {
    'plain': Plain,
    'org': Org,
    'alfred': Alfred
}
