cliist
======

Todoist commandline client is an asynchronous client for the Todoist todo-list web application.
It currently supports:
- addint/updating tasks
- marking tasks complete
- listing all tasks
- listing project tasks
- queryinig using Todoist query format
- listing all projects and labels

The client is currently tested only with Todoist Premium.

## Instalation
Run installation script and type in your API token:
`./install.sh`

## Examples

### List all tasks for today and that are overdue
Input: `./cliist.py`

Output:

```
Overdue and today
=================
- 12345677 05.07.2014 @ 21:59:59 task 1 that was overdue
- 12345678 06.07.2014 @ 21:59:59 task 2 for today
```

### List the tasks with a search string
Input: `./cliist.py that`

Output:

```
Overdue and today
=================
- 12345677 05.07.2014 @ 21:59:59 task 1 that was overdue
```

### List all projects
Input: `./cliist.py -P`

Output:

```
#Inbox
#Proj1
#Proj2
#Proj3
  #Proj3.1
  #Proj3.2
  #Proj3.3
#Proj4
```

### Add a task for project Proj 1 with doe date today and label @happy and first priority
Input: `./cliist.py -a a very important task @happy #Proj1 %%4` (cliist uses %%4 instead of !!4 because ! is a special character in bash).

### Query 
Input: `./cliist.py -q @happy`

Output:

```
@happy
=================
- 12345677 !1 a very important task
- 12345678 12.07.2014 @ 21:59:59 another happy task
```

