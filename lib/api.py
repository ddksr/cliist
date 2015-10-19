import urllib.parse
import urllib.request
import json

from .utils import CliistException

from settings import API_TOKEN

API_URL = 'https://api.todoist.com/API'

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
    except Exception:
        raise CliistException('Error connecting to Todoist API')


