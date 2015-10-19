import os
import json

from .utils import CliistException

try:
    from settings import CACHE_ENABLED, CACHE
except:
    CACHE_ENABLED, CACHE = False, ''

_cache = None
def get(name):
    global _cache
    return _cache.get(name)

def set(name, val):
    global _cache
    _cache[name] = val

def load():
    global _cache
    if not CACHE_ENABLED or not os.path.exists(CACHE):
        _cache = {}
        return
    try:
        with open(CACHE, 'r') as fd:
            _cache = json.loads(fd.read())
    except:
        raise CliistException('Error loading _cache')
    
def save():
    if not CACHE_ENABLED:
        return
    try:
        with open(CACHE, 'w') as fd:
            fd.write(json.dumps(_cache))
    except:
        raise CliistException('Error saving _cache')
