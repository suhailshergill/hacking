import os
import socket

import su.utils

defaultOptions = su.utils.getOptions()
os.chdir(su.utils.findNearestRoot())
_config = __import__('config')
os.chdir(su.utils.CWD)


class Config(object):
    """This is the parent class from which all base configurations derive. In
    c++ lingo this is an abstract class. Specific doc follows:
    ===========================================================================

    """
    __metaclass__ = _config.LocalConfig

    def __init__(self):
        Config.root = su.utils.findNearestRoot()
        Config.CWD = su.utils.CWD
        Config.HOME = su.utils.HOME
        Config.sconsRoot = su.utils.findNearestSconsRoot()
        Config.hostname = socket.gethostname()
        Config.options = defaultOptions
        Config.utils = su.utils

        for propName, propVal in Config.__dict__['__metaclass__'].__dict__.iteritems():
            if not propName.startswith('_'):
                setattr(Config, propName, propVal)
                # Config.__setattr__(self, propName, propVal)
        setattr(Config,'__doc__',Config.__doc__+Config.__dict__['__metaclass__'].__doc__)
