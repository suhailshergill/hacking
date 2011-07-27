import os
import socket

import su.utils

defaultOptions = su.utils.getOptions()
os.chdir(su.utils.findNearestRoot())
_config = __import__('config')
os.chdir(su.utils.CWD)


class Config(_config.LocalConfig):
    """This is the global Configuration class which dynamically derives from the
    pertinent local configuration.
    """

    def __init__(self, *args, **kw):
        Config.root = su.utils.findNearestRoot()
        Config.CWD = su.utils.CWD
        Config.HOME = su.utils.HOME
        Config.sconsRoot = su.utils.findNearestSconsRoot()
        Config.hostname = socket.gethostname()
        Config.options = defaultOptions
        Config.utils = su.utils

        super(Config, self).__init__(*args, **kw)
        setattr(self,'__doc__',super(Config,self).__doc__)
