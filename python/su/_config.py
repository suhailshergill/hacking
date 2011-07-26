"""This module should not be imported as is. It should be loaded with the
su.utils.config_init() function.

KTHXBAI
"""

from glob import glob
from collections import namedtuple
import os
import sys

import su.utils

local_config = None

def publish(fileName,pattern):
    """perform 'publishing' actions on files corresponding to patterns

    ARGUMENTS:
    * fileName: one of the files matching pattern
    * pattern: glob-like pattern
    """
    if hasattr(local_config,'publish'):
        local_config.publish(fileName,pattern)

