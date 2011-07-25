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

def publish(patterns=[]):
    """perform 'publishing' actions on files corresponding to patterns

    ARGUMENTS:
    * patterns: iterable of glob-like patterns
    """
    local_config.publish(patterns)

