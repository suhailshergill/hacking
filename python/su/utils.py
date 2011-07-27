import os
import pwd
import socket
import sys

from collections import namedtuple

HOME = os.getenv('HOME')
CWD = os.getcwd()

###########
# options #
###########
defaultOptions = {
    'debug' : False,
    }

def getOptionParser(args='',debug=defaultOptions['debug']):
    from optparse import OptionParser
    parser = OptionParser(usage="$> python %s [options] %s" %
                          (os.path.basename(getCallingFileName(2)),args))
    parser.add_option('-d', '--debug',
                      action="store_true", dest="debug", default=debug,
                      help="Toggle debugging")
    return parser

def getOptions(debug=defaultOptions['debug']):
    Options = namedtuple('Options',defaultOptions.keys())
    return Options(defaultOptions.values())


###########
# generic #
###########
class Error(Exception):
    def __init__(self, value):
        self.value = value
        if defaultOptions['debug']:
            import ipdb; ipdb.set_trace()
    def __str__(self):
        return repr(self.value)

def getCallingFileName(depth=1):
    """get filename of function at depth

    ARGUMENTS:
    * depth: defaults to 1, i.e., get filename of function calling this function
    """
    import inspect
    return changeFileExtension(inspect.getfile(sys._getframe(depth)),'py')



###########
# file IO #
###########

def readlines(fileName):
    """removes trailing newlines in lines read from file
    """
    ans = []
    with open(fileName,'r') as ifile:
        for line in ifile:
            ans.append(line.strip())
    return ans

def getOwner(fileName):
    return pwd.getpwuid(os.stat(fileName).st_uid).pw_name

def getGroup(fileName):
    return pwd.getpwuid(os.stat(fileName).st_gid).pw_name

def getDefaultOwner():
    return getOwner(HOME)

def getDefaultGroup():
    return getGroup(HOME)

#######################
# configuration stuff #
#######################
def __getImmediateRoot(depth=1):
    """*DEPRECATED*

    get the immediateRoot of the project. We assume then, that the calling
    file is actually an executable and assembled under the hierarchy of
    immediateRoot/bin/callingFileName.

    ARGUMENTS:
    * depth: how far down the call-stack do we have to insepct. defaults to 1
    """
    callingFileName = os.path.realpath(getCallingFileName(depth+1))
    return os.path.abspath(os.path.join(callingFileName,'..','..'))


def findNearestFile(fileName):
    """scans up from pwd to / all the while looking to see if a fileName
    exists. if it finds one it outputs that directory, if not it outputs None.
    """
    curr = CWD
    while curr != "/":
        if os.path.isfile(os.path.join(curr,fileName)):
            break
        curr = os.path.dirname(curr)
    else:
        curr = None
    return curr

def findNearestRoot():
    """scans up from pwd to / all the while looking to see if a .venv file
    exists. if it finds one it outputs that directory, if not it outputs the
    current working directory.
    """
    ans = findNearestFile('.venv')
    return ans if ans else CWD

def findNearestSconsRoot():
    """finds nearest SConstruct file lying at or below 'root'. else returns None
    """
    scons = findNearestFile('SConstruct')
    if scons:
        root = findNearestRoot()
        if os.path.commonprefix([scons,root])!=root:
            scons = None
    return scons


####################
# publishing files #
####################

# POA: ok so need to give the actual publishing function a generator which
# generates file object and tells where/how to move it, what the file
# permissions should be like. we specify the format of 'rules' here. we also
# have interpreters of those rules here. so it's all in one place. then for
# convenience we also define a default rule one which we can use for sandboxed
# stuff. and then we're done, voila

def getPublishingPatterns(patterns=[]):
    PublishingPattern = namedtuple('PublishingPattern',
                                   ['src',
                                    'chmod',
                                    'owner',
                                    'group',
                                    ])
    cwd = CWD
    patternPrefix = cwd
    if not patterns:
        root = findNearestRoot()
        patternFileName = 'publish.patterns'

        patternFile = os.path.join(root,patternFileName)
        if os.path.exists(patternFile):
            patternPrefix = root
        else:
            patternFile = os.path.join(cwd,patternFileName)
        if os.path.exists(patternFile):
            patternPrefix = cwd

        patterns = readlines(patternFile)
    else:
        patterns = [x.strip() for x in patterns]
    for pattern in patterns:
        patternBits = pattern.split(':')
        src = os.path.join(patternPrefix,patternBits[0])
        if src.split(os.path.sep)[-1] in ['.','..']:
            continue
        else:
            src = os.path.abspath(src)
        try:
            chmod = int(patternBits[1])
        except:
            chmod = None
        try:
            owner = patternBits[2]
        except:
            owner = getDefaultOwner()
        try:
            group = patternBits[3]
        except:
            group = getDefaultGroup()
        yield PublishingPattern(src,chmod,owner,group)

def NonNill(*positions):
    def inner(f):
        def innerFun(*args):
            relevantArgs = [args[i] for i in positions]
            if len(relevantArgs) != len(filter(None,relevantArgs)):
                raise Error("Arg at positions %s in function %s must be non-nill"%(str(positions),f.__name__))
            return f(*args)
        return innerFun
    return inner

def NormalizePublishingRuleArgs(f):
    def innerFun(self,src,*args):
        root = findNearestRoot()
        new_args = []
        def _foo(x,y):
            new_arg = None
            if hasattr(y,'__call__'):
                new_arg = y(x,root)
            else:
                new_arg = y
            new_args.append(new_arg)
        _foo(src,args[0])
        for arg in args[1:]:
            _foo(new_args[0],arg)
        return f(self,src,*new_args)

    return innerFun

class PublishingRule(object):
    """class used to encapsulate all the information needed to 'publish' a
    certain kind of file.
    """
    defaultOwner = ':'.join((getDefaultOwner(),getDefaultGroup()))

    @NormalizePublishingRuleArgs
    @NonNill(1,2)
    def __init__(self, src, dest, symbolic, chmod, owner):
        self.src = src
        self.dest = dest
        self.symbolic = symbolic
        self.chmod = chmod
        self.owner = owner

    def __str__(self):
        return str(self.__dict__)

    def publish(self, debug=True):
        def _runCommand(command_line):
            print command_line
            if not debug:
                os.system(command_line)           
                
        import subprocess, shlex
        command_line = None
        commandPrefix,command = '',''
        if self.owner != PublishingRule.defaultOwner:
            commandPrefix = 'sudo '
        if self.symbolic:
            command = 'ln -s'
        else:
            command = 'cp'
        command_line = '%s%s "%s" "%s"'%(commandPrefix,command,self.src,self.dest)
        # FIXME: delete this case. only using it temporarily
        # if os.path.exists(self.dest) and not os.path.islink(self.dest):
        #     os.system('%scp "%s" "%s"'%(commandPrefix,self.dest,self.src))
        #     if commandPrefix:
        #         os.system('%schown shergill:shergill %s'%(commandPrefix,self.src))
        # elif os.path.exists(self.dest):
        if os.path.exists(self.dest) and os.path.islink(self.dest):
            _runCommand('%srm "%s"'%(commandPrefix,self.dest))

        _runCommand(command_line)
        if self.chmod:
            command_line = '%schmod %d "%s"'%(commandPrefix,self.chmod,self.dest)
            _runCommand(command_line)
        if self.owner != PublishingRule.defaultOwner:
            command_line = '%schown %s "%s"'%(commandPrefix,self.owner,self.dest)
            _runCommand(command_line)

def isFileAdded(fileName):
    return os.path.exists(os.path.abspath(pattern))

def isFileRemoved(fileName):
    return not isFileAdded(fileName)


################
# web browsing #
################
def getBrowser(debug=defaultOptions['debug']):
    """Get an instantiation of a mechanize browser with custom settings.

    ARGUMENTS:
    * debug: print debugging output
    """
    import mechanize
    import cookielib

    # Browser
    br = mechanize.Browser()

    # Cookie Jar
    cj = cookielib.LWPCookieJar()
    br.set_cookiejar(cj)

    # Browser options
    br.set_handle_equiv(True)
    br.set_handle_gzip(False)
    br.set_handle_redirect(True)
    br.set_handle_referer(True)
    br.set_handle_robots(False)

    # Follows refresh 0 but not hangs on refresh > 0
    br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=1)

    if debug:
        # Want debugging messages?
        br.set_debug_http(True)
        br.set_debug_redirects(True)
        br.set_debug_responses(True)

        # To make sure you're seeing all debug output:
        import logging
        logger = logging.getLogger("mechanize")
        logger.addHandler(logging.StreamHandler(sys.stdout))
        logger.setLevel(logging.INFO)

    # User-Agent (this is cheating, ok?)
    br.addheaders = [('User-agent', 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1) Gecko/2008071615 Fedora/3.0.1-1.fc9 Firefox/3.0.1')]

    return br



#####################
# format conversion #
#####################
def convertXMLToCSV(inputFile,outputFile):
    from lxml import etree
    # [[http://www.saltycrane.com/blog/2008/11/python-unicodeencodeerror-ascii-codec-cant-encode-character/][source]]
    from django.utils.encoding import smart_str, smart_unicode

    with open(inputFile,'r') as ifile:
        exportedFile = ifile.read()

    exportedRoot = etree.fromstring(exportedFile)
    exportedTable = exportedRoot.getchildren()
    tableName = exportedTable[0].tag
    fieldNames = [x.tag for x in exportedTable[0].getchildren()]
    with open(outputFile,'w') as ofile:
        ofile.write('%s\n'%','.join(['"%s"'%x.replace('"','""') for x in fieldNames]))
        for record in exportedTable:
            fields = record.getchildren()
            assert [x.tag for x in fields] == fieldNames
            try:
                ofile.write('%s\n'%','.join(['"%s"'%(smart_str(x.text).replace('"','""')) if x.text else '' for x in fields]))
            except Exception as e:
                import ipdb; ipdb.set_trace()


def changeFileExtension(fileName,newExtension):
    """Returns name of file with the newer extension (magically overwriting
    previous extension if any
    """
    portions = fileName.split('.')
    if len(portions) == 1:
        return '%s.%s'%(fileName,newExtension)
    else:
        portions[-1] = newExtension
        return '.'.join(portions)

