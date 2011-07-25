#!/usr/bin/python

import pickle
import gnupg
import os
import collections
import subprocess
import sys

HOME=os.getenv('HOME')
keyringFile = os.path.realpath(os.path.join(HOME,".gnupg","python-ring.gpg"))

# subprocess will run the command in sh
# in sh the command source is known as . (dot)
# the env at the end is to dump the environment to stdout
command = ". ./su.keychain_init"

proc = subprocess.Popen(command,
                        stdout = subprocess.PIPE,
                        shell = True,
                        cwd = os.path.join(HOME,'bin'))

for line in proc.stdout:
  (key, _, value) = line.partition("=")
  os.environ[key] = value.strip()

stdout, stderr = proc.communicate()

if proc.returncode is not 0:
    print sys.exit("Error: keychain exited with %d" % proc.returncode)


gpg = gnupg.GPG(use_agent=True)


def storeKeyring(data,keyringFile=keyringFile,gpg=gpg):
    """Stores data into keyring. This *replaces* the existing keyring data from
    that from 'data'. To retain previous entries, make sure that 'data' contains
    keys as obtained by calling the 'loadKeyring' function.

    ARGUMENTS:
    data : collections.defaultdict(dict)
    keyringFile : absolute path to keyring file
    gpg : gnupg.GPG
    """
    pickleData = pickle.dumps(data)
    fingerprint = gpg.list_keys()[0]['fingerprint']
    result = gpg.encrypt(pickleData,fingerprint)

    with open(keyringFile,'w') as ofile:
        ofile.write(str(result))

    os.path.os.chmod(keyringFile, 0600)


def loadKeyring(keyringFile=keyringFile,gpg=gpg):
    """Loads data from keyring.

    ARGUMENTS:
    keyringFile : absolute path to keyring file
    gpg : gnupg.GPG

    RETURNS:
    collections.defaultdict(dict)
    """
    try:
        ifile = open(keyringFile,'r')
        result = gpg.decrypt_file(ifile)
        return pickle.loads(str(result))
    except IOError as e:
        return collections.defaultdict(dict)


def setPassword(domain,username,password,keyringFile=keyringFile,gpg=gpg):
    """Store username,password pair for a domain in keyring
    """
    keyring = loadKeyring(keyringFile,gpg)
    keyring[domain][username] = password
    storeKeyring(keyring, keyringFile, gpg)


def getPassword(domain,username,keyringFile=keyringFile,gpg=gpg):
    """Get password for a particular domain,username
    """
    keyring = loadKeyring(keyringFile,gpg)
    return keyring[domain][username]
