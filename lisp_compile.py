"""
19 Dec 2012

compile all .el files in here
"""

from os import walk, getcwd, chdir
from os.path import exists
from subprocess import Popen, PIPE
from time import sleep

COMPILE = "emacs -batch -L {} -f batch-byte-compile {}"

REPOS = [
    {'dir'   : 'lisp/emacs-jedi',
     'repo'  : 'https://github.com/tkf/emacs-jedi.git',
     'branch': 'master',
     'python': False},
    {'dir'   : 'lisp/emacs-helm',
     'repo'  : 'https://github.com/emacs-helm/helm.git',
     'branch': 'master',
     'python': False},
    {'dir'   : 'lisp/popup-el',
     'repo'  : 'https://github.com/auto-complete/popup-el.git',
     'branch': 'master',
     'python': False},
    {'dir'   : 'lisp/highlight-indent', 
     'repo'  : 'https://github.com/antonj/Highlight-Indentation-for-Emacs.git',
     'branch': 'master', 
     'python': False},
    {'dir'   : 'lisp/ctable', 
     'repo'  : 'https://github.com/kiwanami/emacs-ctable.git',
     'branch': 'master', 
     'python': False},
    {'dir'   : 'lisp/mocker', 
     'repo'  : 'https://github.com/sigma/mocker.el.git',
     'branch': 'master', 
     'python': False},
    {'dir'   : 'lisp/epc', 
     'repo'  : 'https://github.com/kiwanami/emacs-epc.git',
     'branch': 'master', 
     'python': False},
    {'dir'   : 'lisp/ertx', 
     'repo'  : 'https://github.com/emacsmirror/ert-x.git',
     'branch': 'builtin', 
     'python': False},
    {'dir'   : 'lisp/autopair', 
     'repo'  : 'https://github.com/capitaomorte/autopair.git',
     'branch': 'master', 
     'python': False},
    {'dir'   : 'lisp/deferred', 
     'repo'  : 'https://github.com/kiwanami/emacs-deferred.git',
     'branch': 'master', 
     'python': False},
    {'dir'   : 'lisp/auto-complete', 
     'repo'  : 'https://github.com/auto-complete/auto-complete.git',
     'branch': 'master', 
     'python': False},
    {'dir'   : 'python_lib/python-epc', 
     'repo'  : 'https://github.com/tkf/python-epc.git',
     'branch': 'master', 
     'python': True},
    {'dir'   : 'python_lib/jedi', 
     'repo'  : 'https://github.com/davidhalter/jedi.git',
     'branch': 'master', 
     'python': True}]

HERE = getcwd()

LIBS = [HERE + '/lisp',
        HERE + '/lisp/deferred/',
        HERE + '/lisp/emacs-helm/',
        HERE + '/lisp/ctable/',
        HERE + '/lisp/popup-el/',
        HERE + '/lisp/auto-complete/',
        HERE + '/lisp/emacs-jedi/',
        HERE + '/lisp/mocker/',
        HERE + '/lisp/ertx/',
        HERE + '/lisp/autopair/',
        HERE + '/lisp/epc/']

def clone_repo(repo):
    return Popen('git clone {} {}'.format(repo['repo'], repo['dir']), stdout=PIPE,
                 stderr=PIPE, shell=True)


def pull_repo(repo):
    chdir(repo['dir'])
    p = Popen('git pull origin {}'.format(repo['branch']), stdout=PIPE,
              stderr=PIPE, shell=True)
    chdir(HERE)
    return p


def check_repos():
    processes = []
    build = False
    for repo in REPOS:
        if exists(repo['dir']+'/.git'):
            processes.append((repo, pull_repo(repo)))
        else:
            processes.append((repo, clone_repo(repo)))
            if repo['python']:
                build = True
    while processes:
        for proc in processes:
            if not proc[1].poll() is None:
                msgs = proc[1].communicate()
                print 'checking git repo {:<17}'.format(
                    proc[0]['dir'].split('/')[-1]),
                if not "Already up-to-date." in msgs[0]:
                    if 'fatal' in msgs[1]:
                        print '\033[31mFAILED\033[m'
                    else:
                        print '\033[33mUPDATED\033[m'
                    if proc[0]['python']:
                        build = True
                else:
                    print '\033[32mOK\033[m'
                processes.pop(processes.index(proc))
                break
    if not build:
        return
    print '\nInstalling python packages',
    Popen('sudo -s', shell=True,
          stdin=PIPE, stdout=PIPE, stderr=PIPE).communicate()
    for repo in REPOS:
        if repo['python']:
            print repo['dir'].split('/')[-1]
            chdir(repo['dir'])
            Popen('sudo python setup.py install', shell=True)
            chdir(HERE)
    Popen('exit', shell=True).communicate()
    while True:
        sleep(2)
        if not None in [p.poll() for p in processes]:
            break


def lisp_compile():
    lib = ' -L '.join(LIBS)
    processes = []
    for path, _, lisps in walk('.'):
        for lisp in lisps:
            if not lisp.endswith('.el'):
                continue
            if lisp.startswith('test-'):
                continue
            log = '=' * 80 + '\n'
            log += COMPILE.format(lib, '/'.join((path, lisp))) + '\n'
            log += '-' * 80 + '\n'
            processes.append((log, Popen(COMPILE.format(lib, path+'/'+lisp),
                                         shell=True, stdout=PIPE, stderr=PIPE)))
    while processes:
        for proc in processes:
            if not proc[1].poll() is None:
                _, err = proc[1].communicate()
                if 'Error' in err:
                    print proc[0]
                    print err
                processes.pop(processes.index(proc))
                break
    chdir(HERE + '/..')
    

def main():
    """  https://github.com/tkf/python-epc.git python_lib/python-epc
    main function
    """
    print '\nRetrieving last versions from github...'
    check_repos()
    
    print '\nCompiling lisp...'
    lisp_compile()
    
    print '\nThe End\n\n'


if __name__ == "__main__":
    exit(main())
