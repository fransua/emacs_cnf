"""
19 Dec 2012

compile all .el files in here
"""

from os import walk, getcwd, chdir, listdir
from subprocess import Popen, PIPE
from time import sleep

COMPILE = "emacs -batch -L {} -f batch-byte-compile {}"

REPOS = [{'repo': 'https://github.com/tkf/emacs-jedi.git',
          'dir': 'lisp/emacs-jedi', 'py': False},
         {'repo': 'git clone https://github.com/antonj/Highlight-Indentation-for-Emacs.git',
          'dir': 'lisp/highlight-indent', 'py': False},
         {'repo': 'https://github.com/kiwanami/emacs-epc.git',
          'dir': 'lisp/epc', 'py': False},
         {'repo': 'https://github.com/capitaomorte/autopair.git',
          'dir': 'lisp/autopair', 'py': False},
         {'repo': 'https://github.com/kiwanami/emacs-deferred.git',
          'dir': 'lisp/deferred', 'py': False},
         {'repo': 'https://github.com/auto-complete/auto-complete.git',
          'dir': 'lisp/auto-complete', 'py': False},
         {'repo': 'https://github.com/tkf/python-epc.git',
          'dir': 'python_lib/python-epc', 'py': True},
         {'repo': 'https://github.com/davidhalter/jedi.git',
          'dir': 'python_lib/jedi', 'py': True}]
         
HERE = getcwd()

def clone_repo(repo):
    return Popen('git clone %s %s' % (repo['repo'], repo['dir']), shell=True)


def pull_repo(repo):
    chdir(repo['dir'])
    p = Popen('git pull origin master', shell=True)
    chdir(HERE)
    return p


def check_repos():
    processes = []
    for repo in REPOS:
        if '.git' in listdir(repo['dir']):
            processes.append(pull_repo(repo))
        else:
            processes.append(clone_repo(repo))
        if repo['py']:
            chdir(repo['dir'])
            processes.append(Popen('sudo python setup.py install', shell=True))
            chdir(HERE)
    while True:
        sleep(2)
        if not None in [p.poll() for p in processes]:
            break


def main():
    """  https://github.com/tkf/python-epc.git python_lib/python-epc
    main function
    """
    check_repos()

    libs = [HERE + '/lisp',
            HERE + '/lisp/deferred/',
            HERE + '/lisp/auto-complete/',
            HERE + '/lisp/emacs-jedi/',
            HERE + '/lisp/epc/']
    lib = ' -L '.join(libs)
    for path, _, lisps in walk('.'):
        for lisp in lisps:
            if not lisp.endswith('.el'):
                continue
            _, err= Popen(COMPILE.format(lib, path+'/'+lisp),
                            shell=True, stdout=PIPE, stderr=PIPE).communicate()
            print '=' * 80
            print COMPILE.format(lib, '/'.join((path, lisp)))
            print '-' * 80
            if 'Error' in err:
                print err
    chdir(HERE + '/..')
            


if __name__ == "__main__":
    exit(main())
