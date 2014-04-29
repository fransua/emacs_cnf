"""
19 Dec 2012

compile all .el files in here
"""

from optparse   import OptionParser
from os         import walk, getcwd, chdir, environ
from os.path    import exists
from subprocess import Popen, PIPE
from time       import sleep


def get_repos():
    repos = []
    for line in open('REPOS.txt'):
        if line.startswith('#'):
            continue
        if line.startswith('name'):
            repos.append({'name': line.split()[1]})
        elif line != '\n':
            key, val = line.split()
            val = True if val == 'True' else \
                  False if val == 'False' else \
                  None if val == 'None' else val
            repos[-1].update([[key, val]])
    return repos


COMPILE = "emacs -batch -L {} -f batch-byte-compile {}"
HERE    = getcwd()
REPOS   = get_repos()

LIBS    = [HERE + '/lisp',
           HERE + '/lisp/deferred/',
           HERE + '/lisp/emacs-python-environment',
           HERE + '/lisp/emacs-helm/',
           HERE + '/lisp/pari/',
           HERE + '/lisp/direx/',
           HERE + '/lisp/ctable/',
           HERE + '/lisp/popup-el/',
           HERE + '/lisp/auto-complete/',
           HERE + '/lisp/emacs-jedi/',
           HERE + '/lisp/mocker/',
           HERE + '/lisp/ertx/',
           HERE + '/lisp/autopair/',
           HERE + '/lisp/epc/']


def clone_repo(repo):
    if repo['branch']:
        return Popen('git clone {} {} -b {}'.format(repo['repo'], repo['dir'],
                                                    repo['branch']),
                     stdout=PIPE, stderr=PIPE, shell=True)
    else:
        chdir(repo['dir'])
        p = Popen('wget -N {}'.format(repo['repo']), stdout=PIPE, stderr=PIPE,
                  shell=True)
        chdir(HERE)
        return p


def pull_repo(repo):
    chdir(repo['dir'])
    p = Popen('git pull origin {}'.format(repo['branch']), stdout=PIPE,
              stderr=PIPE, shell=True)
    chdir(HERE)
    return p


def check_repos():
    processes = []
    build = False
    i = 0
    while True:
        if len(processes) < 4 and i < len(REPOS):
            repo = REPOS[i]
            if exists(repo['dir']+'/.git'):
                processes.append((repo, pull_repo(repo)))
            else:
                processes.append((repo, clone_repo(repo)))
                if repo['python']:
                    build = True
            i += 1
            continue
        for proc in processes:
            if not proc[1].poll() is None:
                msgs = proc[1].communicate()
                print 'checking git repo {:<17}'.format(
                    proc[0]['name']),
                if not "Already up-to-date." in msgs[0]:
                    if 'fatal' in msgs[1]:
                        print '\033[31mFAILED\033[m'
                    elif 'Server file no newer' in msgs[1]:
                        print '\033[32mOK\033[m'
                    else:
                        print '\033[33mUPDATED\033[m'
                        print msgs[0]
                    if proc[0]['python']:
                        build = True
                else:
                    print '\033[32mOK\033[m'
                processes.pop(processes.index(proc))
                break
        if not processes and i == len(REPOS):
            break
    if not build:
        return
    print '\nInstalling python packages',
    try:
        virt = environ['VIRTUAL_ENV']
    except KeyError:
        virt = ''
    for repo in REPOS:
        if repo['python']:
            print repo['dir'].split('/')[-1]
            Popen('mkdir -p ' + repo['dir'], shell=True).communicate()
            chdir(repo['dir'])
            Popen('{}python setup.py install'.format('' if virt else 'sudo '),
                  shell=True).communicate()
            chdir(HERE)
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
    """
    main function
    """
    opts = get_options()
    if not opts.compile:
        print '\nRetrieving last versions from github...'
        check_repos()

    print '\nCompiling lisp...'
    lisp_compile()

    print '\nThe End\n\n'


def get_options():
    '''
    parse option from call
    '''
    parser = OptionParser(
        usage="%prog [options] file [options [file ...]]",
        description="""\
        Check for updates, download them and compile/install lisp and python
        packages.
        MUST be run from .emacs.d directory
        """
        )
    parser.add_option('--compile', dest='compile', action='store_true',
                      default=False,
                      help='compile current packages, no update.')
    return parser.parse_args()[0]

if __name__ == "__main__":
    exit(main())
