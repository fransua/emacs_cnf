"""
19 Dec 2012

compile all .el files in here
"""

from os import walk, getcwd, chdir
from subprocess import Popen, PIPE

COMPILE = "emacs -batch -L {} -f batch-byte-compile {}"

def main():
    """
    main function
    """
    here = getcwd()
    libs = [here + '/lisp',
            here + '/lisp/deferred/',
            here + '/lisp/auto-complete/',
            here + '/lisp/emacs-jedi/',
            here + '/lisp/epc/']
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
    chdir(here + '/..')
            


if __name__ == "__main__":
    exit(main())
