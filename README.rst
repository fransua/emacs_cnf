=====
EMACS CONFIG
=====

Packages:
---------
* emacs-jedi
...

lisp:
-----
* ert.el: something for testing... not really used
* el-expectations: something for testing... not really used

Lisp Compiler:
--------------

Walks through lisp files and compile them.


Manual changes for perfect compilation
--------------------------------------

Errors:
.......

* deferred-samples.el:144:1:Error: Symbol's function definition is void: gensym
   ::

     perl -p -i -e 's/gensym/cl-gensym/g' lisp/deferred/*.el
     perl -p -i -e "s/(require 'cl\)\)?\n)/\$1\(require 'cl-lib\)\n/g" lisp/deferred/*.el

* lisp/ctable/samples/large-table.el:54:62:Error: &rest without variable name
   ::

     perl -p -i -e 's/rest/rest ignored/g' lisp/ctable/samples/large-table.el
