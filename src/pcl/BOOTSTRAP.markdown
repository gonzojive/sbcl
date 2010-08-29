# The bootstrap

## What's the big picture?

The bootstrapping process goes through four distinct states denoted by
the variable `**boot-state**`: nil, 'early, 'braid, and 'complete.
After `boot.lisp` has loaded, the boot state

* nil -> early: boot.lisp
* early -> braid: braid.lisp
* braid -> complete: fixup.lisp

What do each of these states mean? NIL means very little has been set
up so far.  After `defs.lisp` has loaded, information about the
standard CLOS classes has been stored in structures for further
processing.  In addition, `class.lisp` (a non-pcl file) declared
so-called built-in classes which defclass also processes further.  All
this happens while still in the NIL boot stage.

`boot.lisp` defines the concept of an "early generic function", which
is a regular defun early and is later (setf (gdefinition 'fun)
real-fun) before generic functions are set up, and then replaced by
`!fix-early-generic-functions` which is called when `fixup.lisp` is
executed.  The original functions are defined by `defuns` and later
replaced according to `*!generic-function-fixupes*` which will replace
the entire function with a generic, and then establish methods with
function bodies provided by functions that have been defuned.
`boot.lisp` also defines so-called 'early functions' which are regular
defuns later replaced by defuns (see `*!early-functions*`).  Though at
least `load-defclass` is replaced by other means.


A number of functions that will eventually be implemented as generic
functions are first implemented by defun.

For example, `(setf (gdefinition 'load-defclass) #'real-load-defclass)` 
is used to set the real definition for load-defclass after the 
bootstrap is mostly complete.  

## What files are relevant to the bootstrap?

`defclass.lisp` has support for early class definitions (including an
`early-class-definition` structure -- ECD -- implemented as a list).



# Files

## declarations.lisp

Specials proclaimations for PCL.  Not all specials are declared here but a lot of them are


## early-low.lisp

A few random utility functions like `defstruct-classoid-p`, `structure-type-p` and `format-symbol`

## macros.lisp

Mostly utility macros like `dolist-carefully` and `doplist`, though
`find-class`, `find-class-from-cell` are also implemented here for
some reason.


## compiler-support.lisp

This is the only file that starts out with `(in-package "SB!C")`
rather than `SB!PCL`.  Does things like

* set up function name syntax so that PCL can functions things like
  '(fast-method xxx)

* define some type information and transformation information for the
  compiler to optimize a few SBCL functions
  
* defines "source contexts" for introspecting

## low.lisp

Defines the following structures:

*  `wrapper` (`def!struct`)
*  `standard-funcallable-instance` (`!defstruct-with-alternate-metaclass`)
   * 
*  `standard-instance` (`!defstruct-with-alternate-metaclass`)
*  `%method-function` (`!defstruct-with-alternate-metaclass`)

Also contains a bunch of support functions and macros for working with
funcallable instances (FSC stands for funcallabe standard class) and
standard-instances.
     
Scattered, poorly documented functions/macros:

* `set-fun-name`
* `fsc-instance-*` 
* `std-instance-*`
* `get-slots`, `get-wrapper`
* `structure-type-slot-description-list`
* `structure-slotd-name`, `structure-slotd-reader-function`, etc.
* `precompile-random-code-segments` ???

## slot-name.lisp

Defines how slot accessor functions are named by PCL, both on the host
and target

## defclass.lisp

Defines the defclass macro

## defs.lisp

## fngen.lisp

## wrapper.lisp

The PCL version of a layout.  Actually a wrapper inherits from layout.

## cache.lisp

## dlisp.lisp

why is it called dlisp?

## boot.lisp

Boot.lisp is much too big and varid and should be split up to make the
system more understandable.  It defines both defmethod and defgeneric,
and the early and late versions of functions related to almost all
aspects of the system.  Top-level forms are somewhat scattered.  There
are terrible function names like `set-arg-info1` which are totally
undocumented.  This is _the_ file to refactor for understanability in
PCL.

## vector.lisp

Support for storing CLOS instances as arrays.

## slots-boot.lisp

## combin.lisp

## dfun.lisp

Why is it called dfun?

## ctor.lisp

Function generation machinery.

## braid.lisp

A big part of the bootstrapping occurs here.

## dlisp3.lisp

## generic-functions.lisp

## slots.lisp

## init.lisp

## std-class.lisp

## cpl.lisp

## fsc.lisp

## methods.lisp

## fixup.lisp

## defcombin.lisp

## ctypes.lisp

## env.lisp

## documentation.lisp

## print-object.lisp

## precom1.lisp

## precom2.lisp


