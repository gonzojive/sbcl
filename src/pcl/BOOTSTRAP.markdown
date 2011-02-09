# The bootstrap

## What's the big picture?

The bootstrapping process goes through four distinct states denoted by
the variable `**boot-state**`: nil, 'early, 'braid, and 'complete.
The transition between these states happens in the following files:

* nil -> early: boot.lisp
* early -> braid: braid.lisp
* braid -> complete: fixup.lisp

What do each of these states mean? NIL means very little has been set
up so far.  After `defs.lisp` has loaded, information about the
standard CLOS classes has been stored in structures for further
processing.  In addition, `class.lisp` (a non-pcl file) declared
so-called built-in classes which defclass also processes further.  All
this happens while still in the NIL boot stage.

`boot.lisp` defines a number of functions by DEFUN which are really
supposed to be generic functions.  These functions are later "fixed
up" during the transition from braid -> complete, but boot.lisp mostly
sets up `defgeneric` and `defmethod` so that they will expand
properly.  Once these macros have been set up, the boot state is
changed to 'early, which essentially means that PCL has defined all of
the early classes along with the macros for defining early
generics/methods.  At this point everything is still a non-CLOS
object: we are using early structure representations of the objects
while in the 'early state.

`braid.lisp` takes the early representations of classes, slots,
accessors, and class predicates and creates the actual metaobjects out
of them.  It leaves the list of generic function instances alone,
however, and transitions to the 'braid state.  At the commencement of
'braid, class objects have just become initialized, generic functions
are funcallable but still early, and methods are still in their
early-structure state.  Hence, we are "braiding" between first-class
metaobjects (classes, slots), partially initialized generic functions,
and early methods.  What happens in between 'braid and 'complete is
mostly the definition of many generic functions, and the "real"
implementations of methods that had an early version.

A few other files load while in 'braid.  `generic-functions.lisp`
defines hundreds of generic functions used in PCL and CLOS, while
`slots.lisp, init.lisp, std-class.lisp, fsc.lisp, methods.lisp` define
a number of methods.  All of these methods are early defmethod, which
have restrictions, like the metaclass must be standard-generic-class
but more than a single method is allowed per generic function.
However, all the generics are funcallable, and defmethod installs the
method to be called when the generic is invoked, so everything is
mostly functional except real generic function objects have not yet
been properly initialized, along with their methods. `fixup.lisp`

`!fix-early-generic-functions` creates fully-initialized generics and
methods from the early definitions.  Then in runs a few 'fixup forms
to further process about a dozen of the defgenerics, as defined in
`*!generic-function-fixups*`.

## Bootstrapping in the host

The bootstrapping process could probably take place in the host lisp,
which could then dump everything to the cold image, which could take
further steps to set up CLOS at cold boot.

The strategy for achieving this is so:

It's probably least confusing to rely on PCL to establish actual
usable objects, including funcallable generic functions, rather than
doing things like compiling a generic function using the host's CLOS
and the target CLOS simultaneously.  Maybe.  It might make more sense
later on, but for now let's make that assumption.

Assuming we were able to get PCL completely loaded (into boot state
'complete) on the host, we would have a full class hierarchy and many
instances of those classes instantiated--in particular standard
generic functions, methods, specializers, and a few others.  It should
be fairly easy to at least dump the class hierarchy, since the class
of each class is either standard-class, built-in-class, or
funcallable-standard-class as specified by the MOP.

The strategy for bootstrapping classes is pretty straightforward:
introduce a form called def!class that, when seen by the host, creates
a representation of a class object used by the cross-compiler, much
like sb-xc:defstruct.  At cold init or genesis, link up find-class to
work and maybe take special care t make circularities point to the right place.

The strategy for bootstrapping functions is a much more complicated
because we must compile code both for the host machine to properly
bootstrap things and then for the target to have the "real"
implementations of everything.

## Road map for implementing cold bootstrapping

1. Define macros named according to the clever things PCL does for
early/late definition of functions

2. Rename every PCL-defined CL symbol to use sb!xc instead.

3. Get host to get through NIL boot state

4. Get host to get through early boot state

5. Get host to get through braid boot state

6. Write genesis/!cold-init code for PCL, and wade into cold init land

## How to develop and debug with minimal pain

A fair amount of the development, at least at first, is making things
happen on the host lisp.  This means we can fire up SLIME and try to
load as much of PCL as possible with `C-c C-l`, the REPL, and friends.
To get to that point, you need to load up sb! code into the host lisp.
To accomplish this, I usually just run make.sh until some terrible
error happens, quit out, and then load `make-host-2.lisp` into the
host lisp.

## What files are relevant to the bootstrap?

`defclass.lisp` has support for early class definitions (including an
`early-class-definition` structure -- ECD -- implemented as a list).
...

# Files

## declarations.lisp

Specials proclamations for PCL.  Not all specials are declared here but a lot of them are


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
funcallable instances (FSC stands for funcallable standard class) and
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

Why is it called dlisp?

## boot.lisp

Boot.lisp is much too big and varied and should be split up to make the
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


