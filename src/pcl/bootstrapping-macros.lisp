;;;; this contains some bootstrapping macros and some support for
;;;; working with them.  it may be refactored at some point later but
;;;; was created in august 2010 in an attempt to centralize the
;;;; bootstrapping tricks used by PCL, like function renaming and the
;;;; like

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB!PCL")

;;;; Historically, there have been a few specific types of workarounds
;;;; used to define one set of functionality for bootstrapping, and
;;;; another for the real world.  Here we attempt to make the old,
;;;; undocumented system formal with a series of macros.  We avoid
;;;; top-level forms that do tricky things in favor of defining a
;;;; macro here, where we document the trickiness we are doing
;;;; extensively.

;;;; Function definition workarounds:
;;;;
;;;; 1.  Define some generic functions early with DEFUN and then
;;;; replace them with true generics, but keep the same funcallable
;;;; instance.
;;;;
;;;; 2.  Define some generic functions early with DEFUN and then
;;;; replace them by making the original fmakunbond and rebinding it
;;;; to the real definition
;;;;
;;;; 3.  Early methods are yet another scheme going in the system
;;;; right now.

;;;; Circularity workarounds
;;;;
;;;; 1.  Define safe versions of functions that would otherwise recur
;;;; infinitely

;;;; We also have newfounded workarounds now that we are loading PCL,
;;;; at least partially, into a host lisp.
;;;;
;;;; 1.  Instead of using cl:find-class (and other cl package
;;;; exports), we use sb-xc:find-class to avoid collisions.  These
;;;; functions and macros are defined, perhaps with documented
;;;; quirkiness, to function on both the host and target.


(defvar *!early-function-specs*
  ;; each member is a list of (function-name early-function-name late-function-name)
  '((make-a-method  early-make-a-method real-make-a-method)
    (add-named-method early-add-named-method real-add-named-method)))

(defmacro define-early-function ((name early-name real-name) &body options)
  #!+sb-doc
  "Defines a function named NAME that is initially fbound to the
function EARLY-NAME but when !FIX-EARLY-GENERIC-FUNCTIONS is run
becomes fbound to REAL-NAME.

OPTIONS is a list where each item's car is either :early or :late, in
which case it a DEFUN form is expanded with a name of either
EARLY-NAME or REAL-NAME.  The rest of the option corresponds to the
rest of the forms passed to DEFUN."
  `(progn
     (declaim (notinline ,name))
     (setf *!early-function-specs*
           (cons (list ',name ',early-name ',real-name)
                 (remove ',name *!early-function-specs* :key #'car)))
     ,@(mapcar #'(lambda (option)
                   (ecase (car option)
                     ((:early :late)
                        `(defun ,(if (eql :early (car option)) early-name real-name)
                             ,@(rest option)))))
               options)))

;;; The historical boostrapping phase had this property:
;;;
;;; The cheap generic functions are built using the same
;;; FUNCALLABLE-INSTANCE objects that real generic functions are made
;;; out of.  This means that as PCL is being bootstrapped, the cheap
;;; generic function objects which are being created are the same
;;; objects which will later be real generic functions. This is good
;;; because:
;;;
;;;   - we don't cons garbage structure, and
;;;   - we can keep pointers to the cheap generic function objects
;;;     during booting because those pointers will still point to
;;;     the right object after the generic functions are all fixed up.




