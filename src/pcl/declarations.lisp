;;;; Specials are declared on the host as well as the target because
;;;; they are used by many macros and macro helper functions.

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

;;; FIXME: According to the currnet definition of DECLAIM, declaim
;;; expands into a call to `(sb!xc:proclaim ',spec) at compile, load,
;;; and execute.  But sb!xc:proclaim (quite correctly) only declaims
;;; things for the target.  We thus define declaim-host-and-target 
;;;
;;; CL-USER> (sb-c::info :variable :kind 'sb!pcl::*the-class-structure-object*)
;;; :UNKNOWN
;;; NIL
;;; CL-USER> (sb!c::info :variable :kind 'sb!pcl::*the-class-structure-object*)
;;; :SPECIAL
;;; T

;;; We used to use a plain declaim here, but that is now inadequate
;;; because it has somewhat weird behavior when building target code
;;; from the cross-compiler, as we now do for PCL.  Specifically,
;;; DECLAIM was not effecting specials in the host lisp unless (at
;;; least on SBCL) surrounding it with an (eval-when
;;; (:compile-toplevel ..))  Add to this that DECLAIM does not
;;; guarantee that the declarations persist after loading this file,
;;; and I see ample reason to use proclaim instead.  -- RED 2010-08-22
(defmacro declaim-host-and-target (form)
  `(progn
     #+sb-xc
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (sb!xc:proclaim ',form))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (proclaim ',form))))

(declaim-host-and-target (special
                          *the-class-t*
                          *the-class-vector* *the-class-symbol*
                          *the-class-string* *the-class-sequence*
                          *the-class-rational* *the-class-ratio*
                          *the-class-number* *the-class-null* *the-class-list*
                          *the-class-integer* *the-class-float* *the-class-cons*
                          *the-class-complex* *the-class-character*
                          *the-class-bit-vector* *the-class-array*
                          *the-class-stream* *the-class-file-stream*
                          *the-class-string-stream*

                          *the-class-slot-object*
                          *the-class-structure-object*
                          *the-class-standard-object*
                          *the-class-funcallable-standard-object*
                          *the-class-class*
                          *the-class-generic-function*
                          *the-class-built-in-class*
                          *the-class-slot-class*
                          *the-class-condition-class*
                          *the-class-structure-class*
                          *the-class-std-class*
                          *the-class-standard-class*
                          *the-class-funcallable-standard-class*
                          *the-class-forward-referenced-class*
                          *the-class-method*
                          *the-class-standard-method*
                          *the-class-standard-reader-method*
                          *the-class-standard-writer-method*
                          *the-class-standard-boundp-method*
                          *the-class-global-reader-method*
                          *the-class-global-writer-method*
                          *the-class-global-boundp-method*
                          *the-class-standard-generic-function*
                          *the-class-standard-effective-slot-definition*

                          *the-eslotd-standard-class-slots*
                          *the-eslotd-funcallable-standard-class-slots*

                          *the-wrapper-of-t*
                          *the-wrapper-of-vector* *the-wrapper-of-symbol*
                          *the-wrapper-of-string* *the-wrapper-of-sequence*
                          *the-wrapper-of-rational* *the-wrapper-of-ratio*
                          *the-wrapper-of-number* *the-wrapper-of-null*
                          *the-wrapper-of-list* *the-wrapper-of-integer*
                          *the-wrapper-of-float* *the-wrapper-of-cons*
                          *the-wrapper-of-complex* *the-wrapper-of-character*
                          *the-wrapper-of-bit-vector* *the-wrapper-of-array*))))