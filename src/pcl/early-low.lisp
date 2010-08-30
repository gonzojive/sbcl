;;;; some code pulled out of CMU CL's low.lisp to solve build order problems,
;;;; and some other stuff that just plain needs to be done early

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

(/show "starting early-low.lisp")

;;; FIXME: The PCL package is internal and is used by code in potential
;;; bottlenecks. Access to it might be faster through #.(find-package "SB-PCL")
;;; than through *PCL-PACKAGE*. And since it's internal, no one should be
;;; doing things like deleting and recreating it in a running target Lisp.
;;; So perhaps we should replace it uses of *PCL-PACKAGE* with uses of
;;; (PCL-PACKAGE), and make PCL-PACKAGE a macro which expands into
;;; the SB-PCL package itself. Maybe we should even use this trick for
;;; COMMON-LISP and KEYWORD, too. (And the definition of PCL-PACKAGE etc.
;;; could be made less viciously brittle when SB-FLUID.)
;;; (Or perhaps just define a macro
;;;   (DEFMACRO PKG (NAME)
;;;     #-SB-FLUID (FIND-PACKAGE NAME)
;;;     #+SB-FLUID `(FIND-PACKAGE ,NAME))
;;; and use that to replace all three variables.)
;;; > The above note is in regards to the old use of the *pcl-package*
;;; > defvar instead of a more constant means of accessing the
;;; > package.  I fixed this for the case of SB-PCL but left the
;;; > changes for other packages for later.
;;; > ... on second thought I left it as a defvar, but use the macro
;;; > still.  This makes it possible to cold-compile this file -- RED
;;; >' 8-19-2010

(defvar *pcl-package* (find-package #+(or sb-xc sb-xc-host) "SB!PCL" #-(or sb-xc sb-xc-host) "SB-PCL"))

(defmacro pcl-package ()
  "Expands into an expression for the SB-PCL package (or perhaps
  SB!PCL during cold init)."
  #!+sb-fluid
  `(find-package (if (find :sb-xc *features*) (find :sb-xc-host *features*)
                     "SB!PCL"
                     "SB-PCL"))
  ;;FIXME: We should use a constant means of referencing the package,
  ;;but I'm punting on that for now because right now we cannot
  ;;compile a reference to a package instance with the XC. Would be
  ;;(unquoted) (find-package #!+sb-xc "SB!PCL" #!-sb-xc "SB-PCL")) if we could
  #!-sb-fluid
  '*pcl-package*)

(declaim (inline defstruct-classoid-p))
(defun defstruct-classoid-p (classoid)
  ;; It is non-obvious to me why STRUCTURE-CLASSOID-P doesn't
  ;; work instead of this. -- NS 2008-03-14
  (typep (layout-info (classoid-layout classoid)) 'defstruct-description))

;;; This excludes structure types created with the :TYPE option to
;;; DEFSTRUCT. It also doesn't try to deal with types created by
;;; hairy DEFTYPEs, e.g.
;;;   (DEFTYPE CACHE-STRUCTURE (SIZE)
;;;     (IF (> SIZE 11) 'BIG-CS 'SMALL-CS)).
;;; KLUDGE: In fact, it doesn't seem to deal with DEFTYPEs at all. Perhaps
;;; it needs a more mnemonic name. -- WHN 19991204
(defun structure-type-p (type)
  (and (symbolp type)
       (let ((classoid (find-classoid type nil)))
         (and classoid
              (not (condition-classoid-p classoid))
              (defstruct-classoid-p classoid)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Symbol contruction utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun format-symbol (package format-string &rest format-arguments)
  (without-package-locks
   (intern (apply #'format nil format-string format-arguments) package)))

(defun make-class-symbol (class-name)
  (format-symbol (pcl-package) "*THE-CLASS-~A*" (symbol-name class-name)))

(defun make-wrapper-symbol (class-name)
  (format-symbol (pcl-package) "*THE-WRAPPER-~A*" (symbol-name class-name)))

(defun interned-symbol-p (x)
  #!+sb-doc
  "Returns non-null if X is a symbol and is interned in a package."
  (and (symbolp x) (symbol-package x)))

(defun condition-type-p (type)
  (and (symbolp type)
       (condition-classoid-p (find-classoid type nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Misc. other utilities that used to be in macros.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-declaration (name declarations &optional default)
  "Returns the cdr of the first declaration with name NAME in the list
of declarations forms DECLARATIONS."
  (dolist (d declarations default)
    (dolist (form (cdr d))
      (when (and (consp form) (eq (car form) name))
        (return-from get-declaration (cdr form))))))

(defun declared-specials (declarations)
  #!+sb-doc
  "Returns a list of all the things declared special in the list of
declarations."
  (loop for (declare . specifiers) in declarations
        append (loop for specifier in specifiers
                     when (eq 'special (car specifier))
                     append (cdr specifier))))

(/show "finished with early-low.lisp")
