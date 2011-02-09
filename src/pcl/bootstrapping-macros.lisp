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

(defvar *!early-function-specs* nil
  #!+sb-doc
  "Each member is a list of (function-name early-function-name late-function-name.")

;; currently just MAKE-A-METHOD and ADD-NAMED-METHOD use this
(defmacro define-early-function ((name early-name real-name) &body options)
  #!+sb-doc
  "Defines a function named NAME that is initially fbound to the
function EARLY-NAME but when !FIX-EARLY-GENERIC-FUNCTIONS is run
becomes fbound to REAL-NAME.

OPTIONS is a list where each item's car is either :EARLY or :LATE, in
which case it a DEFUN form is expanded with a name of either
EARLY-NAME or REAL-NAME.  The rest of the option corresponds to the
rest of the forms passed to DEFUN.

Note that this is a method of defining early functions, not generic
functions.  Nothing is done to transform the original"
  `(progn
     (declaim (notinline ,name))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf *!early-function-specs*
             (cons (list ',name ',early-name ',real-name)
                   (remove ',name *!early-function-specs* :key #'car))))
     ,@(mapcar #'(lambda (option)
                   (ecase (car option)
                     ((:early :late)
                        `(define-early-function-implementation ,name ,(car option) ,@(rest option)))))
               options)))

(defmacro define-early-function-implementation (name early-or-late lambda-list &body body)
  #!+sb-doc
  "Defines the late or early implementation of the early-function NAME."
  (let* ((spec (find name *!early-function-specs* :key #'car :test #'equal))
         (early/late-name (if spec
                              (nth (ecase early-or-late (:early 1) (:late 2)) spec)
                              (error "No early function defined with name ~S" name))))
    `(defun ,early/late-name ,lambda-list ,@body)))


(defun !set-fdefinitions-of-early-functions (early-or-late &key (names nil names-p))
  #!+sb-doc
  "Sets the fdefinition of each function defined by
DEFINE-EARLY-FUNCTION, or those with names NAMES if NAMES is
specified, to the :EARLY or :LATE definition."
  (mapcar #'(lambda (spec)
              (let* ((early/late-name (nth (ecase early-or-late (:early 1) (:late 2))
                                           spec))
                     (function (fdefinition early/late-name))
                     (name (first spec)))

                ;; We used to do this in order to set the fdefinition,
                ;; but I don't see how it's necessary. -- RED 08/2010
                ;; (setf (gdefinition name) (set-fun-name
                ;;                           (lambda (&rest args)
                ;;                             (apply (fdefinition early-name) args))
                ;;                           name))
                (setf (fdefinition name) function)))
          (if names-p
              (remove-if-not #'(lambda (spec-name)
                                 (member spec-name names :test #'equal))
                             *!early-function-specs*
                             :key #'first)
              *!early-function-specs*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Early generic function -- implemented as defuns early on
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *!GENERIC-FUNCTION-FIXUPS* is used by !FIX-EARLY-GENERIC-FUNCTIONS
;;; to convert the few functions in the bootstrap which are supposed
;;; to be generic functions but can't be early on.
;;;
;;; each entry is a list of name and lambda-list, class names as
;;; specializers, and method body function name.
(defvar *!generic-function-fixups* nil)

(defvar *!explicit-generic-function-fixups*
  '((add-method
     ((generic-function method)
      (standard-generic-function method)
      real-add-method))
    (remove-method
     ((generic-function method)
      (standard-generic-function method)
      real-remove-method))
    (get-method
     ((generic-function qualifiers specializers &optional (errorp t))
      (standard-generic-function t t)
      real-get-method))
    (ensure-generic-function-using-class
     ((generic-function fun-name
                        &key generic-function-class environment
                        &allow-other-keys)
      (generic-function t)
      real-ensure-gf-using-class--generic-function)
     ((generic-function fun-name
                        &key generic-function-class environment
                        &allow-other-keys)
      (null t)
      real-ensure-gf-using-class--null))
    (make-method-lambda
     ((proto-generic-function proto-method lambda-expression environment)
      (standard-generic-function standard-method t t)
      real-make-method-lambda))
    (make-method-specializers-form
     ((proto-generic-function proto-method specializer-names environment)
      (standard-generic-function standard-method t t)
      real-make-method-specializers-form))
    (parse-specializer-using-class
     ((generic-function specializer)
      (standard-generic-function t)
      real-parse-specializer-using-class))
    (unparse-specializer-using-class
     ((generic-function specializer)
      (standard-generic-function t)
      real-unparse-specializer-using-class))
    (make-method-initargs-form
     ((proto-generic-function proto-method
                              lambda-expression
                              lambda-list environment)
      (standard-generic-function standard-method t t t)
      real-make-method-initargs-form))
    (compute-effective-method
     ((generic-function combin applicable-methods)
      (generic-function standard-method-combination t)
      standard-compute-effective-method))))

(defun ensure-early-generic-fixup (generic-name specializers lambda-list defun-name)
  #!+sb-doc
  "Creates or replaces an generic function 'fixup'--a primary method
implementation for a generic function."
  ;; verify that the fixup is known
  (let* ((specs (cdr (assoc generic-name *!explicit-generic-function-fixups*)))
         (match (find specializers specs :key #'second :test #'equal)))
    (unless match
      (error "No known fixup for ~S with specializers ~S" generic-name specializers)))

  (setf *!generic-function-fixups*
        (cons (cons generic-name
                    (cons (list lambda-list specializers defun-name)
                          (remove specializers
                                  (cdr (assoc generic-name *!generic-function-fixups*))
                                  :key #'second
                                  :test #'equal)))
              (remove generic-name *!generic-function-fixups* :test #'equal :key #'car))))

(defmacro define-early-generic (generic-spec specialized-lambda-list &body body)
  #!+sb-doc
  "Defines an early generic function with name GENERIC-NAME by using
defun to define a function with body BODY that does not specialize on
its arguments until fixup time.  At fixup time, the function will act
like the primary method of the generic function and specialize on its
arguments according to the specializers in the
SPECIALIZED-LAMBDA-LIST.

GENERIC-SPEC ::= GENERIC-NAME
               | (GENERIC-NAME [DEFUN-NAME])

Fixup time is when !FIX-EARLY-GENERIC-FUNCTIONS is run."
  (let* ((generic-name (if (atom generic-spec)
                           generic-spec
                           (first generic-spec)))
         (defun-name (or (and (consp generic-spec) (second generic-spec))
                         (intern (format nil "REAL-~A" (symbol-name generic-name))))))
    (multiple-value-bind (params lambda-list specializers)
        (parse-specialized-lambda-list specialized-lambda-list)
      (declare (ignore params))
      `(progn
         (defun ,defun-name ,lambda-list ,@body)

         (ensure-early-generic-fixup ',generic-name ',specializers ',lambda-list ',defun-name)
         ;(setf *!generic-function-fixups*
         ;      (cons (list ',generic-name (list ',lambda-list ',specializers ',defun-name))
         ;            (remove ',generic-name *!generic-function-fixups* :key #'car)))

         (unless (fboundp ',generic-name)
           (setf (gdefinition ',generic-name)
                 (symbol-function ',defun-name)))))))

