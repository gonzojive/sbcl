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

#|

The CommonLoops evaluator is meta-circular.

Most of the code in PCL is methods on generic functions, including
most of the code that actually implements generic functions and method
lookup.

So, we have a classic bootstrapping problem. The solution to this is
to first get a cheap implementation of generic functions running,
these are called early generic functions. These early generic
functions and the corresponding early methods and early method lookup
are used to get enough of the system running that it is possible to
create real generic functions and methods and implement real method
lookup. At that point (done in the file FIXUP) the function
!FIX-EARLY-GENERIC-FUNCTIONS is called to convert all the early generic
functions to real generic functions.

The cheap generic functions are built using the same
FUNCALLABLE-INSTANCE objects that real generic functions are made out of.
This means that as PCL is being bootstrapped, the cheap generic
function objects which are being created are the same objects which
will later be real generic functions. This is good because:
  - we don't cons garbage structure, and
  - we can keep pointers to the cheap generic function objects
    during booting because those pointers will still point to
    the right object after the generic functions are all fixed up.

This file defines the DEFMETHOD macro and the mechanism used to expand
it. This includes the mechanism for processing the body of a method.
DEFMETHOD basically expands into a call to LOAD-DEFMETHOD, which
basically calls ADD-METHOD to add the method to the generic function.
These expansions can be loaded either during bootstrapping or when PCL
is fully up and running.

An important effect of this arrangement is it means we can compile
files with DEFMETHOD forms in them in a completely running PCL, but
then load those files back in during bootstrapping. This makes
development easier. It also means there is only one set of code for
processing DEFMETHOD. Bootstrapping works by being sure to have
LOAD-METHOD be careful to call only primitives which work during
bootstrapping.

|#

(declaim (notinline make-a-method add-named-method
                    ensure-generic-function-using-class
                    add-method remove-method))

;;; For each of the early functions, arrange to have it point to its
;;; early definition. Do this in a way that makes sure that if we
;;; redefine one of the early definitions the redefinition will take
;;; effect. This makes development easier.

(!set-fdefinitions-of-early-functions :early)



;;;; early generic function support



(defparameter *!xc-generic-functions* nil)

#+sb-xc-host
(defun xc-ensure-generic-function (fun-name
                                   &rest all-keys
                                   &key environment source-location
                                   &allow-other-keys)
  (declare (ignore environment source-location))
  (declare (optimize (debug 3)))
  (let ((existing (cdr (assoc fun-name *!xc-generic-functions* :test #'equal))))
    (cond ((and existing
                (eq **boot-state** 'complete)
                (null (generic-function-p existing)))
           (generic-clobbers-function fun-name)
           (fmakunbound fun-name)
           (apply 'xc-ensure-generic-function fun-name all-keys))
          (t
           (apply #'ensure-generic-function-using-class
                  existing fun-name all-keys)))))


(defun !fix-early-generic-functions ()
  #!+sb-doc
  "Run at the end of the braid stage to boot PCL into the fully
functional 'complete stage.  This converts the partially initialized
early generic functions and their methods into fully-functional
generics and methods."
  (let ((accessors nil))
    ;; Rearrange *!EARLY-GENERIC-FUNCTIONS* to speed up
    ;; FIX-EARLY-GENERIC-FUNCTIONS.  -- accessors were already
    ;; processed by 'BRAID
    (dolist (early-gf-spec *!early-generic-functions*)
      (when (every #'early-method-standard-accessor-p
                   (early-gf-methods (gdefinition early-gf-spec)))
        (push early-gf-spec accessors)))
    (dolist (spec (nconc accessors
                         '(accessor-method-slot-name
                           generic-function-methods
                           method-specializers
                           specializerp
                           specializer-type
                           specializer-class
                           slot-definition-location
                           slot-definition-name
                           class-slots
                           gf-arg-info
                           class-precedence-list
                           slot-boundp-using-class
                           (setf slot-value-using-class)
                           slot-value-using-class
                           structure-class-p
                           standard-class-p
                           funcallable-standard-class-p
                           specializerp)))
      (/show spec)
      (setq *!early-generic-functions*
            (cons spec
                  (delete spec *!early-generic-functions* :test #'equal))))
    
    ;; Modify each generic function by making a real method for
    ;; each of its early methods, setting the method-class to the now
    ;; non-early class *the-class-standard-class* (and same for method combo)
    (dolist (early-gf-spec *!early-generic-functions*)
      (/show early-gf-spec)
      (let* ((gf (gdefinition early-gf-spec))
             (methods (mapcar (lambda (early-method)
                                (let ((args (copy-list (fifth
                                                        early-method))))
                                  (setf (fourth args)
                                        (early-method-specializers
                                         early-method t))
                                  (apply #'real-make-a-method args)))
                              (early-gf-methods gf))))
        ;; What magic happens so that we can EARLY-GF-METHODS above
        ;; (when processing accessors) and non-early
        ;; GENERIC-FUNCTION-METHOD-CLASS now?  When does the class of
        ;; the instance change from early to SGF?  Actually, it has
        ;; been an SGF all along--or at least, had the layout of an
        ;; SGF all along.  The only difference now is that all its
        ;; slots are initialized properly
        (aver (early-gf-p gf))
        (setf (generic-function-method-class gf) *the-class-standard-method*)
        (setf (generic-function-method-combination gf)
              *standard-method-combination*)
        (set-methods gf methods)
        (aver (not (early-gf-p gf)))))

    ;; Modify the simple early-function-specs to their real version.
    (!set-fdefinitions-of-early-functions :late)

    ;; Run all the fixupes
    (dolist (fixup *!generic-function-fixups*)
      (/show fixup)
      (let* ((fspec (car fixup))
             (gf (gdefinition fspec))
             (methods (mapcar (lambda (method)
                                (let* ((lambda-list (first method))
                                       (specializers (mapcar #'sb-xc:find-class (second method)))
                                       (method-fn-name (third method))
                                       (fn-name (or method-fn-name fspec))
                                       (fn (fdefinition fn-name))
                                       (initargs
                                        (list :function
                                              (set-fun-name
                                               (lambda (args next-methods)
                                                 (declare (ignore
                                                           next-methods))
                                                 (apply fn args))
                                               `(call ,fn-name)))))
                                  (declare (type function fn))
                                  (make-a-method 'standard-method
                                                 ()
                                                 lambda-list
                                                 specializers
                                                 initargs
                                                 nil)))
                              (cdr fixup))))
        (setf (generic-function-method-class gf) *the-class-standard-method*)
        (setf (generic-function-method-combination gf)
              *standard-method-combination*)
        (set-methods gf methods))))
  (/show "leaving !FIX-EARLY-GENERIC-FUNCTIONS"))


;;; FIXME: In here there was a #!-CMU definition of SYMBOL-MACROLET
;;; which used %WALKER stuff. That suggests to me that maybe the code
;;; walker stuff was only used for implementing stuff like that; maybe
;;; it's not needed any more? Hunt down what it was used for and see.

(defun extract-the (form)
  #!+sb-doc
  "If the form is like (THE TYPE-FORM VALUE-FORM), returns VALUE-FORM.
If there is no THE, returns the form itself."
  (cond ((and (consp form) (eq (car form) 'the))
         (aver (proper-list-of-length-p form 3))
         (third form))
        (t
         form)))

(setq **boot-state** 'early)

(sb-xc:defmacro with-slots (slots instance &body body)
  (let ((in (gensym)))
    `(let ((,in ,instance))
       (declare (ignorable ,in))
       ,@(let ((instance (extract-the instance)))
           (and (symbolp instance)
                `((declare (%variable-rebinding ,in ,instance)))))
       ,in
       (symbol-macrolet ,(mapcar (lambda (slot-entry)
                                   (let ((var-name
                                          (if (symbolp slot-entry)
                                              slot-entry
                                              (car slot-entry)))
                                         (slot-name
                                          (if (symbolp slot-entry)
                                              slot-entry
                                              (cadr slot-entry))))
                                     `(,var-name
                                       (slot-value ,in ',slot-name))))
                                 slots)
                        ,@body))))

(sb-xc:defmacro with-accessors (slots instance &body body)
  (let ((in (gensym)))
    `(let ((,in ,instance))
       (declare (ignorable ,in))
       ,@(let ((instance (extract-the instance)))
           (and (symbolp instance)
                `((declare (%variable-rebinding ,in ,instance)))))
       ,in
       (symbol-macrolet ,(mapcar (lambda (slot-entry)
                                   (let ((var-name (car slot-entry))
                                         (accessor-name (cadr slot-entry)))
                                     `(,var-name (,accessor-name ,in))))
                                 slots)
          ,@body))))
