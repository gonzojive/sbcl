;;;; macros, global variable definitions, and other miscellaneous support stuff
;;;; used by the rest of the PCL subsystem

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

(/show "starting pcl/macros.lisp")

;; FIXME: DECLAIM is not very ANSI of us since it only guarantees
;; declarations for this file
(declaim (declaration
          ;; As of sbcl-0.7.0.6, SBCL actively uses this declaration
          ;; to propagate information needed to set up nice debug
          ;; names (as seen e.g. in BACKTRACE) for method functions.
          %method-name
          ;; These nonstandard declarations seem to be used privately
          ;; within PCL itself to pass information around, so we can't
          ;; just delete them.
          %class
          %method-lambda-list
          ;; This declaration may also be used within PCL to pass
          ;; information around, I'm not sure. -- WHN 2000-12-30
          %variable-rebinding))

(def!constant n-fixnum-bits (integer-length sb-xc:most-positive-fixnum))

;;; This DEFVAR was originally in defs.lisp, now moved here.
;;;
;;; Possible values are NIL, EARLY, BRAID, or COMPLETE.
(declaim (type (member nil early braid complete) **boot-state**))
(defglobal **boot-state** nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *optimize-speed*
    '(optimize (speed 3) (safety 0))))


(/show "done with DECLAIM DECLARATION")

(defmacro doplist ((key val) plist &body body)
  "Loops over the property list PLIST binding KEY and VAL at each
iteration.  In addition, .plist-tail. is bound to the tail of the
plist that has not yet been processed in the loop."
  `(let ((.plist-tail. ,plist) ,key ,val)
     (loop (when (null .plist-tail.) (return nil))
           (setq ,key (pop .plist-tail.))
           (when (null .plist-tail.)
             (error "malformed plist, odd number of elements"))
           (setq ,val (pop .plist-tail.))
           (progn ,@body))))

(defmacro dolist-carefully ((var list improper-list-handler) &body body)
  "Loops over the LIST as in DOLIST, but if the list turns out to be
improper (current head of list isn't a cons calls the handler named by
IMPROPER-LIST-HANDLER"
  `(let ((,var nil)
         (.dolist-carefully. ,list))
     (loop (when (null .dolist-carefully.) (return nil))
           (if (consp .dolist-carefully.)
               (progn
                 (setq ,var (pop .dolist-carefully.))
                 ,@body)
               (,improper-list-handler)))))

(defmacro dotimes-fixnum ((var count &optional (result nil)) &body body)
  `(dotimes (,var (the fixnum ,count) ,result)
     (declare (fixnum ,var))
     ,@body))

(declaim (inline random-fixnum))
(defun random-fixnum ()
  (random (1+ most-positive-fixnum)))

;;; Lambda which executes its body (or not) randomly. Used to drop
;;; random cache entries.
(defmacro randomly-punting-lambda (lambda-list &body body)
  (with-unique-names (drops drop-pos)
    `(let ((,drops (random-fixnum))
           (,drop-pos ,n-fixnum-bits))
       (declare (fixnum ,drops)
                (type (integer 0 ,n-fixnum-bits) ,drop-pos))
       (lambda ,lambda-list
         (when (logbitp (the unsigned-byte (decf ,drop-pos)) ,drops)
           (locally ,@body))
         (when (zerop ,drop-pos)
           (setf ,drops (random-fixnum)
                 ,drop-pos ,n-fixnum-bits))))))

(defmacro function-funcall (form &rest args)
  `(funcall (the function ,form) ,@args))

(defmacro function-apply (form &rest args)
  `(apply (the function ,form) ,@args))


;;;; FIND-CLASS
;;;;
;;;; This is documented in the CLOS specification.
;;; FIXME: Only compile CL-conflicting stuff to target for now.  We
;;; might want to find a way to shadow find-class on the host so we
;;; can use our versions of find-class etc in code that uses SB!PCL
;;; (SB-PCL wouldn't shadow CL's symbols, but define them).  There's
;;; probably a way to do this already, but RED doesn't know about it.
(defun sb-xc:find-class (symbol &optional (errorp t) environment)

  (declare (ignore environment))
  #+sb-xc-host
  (error "Unimplemented so far")
  #+sb-xc
  (find-class-from-cell symbol
                        (find-classoid-cell symbol)
                        errorp))

(defun (setf sb-xc:find-class) (new-value name &optional errorp environment)
  (declare (ignore errorp environment))
  #+sb-xc-host
  (error "Unimplemented so far")
  #+sb-xc
  (cond ((legal-class-name-p name)
         (with-single-package-locked-error
             (:symbol name "Using ~A as the class-name argument in ~
                           (SETF FIND-CLASS)"))
         (with-world-lock ()
           (let ((cell (find-classoid-cell name :create new-value)))
             (cond (new-value
                    (setf (classoid-cell-pcl-class cell) new-value)
                    (when (eq **boot-state** 'complete)
                      (let ((classoid (class-classoid new-value)))
                        (setf (find-classoid name) classoid)
                        (%set-class-type-translation new-value classoid))))
                   (cell
                    (%clear-classoid name cell)))
             (when (or (eq **boot-state** 'complete)
                       (eq **boot-state** 'braid))
               (update-ctors 'setf-find-class :class new-value :name name))
             new-value)))
        (t
         (error "~S is not a legal class name." name))))

(/show "pcl/macros.lisp 119")

(declaim (inline legal-class-name-p))
(defun legal-class-name-p (x)
  (symbolp x))

(defun get-setf-fun-name (name)
  `(setf ,name))

(declaim (inline class-classoid))
(defun class-classoid (class)
  (layout-classoid (class-wrapper class)))

(defvar *create-classes-from-internal-structure-definitions-p* t)

(defun find-class-from-cell (symbol cell &optional (errorp t))
  (or (when cell
        (or (classoid-cell-pcl-class cell)
            (when *create-classes-from-internal-structure-definitions-p*
              (let ((classoid (classoid-cell-classoid cell)))
                (when (and classoid
                           (or (condition-classoid-p classoid)
                               (defstruct-classoid-p classoid)))
                  (ensure-non-standard-class symbol classoid))))))
      (cond ((null errorp) nil)
            ((legal-class-name-p symbol)
             (error "There is no class named ~S." symbol))
            (t
             (error "~S is not a legal class name." symbol)))))



(/show "pcl/macros.lisp 187")

;;; FIXME: defining compiler macros for CL forms has unspecified
;;; behavior (could be an error
#+sb-xc
(define-compiler-macro sb-xc:find-class (&whole form
                                          symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (if (and (constantp symbol)
           (legal-class-name-p (setf symbol (constant-form-value symbol)))
           (constantp errorp)
           (boundp '**boot-state**)
           (member **boot-state** '(braid complete)))
      (let ((errorp (not (null (constant-form-value errorp))))
            (cell (make-symbol "CLASSOID-CELL")))
        `(let ((,cell (load-time-value (find-classoid-cell ',symbol :create t))))
           (or (classoid-cell-pcl-class ,cell)
               ,(if errorp
                    `(find-class-from-cell ',symbol ,cell t)
                    `(when (classoid-cell-classoid ,cell)
                       (find-class-from-cell ',symbol ,cell nil))))))
      form))




#+sb-xc
(defsetf slot-value set-slot-value)

#+sb-xc-host
(defsetf slot-value-xs set-slot-value)

(/show "finished with pcl/macros.lisp")
