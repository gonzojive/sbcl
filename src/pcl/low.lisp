;;;; This file contains portable versions of low-level functions and macros
;;;; which are ripe for implementation specific customization. None of the code
;;;; in this file *has* to be customized for a particular Common Lisp
;;;; implementation. Moreover, in some implementations it may not make any
;;;; sense to customize some of this code.
;;;;
;;;; The original version was intended to support portable customization to
;;;; lotso different Lisp implementations. This functionality is gone in the
;;;; current version, and it now runs only under SBCL. (Now that ANSI Common
;;;; Lisp has mixed CLOS into the insides of the system (e.g. error handling
;;;; and printing) so deeply that it's not very meaningful to bootstrap Common
;;;; Lisp without CLOS, the old functionality is of dubious use. -- WHN
;;;; 19981108)

;;;; This software is part of the SBCL system. See the README file for more
;;;; information.

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


;;;; early definition of WRAPPER
;;;;
;;;; Most WRAPPER stuff is defined later, but the DEFSTRUCT itself
;;;; is here early so that things like (TYPEP .. 'WRAPPER) can be
;;;; compiled efficiently.

;;; Note that for SBCL, as for CMU CL, the WRAPPER of a built-in or
;;; structure class will be some other kind of SB!KERNEL:LAYOUT, but
;;; this shouldn't matter, since the only two slots that WRAPPER adds
;;; are meaningless in those cases.
(def!struct (wrapper
            (:include layout
                      ;; KLUDGE: In CMU CL, the initialization default
                      ;; for LAYOUT-INVALID was NIL. In SBCL, that has
                      ;; changed to :UNINITIALIZED, but PCL code might
                      ;; still expect NIL for the initialization
                      ;; default of WRAPPER-INVALID. Instead of trying
                      ;; to find out, I just overrode the LAYOUT
                      ;; default here. -- WHN 19991204
                      (invalid nil)
                      ;; This allows quick testing of wrapperness.
                      (for-std-class-p t))
            (:constructor make-wrapper-internal)
            (:copier nil))
  (instance-slots-layout nil :type list)
  (class-slots nil :type list))
#!-sb-fluid (declaim (sb!ext:freeze-type wrapper))

;;;; PCL's view of funcallable instances

(!defstruct-with-alternate-metaclass standard-funcallable-instance
  ;; KLUDGE: Note that neither of these slots is ever accessed by its
  ;; accessor name as of sbcl-0.pre7.63. Presumably everything works
  ;; by puns based on absolute locations. Fun fun fun.. -- WHN 2001-10-30
  :slot-names (clos-slots name hash-code)
  :boa-constructor %make-standard-funcallable-instance
  :superclass-name function
  :metaclass-name standard-classoid
  :metaclass-constructor make-standard-classoid
  :dd-type funcallable-structure
  ;; Only internal implementation code will access these, and these
  ;; accesses (slot readers in particular) could easily be a
  ;; bottleneck, so it seems reasonable to suppress runtime type
  ;; checks.
  ;;
  ;; (Except note KLUDGE above that these accessors aren't used at all
  ;; (!) as of sbcl-0.pre7.63, so for now it's academic.)
  :runtime-type-checks-p nil)

#+sb-xc-host
(defstruct (xc-standard-funcallable-instance
             (:constructor %make-standard-funcallable-instance (clos-slots name hash-code)))
  clos-slots
  name
  hash-code

  fun
  layout
  )


(defun pcl-funcallable-instance-p (x)
  ;; On the host we should only be interested in whether things that
  ;; are being cross-compiled are funcallable instances
  #+sb-xc-host
  (or (xc-standard-funcallable-instance-p x)
      (%method-function-p x))
  #+sb-xc
  (sb!kernel:funcallable-instance-p x))

(defun set-funcallable-instance-function (fin new-value)
  (declare (type function new-value))
  (aver (pcl-funcallable-instance-p fin))
  (setf #+sb-xc      (funcallable-instance-fun fin)
        #+sb-xc-host (xc-standard-funcallable-instance-fun fin)
        new-value))

;;; FIXME: these macros should just go away.  It's not clear whether
;;; the inline functions defined by
;;; !DEFSTRUCT-WITH-ALTERNATE-METACLASS are as efficient as they could
;;; be; ordinary defstruct accessors are defined as source transforms.
(defun fsc-instance-p (fin)
  (pcl-funcallable-instance-p fin))

#+sb-xc
(define-compiler-macro fsc-instance-p (fin)
  `(pcl-funcallable-instance-p ,fin))

#+sb-xc (declaim (inline fsc-instance-wrapper))
(defun fsc-instance-wrapper (fin)
  #+sb-xc-host
  `(xc-standard-funcallable-instance-layout ,fin)
  #+sb-xc
  `(%funcallable-instance-layout ,fin))

(defmacro fsc-instance-slots (fin)
  #+sb-xc-host
  `(xc-standard-funcallable-instance-clos-slots ,fin)
  #+sb-xc
  `(%funcallable-instance-info ,fin 1))

(defmacro fsc-instance-hash (fin)
  #+sb-xc-host
  `(xc-standard-funcallable-instance-hash-code ,fin)
  #+sb-xc
  `(%funcallable-instance-info ,fin 3))

#+sb-xc
(declaim (inline clos-slots-ref (setf clos-slots-ref)))
(declaim (ftype (function (simple-vector index) t) clos-slots-ref))
(defun clos-slots-ref (slots index)
  (svref slots index))

(declaim (ftype (function (t simple-vector index) t) (setf clos-slots-ref)))
(defun (setf clos-slots-ref) (new-value slots index)
  (setf (svref slots index) new-value))

;;; Note on implementation under CMU CL >=17 and SBCL: STD-INSTANCE-P
;;; is only used to discriminate between functions (including FINs)
;;; and normal instances, so we can return true on structures also. A
;;; few uses of (OR STD-INSTANCE-P FSC-INSTANCE-P) are changed to
;;; PCL-INSTANCE-P.
(defun std-instance-p (x)
  #+sb-xc-host
  (xc-standard-instance-p x)
  #+sb-xc
  (%instancep x))

#+sb-xc
(define-compiler-macro std-instance-p (x)
  #+sb-xc
  `(%instancep ,x))

;; a temporary definition used for debugging the bootstrap
#!+sb-show
(defun print-std-instance (instance stream depth)
  (declare (ignore depth))
  (print-unreadable-object (instance stream :type t :identity t)
    (let ((class (sb-xc:class-of instance)))
      (when (or (eq class (sb-xc:find-class 'standard-class nil))
                (eq class (sb-xc:find-class 'funcallable-standard-class nil))
                (eq class (sb-xc:find-class 'built-in-class nil)))
        (princ (early-class-name instance) stream)))))

;;; This is the value that we stick into a slot to tell us that it is
;;; unbound. It may seem gross, but for performance reasons, we make
;;; this an interned symbol. That means that the fast check to see
;;; whether a slot is unbound is to say (EQ <val> '..SLOT-UNBOUND..).
;;; That is considerably faster than looking at the value of a special
;;; variable. Be careful, there are places in the code which actually
;;; use ..SLOT-UNBOUND.. rather than this variable. So much for
;;; modularity..
;;;
;;; FIXME: Now that we're tightly integrated into SBCL, we could use
;;; the SBCL built-in unbound value token instead. Perhaps if we did
;;; so it would be a good idea to define collections of CLOS slots as
;;; a new type of heap object, instead of using bare SIMPLE-VECTOR, in
;;; order to avoid problems (in the debugger if nowhere else) with
;;; SIMPLE-VECTORs some of whose elements are unbound tokens.
(defconstant +slot-unbound+ '..slot-unbound..)

(defmacro %allocate-static-slot-storage--class (no-of-slots)
  `(make-array ,no-of-slots :initial-element +slot-unbound+))

(defmacro std-instance-class (instance)
  `(wrapper-class* (std-instance-wrapper ,instance)))

;;; When given a funcallable instance, SET-FUN-NAME *must* side-effect
;;; that FIN to give it the name. When given any other kind of
;;; function SET-FUN-NAME is allowed to return a new function which is
;;; "the same" except that it has the name.
;;;
;;; In all cases, SET-FUN-NAME must return the new (or same)
;;; function. (Unlike other functions to set stuff, it does not return
;;; the new value.)
#+sb-xc
(defun set-fun-name (fun new-name)
  #!+sb-doc
  "Set the name of a compiled function object. Return the function."
  (when (valid-function-name-p fun)
    (setq fun (fdefinition fun)))
  (typecase fun
    (%method-function (setf (%method-function-name fun) new-name))
    #!+sb-eval
    (sb!eval:interpreted-function
     (setf (sb!eval:interpreted-function-name fun) new-name))
    (funcallable-instance ;; KLUDGE: probably a generic function...
     (cond ((if (eq **boot-state** 'complete)
                (typep fun 'generic-function)
                (eq (sb-xc:class-of fun) *the-class-standard-generic-function*))
            (setf (%funcallable-instance-info fun 2) new-name))
           (t
            (bug "unanticipated function type")))))
  ;; Fixup name-to-function mappings in cases where the function
  ;; hasn't been defined by DEFUN.  (FIXME: is this right?  This logic
  ;; comes from CMUCL).  -- CSR, 2004-12-31
  (when (and (consp new-name)
             (member (car new-name) '(slow-method fast-method slot-accessor)))
    (setf (fdefinition new-name) fun))
  fun)

;;; FIXME -- ensure this is not needed on the target
#+sb-xc-host
(defun set-fun-name (fun new-name)
  #!+sb-doc
  "Set the name of a compiled function object. Return the function."
  (when (valid-function-name-p fun)
    (setq fun (fdefinition fun)))
  (typecase fun
    (%method-function
       (setf (%method-function-name fun) new-name)
       (aver (%method-function-fast-function fun)))

    #+sb-xc-host
    (xc-standard-funcallable-instance
       (setf (xc-standard-funcallable-instance-name fun) new-name)
       (aver (xc-standard-funcallable-instance-fun fun)))

    (function
       (warn "SET-FUN-NAME called, but no precautions to ensure function will be defined on target.")
       (warn "SET-FUN-NAME called, but no fdefiniton set for host.")
       )
    (t (error "Instance ~S is not a supported funcallable-instance type by the host" fun)))

  ;; Fixup name-to-function mappings in cases where the function
  ;; hasn't been defined by DEFUN.  (FIXME: is this right?  This logic
  ;; comes from CMUCL).  -- CSR, 2004-12-31
  ;(when (and (consp new-name)
  ;(member (car new-name) '(slow-method fast-method slot-accessor)))
  ;(setf (fdefinition new-name) fun))
  fun)

;;; FIXME: probably no longer needed after init
(defmacro precompile-random-code-segments (&optional system)
  `(progn
     (eval-when (:compile-toplevel)
       (update-dispatch-dfuns))
     (precompile-function-generators ,system)
     (precompile-dfun-constructors ,system)
     (precompile-ctors)))

;;; This definition is for interpreted code.
(defun pcl-instance-p (x)
  (typep (layout-of x) 'wrapper))

;;; CMU CL comment:
;;;   We define this as STANDARD-INSTANCE, since we're going to
;;;   clobber the layout with some standard-instance layout as soon as
;;;   we make it, and we want the accessor to still be type-correct.
#|
(defstruct (standard-instance
            (:predicate nil)
            (:constructor %%allocate-instance--class ())
            (:copier nil)
            (:alternate-metaclass instance
                                  cl:standard-class
                                  make-standard-class))
  (slots nil))
|#
(!defstruct-with-alternate-metaclass standard-instance
  :slot-names (slots hash-code)
  :boa-constructor %make-standard-instance
  :superclass-name t
  :metaclass-name standard-classoid
  :metaclass-constructor make-standard-classoid
  :dd-type structure
  :runtime-type-checks-p nil)

#+sb-xc-host
(defstruct (xc-standard-instance
             (:constructor %make-standard-instance (slots hash-code)))
  slots
  hash-code
  layout
)





;;; Both of these operations "work" on structures, which allows the above
;;; weakening of STD-INSTANCE-P.
(defmacro std-instance-slots (x)
  #+sb-xc-host
  `(xc-standard-instance-slots ,x)
  #+sb-xc
  `(%instance-ref ,x 1))

(defmacro std-instance-wrapper (x)
  #+sb-xc-host
  `(xc-standard-instance-layout ,x)
  #+sb-xc
  `(%instance-layout ,x))

;;; KLUDGE: This one doesn't "work" on structures.  However, we
;;; ensure, in SXHASH and friends, never to call it on structures.
(defmacro std-instance-hash (x)
  `(%instance-ref ,x 2))

;;; FIXME: These functions are called every place we do a
;;; CALL-NEXT-METHOD, and probably other places too. It's likely worth
;;; selectively optimizing them with DEFTRANSFORMs and stuff, rather
;;; than just indiscriminately expanding them inline everywhere.
(declaim (inline get-slots get-slots-or-nil))
(declaim (ftype (function (t) simple-vector) get-slots))
(declaim (ftype (function (t) (or simple-vector null)) get-slots-or-nil))
(defun get-slots (instance)
  (if (std-instance-p instance)
      (std-instance-slots instance)
      (fsc-instance-slots instance)))

(defun get-slots-or-nil (instance)
  ;; Suppress a code-deletion note.  FIXME: doing the FIXME above,
  ;; integrating PCL more with the compiler, would remove the need for
  ;; this icky stuff.
  #-sb-xc-host
  (declare (optimize (inhibit-warnings 3)))
  (when (pcl-instance-p instance)
    (get-slots instance)))

(defmacro get-wrapper (inst)
  (once-only ((wrapper `(wrapper-of ,inst)))
    `(progn
       (aver (typep ,wrapper 'wrapper))
       ,wrapper)))

;;; FIXME: could be an inline function or ordinary function (like many
;;; other things around here)
(defmacro get-instance-wrapper-or-nil (inst)
  (once-only ((wrapper `(wrapper-of ,inst)))
    `(if (typep ,wrapper 'wrapper)
         ,wrapper
         nil)))

;;;; support for useful hashing of PCL instances

(defvar *instance-hash-code-random-state* (make-random-state))
(defun get-instance-hash-code ()
  ;; ANSI SXHASH wants us to make a good-faith effort to produce
  ;; hash-codes that are well distributed within the range of
  ;; non-negative fixnums, and this RANDOM operation does that, unlike
  ;; the sbcl<=0.8.16 implementation of this operation as
  ;; (INCF COUNTER).
  ;;
  ;; Hopefully there was no virtue to the old counter implementation
  ;; that I am insufficiently insightful to insee. -- WHN 2004-10-28
  (random most-positive-fixnum
          *instance-hash-code-random-state*))

(defun sb!impl::sxhash-instance (x)
  (cond
    ((std-instance-p x) (std-instance-hash x))
    ((fsc-instance-p x) (fsc-instance-hash x))
    (t (bug "SXHASH-INSTANCE called on some weird thing: ~S" x))))

;;;; structure-instance stuff
;;;;
;;;; FIXME: Now that the code is SBCL-only, this extra layer of
;;;; abstraction around our native structure representation doesn't
;;;; seem to add anything useful, and could probably go away.

;;; The definition of STRUCTURE-TYPE-P was moved to early-low.lisp.

(defun structure-type-slot-description-list (type)
  (let* ((dd (find-defstruct-description type))
         (include (dd-include dd))
         (all-slots (dd-slots dd)))
    (multiple-value-bind (super slot-overrides)
        (if (consp include)
            (values (car include) (mapcar #'car (cdr include)))
            (values include nil))
      (let ((included-slots
             (when super
               (dd-slots (find-defstruct-description super)))))
        (loop for slot = (pop all-slots)
              for included-slot = (pop included-slots)
              while slot
              when (or (not included-slot)
                       (member (dsd-name included-slot) slot-overrides :test #'eq))
              collect slot)))))

(defun structure-slotd-name (slotd)
  (dsd-name slotd))

(defun structure-slotd-accessor-symbol (slotd)
  (dsd-accessor-name slotd))

(defun structure-slotd-reader-function (slotd)
  (fdefinition (dsd-accessor-name slotd)))

(defun structure-slotd-writer-function (type slotd)
  (if (dsd-read-only slotd)
      (let ((dd (find-defstruct-description type)))
        (coerce (slot-setter-lambda-form dd slotd) 'function))
      (fdefinition `(setf ,(dsd-accessor-name slotd)))))

(defun structure-slotd-type (slotd)
  (dsd-type slotd))

(defun structure-slotd-init-form (slotd)
  (dsd-default slotd))

;;; method function stuff.
;;;
;;; PCL historically included a so-called method-fast-function, which
;;; is essentially a method function but with (a) a precomputed
;;; continuation for CALL-NEXT-METHOD and (b) a permutation vector for
;;; slot access.  [ FIXME: see if we can understand these two
;;; optimizations before commit. ]  However, the presence of the
;;; fast-function meant that we violated AMOP and the effect of the
;;; :FUNCTION initarg, and furthermore got to potentially confusing
;;; situations where the function and the fast-function got out of
;;; sync, so that calling (method-function method) with the defined
;;; protocol would do different things from (call-method method) in
;;; method combination.
;;;
;;; So we define this internal method function structure, which we use
;;; when we create a method function ourselves.  This means that we
;;; can hang the various bits of information that we want off the
;;; method function itself, and also that if a user overrides method
;;; function creation there is no danger of having the system get
;;; confused.
(!defstruct-with-alternate-metaclass %method-function
  :slot-names (fast-function name)
  :boa-constructor %make-method-function
  :superclass-name function
  :metaclass-name static-classoid
  :metaclass-constructor make-static-classoid
  :dd-type funcallable-structure)

#+sb-xc-host
(defstruct (%method-function
             (:constructor %make-method-function (fast-function name)))
  fast-function
  name

  host-method-fun)


