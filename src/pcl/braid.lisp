;;;; bootstrapping the meta-braid
;;;;
;;;; The code in this file takes the early definitions that have been
;;;; saved up and actually builds those class objects. This work is
;;;; largely driven off of those class definitions, but the fact that
;;;; STANDARD-CLASS is the class of all metaclasses in the braid is
;;;; built into this code pretty deeply.

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

;;; cross-compiler versions of find-class used during bootstrapping
;; #+sb-xc-host
;; (defparameter *!bootstrap-class-objects* ()) ;; alist of (name xc-standard-instance)classes defined with-find-class

;; (defun !bootstrap-find-class  (symbol &optional (errorp t))
;;   (sb-xc:find-class symbol error
;;   (or (cdr (find symbol *!bootstrap-class-objects* :key #'car :test #'equal))
;;       (and errorp (error "Found no class named ~S" symbol))))

;; #+sb-xc-host
;; (defun (setf !bootstrap-find-class) (new-value name)
;;   (declare (optimize (debug 3)))
;;   (setf *!bootstrap-class-objects*
;;         (cons (cons name new-value)
;;               (remove name *!bootstrap-class-objects* :key #'car :test #'equal))))



;;; FIXME: allocate-standard-instance and
;;; allocate-standard-funcallable-instance are almost identical.
(defun allocate-standard-instance (wrapper
                                   &optional (slots-init nil slots-init-p))
  #!+sb-doc
  "Allocates an instance of STANDARD-INSTANCE with wrapper WRAPPER.
If a value is passed for SLOTS-INIT then the slots arra of the
instance is initialized with the contents of the list SLOTS-INIT."
  #+sb-xc-host
  (declare (optimize (debug 3)))
  (let ((instance (%make-standard-instance nil (get-instance-hash-code)))
        (no-of-slots (wrapper-no-of-instance-slots wrapper)))
    (setf (std-instance-wrapper instance) wrapper)
    (setf (std-instance-slots instance)
          (cond (slots-init-p
                 ;; Inline the slots vector allocation and initialization.
                 (let ((slots (make-array no-of-slots :initial-element 0)))
                   (do ((rem-slots slots-init (rest rem-slots))
                        (i 0 (1+ i)))
                       ((>= i no-of-slots)) ;endp rem-slots))
                     (declare (list rem-slots)
                              (type index i))
                     (setf (aref slots i) (first rem-slots)))
                   slots))
                (t
                 (make-array no-of-slots
                             :initial-element +slot-unbound+))))
    instance))

(defmacro allocate-standard-funcallable-instance-slots
    (wrapper &optional slots-init-p slots-init)
  #!+sb-doc
  "If a non-null value is passed for SLOTS-INIT then the slots array
of the instance is initialized with the contents of the list
SLOTS-INIT; otherwise the initial contents of the slots is
+SLOT-UNBOUND+."
  `(let ((no-of-slots (wrapper-no-of-instance-slots ,wrapper)))
    ,(if slots-init-p
         `(if ,slots-init-p
           (make-array no-of-slots :initial-contents ,slots-init)
           (make-array no-of-slots :initial-element +slot-unbound+))
         `(make-array no-of-slots :initial-element +slot-unbound+))))

(define-condition unset-funcallable-instance-function
    (reference-condition simple-error)
  ()
  (:default-initargs
   :references (list '(:amop :generic-function allocate-instance)
                     '(:amop :function set-funcallable-instance-function))))

(defun allocate-standard-funcallable-instance
    (wrapper &optional (slots-init nil slots-init-p))
  #!+sb-doc
  "Allocates an instance of STANDARD-FUNCALLABLE-INSTANCE with wrapper WRAPPER.
If a value is passed for SLOTS-INIT then the slots array of the
instance is initialized with the contents of the list SLOTS-INIT."

  #+sb-xc-host
  (declare (optimize (debug 3)))
  (let ((fin (%make-standard-funcallable-instance
              nil nil (get-instance-hash-code))))
    (set-funcallable-instance-function
     fin
     #'(lambda (&rest args)
         (declare (ignore args))
         (error 'unset-funcallable-instance-function
                :format-control "~@<The function of funcallable instance ~
                                 ~S has not been set.~@:>"
                :format-arguments (list fin))))
    (setf (fsc-instance-wrapper fin) wrapper
          (fsc-instance-slots fin)
          (allocate-standard-funcallable-instance-slots
           wrapper slots-init-p slots-init))
    fin))

;;;; BOOTSTRAP-META-BRAID
;;;;
;;;; This function builds the base metabraid from the early class definitions.

(defmacro !initial-classes-and-wrappers (&rest classes)
  #!+sb-doc
  "Expands each class name in the list of class names CLASSES into
forms that setf the wrapper, class, and (find-class class)."
  `(progn
     ,@(mapcar (lambda (class)
                 (let ((wr (format-symbol (pcl-package) "~A-WRAPPER" class)))
                   `(setf ,wr ,(if (eq class 'standard-generic-function)
                                   '*sgf-wrapper*
                                   `(boot-make-wrapper
                                     (early-class-size ',class)
                                     ',class))
                          ,class (allocate-standard-instance
                                  ,(if (eq class 'standard-generic-function)
                                       'funcallable-standard-class-wrapper
                                       'standard-class-wrapper))
                          (wrapper-class ,wr) ,class
                          (sb-xc:find-class ',class) ,class)))
               classes)))

(defun !bootstrap-meta-braid ()
  #+sb-xc-host
  (declare (optimize (debug 3)))
  (let* ((*create-classes-from-internal-structure-definitions-p* nil)
         standard-class-wrapper standard-class
         funcallable-standard-class-wrapper funcallable-standard-class
         slot-class-wrapper slot-class
         built-in-class-wrapper built-in-class
         structure-class-wrapper structure-class
         condition-class-wrapper condition-class
         standard-direct-slot-definition-wrapper
         standard-direct-slot-definition
         standard-effective-slot-definition-wrapper
         standard-effective-slot-definition
         class-eq-specializer-wrapper class-eq-specializer
         standard-generic-function-wrapper standard-generic-function)

    ;; 1.  Make a real class metaobject for the basic classes.
    ;; ALLOCATE-STANDARD-INSTANCE each metaclass and establish a
    ;; wrapper for it with BOOT-MAKE-WRAPPER.  Also set up (find-class
    ;; class-name) to point to the right instance
    (!initial-classes-and-wrappers standard-class
                                   funcallable-standard-class
                                   slot-class
                                   built-in-class
                                   structure-class
                                   condition-class
                                   standard-direct-slot-definition
                                   standard-effective-slot-definition
                                   class-eq-specializer
                                   standard-generic-function)
    ;; 2.  Allocate a real class metaobject for each of the early classes.
    (dolist (definition *early-class-definitions*)
      (let* ((name (ecd-class-name definition))
             (meta (ecd-metaclass definition))
             (wrapper (ecase meta
                        (slot-class slot-class-wrapper)
                        (standard-class standard-class-wrapper)
                        (funcallable-standard-class
                         funcallable-standard-class-wrapper)
                        (built-in-class built-in-class-wrapper)
                        (structure-class structure-class-wrapper)
                        (condition-class condition-class-wrapper)))
             (class (or (sb-xc:find-class name nil)
                        (allocate-standard-instance wrapper))))
        (setf (sb-xc:find-class name) class)))
    ;; 3.  Initialize the real class metaobjects just allocated for
    ;; each of the early classes.
    (dolist (definition *early-class-definitions*)
      (let ((name (ecd-class-name definition))
            (meta (ecd-metaclass definition))
            (source (ecd-source-location definition))
            (direct-supers (ecd-superclass-names definition))
            (direct-slots  (ecd-canonical-slots definition))
            (other-initargs (ecd-other-initargs definition)))
        (let ((direct-default-initargs
               (getf other-initargs :direct-default-initargs)))
          ;; Determine slot definitions, initargs, subclass names for
          ;; the early class, and set up its wrapper
          (multiple-value-bind (slots cpl default-initargs direct-subclasses)
              (early-collect-inheritance name)
            (let* ((class (sb-xc:find-class name))
                   (wrapper (cond ((eq class slot-class)
                                   slot-class-wrapper)
                                  ((eq class standard-class)
                                   standard-class-wrapper)
                                  ((eq class funcallable-standard-class)
                                   funcallable-standard-class-wrapper)
                                  ((eq class standard-direct-slot-definition)
                                   standard-direct-slot-definition-wrapper)
                                  ((eq class
                                       standard-effective-slot-definition)
                                   standard-effective-slot-definition-wrapper)
                                  ((eq class built-in-class)
                                   built-in-class-wrapper)
                                  ((eq class structure-class)
                                   structure-class-wrapper)
                                  ((eq class condition-class)
                                   condition-class-wrapper)
                                  ((eq class class-eq-specializer)
                                   class-eq-specializer-wrapper)
                                  ((eq class standard-generic-function)
                                   standard-generic-function-wrapper)
                                  (t
                                   (boot-make-wrapper (length slots) name))))
                   (proto nil))
              (when (eq name t)
                (setq *the-wrapper-of-t* wrapper))
              (let ((class-sym (make-class-symbol name)))
                (set class-sym  class))
              (dolist (slot slots)
                (unless (eq (getf slot :allocation :instance) :instance)
                  (error "Slot allocation ~S is not supported in bootstrap."
                         (getf slot :allocation))))

              (when (typep wrapper 'wrapper) ;; When will a wrapper not be of type 'wrapper?
                (setf (wrapper-instance-slots-layout wrapper)
                      (mapcar #'canonical-slot-name slots))
                (setf (wrapper-class-slots wrapper)
                      ()))

              (setq proto (if (eq meta 'funcallable-standard-class)
                              (allocate-standard-funcallable-instance wrapper)
                              (allocate-standard-instance wrapper)))

              ;; Make the slot definitions!
              (setq direct-slots
                    (!bootstrap-make-slot-definitions
                     name class direct-slots
                     standard-direct-slot-definition-wrapper nil))
              (setq slots
                    (!bootstrap-make-slot-definitions
                     name class slots
                     standard-effective-slot-definition-wrapper t))

              (setf (layout-slot-table wrapper) (make-slot-table class slots t))

              (case meta
                ((standard-class funcallable-standard-class)
                 (!bootstrap-initialize-class
                  meta
                  class name class-eq-specializer-wrapper source
                  direct-supers direct-subclasses cpl wrapper proto
                  direct-slots slots direct-default-initargs default-initargs))
                (built-in-class         ; *the-class-t*
                 (!bootstrap-initialize-class
                  meta
                  class name class-eq-specializer-wrapper source
                  direct-supers direct-subclasses cpl wrapper proto))
                (slot-class             ; *the-class-slot-object*
                 (!bootstrap-initialize-class
                  meta
                  class name class-eq-specializer-wrapper source
                  direct-supers direct-subclasses cpl wrapper proto))
                (structure-class        ; *the-class-structure-object*
                 (!bootstrap-initialize-class
                  meta
                  class name class-eq-specializer-wrapper source
                  direct-supers direct-subclasses cpl wrapper))
                (condition-class
                 (!bootstrap-initialize-class
                  meta
                  class name class-eq-specializer-wrapper source
                  direct-supers direct-subclasses cpl wrapper))))))))

    ;; 4.  Perform extra steps to set up standard method classes
    (setq **standard-method-classes**
          (mapcar (lambda (name)
                    (symbol-value (make-class-symbol name)))
                  *standard-method-class-names*))

    ;; 5.  Perform extra steps to set up standard method combination class
    (let* ((smc-class (sb-xc:find-class 'standard-method-combination))
           (smc-wrapper (!bootstrap-get-slot 'standard-class
                                             smc-class
                                             'wrapper))
           (smc (allocate-standard-instance smc-wrapper)))
      (flet ((set-slot (name value)
               (!bootstrap-set-slot 'standard-method-combination
                                    smc
                                    name
                                    value)))
        (set-slot 'source nil)
        (set-slot 'type-name 'standard)
        (set-slot '%documentation "The standard method combination.")
        (set-slot 'options ()))
      (setq *standard-method-combination* smc))))

;;; Initialize a class metaobject.
(defun !bootstrap-initialize-class
       (metaclass-name class name
        class-eq-wrapper source direct-supers direct-subclasses cpl wrapper
        &optional
        (proto nil proto-p)
        direct-slots slots direct-default-initargs default-initargs)
  #!+sb-doc
  "Sets up the slots for an already allocated class object CLASS with
metaclass named METACLASS-NAME, name NAME, etc.

PROTO is the prototype object of the class, if one has been
instatiated already.

DIRECT-SUPERS, DIRECT-SUBCLASSES, and CPL may be lists of class names,
not the class objects themselves, in which case find-class
should be used to find the actual class objects

DIRECT-SLOTS and SLOTS are real slot definition objects instantiated
with !BOOTSTRAP-MAKE-SLOT-DEFINITIONS"

  #+sb-xc-host
  (declare (optimize (debug 3)))

  (flet ((classes (names) (mapcar #'sb-xc:find-class names))
         (set-slot (slot-name value)
           (!bootstrap-set-slot metaclass-name class slot-name value)))
    (set-slot 'name name)
    (set-slot 'finalized-p t)
    (set-slot 'source source)
    (set-slot 'safe-p nil)
    (set-slot '%type (if (eq class (sb-xc:find-class t))
                         t
                         ;; FIXME: Could this just be CLASS instead
                         ;; of `(CLASS ,CLASS)? If not, why not?
                         ;; (See also similar expression in
                         ;; SHARED-INITIALIZE :BEFORE (CLASS).)
                         `(class ,class)))
    (set-slot 'class-eq-specializer
              (let ((spec (allocate-standard-instance class-eq-wrapper)))
                (!bootstrap-set-slot 'class-eq-specializer spec '%type
                                     `(class-eq ,class))
                (!bootstrap-set-slot 'class-eq-specializer spec 'object
                                     class)
                spec))
    (set-slot '%class-precedence-list (classes cpl))
    (set-slot 'cpl-available-p t)
    (set-slot 'can-precede-list (classes (cdr cpl)))
    (set-slot 'incompatible-superclass-list nil)
    (set-slot 'direct-superclasses (classes direct-supers))
    (set-slot 'direct-subclasses (classes direct-subclasses))
    (set-slot 'direct-methods (cons nil nil))
    (set-slot 'wrapper wrapper)
    (set-slot '%documentation nil)
    (set-slot 'plist
              `(,@(and direct-default-initargs
                       `(direct-default-initargs ,direct-default-initargs))
                ,@(and default-initargs
                       `(default-initargs ,default-initargs))))
    (when (find metaclass-name '(standard-class funcallable-standard-class
                                 structure-class condition-class
                                 slot-class))
      (set-slot 'direct-slots direct-slots)
      (set-slot 'slots slots)
      (setf (layout-slot-table wrapper)
            (make-slot-table class slots
                             (member metaclass-name
                                     '(standard-class funcallable-standard-class)))))

    ;; For all direct superclasses SUPER of CLASS, make sure CLASS is
    ;; a direct subclass of SUPER.  Note that METACLASS-NAME doesn't
    ;; matter here for the slot DIRECT-SUBCLASSES, since every class
    ;; inherits the slot from class CLASS.
    (dolist (super direct-supers)
      (let* ((super (sb-xc:find-class super))
             (subclasses (!bootstrap-get-slot metaclass-name super
                                              'direct-subclasses)))
        (cond ((eq +slot-unbound+ subclasses)
               (!bootstrap-set-slot metaclass-name super 'direct-subclasses
                                    (list class)))
              ((not (memq class subclasses))
               (!bootstrap-set-slot metaclass-name super 'direct-subclasses
                                    (cons class subclasses))))))


    (case metaclass-name
      (structure-class
         (let ((constructor-sym '|STRUCTURE-OBJECT class constructor|))
           (set-slot 'defstruct-form
                     `(defstruct (structure-object (:constructor
                                                    ,constructor-sym)
                                   (:copier nil))))
           (set-slot 'defstruct-constructor constructor-sym)
           (set-slot 'from-defclass-p t)
           (set-slot 'plist nil)
           (set-slot 'prototype (funcall constructor-sym))))
      (condition-class
         (set-slot 'prototype (make-condition name)))
      (t
         (if (and proto-p (consp proto) (eq :late (car proto)))
             (set-slot 'prototype (eval (second proto)))
             (set-slot 'prototype
                       (if proto-p proto (allocate-standard-instance wrapper))))))
    class))

(defun !bootstrap-make-slot-definitions (class-name class early-slots slot-wrapper effective-p)
  #!+sb-doc
  "Makes the real slot definitions for the class CLASS, which has
early slot definitions EARLY-SLOTS.  SLOT-WRAPPER is the wrapper
instance to use for this slot definition."

  #+sb-xc-host
  (declare (optimize (debug 3)))
  (let ((index -1))
    (mapcar (lambda (early-slot)
              (incf index)
              (!bootstrap-make-slot-definition
               class-name class early-slot slot-wrapper effective-p index))
            early-slots)))

(defun !bootstrap-make-slot-definition
    (class-name class early-slot slot-wrapper effective-p index)
  #!+sb-doc
  "Makes the real slot definition for the class CLASS, which has
early slot definitions EARLY-SLOTS.  SLOT-WRAPPER is the wrapper
instance to use for this slot definition."

  #+sb-xc-host
  (declare (optimize (debug 3)))

  (let* ((slotd-class-name (if effective-p
                               'standard-effective-slot-definition
                               'standard-direct-slot-definition))
         (slotd (allocate-standard-instance slot-wrapper))
         (slot-name (getf early-slot :name)))
    (flet ((get-val (name) (getf early-slot name))
           (set-val (name val)
                    (!bootstrap-set-slot slotd-class-name slotd name val)))
      (set-val 'name         slot-name)
      (set-val 'initform     (get-val :initform))
      (set-val 'initfunction (get-val :initfunction))
      (set-val 'initargs     (get-val :initargs))
      (set-val 'readers      (get-val :readers))
      (set-val 'writers      (get-val :writers))
      (set-val 'allocation   :instance)
      (set-val '%type        (or (get-val :type) t))
      (set-val '%type-check-function (get-val 'type-check-function))
      (set-val '%documentation (or (get-val :documentation) ""))
      (set-val '%class   class)
      (when effective-p
        (set-val 'location index)
        (let ((fsc-p nil))
          (set-val 'reader-function (make-optimized-std-reader-method-function
                                     fsc-p nil slot-name index))
          (set-val 'writer-function (make-optimized-std-writer-method-function
                                     fsc-p nil slot-name index))
          (set-val 'boundp-function (make-optimized-std-boundp-method-function
                                     fsc-p nil slot-name index)))
        (set-val 'accessor-flags 7))
      (when (and (eq class-name 'standard-class)
                 (eq slot-name 'slots) effective-p)
        (setq *the-eslotd-standard-class-slots* slotd))
      (when (and (eq class-name 'funcallable-standard-class)
                 (eq slot-name 'slots) effective-p)
        (setq *the-eslotd-funcallable-standard-class-slots* slotd))
      slotd)))

(defun !bootstrap-accessor-definitions (early-p)
  #!+sb-doc
  "Makes the accessor definitionss for the early class definitions."

  #+sb-xc-host
  (declare (optimize (debug 3)))

  (let ((*early-p* early-p))
    (dolist (definition *early-class-definitions*)
      (let ((name (ecd-class-name definition))
            (meta (ecd-metaclass definition)))
        (unless (eq meta 'built-in-class)
          (let ((direct-slots  (ecd-canonical-slots definition)))
            (dolist (slotd direct-slots)
              (let ((slot-name (getf slotd :name))
                    (readers (getf slotd :readers))
                    (writers (getf slotd :writers)))
                (!bootstrap-accessor-definitions1
                 name
                 slot-name
                 readers
                 writers
                 nil
                 (ecd-source-location definition))))))))))

(defun !bootstrap-ensure-generic-function (fun-name
                                          &rest all-keys
                                          &key environment source-location
                                          &allow-other-keys)
  #!+sb-doc
  "!bootstrap-ensure-generic-function works on early-generic functions
before they have been converted into first-order generics."
  (declare (ignore environment source-location))
  (apply 'sb-xc:ensure-generic-function fun-name all-keys))

(defun !bootstrap-add-method (generic-function method)
  #+sb-xc-host
  (declare (optimize (debug 3)))
  (when (not (fsc-instance-p generic-function))
    (error "Early ADD-METHOD didn't get a funcallable instance."))
  (when (not (and (listp method) (eq (car method) :early-method)))
    (error "Early ADD-METHOD didn't get an early method."))
  (push method (early-gf-methods generic-function))
  ;; (set-arg-info generic-function :new-method method)
  (unless (assoc (!early-gf-name generic-function)
                 *!generic-function-fixups*
                 :test #'equal)
    (update-dfun generic-function))
  (error "not implemented"))

(defun !bootstrap-accessor-definition (class-name accessor-name slot-name type source-location)
  #!+sb-doc
  "Makes a single accessor definition for the early class named
CLASS-NAME.  Type is one of the symbols READER WRITER BOUNDP."

  #+sb-xc-host
  (declare (optimize (debug 3)))

  (multiple-value-bind (accessor-class make-method-function arglist specls doc)
      (ecase type
        (reader (values 'standard-reader-method
                        #'make-std-reader-method-function
                        (list class-name)
                        (list class-name)
                        "automatically generated reader method"))
        (writer (values 'standard-writer-method
                        #'make-std-writer-method-function
                        (list 'new-value class-name)
                        (list t class-name)
                        "automatically generated writer method"))
        (boundp (values 'standard-boundp-method
                        #'make-std-boundp-method-function
                        (list class-name)
                        (list class-name)
                        "automatically generated boundp method")))
    (let ((gf (!bootstrap-ensure-generic-function accessor-name :lambda-list arglist)))
      (if (find specls (early-gf-methods gf)
                :key #'early-method-specializers
                :test 'equal)
          (unless (assoc accessor-name *!generic-function-fixups*
                         :test #'equal)
            (update-dfun gf))
          (add-method  gf
                       (early-make-a-method
                        accessor-class
                        ()
                        arglist specls
                        (funcall make-method-function
                                 class-name slot-name)
                        doc
                        :slot-name slot-name
                        :object-class class-name
                        :method-class-function (constantly (sb-xc:find-class accessor-class))
                        :definition-source source-location))))))

(defun !bootstrap-accessor-definitions1 (class-name
                                         slot-name
                                         readers
                                         writers
                                         boundps
                                         source-location)
  #!+sb-doc
  "Makes the accessor definitionss for the early class named
CLASS-NAME.  READERS, WRITERS, and BOUNDPS are lists with names of
generic functions for which accessors are to be defined."
  (flet ((do-reader-definition (reader)
           (!bootstrap-accessor-definition class-name
                                           reader
                                           slot-name
                                           'reader
                                           source-location))
         (do-writer-definition (writer)
           (!bootstrap-accessor-definition class-name
                                           writer
                                           slot-name
                                           'writer
                                           source-location))
         (do-boundp-definition (boundp)
           (!bootstrap-accessor-definition class-name
                                           boundp
                                           slot-name
                                           'boundp
                                           source-location)))
    (dolist (reader readers) (do-reader-definition reader))
    (dolist (writer writers) (do-writer-definition writer))
    (dolist (boundp boundps) (do-boundp-definition boundp))))

;;; FIXME: find a better name.
(defun !bootstrap-class-predicates (early-p)
  (let ((*early-p* early-p))
    (dolist (ecp *early-class-predicates*)
      (let ((class-name (car ecp))
            (predicate-name (cadr ecp)))
        (!bootstrap-make-class-predicate (sb-xc:find-class class-name) predicate-name)))))

;;; as a reminder, built-in-classes are things like FIXNUM and the
;;; like, not things like STANDARD-CLASS
(defun !bootstrap-built-in-classes ()
  #!+sb-doc
  "Bootstraps the built-in-classes (things like FIXNUM etc.)."

  ;; First make sure that all the supers listed in
  ;; *BUILT-IN-CLASS-LATTICE* are themselves defined by
  ;; *BUILT-IN-CLASS-LATTICE*. This is just to check for typos and
  ;; other sorts of brainos.
  (dolist (e *built-in-classes*)
    (dolist (super (cadr e))
      (unless (or (eq super t)
                  (assq super *built-in-classes*))
        (error "in *BUILT-IN-CLASSES*: ~S has ~S as a super,~%~
                but ~S is not itself a class in *BUILT-IN-CLASSES*."
               (car e) super super))))

  ;; In the first pass, we create a skeletal object to be bound to the
  ;; class name.
  (let* ((the-class-built-in-class (sb-xc:find-class 'built-in-class))
         (the-class-built-in-class-wrapper (class-wrapper the-class-built-in-class)))
    (dolist (e *built-in-classes*)
      (let ((class (allocate-standard-instance the-class-built-in-class-wrapper)))
        (setf (sb-xc:find-class (car e)) class))))

  ;; In the second pass, we initialize the class objects.
  (let ((class-eq-wrapper (class-wrapper (sb-xc:find-class 'class-eq-specializer))))
    (dolist (e *built-in-classes*)
      (destructuring-bind (name supers subs cpl prototype) e
        (let* ((class (sb-xc:find-class name))
               (lclass (find-classoid name))
               (wrapper (classoid-layout lclass)))
          (set (get-built-in-class-symbol name) class)
          (set (get-built-in-wrapper-symbol name) wrapper)
          (setf (classoid-pcl-class lclass) class)

          (!bootstrap-initialize-class 'built-in-class class
                                       name class-eq-wrapper nil
                                       supers subs
                                       (cons name cpl)
                                       wrapper prototype))))))

#!-sb-fluid (declaim (inline wrapper-of))
(defun wrapper-of (x)
  (layout-of x))

(defun sb-xc:class-of (x)
  (wrapper-class* (wrapper-of x)))

(defun eval-form (form)
  (lambda () (eval form)))

(defun ensure-non-standard-class (name classoid &optional existing-class)
  (flet
      ((ensure (metaclass &optional (slots nil slotsp))
         (let ((supers (mapcar #'classoid-name (classoid-direct-superclasses classoid))))
           (if slotsp
               (ensure-class-using-class existing-class name
                                         :metaclass metaclass :name name
                                         :direct-superclasses supers
                                         :direct-slots slots)
               (ensure-class-using-class existing-class name
                                         :metaclass metaclass :name name
                                         :direct-superclasses supers))))
       (slot-initargs-from-structure-slotd (slotd)
         (let ((accessor (structure-slotd-accessor-symbol slotd)))
           `(:name ,(structure-slotd-name slotd)
             :defstruct-accessor-symbol ,accessor
             ,@(when (fboundp accessor)
                 `(:internal-reader-function
                   ,(structure-slotd-reader-function slotd)
                   :internal-writer-function
                   ,(structure-slotd-writer-function name slotd)))
             :type ,(or (structure-slotd-type slotd) t)
             :initform ,(structure-slotd-init-form slotd)
             :initfunction ,(eval-form (structure-slotd-init-form slotd)))))
       (slot-initargs-from-condition-slot (slot)
         `(:name ,(condition-slot-name slot)
           :initargs ,(condition-slot-initargs slot)
           :readers ,(condition-slot-readers slot)
           :writers ,(condition-slot-writers slot)
           ,@(when (condition-slot-initform-p slot)
               (let ((form-or-fun (condition-slot-initform slot)))
                 (if (functionp form-or-fun)
                     `(:initfunction ,form-or-fun)
                     `(:initform ,form-or-fun
                       :initfunction ,(lambda () form-or-fun)))))
           :allocation ,(condition-slot-allocation slot)
           :documentation ,(condition-slot-documentation slot))))
    (cond ((structure-type-p name)
           (ensure 'structure-class
                   (mapcar #'slot-initargs-from-structure-slotd
                           (structure-type-slot-description-list name))))
          ((condition-type-p name)
           (ensure 'condition-class
                   (mapcar #'slot-initargs-from-condition-slot
                           (condition-classoid-slots classoid))))
          (t
           (error "~@<~S is not the name of a class.~@:>" name)))))

(defun ensure-deffoo-class (classoid)
  (let ((class (classoid-pcl-class classoid)))
    (cond (class
           (ensure-non-standard-class (class-name class) classoid class))
          ((eq 'complete **boot-state**)
           (ensure-non-standard-class (classoid-name classoid) classoid)))))

#+sb-xc
(pushnew 'ensure-deffoo-class sb!kernel::*defstruct-hooks*)
#+sb-xc
(pushnew 'ensure-deffoo-class sb!kernel::*define-condition-hooks*)

;;; FIXME: only needed during bootstrap, and uses COMPILE
(defun !bootstrap-make-class-predicate (class name)
  #!+sb-doc
  "Creates a method, and maybe generic function, with name NAME for
the class CLASS."
  (let* ((gf (!bootstrap-ensure-generic-function name :lambda-list '(object)))
         ;; list of methods.  at first glance, the if and then clauses
         ;; are reversed, but they are probably correct and I just
         ;; don't understand whats goin on exactly -- RED
         (mlist (if (eq **boot-state** 'complete)
                    (early-gf-methods gf)
                    (generic-function-methods gf))))
    (unless mlist
      (unless (eq class *the-class-t*)
        (let* ((default-method-function #'constantly-nil)
               (default-method-initargs (list :function default-method-function
                                              'plist '(:constant-value nil)))
               (default-method (make-a-method
                                'standard-method
                                ()
                                (list 'object)
                                (list *the-class-t*)
                                default-method-initargs
                                "class predicate default method")))
          (add-method gf default-method)))
      (let* ((class-method-function #'constantly-t)
             (class-method-initargs (list :function class-method-function
                                          'plist '(:constant-value t)))
             (class-method (make-a-method 'standard-method
                                          ()
                                          (list 'object)
                                          (list class)
                                          class-method-initargs
                                          "class predicate class method")))
        (add-method gf class-method)))
    gf))

;;; Set the inherits from CPL, and register the layout. This actually
;;; installs the class in the Lisp type system.
(defun %update-lisp-class-layout (class layout)
  #!+sb-doc
  "Set the inherits from class precedence list, and register the
layout. This actually installs the class in the Lisp type system."
  ;; Protected by *world-lock* in callers.
  (let ((classoid (layout-classoid layout))
        (olayout (class-wrapper class)))
    (declare (ignore olayout))
    (unless (eq (classoid-layout classoid) layout)
      (setf (layout-inherits layout)
            (order-layout-inherits
             (map 'simple-vector #'class-wrapper
                  (reverse (rest (class-precedence-list class))))))
      (register-layout layout :invalidate t)

      ;; FIXME: I don't think this should be necessary, but without it
      ;; we are unable to compile (TYPEP foo '<class-name>) in the
      ;; same file as the class is defined.  If we had environments,
      ;; then I think the classsoid whould only be associated with the
      ;; name in that environment...  Alternatively, fix the compiler
      ;; so that TYPEP foo '<class-name> is slow but compileable.
      (let ((name (class-name class)))
        (when (and name (symbolp name) (eq name (classoid-name classoid)))
          (setf (find-classoid name) classoid))))))

(defun %set-class-type-translation (class classoid)
  (when (not (typep classoid 'classoid))
    (setq classoid (find-classoid classoid nil)))
  (etypecase classoid
    (null)
    (built-in-classoid
     (let ((translation (built-in-classoid-translation classoid)))
       (cond
         (translation
          (aver (ctype-p translation))
          (setf (info :type :translator class)
                (lambda (spec) (declare (ignore spec)) translation)))
         (t
          (setf (info :type :translator class)
                (lambda (spec) (declare (ignore spec)) classoid))))))
    (classoid
     (setf (info :type :translator class)
           (lambda (spec) (declare (ignore spec)) classoid)))))


(defun !update-lisp-class-layouts-and-set-class-type-translations ()
  (dohash ((name x) sb!kernel::*classoid-cells*)
    (when (classoid-cell-pcl-class x)
      (let* ((class (find-class-from-cell name x))
             (layout (class-wrapper class))
             (lclass (layout-classoid layout))
             (lclass-pcl-class (classoid-pcl-class lclass))
             (olclass (find-classoid name nil)))
        (if lclass-pcl-class
            (aver (eq class lclass-pcl-class))
            (setf (classoid-pcl-class lclass) class))

        (%update-lisp-class-layout class layout)

        (cond (olclass
               (aver (eq lclass olclass)))
              (t
               (setf (find-classoid name) lclass)))

        (%set-class-type-translation class name)))))

;;;; Bootstrap!
#+sb-xc
(progn
  (!bootstrap-meta-braid)
  (!bootstrap-accessor-definitions t)
  (!bootstrap-class-predicates t)
  (!bootstrap-accessor-definitions nil)
  (!bootstrap-class-predicates nil)
  (!bootstrap-built-in-classes)
  (!update-lisp-class-layouts-and-set-class-type-translations))

(setq **boot-state** 'braid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  No applicable method and similar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod no-applicable-method (generic-function &rest args)
  (error "~@<There is no applicable method for the generic function ~2I~_~S~
          ~I~_when called with arguments ~2I~_~S.~:>"
         generic-function
         args))

(defmethod no-next-method ((generic-function standard-generic-function)
                           (method standard-method) &rest args)
  (error "~@<There is no next method for the generic function ~2I~_~S~
          ~I~_when called from method ~2I~_~S~I~_with arguments ~2I~_~S.~:>"
         generic-function
         method
         args))

;;; An extension to the ANSI standard: in the presence of e.g. a
;;; :BEFORE method, it would seem that going through
;;; NO-APPLICABLE-METHOD is prohibited, as in fact there is an
;;; applicable method.  -- CSR, 2002-11-15
(define-condition no-primary-method (reference-condition error)
  ((generic-function :initarg :generic-function :reader no-primary-method-generic-function)
   (args :initarg :args :reader no-primary-method-args))
  (:report
   (lambda (c s)
     (format s "~@<There is no primary method for the generic function ~2I~_~S~
                ~I~_when called with arguments ~2I~_~S.~:>"
             (no-primary-method-generic-function c)
             (no-primary-method-args c))))
  (:default-initargs :references (list '(:ansi-cl :section (7 6 6 2)))))
(defmethod no-primary-method (generic-function &rest args)
  (error 'no-primary-method :generic-function generic-function :args args))

(defmethod invalid-qualifiers ((gf generic-function)
                               combin
                               method)
  (let ((qualifiers (method-qualifiers method)))
    (let ((why (cond
                 ((cdr qualifiers) "has too many qualifiers")
                 (t (aver (not (member (car qualifiers)
                                       '(:around :before :after))))
                    "has an invalid qualifier"))))
      (invalid-method-error
       method
       "The method ~S on ~S ~A.~%~
        Standard method combination requires all methods to have one~%~
        of the single qualifiers :AROUND, :BEFORE and :AFTER or to~%~
        have no qualifier at all."
       method gf why))))
