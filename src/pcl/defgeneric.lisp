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

(defvar *!early-generic-functions* ()
  #!+sb-doc
  "A list of early generic function names (SPECs more specifically).
ENSURE-GENERIC-FUNCTION-USING-CLASS keeps track of early generic
functions as they are defined and conses up this list of their
names.")

;; The main defgeneric macro.  
(sb-xc:defmacro defgeneric (fun-name lambda-list &body options)
  (declare (type list lambda-list))
  (unless (legal-fun-name-p fun-name)
    (error 'simple-program-error
           :format-control "illegal generic function name ~S"
           :format-arguments (list fun-name)))
  (check-gf-lambda-list lambda-list)
  (let ((initargs ())
        (methods ()))
    (flet ((duplicate-option (name)
             (error 'simple-program-error
                    :format-control "The option ~S appears more than once."
                    :format-arguments (list name)))
           (expand-method-definition (qab) ; QAB = qualifiers, arglist, body
             (let* ((arglist-pos (position-if #'listp qab))
                    (arglist (elt qab arglist-pos))
                    (qualifiers (subseq qab 0 arglist-pos))
                    (body (nthcdr (1+ arglist-pos) qab)))
               `(push (defmethod ,fun-name ,@qualifiers ,arglist ,@body)
                      (generic-function-initial-methods (fdefinition ',fun-name))))))
      (macrolet ((initarg (key) `(getf initargs ,key)))
        (dolist (option options)
          (let ((car-option (car option)))
            (case car-option
              (declare
                 (when (and
                        (consp (cadr option))
                        (member (first (cadr option))
                                ;; FIXME: this list is slightly weird.
                                ;; ANSI (on the DEFGENERIC page) in one
                                ;; place allows only OPTIMIZE; in
                                ;; another place gives this list of
                                ;; disallowed declaration specifiers.
                                ;; This seems to be the only place where
                                ;; the FUNCTION declaration is
                                ;; mentioned; TYPE seems to be missing.
                                ;; Very strange.  -- CSR, 2002-10-21
                                '(declaration ftype function
                                  inline notinline special)))
                   (error 'simple-program-error
                          :format-control "The declaration specifier ~S ~
                                         is not allowed inside DEFGENERIC."
                          :format-arguments (list (cadr option))))
                 (push (cadr option) (initarg :declarations)))
              (:method-combination
                 (when (initarg car-option)
                   (duplicate-option car-option))
                 (unless (symbolp (cadr option))
                   (error 'simple-program-error
                          :format-control "METHOD-COMBINATION name not a ~
                                         symbol: ~S"
                          :format-arguments (list (cadr option))))
                 (setf (initarg car-option)
                       `',(cdr option)))
              (:argument-precedence-order
                 (let* ((required (parse-lambda-list lambda-list))
                        (supplied (cdr option)))
                   (unless (= (length required) (length supplied))
                     (error 'simple-program-error
                            :format-control "argument count discrepancy in ~
                                           :ARGUMENT-PRECEDENCE-ORDER clause."
                            :format-arguments nil))
                   (when (set-difference required supplied)
                     (error 'simple-program-error
                            :format-control "unequal sets for ~
                                           :ARGUMENT-PRECEDENCE-ORDER clause: ~
                                           ~S and ~S"
                            :format-arguments (list required supplied)))
                   (setf (initarg car-option)
                         `',(cdr option))))
              ((:documentation :generic-function-class :method-class)
                 (unless (proper-list-of-length-p option 2)
                   (error "bad list length for ~S" option))
                 (if (initarg car-option)
                     (duplicate-option car-option)
                     (setf (initarg car-option) `',(cadr option))))
              (:method
                 (push (cdr option) methods))
              (t
                 ;; ANSI requires that unsupported things must get a
                 ;; PROGRAM-ERROR.
                 (error 'simple-program-error
                        :format-control "unsupported option ~S"
                        :format-arguments (list option))))))

        (when (initarg :declarations)
          (setf (initarg :declarations)
                `',(initarg :declarations))))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (compile-or-load-defgeneric ',fun-name))
         (load-defgeneric ',fun-name ',lambda-list
                          (sb!c:source-location) ,@initargs)
         ,@(mapcar #'expand-method-definition methods)
         (fdefinition ',fun-name)))))

(defun compile-or-load-defgeneric (fun-name)
  (proclaim-as-fun-name fun-name)
  (note-name-defined fun-name :function)
  (unless (eq (info :function :where-from fun-name) :declared)
    (setf (info :function :where-from fun-name) :defined)
    (setf (info :function :type fun-name)
          (specifier-type 'function))))

(defun load-defgeneric (fun-name lambda-list source-location &rest initargs)
  (when (fboundp fun-name)
    (let ((fun (fdefinition fun-name)))
      (warn 'sb!kernel:redefinition-with-defgeneric :name fun-name
            :old fun :new-location source-location)
      (when (generic-function-p fun)
        (loop for method in (generic-function-initial-methods fun)
              do (remove-method fun method))
        (setf (generic-function-initial-methods fun) '()))))
  (apply #'ensure-generic-function
         fun-name
         :lambda-list lambda-list
         :definition-source source-location
         initargs))

(define-condition generic-function-lambda-list-error
    (reference-condition simple-program-error)
  ()
  (:default-initargs :references (list '(:ansi-cl :section (3 4 2)))))

(defun check-gf-lambda-list (lambda-list)
  (flet ((ensure (arg ok)
           (unless ok
             (error 'generic-function-lambda-list-error
                    :format-control
                    "~@<invalid ~S ~_in the generic function lambda list ~S~:>"
                    :format-arguments (list arg lambda-list)))))
    (multiple-value-bind (required optional restp rest keyp keys allowp
                          auxp aux morep more-context more-count)
        (parse-lambda-list lambda-list)
      (declare (ignore required)) ; since they're no different in a gf ll
      (declare (ignore restp rest)) ; since they're no different in a gf ll
      (declare (ignore allowp)) ; since &ALLOW-OTHER-KEYS is fine either way
      (declare (ignore aux)) ; since we require AUXP=NIL
      (declare (ignore more-context more-count)) ; safely ignored unless MOREP
      ;; no defaults allowed for &OPTIONAL arguments
      (dolist (i optional)
        (ensure i (or (symbolp i)
                      (and (consp i) (symbolp (car i)) (null (cdr i))))))
      ;; no defaults allowed for &KEY arguments
      (when keyp
        (dolist (i keys)
          (ensure i (or (symbolp i)
                        (and (consp i)
                             (or (symbolp (car i))
                                 (and (consp (car i))
                                      (symbolp (caar i))
                                      (symbolp (cadar i))
                                      (null (cddar i))))
                             (null (cdr i)))))))
      ;; no &AUX allowed
      (when auxp
        (error "&AUX is not allowed in a generic function lambda list: ~S"
               lambda-list))
      ;; Oh, *puhlease*... not specifically as per section 3.4.2 of
      ;; the ANSI spec, but the CMU CL &MORE extension does not
      ;; belong here!
      (aver (not morep)))))

(defun generic-function-name-p (name)
  (and (legal-fun-name-p name)
       (fboundp name)
       (if (eq **boot-state** 'complete)
           (standard-generic-function-p (gdefinition name))
           (pcl-funcallable-instance-p (gdefinition name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Ensure-generic-function and friends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ensure-generic-function (fun-name
                                &rest all-keys
                                &key environment source-location
                                &allow-other-keys)
  (declare (ignore environment source-location))
  (let ((existing (and (fboundp fun-name)
                       (gdefinition fun-name))))
    (cond ((and existing
                (eq **boot-state** 'complete)
                (null (generic-function-p existing)))
           (generic-clobbers-function fun-name)
           (fmakunbound fun-name)
           (apply #'ensure-generic-function fun-name all-keys))
          (t
           (apply #'ensure-generic-function-using-class
                  existing fun-name all-keys)))))

(defun generic-clobbers-function (fun-name)
  (cerror "Replace the function binding"
          'simple-program-error
          :format-control "~S already names an ordinary function or a macro."
          :format-arguments (list fun-name)))

(defvar *sgf-wrapper*
  (boot-make-wrapper (early-class-size 'standard-generic-function)
                     'standard-generic-function))

(defvar *sgf-slots-init*
  (mapcar (lambda (canonical-slot)
            (if (memq (getf canonical-slot :name) '(arg-info source))
                +slot-unbound+
                (let ((initfunction (getf canonical-slot :initfunction)))
                  (if initfunction
                      (funcall initfunction)
                      +slot-unbound+))))
          (early-collect-inheritance 'standard-generic-function)))

(defconstant +sgf-method-class-index+
  (!bootstrap-slot-index 'standard-generic-function 'method-class))

(defun early-gf-p (x)
  (and (fsc-instance-p x)
       (eq (clos-slots-ref (get-slots x) +sgf-method-class-index+)
           +slot-unbound+)))

(defconstant +sgf-methods-index+
  (!bootstrap-slot-index 'standard-generic-function 'methods))

(defmacro early-gf-methods (gf)
  `(clos-slots-ref (get-slots ,gf) +sgf-methods-index+))

(defun safe-generic-function-methods (generic-function)
  (if (eq (class-of generic-function) *the-class-standard-generic-function*)
      (clos-slots-ref (get-slots generic-function) +sgf-methods-index+)
      (generic-function-methods generic-function)))

(defconstant +sgf-arg-info-index+
  (!bootstrap-slot-index 'standard-generic-function 'arg-info))

(defmacro early-gf-arg-info (gf)
  `(clos-slots-ref (get-slots ,gf) +sgf-arg-info-index+))

(defconstant +sgf-dfun-state-index+
  (!bootstrap-slot-index 'standard-generic-function 'dfun-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Arg-info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (arg-info
            (:conc-name nil)
            (:constructor make-arg-info ())
            (:copier nil))
  (arg-info-lambda-list :no-lambda-list)
  arg-info-precedence
  arg-info-metatypes
  arg-info-number-optional
  arg-info-key/rest-p
  arg-info-keys   ;nil        no &KEY or &REST allowed
                  ;(k1 k2 ..) Each method must accept these &KEY arguments.
                  ;T          must have &KEY or &REST

  gf-info-simple-accessor-type ; nil, reader, writer, boundp
  (gf-precompute-dfun-and-emf-p nil) ; set by set-arg-info

  gf-info-static-c-a-m-emf
  (gf-info-c-a-m-emf-std-p t)
  gf-info-fast-mf-p)

#!-sb-fluid (declaim (sb!ext:freeze-type arg-info))

(defun arg-info-valid-p (arg-info)
  (not (null (arg-info-number-optional arg-info))))

(defun arg-info-applyp (arg-info)
  (or (plusp (arg-info-number-optional arg-info))
      (arg-info-key/rest-p arg-info)))

(defun arg-info-number-required (arg-info)
  (length (arg-info-metatypes arg-info)))

(defun arg-info-nkeys (arg-info)
  (count-if (lambda (x) (neq x t)) (arg-info-metatypes arg-info)))

(defun create-gf-lambda-list (lambda-list)
  ;;; Create a gf lambda list from a method lambda list
  (loop for x in lambda-list
        collect (if (consp x) (list (car x)) x)
        if (eq x '&key) do (loop-finish)))

(defun set-arg-info (gf &key new-method (lambda-list nil lambda-list-p)
                        argument-precedence-order)
  #+sb-xc-host
  (declare (optimize (debug 3)))
  (let* ((arg-info (if (eq **boot-state** 'complete)
                       (gf-arg-info gf)
                       (early-gf-arg-info gf)))
         (methods (if (eq **boot-state** 'complete)
                      (generic-function-methods gf)
                      (early-gf-methods gf)))
         (was-valid-p (integerp (arg-info-number-optional arg-info)))
         (first-p (and new-method (null (cdr methods)))))
    (when (and (not lambda-list-p) methods)
      (setq lambda-list (gf-lambda-list gf)))
    (when (or lambda-list-p
              (and first-p
                   (eq (arg-info-lambda-list arg-info) :no-lambda-list)))
      (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
          (analyze-lambda-list lambda-list)
        (when (and methods (not first-p))
          (let ((gf-nreq (arg-info-number-required arg-info))
                (gf-nopt (arg-info-number-optional arg-info))
                (gf-key/rest-p (arg-info-key/rest-p arg-info)))
            (unless (and (= nreq gf-nreq)
                         (= nopt gf-nopt)
                         (eq (or keysp restp) gf-key/rest-p))
              (error "The lambda-list ~S is incompatible with ~
                     existing methods of ~S."
                     lambda-list gf))))
        (setf (arg-info-lambda-list arg-info)
              (if lambda-list-p
                  lambda-list
                   (create-gf-lambda-list lambda-list)))
        (when (or lambda-list-p argument-precedence-order
                  (null (arg-info-precedence arg-info)))
          (setf (arg-info-precedence arg-info)
                (compute-precedence lambda-list nreq argument-precedence-order)))
        (setf (arg-info-metatypes arg-info) (make-list nreq))
        (setf (arg-info-number-optional arg-info) nopt)
        (setf (arg-info-key/rest-p arg-info) (not (null (or keysp restp))))
        (setf (arg-info-keys arg-info)
              (if lambda-list-p
                  (if allow-other-keys-p t keywords)
                  (arg-info-key/rest-p arg-info)))))
    (when new-method
      (check-method-arg-info gf arg-info new-method))
    (set-arg-info1 gf arg-info new-method methods was-valid-p first-p)
    arg-info))

(defun check-method-arg-info (gf arg-info method)
  (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
      (analyze-lambda-list (if (consp method)
                               (early-method-lambda-list method)
                               (method-lambda-list method)))
    (flet ((lose (string &rest args)
             (error 'simple-program-error
                    :format-control "~@<attempt to add the method~2I~_~S~I~_~
                                     to the generic function~2I~_~S;~I~_~
                                     but ~?~:>"
                    :format-arguments (list method gf string args)))
           (comparison-description (x y)
             (if (> x y) "more" "fewer")))
      (let ((gf-nreq (arg-info-number-required arg-info))
            (gf-nopt (arg-info-number-optional arg-info))
            (gf-key/rest-p (arg-info-key/rest-p arg-info))
            (gf-keywords (arg-info-keys arg-info)))
        (unless (= nreq gf-nreq)
          (lose
           "the method has ~A required arguments than the generic function."
           (comparison-description nreq gf-nreq)))
        (unless (= nopt gf-nopt)
          (lose
           "the method has ~A optional arguments than the generic function."
           (comparison-description nopt gf-nopt)))
        (unless (eq (or keysp restp) gf-key/rest-p)
          (lose
           "the method and generic function differ in whether they accept~_~
            &REST or &KEY arguments."))
        (when (consp gf-keywords)
          (unless (or (and restp (not keysp))
                      allow-other-keys-p
                      (every (lambda (k) (memq k keywords)) gf-keywords))
            (lose "the method does not accept each of the &KEY arguments~2I~_~
                   ~S."
                  gf-keywords)))))))

;;; This is the early definition of ENSURE-GENERIC-FUNCTION-USING-CLASS.
;;;
;;; The STATIC-SLOTS field of the funcallable instances used as early
;;; generic functions is used to store the early methods and early
;;; discriminator code for the early generic function. The static
;;; slots field of the fins contains a list whose:
;;;    CAR    -   a list of the early methods on this early gf
;;;    CADR   -   the early discriminator code for this method
(defun ensure-generic-function-using-class (existing spec &rest keys
                                            &key (lambda-list nil
                                                              lambda-list-p)
                                            argument-precedence-order
                                            source-location
                                            documentation
                                            &allow-other-keys)
  (declare (ignore keys))
  (cond ((and existing (early-gf-p existing))
         (when lambda-list-p
           (set-arg-info existing :lambda-list lambda-list))
         existing)

        ((assoc spec *!generic-function-fixups* :test #'equal)
         (if existing
             (make-early-gf spec lambda-list lambda-list-p existing
                            argument-precedence-order source-location
                            documentation)
             (bug "The function ~S is not already defined." spec)))
        (existing
         (bug "~S should be on the list ~S."
              spec '*!generic-function-fixups*))
        (t
         (pushnew spec *!early-generic-functions* :test #'equal)
         (make-early-gf spec lambda-list lambda-list-p nil
                        argument-precedence-order source-location
                        documentation))))

(defun make-early-gf (spec &optional lambda-list lambda-list-p
                      function argument-precedence-order source-location
                      documentation)
  ;; Important note!  Unlike other early structures like classes and
  ;; slot definitions, in SBCL we represent generic functions in the
  ;; same memory location from early into late.  This means pointers
  ;; to the early function do not need to be overwritten, and during
  ;; the fixup phase no new object is created.  Where does this
  ;; actually matter, and what happens if we take away this features?
  ;; Dont' know.  -- RED 08/31/2010
  #+sb-doc
  "Creats an early generic function named SPEC "
  (let ((fin (allocate-standard-funcallable-instance
              *sgf-wrapper* *sgf-slots-init*)))
    (set-funcallable-instance-function
     fin
     (or function
         (if (eq spec 'print-object)
             #'(lambda (instance stream)
                 (print-unreadable-object (instance stream :identity t)
                   (format stream "std-instance")))
             #'(lambda (&rest args)
                 (declare (ignore args))
                 (error "The function of the funcallable-instance ~S~
                         has not been set." fin)))))
    (setf (gdefinition spec) fin)
    (!bootstrap-set-slot 'standard-generic-function fin 'name spec)
    (!bootstrap-set-slot 'standard-generic-function fin
                         'source source-location)
    (!bootstrap-set-slot 'standard-generic-function fin
                         '%documentation documentation)
    (set-fun-name fin spec)
    (let ((arg-info (make-arg-info)))
      (setf (early-gf-arg-info fin) arg-info)
      (when lambda-list-p
        (setf (info :function :type spec)
              (specifier-type
               (ftype-declaration-from-lambda-list lambda-list spec))
              (info :function :where-from spec) :defined-method)
        (if argument-precedence-order
            (set-arg-info fin
                          :lambda-list lambda-list
                          :argument-precedence-order argument-precedence-order)
            (set-arg-info fin :lambda-list lambda-list))))
    fin))

(defun safe-gf-dfun-state (generic-function)
  (if (eq (class-of generic-function) *the-class-standard-generic-function*)
      (clos-slots-ref (fsc-instance-slots generic-function) +sgf-dfun-state-index+)
      (gf-dfun-state generic-function)))
(defun (setf safe-gf-dfun-state) (new-value generic-function)
  (if (eq (class-of generic-function) *the-class-standard-generic-function*)
      (setf (clos-slots-ref (fsc-instance-slots generic-function)
                            +sgf-dfun-state-index+)
            new-value)
      (setf (gf-dfun-state generic-function) new-value)))

(defun set-dfun (gf &optional dfun cache info)
  (let ((new-state (if (and dfun (or cache info))
                       (list* dfun cache info)
                       dfun)))
    (cond
      ((eq **boot-state** 'complete)
       ;; Check that we are under the lock.
       #!+sb-thread
       (aver (eq sb!thread:*current-thread* (sb!thread::spinlock-value (gf-lock gf))))
       (setf (safe-gf-dfun-state gf) new-state))
      (t
       (setf (clos-slots-ref (get-slots gf) +sgf-dfun-state-index+)
             new-state))))
  dfun)



(defun gf-dfun-cache (gf)
  (let ((state (if (eq **boot-state** 'complete)
                   (safe-gf-dfun-state gf)
                   (clos-slots-ref (get-slots gf) +sgf-dfun-state-index+))))
    (typecase state
      (function nil)
      (cons (cadr state)))))

(defun gf-dfun-info (gf)
  (let ((state (if (eq **boot-state** 'complete)
                   (safe-gf-dfun-state gf)
                   (clos-slots-ref (get-slots gf) +sgf-dfun-state-index+))))
    (typecase state
      (function nil)
      (cons (cddr state)))))

(defconstant +sgf-name-index+
  (!bootstrap-slot-index 'standard-generic-function 'name))

(defun !early-gf-name (gf)
  (clos-slots-ref (get-slots gf) +sgf-name-index+))

(defun gf-lambda-list (gf)
  (let ((arg-info (if (eq **boot-state** 'complete)
                      (gf-arg-info gf)
                      (early-gf-arg-info gf))))
    (if (eq :no-lambda-list (arg-info-lambda-list arg-info))
        (let ((methods (if (eq **boot-state** 'complete)
                           (generic-function-methods gf)
                           (early-gf-methods gf))))
          (if (null methods)
              (progn
                (warn "no way to determine the lambda list for ~S" gf)
                nil)
              (let* ((method (car (last methods)))
                     (ll (if (consp method)
                             (early-method-lambda-list method)
                             (method-lambda-list method))))
                (create-gf-lambda-list ll))))
        (arg-info-lambda-list arg-info))))

(defmacro real-ensure-gf-internal (gf-class all-keys env)
  `(progn
     (cond ((symbolp ,gf-class)
            (setq ,gf-class (sb-xc:find-class ,gf-class t ,env)))
           ((classp ,gf-class))
           (t
            (error "The :GENERIC-FUNCTION-CLASS argument (~S) was neither a~%~
                    class nor a symbol that names a class."
                   ,gf-class)))
     (unless (class-finalized-p ,gf-class)
       (if (class-has-a-forward-referenced-superclass-p ,gf-class)
           ;; FIXME: reference MOP documentation -- this is an
           ;; additional requirement on our users
           (error "The generic function class ~S is not finalizeable" ,gf-class)
           (finalize-inheritance ,gf-class)))
     (remf ,all-keys :generic-function-class)
     (remf ,all-keys :environment)
     (let ((combin (getf ,all-keys :method-combination '.shes-not-there.)))
       (unless (eq combin '.shes-not-there.)
         (setf (getf ,all-keys :method-combination)
               (find-method-combination (class-prototype ,gf-class)
                                        (car combin)
                                        (cdr combin)))))
    (let ((method-class (getf ,all-keys :method-class '.shes-not-there.)))
      (unless (eq method-class '.shes-not-there.)
        (setf (getf ,all-keys :method-class)
              (cond ((classp method-class)
                     method-class)
                    (t (sb-xc:find-class method-class t ,env))))))))

(defun note-gf-signature (fun-name lambda-list-p lambda-list)
  (unless lambda-list-p
    ;; Use the existing lambda-list, if any. It is reasonable to do eg.
    ;;
    ;;   (if (fboundp name)
    ;;       (ensure-generic-function name)
    ;;       (ensure-generic-function name :lambda-list '(foo)))
    ;;
    ;; in which case we end up here with no lambda-list in the first leg.
    (setf (values lambda-list lambda-list-p)
          (handler-case
              (values (generic-function-lambda-list (fdefinition fun-name))
                      t)
            ((or warning error) ()
              (values nil nil)))))
  (let ((gf-type
         (specifier-type
          (if lambda-list-p
              (ftype-declaration-from-lambda-list lambda-list fun-name)
              'function)))
        (old-type nil))
    ;; FIXME: Ideally we would like to not clobber it, but because generic
    ;; functions assert their FTYPEs callers believing the FTYPE are left with
    ;; unsafe assumptions. Hence the clobbering. Be quiet when the new type
    ;; is a subtype of the old one, though -- even though the type is not
    ;; trusted anymore, the warning is still not quite as interesting.
    (when (and (eq :declared (info :function :where-from fun-name))
               (not (csubtypep gf-type (setf old-type (info :function :type fun-name)))))
      (style-warn "~@<Generic function ~S clobbers an earlier ~S proclamation ~S ~
                   for the same name with ~S.~:@>"
                  fun-name 'ftype
                  (type-specifier old-type)
                  (type-specifier gf-type)))
    (setf (info :function :type fun-name) gf-type
          (info :function :where-from fun-name) :defined-method)
    fun-name))

(defun real-ensure-gf-using-class--generic-function
       (existing
        fun-name
        &rest all-keys
        &key environment (lambda-list nil lambda-list-p)
        (generic-function-class 'standard-generic-function)
        &allow-other-keys)
  (real-ensure-gf-internal generic-function-class all-keys environment)
  ;; KLUDGE: the above macro does SETQ on GENERIC-FUNCTION-CLASS,
  ;; which is what makes the next line work
  (unless (eq (class-of existing) generic-function-class)
    (change-class existing generic-function-class))
  (prog1
      (apply #'reinitialize-instance existing all-keys)
    (note-gf-signature fun-name lambda-list-p lambda-list)))

(defun real-ensure-gf-using-class--null
       (existing
        fun-name
        &rest all-keys
        &key environment (lambda-list nil lambda-list-p)
             (generic-function-class 'standard-generic-function)
        &allow-other-keys)
  (declare (ignore existing))
  (real-ensure-gf-internal generic-function-class all-keys environment)
  (prog1
      (setf (gdefinition fun-name)
            (apply #'make-instance generic-function-class
                   :name fun-name all-keys))
    (note-gf-signature fun-name lambda-list-p lambda-list)))

(defun safe-gf-arg-info (generic-function)
  (if (eq (class-of generic-function) *the-class-standard-generic-function*)
      (clos-slots-ref (fsc-instance-slots generic-function)
                      +sgf-arg-info-index+)
      (gf-arg-info generic-function)))

;;; FIXME: this function took on a slightly greater role than it
;;; previously had around 2005-11-02, when CSR fixed the bug whereby
;;; having more than one subclass of standard-generic-function caused
;;; the whole system to die horribly through a metacircle in
;;; GF-ARG-INFO.  The fix is to be slightly more disciplined about
;;; calling accessor methods -- we call GET-GENERIC-FUN-INFO when
;;; computing discriminating functions, so we need to be careful about
;;; having a base case for the recursion, and we provide that with the
;;; STANDARD-GENERIC-FUNCTION case below.  However, we are not (yet)
;;; as disciplined as CLISP's CLOS/MOP, and it would be nice to get to
;;; that stage, where all potentially dangerous cases are enumerated
;;; and stopped.  -- CSR, 2005-11-02.
(defun get-generic-fun-info (gf)
  ;; values   nreq applyp metatypes nkeys arg-info
  (multiple-value-bind (applyp metatypes arg-info)
      (let* ((arg-info (if (early-gf-p gf)
                           (early-gf-arg-info gf)
                           (safe-gf-arg-info gf)))
             (metatypes (arg-info-metatypes arg-info)))
        (values (arg-info-applyp arg-info)
                metatypes
                arg-info))
    (values (length metatypes) applyp metatypes
            (count-if (lambda (x) (neq x t)) metatypes)
            arg-info)))