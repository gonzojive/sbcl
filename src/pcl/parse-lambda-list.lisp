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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Normal lambda list processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun analyze-lambda-list (lambda-list)
  #!+sb-doc
  "Analyzes a normal lambda list and returns values

   (nrequired noptional keysp restp allow-other-keys-p keywords  keyword-parameters)

KEYWORDS are the keywords that must be passed to a function with the
given list.  KEYWORD-PARAMETERS are the raw keyword arguments.
"
  (flet (;; FIXME: Is this redundant with SB!C::MAKE-KEYWORD-FOR-ARG ?
         (parse-key-arg (arg)
           (if (listp arg)
               (if (listp (car arg))
                   (caar arg)
                   (keywordicate (car arg)))
               (keywordicate arg))))
    (let ((nrequired 0)
          (noptional 0)
          (keysp nil)
          (restp nil)
          (nrest 0)
          (allow-other-keys-p nil)
          (keywords ())
          (keyword-parameters ())
          (state 'required))
      (dolist (x lambda-list)
        (if (find x lambda-list-keywords :test 'eq)
            (case x
              (&optional         (setq state 'optional))
              (&key              (setq keysp t
                                       state 'key))
              (&allow-other-keys (setq allow-other-keys-p t))
              (&rest             (setq restp t
                                       state 'rest))
              (&aux           (return t))
              (otherwise
                (error "encountered the non-standard lambda list keyword ~S"
                       x)))
            (ecase state
              (required  (incf nrequired))
              (optional  (incf noptional))
              (key       (push (parse-key-arg x) keywords)
                         (push x keyword-parameters))
              (rest      (incf nrest)))))
     (when (and restp (zerop nrest))
        (error "Error in lambda-list:~%~
                After &REST, a DEFGENERIC lambda-list ~
                must be followed by at least one variable."))
      (values nrequired noptional keysp restp allow-other-keys-p
              (reverse keywords)
              (reverse keyword-parameters)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Establishing bindings to a lambda list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro bind-args ((lambda-list args) &body body)
  #!+sb-doc
  "Binds the variables defined in the lambda list LAMBDA-LIST to the
list of arguments ARGS (evaluated), and then executes body."
  (let ((args-tail '.args-tail.)
        (key '.key.)
        (state 'required))
    (flet ((process-var (var)
             (if (memq var lambda-list-keywords)
                 (progn
                   (case var
                     (&optional       (setq state 'optional))
                     (&key            (setq state 'key))
                     (&allow-other-keys)
                     (&rest           (setq state 'rest))
                     (&aux            (setq state 'aux))
                     (otherwise
                      (error
                       "encountered the non-standard lambda list keyword ~S"
                       var)))
                   nil)
                 (case state
                   (required `((,var (pop ,args-tail))))
                   (optional (cond ((not (consp var))
                                    `((,var (when ,args-tail
                                              (pop ,args-tail)))))
                                   ((null (cddr var))
                                    `((,(car var) (if ,args-tail
                                                      (pop ,args-tail)
                                                      ,(cadr var)))))
                                   (t
                                    `((,(caddr var) (not (null ,args-tail)))
                                      (,(car var) (if ,args-tail
                                                      (pop ,args-tail)
                                                      ,(cadr var)))))))
                   (rest `((,var ,args-tail)))
                   (key (cond ((not (consp var))
                               `((,var (car
                                        (get-key-arg-tail ,(keywordicate var)
                                                          ,args-tail)))))
                              ((null (cddr var))
                               (multiple-value-bind (keyword variable)
                                   (if (consp (car var))
                                       (values (caar var)
                                               (cadar var))
                                       (values (keywordicate (car var))
                                               (car var)))
                                 `((,key (get-key-arg-tail ',keyword
                                                           ,args-tail))
                                   (,variable (if ,key
                                                  (car ,key)
                                                  ,(cadr var))))))
                              (t
                               (multiple-value-bind (keyword variable)
                                   (if (consp (car var))
                                       (values (caar var)
                                               (cadar var))
                                       (values (keywordicate (car var))
                                               (car var)))
                                 `((,key (get-key-arg-tail ',keyword
                                                           ,args-tail))
                                   (,(caddr var) (not (null,key)))
                                   (,variable (if ,key
                                                  (car ,key)
                                                  ,(cadr var))))))))
                   (aux `(,var))))))
      (let ((bindings (mapcan #'process-var lambda-list)))
        `(let* ((,args-tail ,args)
                ,@bindings
                (.dummy0.
                 ,@(when (eq state 'optional)
                     `((unless (null ,args-tail)
                         (error 'simple-program-error
                                :format-control "surplus arguments: ~S"
                                :format-arguments (list ,args-tail)))))))
           (declare (ignorable ,args-tail .dummy0.))
           ,@body)))))

(defun get-key-arg-tail (keyword list)
  #!+sb-doc
  "Finds the first even-positioned element in LIST that is EQ to to
KEYWORD and returns everything in the list after it."
  (loop for (key . tail) on list by #'cddr
        when (null tail) do
          ;; FIXED: Do we want to export this symbol? Or maybe use an
          ;; (ERROR 'SIMPLE-PROGRAM-ERROR) form?
          ;;   (sb-c::%odd-key-args-error)
          (error 'simple-program-error 
                 :format-control "List has odd number of keyword arguments: ~S"
                 :format-arguments (list list))
        when (eq key keyword)
          return tail))

(defun lambda-list-parameter-names (lambda-list)
  ;; Given a valid lambda list, extract the parameter names.
  (loop for x in lambda-list
        with res = nil
        do (unless (member x lambda-list-keywords :test #'eq)
             (if (consp x)
                 (let ((name (car x)))
                   (if (consp name)
                       ;; ... ((:BAR FOO) 1)
                       (push (second name) res)
                       ;; ... (FOO 1)
                       (push name res))
                   ;; ... (... 1 FOO-P)
                   (let ((name-p (cddr x)))
                     (when name-p
                       (push (car name-p) res))))
                 ;; ... FOO
                 (push x res)))
        finally (return res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Specialized lambda list processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition specialized-lambda-list-error
    (reference-condition simple-program-error)
  ()
  (:default-initargs :references (list '(:ansi-cl :section (3 4 3)))))

(defun parse-specialized-lambda-list
    (arglist
     &optional supplied-keywords (allowed-keywords '(&optional &rest &key &aux))
     &aux (specialized-lambda-list-keywords
           '(&optional &rest &key &allow-other-keys &aux)))
  #!+sb-doc
  "Parses a specialized-lambda-list and returns the following 4 values:

   (parameters lambda-list specializers required-parameters)

PARAMETERS is a list with any lambda-list keywords (like &optional)
stripped out, default argument values removed, and keyword arguments
either as symbols are as lists of the form (keyword
keyword-variable-name).

LAMBDA-LIST is an ordinary lambda list with specializers removed

SPECIALIZERS is the value of the specializers provided with required
parameters, or T when no specializer was specified.

REQUIRED-PARAMETERS is a list of the names of the required parts of
the lambda list."
  (let ((arg (car arglist)))
    (cond ((null arglist) (values nil nil nil nil))
          ((eq arg '&aux)
           (values nil arglist nil nil))
          ((find arg lambda-list-keywords)
           ;; non-standard lambda-list-keywords are errors.
           (unless (find arg specialized-lambda-list-keywords)
             (error 'specialized-lambda-list-error
                    :format-control "unknown specialized-lambda-list ~
                                     keyword ~S~%"
                    :format-arguments (list arg)))
           ;; no multiple &rest x &rest bla specifying
           (when (find arg supplied-keywords)
             (error 'specialized-lambda-list-error
                    :format-control "multiple occurrence of ~
                                     specialized-lambda-list keyword ~S~%"
                    :format-arguments (list arg)))
           ;; And no placing &key in front of &optional, either.
           (unless (find arg allowed-keywords)
             (error 'specialized-lambda-list-error
                    :format-control "misplaced specialized-lambda-list ~
                                     keyword ~S~%"
                    :format-arguments (list arg)))
           ;; When we are at a lambda-list keyword, the parameters
           ;; don't include the lambda-list keyword; the lambda-list
           ;; does include the lambda-list keyword; and no
           ;; specializers are allowed to follow the lambda-list
           ;; keywords (at least for now).
           (multiple-value-bind (parameters lambda-list)
               (parse-specialized-lambda-list (cdr arglist)
                                              (cons arg supplied-keywords)
                                              (if (eq arg '&key)
                                                  (cons '&allow-other-keys
                                                        (cdr (member arg allowed-keywords)))
                                                (cdr (member arg allowed-keywords))))
             (when (and (eq arg '&rest)
                        (or (null lambda-list)
                            (find (car lambda-list)
                                  specialized-lambda-list-keywords)
                            (not (or (null (cadr lambda-list))
                                     (find (cadr lambda-list)
                                           specialized-lambda-list-keywords)))))
               (error 'specialized-lambda-list-error
                      :format-control
                      "in a specialized-lambda-list, excactly one ~
                       variable must follow &REST.~%"
                      :format-arguments nil))
             (values parameters
                     (cons arg lambda-list)
                     ()
                     ())))
          (supplied-keywords
           ;; After a lambda-list keyword there can be no specializers.
           (multiple-value-bind (parameters lambda-list)
               (parse-specialized-lambda-list (cdr arglist)
                                              supplied-keywords
                                              allowed-keywords)
             (values (cons (if (listp arg) (car arg) arg) parameters)
                     (cons arg lambda-list)
                     ()
                     ())))
          (t
           (multiple-value-bind (parameters lambda-list specializers required)
               (parse-specialized-lambda-list (cdr arglist))
             ;; Check for valid arguments.
             (unless (or (and (symbolp arg) (not (null arg)))
                         (and (consp arg)
                              (consp (cdr arg))
                              (null (cddr arg))))
               (error 'specialized-lambda-list-error
                      :format-control "arg is not a non-NIL symbol or a list of two elements: ~A"
                      :format-arguments (list arg)))
             (values (cons (if (listp arg) (car arg) arg) parameters)
                     (cons (if (listp arg) (car arg) arg) lambda-list)
                     (cons (if (listp arg) (cadr arg) t) specializers)
                     (cons (if (listp arg) (car arg) arg) required)))))))

(defun extract-parameters (specialized-lambda-list)
  #!+sb-doc
  "Parses a specialized lambda list and returns PARAMETERS, a list
with any lambda-list keywords (like &optional) stripped out, default
argument values removed, and keyword arguments either as
symbols are as lists of the form (keyword keyword-variable-name)."
  (multiple-value-bind (parameters ignore1 ignore2)
      (parse-specialized-lambda-list specialized-lambda-list)
    (declare (ignore ignore1 ignore2))
    parameters))

(defun extract-lambda-list (specialized-lambda-list)
  #!+sb-doc
  "Parses a specialized lambda list and returns an ordinary lambda
list (without specializers)."
  (multiple-value-bind (ignore1 lambda-list ignore2)
      (parse-specialized-lambda-list specialized-lambda-list)
    (declare (ignore ignore1 ignore2))
    lambda-list))

(defun extract-specializer-names (specialized-lambda-list)
  #!+sb-doc
  "Parses a specialized lambda list and returns SPECIALIZER-NAMES, the
value of the specializers provided with required parameters, or T when
no specializer was specified."
  (multiple-value-bind (ignore1 ignore2 specializers)
      (parse-specialized-lambda-list specialized-lambda-list)
    (declare (ignore ignore1 ignore2))
    specializers))

(defun extract-required-parameters (specialized-lambda-list)
  #!+sb-doc
  "Parses a specialized lambda list and returns the a list of the
names of the required parts of the lambda list, with specializer
designators removed."
  (multiple-value-bind (ignore1 ignore2 ignore3 required-parameters)
      (parse-specialized-lambda-list specialized-lambda-list)
    (declare (ignore ignore1 ignore2 ignore3))
    required-parameters))

