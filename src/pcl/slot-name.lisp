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


;;; FIXME -- we will probably need to maintain a list of these and
;;; rename them to the target platform's preferred format at cold init
;;; time.
#+sb-xc-host
(defun make-slot-symbol (slot-name type)
  #!+sb-doc
  "Generates a symbol to use as the reader/writer/boundp function for
a particular slot."
  (declare (type symbol slot-name))
  (unless (symbol-package slot-name)
    (error "On the host, slot symbols must have a package and ~S lacks one" slot-name))
  (intern (format nil "~A::~A slot ~a" 
                        (package-name (symbol-package slot-name))
                        (symbol-name slot-name)
                        type)
          (pcl-package)))      ;*slot-accessor-name-package*)))

(defun slot-reader-name (slot-name)
  #+sb-xc
  (list 'slot-accessor :global slot-name 'reader)
  #+sb-xc-host
  (make-slot-symbol slot-name 'reader))

(defun slot-writer-name (slot-name)
  #+sb-xc
  (list 'slot-accessor :global slot-name 'writer)
  #+sb-xc-host
  (make-slot-symbol slot-name 'reader))

(defun slot-boundp-name (slot-name)
  #+sb-xc
  (list 'slot-accessor :global slot-name 'boundp)
  #+sb-xc-host
  (make-slot-symbol slot-name 'reader))