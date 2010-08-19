;;;; "warm initialization": initialization which comes after cold init

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "COMMON-LISP-USER")

;;;; general warm init compilation policy

(proclaim '(optimize (compilation-speed 1)
                     (debug #+sb-show 2 #-sb-show 1)
                     (inhibit-warnings 2)
                     (safety 2)
                     (space 1)
                     (speed 2)))


;;;; package hacking

;;; Our cross-compilation host is out of the picture now, so we no
;;; longer need to worry about collisions between our package names
;;; and cross-compilation host package names, so now is a good time to
;;; rename any package with a bootstrap-only name SB!FOO to its
;;; permanent name SB-FOO.
;;;
;;; (In principle it might be tidier to do this when dumping the cold
;;; image in genesis, but in practice the logic might be a little
;;; messier because genesis dumps both symbols and packages, and we'd
;;; need to make sure that dumped symbols were renamed in the same way
;;; as dumped packages. Or we could do it in cold init, but it's
;;; easier to experiment with and debug things here in warm init than
;;; in cold init, so we do it here instead.)
(let ((boot-prefix "SB!")
      (perm-prefix "SB-"))
  (dolist (package (list-all-packages))
    (let ((old-package-name (package-name package)))
      (when (and (>= (length old-package-name) (length boot-prefix))
                 (string= boot-prefix old-package-name
                          :end2 (length boot-prefix)))
        (let ((new-package-name (concatenate 'string
                                             perm-prefix
                                             (subseq old-package-name
                                                     (length boot-prefix)))))
          (rename-package package
                          new-package-name
                          (package-nicknames package)))))))

;;; FIXME: This nickname is a deprecated hack for backwards
;;; compatibility with code which assumed the CMU-CL-style
;;; SB-ALIEN/SB-C-CALL split. That split went away and was deprecated
;;; in 0.7.0, so we should get rid of this nickname after a while.
(let ((package (find-package "SB-ALIEN")))
  (rename-package package
                  (package-name package)
                  (cons "SB-C-CALL" (package-nicknames package))))

(let ((package (find-package "SB-SEQUENCE")))
  (rename-package package (package-name package) (list "SEQUENCE")))

;;;; compiling and loading more of the system

;;; FIXME: CMU CL's pclcom.lisp had extra optional stuff wrapped around
;;; COMPILE-PCL, at least some of which we should probably have too:
;;;
;;; (with-compilation-unit
;;;     (:optimize '(optimize (debug #+(and (not high-security) small) .5
;;;                               #-(or high-security small) 2
;;;                               #+high-security 3)
;;;                        (speed 2) (safety #+(and (not high-security) small) 0
;;;                                          #-(or high-security small) 2
;;;                                          #+high-security 3)
;;;                        (inhibit-warnings 2))
;;;      :optimize-interface '(optimize-interface #+(and (not high-security) small)
;;; (safety 1)
;;;                                            #+high-security (safety 3))
;;;      :context-declarations
;;;      '((:external (declare (optimize-interface (safety #-high-security 2 #+high-
;;; security 3)
;;;                                             (debug #-high-security 1 #+high-s
;;; ecurity 3))))
;;;     ((:or :macro (:match "$EARLY-") (:match "$BOOT-"))
;;;     (declare (optimize (speed 0))))))
;;;
;;; FIXME: This has mutated into a hack which crudely duplicates
;;; functionality from the existing mechanism to load files from
;;; build-order.lisp-expr, without being quite parallel. (E.g. object
;;; files end up alongside the source files instead of ending up in
;;; parallel directory trees.) Maybe we could merge the filenames here
;;; into build-order.lisp-expr with some new flag (perhaps :WARM) to
;;; indicate that the files should be handled not in cold load but
;;; afterwards.
;;;
;;; Note RED 2009-10-23: Pathnames are now merged with those of the
;;; cold load but the below code is still a blatent cut-and-paste from
;;; src/cold/shared.lisp file.  This issue I had getting the rest over
;;; is that the code in shared.lisp is in the SB-COLD package that no
;;; longer exists once we are on the target.  To resolve this we need
;;; to put the stem code into a package that both SB-COLD and this
;;; file can load.  Not quite there yet.
(dolist (stem-src-and-obj-pathnames
          ;; Before 2009-10-23 this was a hard-coded list of
          ;; pathnames.  Now we reuse the build-order.lisp-expr
          ;; previously loaded and parsed during the cold build.  At
          ;; some point the SB-COLD emitted an s-expression with a
          ;; list of the warm init files that we should compile and
          ;; load now.  That's exactly what we do. -- RED 2009-10-23
          (with-open-file (s "output/warm-init-stems.lisp-expr") (read s)))

  (let* ((source-pathname (first stem-src-and-obj-pathnames))
         (object-pathname (second stem-src-and-obj-pathnames)))
    ;; FIXME: We should make object-pathname into an absolute pathname
    ;; so that COMPILE-FILE knows exactly what to do with it.  Even
    ;; better, fix the code in src/cold/shared.lisp, and use that code
    ;; here Right now we ignore the suggested object pathname with and
    ;; just use the default compile-file behavior.
    (declare (ignore object-pathname))

    (sb-int:/show "about to compile" source-pathname)
    (flet ((report-recompile-restart (stream)
             (format stream "Recompile file ~S" source-pathname))
           (report-continue-restart (stream)
             (format stream
                     "Continue, using possibly bogus file ~S"
                     (compile-file-pathname source-pathname))))
      (tagbody
       retry-compile-file
         (multiple-value-bind (output-truename warnings-p failure-p)
             (if *compile-files-p*
                 (compile-file source-pathname)
                 (compile-file-pathname source-pathname))
           (declare (ignore warnings-p))
           (sb-int:/show "done compiling" source-pathname)
           (cond ((not output-truename)
                  (error "COMPILE-FILE of ~S failed." source-pathname))
                 (failure-p
                  (unwind-protect
                       (restart-case
                           (error "FAILURE-P was set when creating ~S."
                                  output-truename)
                         (recompile ()
                           :report report-recompile-restart
                           (go retry-compile-file))
                         (continue ()
                           :report report-continue-restart
                           (setf failure-p nil)))
                    ;; Don't leave failed object files lying around.
                    (when (and failure-p (probe-file output-truename))
                      (delete-file output-truename)
                      (format t "~&deleted ~S~%" output-truename))))
                 ;; Otherwise: success, just fall through.
                 (t nil))
           (unless (load output-truename)
             (error "LOAD of ~S failed." output-truename))
           (sb-int:/show "done loading" output-truename))))))

;;;; setting package documentation

;;; While we were running on the cross-compilation host, we tried to
;;; be portable and not overwrite the doc strings for the standard
;;; packages. But now the cross-compilation host is only a receding
;;; memory, and we can have our way with the doc strings.
(sb-int:/show "setting package documentation")
#+sb-doc (setf (documentation (find-package "COMMON-LISP") t)
"public: home of symbols defined by the ANSI language specification")
#+sb-doc (setf (documentation (find-package "COMMON-LISP-USER") t)
               "public: the default package for user code and data")
#+sb-doc (setf (documentation (find-package "KEYWORD") t)
               "public: home of keywords")


