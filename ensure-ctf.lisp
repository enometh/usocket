(defpackage "ENSURE-CTF"
  (:use "CL")
  (:export "ENSURE-CTF"))
(in-package "ENSURE-CTF")

;; posted in comp.lang.lisp as
;; REQUIRE-COMPILE-TIME-FEATURES-AT-RUN-TIME

;; Date: Wed, 16 Feb 2022 01:46:19 +0530
;; Message-ID: <m37d9v51ik.fsf@leonis4.robolove.meer.net>

(defvar *compile-time-value-definer* nil
  "For internal only. Used by DEFCTV.  At the top level this should
always be bound to NIL." )

(defmacro defctv (var expr &optional default)
  "Like DEFVAR but defines a \"compile time value\".  EXPR is evaluated
only at compile time.  VAR is bound to the result (from the
evaluation of EXPR when the defctv form is compiled) or to DEFAULT if
there was no compilation."
  `(progn
     (defvar ,var ,default)
     (setq *compile-time-value-definer* nil)
     (eval-when (:compile-toplevel)
       (setq *compile-time-value-definer*
	     `(setq ,',var ',(eval ',expr))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (macrolet ((define-compile-time-value ()
		    *compile-time-value-definer*))
	 (eval-when (:load-toplevel :execute)
	   #+nil
	   (format t "LOAD EVAL DEFINED ~S: *CTV* ~S~&" (define-compile-time-value) *compile-time-value-definer*)
	   (define-compile-time-value))))))


#||
(defun cl-user::abazonk () (format t "BAR~&") 'v)
(cl-user::string->file "(ensure-ctf::defctv cl-user::foo (cl-user::abazonk) 10)" "/dev/shm/1.l")
(unintern 'cl-user::foo 'cl-user)
(compile-file "/dev/shm/1.l")
(assert (eql cl-user::foo 'v))
(unintern 'cl-user::foo 'cl-user)
(load (compile-file-pathname "/dev/shm/1.l"))
(assert (eq cl-user::foo 'v))
(unintern 'cl-user::foo 'cl-user)
(load "/dev/shm/1.l")
(assert (= cl-user::foo 10))
||#

(defmacro ensure-ctf (&rest features)
  "Ensure that features that were present (or absent) at compile time
are also present (or absent) at run time. FEATURES is a list of
keywords."
  (dolist (feat features) (check-type feat keyword))
  (let ((var #-clisp(gensym) ; XXX do other lisps need to intern the symbol?
	     #+clisp(intern (symbol-name (gensym)) "ENSURE-CTF")))
    `(progn
       ;; XXX does read-from-string always happen at compile time?
       (defctv ,var (read-from-string (format nil "(~{#+~(~a~) t ~:*#-~(~a~) nil~^ ~})" ',features)))
       (eval-when (load eval)
	 (let ((mismatches
		(loop for ctv in ,var
		      for feat in ',features
		      unless (eq ctv (and (find feat *features*) t))
		      append (list feat ctv))))
	   (assert (endp mismatches) nil
	       "~{~(~s~) was ~:[absent~;present~] at compile time but is ~:*~:[present~;absent~] at runtime.~^~%~}" mismatches))))))

#||
(defmacro with-features ((&rest features-spec) &body body)
  "FEATURES-SPEC is a list of keywords which are to be included in
feature. features after an &exclude token are removed from *features*"
  (let (included excluded (state 'include))
    (dolist (sym features-spec)
      (case sym
	(&include (setq state 'include))
	(&exclude (setq state 'exclude))
	(otherwise (ecase state
		     (include (push sym included))
		     (exclude (push sym excluded))))))
    `(let ((*features* *features*))
       (dolist (sym ',included) (pushnew sym *features*))
       (dolist (sym ',excluded) (setq *features* (delete sym *features*)))
       ,@body)))

(cl-user::string->file "(ensure-ctf:ensure-ctf :foo :bar)" "/dev/shm/1.l")
(with-features (:foo &exclude :bar) (compile-file "/dev/shm/1.l"))
(let ((*read-eval* nil))
  (with-features (:foo &exclude :bar) (compile-file "/dev/shm/1.l")))
(load (compile-file-pathname "/dev/shm/1.l"))
||#

