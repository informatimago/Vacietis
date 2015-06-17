(in-package #:vacietis)
(in-readtable vacietis)

(defun pp-defines (pkg)
  (intern "*PREPROCESSOR-DEFINES*" pkg))

(defmacro define (name value &rest docstring)
  "Like a #define but also visible to Lisp code."
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,(pp-defines *package*) (make-hash-table))
       (setf (gethash ',name ,(pp-defines *package*)) ,(prin1-to-string value)))
     (defparameter ,name ,value ,@docstring)))

(defmacro defun/1 (name arglist &body body)
  "Lisp-1 defun; makes function pointers work."
  `(progn
     (defun ,name ,arglist
       ,@body)
     (defparameter ,name (vacietis.c:mkptr& (symbol-function ',name)))))




;; #include <> searches files only in *include-bracket-directories*
;;
;; #include "" searches files
;; in the current directory unless *include-disable-current-directory* is true,
;; then in *include-quote-directories*
;; and finally in *include-bracket-directories*.

(defvar *include-disable-current-directory* nil
  "When true, files are not searched in the current directory.
NOTE: current directory is defined as:
       (or *load-truename* *compile-file-truename*
           *default-pathname-defaults*)
")

(defvar *include-quote-directories*   '()
  "Directories where #include \"\" files are searched.")

(defvar *include-bracket-directories* '(:libc)
  "Directories where #include <> files are searched.
May contain :LIBC to indicate searching Vacietis libc system includes.")



(defun search-libc-file (include-file)
  "
RETURN: NIL if include-file is not found,
        T   if include-file is already included (as a lisp package), or
        a pathname to the include-file to be loaded.
"
  (let ((libc-package (find-package (format nil "VACIETIS.LIBC.~:@(~A~)"
                                            include-file))))
    (unless libc-package
      (return-from search-libc-file nil))
    (use-package libc-package)
    (awhen (and (boundp       (pp-defines libc-package))
                (symbol-value (pp-defines libc-package)))
      (maphash (lambda (name expansion)
                 (setf (gethash (intern (symbol-name name) *package*)
                                (compiler-state-pp *compiler-state*))
                       expansion))
               it))
    (awhen (probe-file
            (merge-pathnames
             (format nil "../libc/~a" include-file)
             #.(or *compile-file-truename* *load-truename*)))
      it)
    t))


(defun search-file-in-directories (include-file directories)
  (loop
    :for directory :in directories
    :for path = (if (eq :libc directory)
                    (search-libc-file include-file)
                    (merge-pathnames include-file directory))
    :when (and path (or (eq t path) (probe-file path)))
      :do (return path)
    :finally (return nil)))


(defun include-directories (mode)
  (append (if (eq mode :quote)
              (remove-duplicates
               (append
                (unless *include-disable-current-directory*
                  (list (make-pathname :name nil :type nil :version nil
                                       :defaults (or *load-truename* *compile-file-truename*
                                                     *default-pathname-defaults*))))
                *include-quote-directories*)
               :test (function equal))
              '())
          (remove-duplicates
           *include-bracket-directories*
           :test (function equal))))


(defun include-file (include-file mode)
  (let* ((include-directories (include-directories mode))
         (path (search-file-in-directories include-file include-directories)))
    (cond ((eq t path) #|done|#)
          (path        (%load-c-file path *compiler-state*))
          (t           (error "Cannot find a file ~C~A~C in the include directories ~S"
                              (if (eq mode :quote) #\" #\<)
                              include-file
                              (if (eq mode :quote) #\" #\>)
                              include-directories)))))


(defmacro libc-dir ()
  (directory-namestring (or *load-truename* *compile-file-truename*)))

(defmacro load-libc-file (file libc-dir)
  `(eval-when (:compile-toplevel :load-toplevel)
     (load-c-file
      (merge-pathnames ,file ,libc-dir))))
