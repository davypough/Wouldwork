(in-package :wouldwork)

(defun help ()  ;;; text which appears at the start of the program
  (format t "
;; --------------------- WOULDWORK (2024) Dave Brown <davypough@gmail.com> ----------------- ;;

INSTALLATION AND START:

      ;; when you git-cloned the respository, then you can manually load the asd file
      ;; to asdf
      (asdf:load-asd \"/path/to/wouldwork/wouldwork.asd\")
      ;; after that, you can (ql:quickload :wouldwork) without any problems.
      ;; you can place this to your personal .sbclrc file so that it asdf and quicklisp always
      ;; can recognize your package and its position in the system.

      (ql:quickload :wouldwork)  ;; install/load/import wouldwork package
      (in-package :wouldwork)    ;; enter the namespace
                                 ;; otherwise you need to prepone 'ww:' to your commands

ADD YOUR PROBLEM FOLDER:

      ;; the problem folder is in the package source folder 'src' in your quicklisp's local-folder
      ;; to see the exact location of your package, you can run:
      (ww::get-package-root :wouldwork)
      (get-src-folder-path)   ;; returns the exact location where you should place your
                                 ;; problem-<name>.lisp files - replace <name> by your problem name.

      ;; but if you want to add your problem files to a custom folder,
      ;; add them in this way:
      (add-problem-folder #P\"/path/to/your/folder/\")

      ;; this returns a list of all folder paths in which wouldwork will search for problem folders
      ;; this list of folders is saved in the global variable
      *problem-folder-paths*

      ;; to remove your custom folder, run:
      (remove-problem-folder #P\"/path/to/your/folder/\")

SOLVE PROBLEMS:

SOLVE A SINGLE PROBLEM:

      (list-all)

      ;; pick one of the listed problems, e.g. \"array-path\" and run (load and solve) the problem:

      (run \"array-path\")

SOLVE ALL AVAILABLE PROBLEMS:

      (run-test-problems)
      
      ;; or shorter:

      (run-all)

USE MULTIPLE CORES:
      
      ;; use 3 cores
      (setf *threads* 3)

      ;; don't use parallelization
      (setf *threads* 0) ;; default value

      ;; how many cores are available?
      (ql:quickload :serapeum) ;; serapeum is the follow up of the alexandria package
      (serapeum:count-cpus) ;; => will tell you how many cores your computer has


;; --------------------- WOULDWORK (2024) Dave Brown <davypough@gmail.com> ----------------- ;;
"))


(defun %main (argv)
  "Parse CLI args."
  (when (member "-h" argv :test #'equal)
    ;; To properly parse command line arguments, use a third-party library such as
    ;; clingon, unix-opts, defmain, adopt… when needed.
    (help)
    (uiop:quit))
  (format t "%main was invoked"))

(defun main ()
  "Entry point for the executable.
  Reads command line arguments."
  ;; uiop:command-line-arguments returns a list of arguments (sans the script name).
  ;; We defer the work of parsing to %main because we call it also from the Roswell script.
  (%main (uiop:command-line-arguments)))

;; -------------------- some basic string functions ------------------ ;;

(defun string-prefix-p (prefix str)
  "Return T if PREFIX is a prefix of STR, otherwise NIL."
  (and (<= (length prefix) (length str))
       (string= prefix (subseq str 0 (length prefix)))))

(defun string-suffix-p (suffix str)
  "Return T if SUFFIX is a suffix of STR, otherwise NIL."
  (and (<= (length suffix) (length str))
       (string= suffix (subseq str (- (length str) (length suffix))))))

(defun lstrip (str prefix)
  "Removes prefix from str (only 1x)."
  (let ((result str))
    (when (string-prefix-p prefix result)
      (setf result (subseq result (length prefix))))
    result))

(defun rstrip (str suffix)
  "Removes suffix from str (only 1x)."
  (let ((result str))
    (when (string-suffix-p suffix result)
      (setf result (subseq result 0 (- (length result) (length suffix)))))
    result))

(defun strip-name (str prefix suffix)
  "Removes prefix and suffix from str."
  (let ((res (lstrip str prefix)))
    (rstrip res suffix)))

;; -------------------- plist lookup customizable -------------------- ;;

(defun lookup (key plist &key (test #'string=) (default))
  "Key value lookup in plist with #'string= or any other function as test.
   The plist-related getf can only handle eql."
  (let ((res nil)
	(foundp nil))
    (loop for (k v) on plist by #'cddr
	  when (funcall test k key)
	    do (setf res v
		     foundp t)
	  finally (return (values (if res res default) foundp)))))

;; --------------------- file handling ------------------------------- ;;

(defun copy-file-content (source-file target-file)
  "Replace the content of target-file by the content of source-file."
  (with-open-file (in source-file :direction :input)
    (with-open-file (out target-file :direction :output :if-exists :supersede)
      (loop for line = (read-line in nil nil)
	    while line
	    do (write-line line out)))))


;; -------------------- pathname handling ---------------------------- ;;

(defun get-package-root (system-name)
  "Return the root directory of the ASDF system associated with the given package name."
  (let ((system (asdf:find-system system-name)))
    (when system
      (asdf:system-source-directory system))))

(Defun get-src-folder-path ()
  (add-dir (get-package-root :wouldwork) "src"))

(defun add-dir (root dir)
  "Add to absolute path an additional directory"
  (merge-pathnames (make-pathname :directory `(:relative ,dir)) root))

(defun add-file (root file)
  "Add to absolute path a filename"
  (merge-pathnames (pathname file) root))

(defun directory-exists-p (directory)
  "Returns pathname if the directory exists and is a directory.
   Currently only works with SBCL - but not CLISP!"
  (let ((path (pathname directory)))
    (and (probe-file path)
         (string-suffix-p "/" (format nil "~a" (probe-file path))))))

(defun in-src (filename)
  "Shortcut to add filename to current package directory's src folder"
  (add-file (get-src-folder-path) filename))

(defun correct-wildcard (path)
  "Eliminate unwanted wildcard escape in path strings."
  (let ((chars (coerce (format nil "~a" path) 'list)))
    (pathname (coerce (remove-if (lambda (x) (eql #\\ x)) chars) 'string))))

;; -------------------- problem.lisp file handling ------------------------ ;;

(defparameter *problem-folder-paths* (list (get-src-folder-path))
"This variable holds all folder pathnames which can hold problems in this system.
   The user cann add custom folder pathnames to this folder using the function
   `add-problem-folder` and remove by `remove-problem-folder`.
   The Package directory's `src` folder, however will always persist.")

(defun add-problem-folder (folder-path)
  "Adds an additional path to a folder containing problem-*.lisp files to the
   global list `*problem-folder-paths*`."
  (let ((path (pathname folder-path)))
    (if (directory-exists-p path) ;; SBCL specific!
        (push (probe-file path) *problem-folder-paths*)
        (format t "\"~a\" is either not a path to a folder or there are other problems."
                path))))

(defun remove-problem-folder (folder-path)
  "Removes folder-path from global `*problem-folder-paths*` list.
   It always leaves the packages' `src` folder present!"
  (let ((path (probe-file (pathname folder-path))))
    (cond ((<= (length *problem-folder-paths*) 1)
           (Format t "Not removing anything, because *problem-folder-paths* contains only the src folder")
           *problem-folder-paths*)
          (t
           (setf *problem-folder-paths* (remove-if (lambda (p) (string= (format nil "~a" p)
                                                                        (format nil "~a" path)))
                                                   *problem-folder-paths*))
           *problem-folder-paths*))))

;; so using <add-problem-folder> and <remove-problem-folder> each with path,
;; user kann add or remove custom folder from the global variable.


(defun list-problem-files-plist (&optional (prefix "problem-*") (suffix "lisp"))
  "Return a plist of files in the 'src' directory that start with 'problem-'.
   The key is the filename without 'problem-' and '.lisp'.
   The value is the full path of the file. Uses the root directory of the 'wouldwork' system."
  (let ((files)
        (result))
    (loop for dir in *problem-folder-paths*
          do (setf files (append
			  (directory (format nil "~a/~a"
					     dir
					     (format nil "~a.~a" prefix suffix)))
			  files)))
    (dolist (file files)
      (let* ((filename (file-namestring file))
	     (name (strip-name filename "problem-" ".lisp")))
	(when name
	  (setq result (append result (list name file))))))
    result))

(defun list-problem-names ()
  (let* ((plist (list-problem-files-plist)))
    (loop for (k v) on plist by #'cddr
	  collect k)))

(defun exchange-problem-file (problem-name &optional (problem-file "problem.lisp"))
  "Copies problem path to 'src/problem.lisp'"
  (let* ((plist (list-problem-files-plist))
	 (path (lookup problem-name plist)))
    (copy-file-content path (in-src problem-file))))

(Defun reload-with-new-problem (problem-name &optional (problem-file "problem.lisp") (system-name :wouldwork))
  "This function is crucial for loading problems.
   Given a problem-name, it replaces the content of the problem.lisp file by
   the content of the correponsing problem file.
   And then reloads the entire package anew (which leads to re-compilation)."
  (exchange-problem-file problem-name problem-file)
  ;; (asdf:operate 'asdf:load-op :wouldwork :force-not '(:iterate :alexandria :lparallel)))
  (let ((cores *threads*))
    (asdf:load-system system-name :force t)
    (setf *threads* cores)))


(declaim (ftype (function () t) solve))  ;function solve located in searcher.lisp

(defparameter *problem-files*
  '("problem-blocks3.lisp" "problem-blocks4.lisp" "problem-boxes.lisp"
    "problem-jugs2.lisp" "problem-jugs4.lisp" "problem-queens4.lisp"
    "problem-queens8.lisp" "problem-captjohn-csp.lisp" "problem-quern.lisp" 
    "problem-graveyard.lisp" "problem-sentry.lisp" "problem-crossword5-11.lisp"
    "problem-array-path.lisp" "problem-tiles1a-heuristic.lisp" "problem-tiles7a-heuristic.lisp"
    "problem-triangle-xy.lisp" "problem-triangle-xyz.lisp" "problem-triangle-heuristic.lisp"
    "problem-triangle-macros.lisp" "problem-triangle-macros-one.lisp"
    "problem-tsp.lisp" "problem-u2.lisp" "problem-donald.lisp"
    "problem-knap4a.lisp" "problem-knap4b.lisp" "problem-knap19.lisp"
    "problem-smallspace.lisp" "problem-crater.lisp")
  "List of all problem filenames which are correct.")

(defparameter *problem-names* (mapcar (lambda (pn) (strip-name pn "problem-" ".lisp"))
				      *problem-files*)
  "List of all problem names of problem files which are correct.")


(defmacro with-silenced-compilation (&body body)
  "Macro to allow certain settings -
   - silenced *compile-verbose*
   - silenced *compile-print*
   - and certain *debug-print-variable-alist* settings"
  `(let ((*compile-verbose* nil)
	 (*compile-print* nil)
	 (sb-ext:*debug-print-variable-alist* '((*print-length* . 30)
						(*print-level* . 6)
						(*print-pretty* . t))))
     ,@body))

(defun run-test-problems (&optional (problem-file "problem.lisp") (cpu-cores *threads*))
  (with-silenced-compilation
      (let ((problem-names (list-problem-names)))
	(loop for problem in problem-names
	      when (member problem *problem-names* :test #'string=)
		do (progn
		     (format t "=====================================================~%~%")
		     (format t "starting to analyze \"~a\"~%~%" problem)
		     (format t "=====================================================~%~%")
		     (reload-with-new-problem problem problem-file)
                     (let ((*threads* cpu-cores))
		       (solve))
		     (format t "=====================================================~%~%")
		     (format t "problem \"~a\" successfully solved.~%~%" problem)
		     (format t "=====================================================~%~%"))
	      finally (progn
			(exchange-problem-file "array-path" "problem.lisp")
			(cond ((= (1+ (position problem problem-names :test #'string=)) (length problem-names))
			       (format t "All problems successfully solved.~%~%")
			       (return t))
			      (t
			       (format t "Error at problem ~a~%~%" problem)
			       (return nil))))))))
;; alias:
(setf (fdefinition 'run-all) #'run-test-problems)



(defun run (problem-name &optional (cpu-cores *threads*))
  "Loads, reloads and solves a single problem."
  (with-silenced-compilation
      (cond ((member problem-name (list-all) :test #'string=)
             (reload-with-new-problem problem-name)
             (let ((*threads* cpu-cores))
               (solve)))
            (t
             (format t "The problem \"~a\" was not found. Please check spelling (and the path)." problem-name)))))

(defun list-all (&optional (prettyp nil))
  "List all problem names in the problem folder.
   One-per-line: (list-all t) or (list-all :pretty)"
  (if prettyp
      (loop for name in (list-problem-names)
            do (format t "~a~%" name))
      (list-problem-names)))


