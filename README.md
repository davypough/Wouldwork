# Wouldwork
CLASSICAL PLANNING WITH THE WOULDWORK PLANNER

The Wouldwork Planner is yet one more in a long line of classical planners.  A brief listing of some other well-known classical planners would include Fast Forward, LPG, MIPS-XXL, SATPLAN, SGPLAN, Metric-FF Planner, Optop, SHOP3 and PDDL4j.  All of these planners are major developments by small research teams to investigate the performance of a wide variety of planning algorithms.  But each has its own limitations in its ability to specify and deal with certain kinds of problems.  In contrast, the Wouldwork Planner was developed by one individual, not to investigate different planning algorithms, but to extend the baseline capabilities for handling a wider variety of classical problems.  It focuses on the data structures and programming interface that allow a user to flexibly and conveniently specify a problem of modest size, and perform an efficient search for a solution.  The core planning algorithm itself performs a simple depth-first search through state-space, optimized for efficiently examining thousands of states.  The program attempts to combine many of the interface capabilities of the other planners into one package.  Some of the basic features of the user interface include:

-General conformance with the expressive capabilities of the PDDL language, plus extensions, for problem specification

-Arbitrary object type hierarchies

-Mixing of object types to allow efficient selection of objects during search

-Action rules with preconditions and effects, based on predicate logic

-Full nested predicate logic expressiveness in action rules with quantifiers and equality

-Specification of initial conditions

-Goal specification

-Fluents (ie, continuous variables, in addition to discrete variables)

-Durative actions taking time to complete

-Exogenous events (ie, happenings occurring independently of the planning agent’s actions)

-Temporal plan generation (ie, action schedules)

-Global constraint specification, independent of action preconditions

-Derived relations for simplifying action preconditions

-Function specification for on-the-fly, possibly recursive, computations

-Inclusion of arbitrary Lisp code in action rules, derived relations, constraints, and functions

-User control over search depth

-Generation of shortest plan found, plus other possible plans

-Optional parallel processing to speed up search

-Output diagnostics describing details of the search

For additional information, please see the Wouldwork user manual.
Dave Brown, davypough@gmail.com


# Quick Usage

## Install Wouldwork

### Custom Folder

Currently, choose a folder and git clone this repository there:

```
git clone git@github.com:gwangjinkim/Wouldwork.git
```

Open your SBCL:
```
;; define this function
(defun add-folder (folder)
  "Adds the given FOLDER to Quicklisp's local project directories."
  (let ((absolute-path (uiop:ensure-directory-pathname folder)))
    (unless (probe-file absolute-path)
      (error "The folder ~A does not exist." folder))
    ;; Add folder to *local-project-directories* if it's not already there
    (unless (member absolute-path ql:*local-project-directories* :test #'equal)
      (push absolute-path ql:*local-project-directories*)
      (format t "Added ~A to Quicklisp's local project directories.~%" absolute-path))))

;; and add the folder to your quicklisp *local-project-directories*
;; by calling:
(add-folder #P"/path/to/your/folder/containing/Wouldwork/")
```
Of course, you could do directly `(push your-folder-path ql:*local-project-directories*)`, but the checks ensure 
that there are no typos and that the format is correct (folders in Common lisp should
end - like in elisp - with a "/").

Now, `ql:*local-project-directories*` contains this folder too, so that you can run:

```
(ql:quickload :wouldwork)
```

### Easier: using `local-projects` folder of quicklisp

Or easier: Just go to `~/quicklisp/local-projects/` and `git clone git@github.com:gwangjinkim/Wouldwork.git`.
Open then SBCL and run `(ql:quickload :wouldwork)`.

Be aware: When you are using roswell, then your `local-projects` folder is usually in:
`~/.roswell/local-projects/` - or wherever you have it. 

That is the reason, why the other method is more robust - you then don't have to search for the `local-projects` folder first.

Another point is - be aware, if you want to use your roswell sbcl, then you have to start with
`ros run`.

If you just run `sbcl` then check with `which sbcl` where its location is.
and your `~/.sbclrc` would tell you which quicklisp it is using. And in this case, you have to use mostly
`~/quicklisp/local-projects/`. But of course this depends on where you installed your `quicklisp` for your global `sbcl`.



## Run Wouldwork

```
(ql:quickload :wouldwork)
(in-package :wouldwork)

(help)  ;; runs (ww:help)
```

And follow the instructions there!

these are:

```
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
      (setf *threads* 3)     ;; this doesn't work yet - we are working on it!

      ;; don't use parallelization
      (setf *threads* 0) ;; default value

      ;; how many cores are available?
      (ql:quickload :serapeum) ;; serapeum is the follow up of the alexandria package
      (serapeum:count-cpus) ;; => will tell you how many cores your computer has


;; --------------------- WOULDWORK (2024) Dave Brown <davypough@gmail.com> ----------------- ;;
```

Quickstart Setup:

1. Install (or update) SBCL and Quicklisp
2. Clone or download the Wouldwork repo (eg, to your local-projects directory under quicklisp)
3. Start SBCL from the command line
4. At the SBCL prompt, enter (ql:quickload "wouldwork")
5. Enter (in-package :ww) to switch from the cl-user package to the wouldwork package
6. Enter (run-test-problems) to verify everything is loaded & working properly
7. Enter (list-all) to display all pre-defined problem specifications in the src directory
8. Create your own problem specification by emulating any of the problem-<label>.lisp files in the src directory
9. Enter (run "<label>") to let Wouldwork search for a solution to your problem.
