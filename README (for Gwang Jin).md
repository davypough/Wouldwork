# Wouldwork
CLASSICAL PLANNING WITH THE WOULDWORK PLANNER

The Wouldwork Planner is yet one more in a long line of classical planners.  A brief listing of some other well-known classical planners would include Fast Forward, LPG, MIPS-XXL, SATPLAN, SGPLAN, Metric-FF Planner, Optop, SHOP3 and PDDL4j.  All of these planners are major developments by small research teams to investigate the performance of a wide variety of planning algorithms.  But each has its own limitations in its ability to specify and deal with certain kinds of problems.  In contrast, the Wouldwork Planner was developed by one individual, not to investigate different planning algorithms, but to extend the baseline capabilities for handling a wider variety of classical problems.  It focuses on the data structures and programming interface that allow a user to flexibly and conveniently specify a problem of modest size, and perform an efficient search for a solution.  The core planning algorithm itself performs a simple depth-first search through state-space, optimized for efficiently examining thousands or millions of states.  The program attempts to combine many of the interface capabilities of the other planners into one package.  Some of the basic features of the user interface include:

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

For additional information, please see the Wouldwork User Manual.
Dave Brown, davypough@gmail.com

# Quickstart Setup:

1. Install (or update) SBCL and Quicklisp
2. Clone or download the Wouldwork repo (eg, to your local-projects directory under quicklisp)
3. Start SBCL from the command line
4. At the SBCL prompt, enter `(ql:quickload "wouldwork")`
5. Enter `(in-package :ww)` to switch from the cl-user package to the wouldwork package
6. Enter `(run-test-problems)` to verify everything is loaded & working properly
7. Enter `(list-all)` to display all pre-defined problem specifications in the src directory
8. Create your own problem specification by emulating any of the `problem-<label>.lisp` files in the src directory
9. Enter `(run "<label>")` to tell Wouldwork to search for a solution to your problem.



# Additional Information

1. INSTALLING WOULDWORK TO A CUSTOM DIRECTORY

Choose a local folder and git clone or download the Wouldwork repository there:
(eg, `git clone git@github.com:gwangjinkim/Wouldwork.git`)

Open SBCL and define this function:
```
    (defun add-folder (folder)
      "Adds the given FOLDER to Quicklisp's local project directories."
      (let ((absolute-path (uiop:ensure-directory-pathname folder)))
        (unless (probe-file absolute-path)
          (error "The folder ~A does not exist." folder))
        ;; Add folder to *local-project-directories* if it's not already there
        (unless (member absolute-path ql:*local-project-directories* :test #'equal)
          (push absolute-path ql:*local-project-directories*)
          (format t "Added ~A to Quicklisp's local project directories.~%" absolute-path))))
```
And add the folder to your quicklisp `*local-project-directories*` by calling:
`(add-folder #P"/path/to/your/directory/containing/Wouldwork/")`

You could do directly `(push your-directory-path ql:*local-project-directories*)`, but the checks ensure 
that there are no typos and that the format is correct (Note that directories in Common Lisp should
end - like in elisp - with a "/").

Now, `ql:*local-project-directories*` contains this directory, so that Quicklisp knows how to find Wouldwork.


2. INSTALLING WOULDWORK TO THE LOCAL-PROJECTS DIRECTORY

Or easier: Just go to `~/quicklisp/local-projects/` and `git clone git@github.com:gwangjinkim/Wouldwork.git` there.

Be aware: When you are using roswell, then your `local-projects` folder is usually in 
`~/.roswell/local-projects/`. Also, if you want to use your roswell SBCL, then you have to start with `ros run`.


3. ACCESS MORE HELP

After loading Wouldwork with `(ql:quickload "wouldwork")` and `(in-package :ww)`, you can get more technical help by entering `(help)` at the SBCL prompt.
