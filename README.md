# Wouldwork
CLASSICAL PLANNING WITH THE WOULDWORK PLANNER

The Wouldwork Planner is yet one more in a long line of classical planners.  A brief listing of some other well-known classical planners would include Fast Forward, LPG, MIPS-XXL, SATPLAN, SGPLAN, Metric-FF Planner, Optop, and PDDL4j.  All of these planners are major developments by small research teams to investigate the performance of a wide variety of planning algorithms.  But each has its own limitations in its ability to specify and deal with certain kinds of problems.  In contrast, the Wouldwork Planner was developed by one individual, not to investigate different planning algorithms, but to extend the baseline capabilities for handling a wider variety of classical problems.  It focuses on the data structures and programming interface that allow a user to flexibly and conveniently specify a problem of modest size, and perform an efficient search for a solution.  The core planning algorithm itself performs a simple depth-first search through state-space, optimized for efficiently examining thousands of states.  The program attempts to combine many of the interface capabilities of the other planners into one package.  Some of the basic features of the user interface include:

-General conformance with the expressive capabilities of the PDDL language for problem specification

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

-Output diagnostics describing details of the search

For additional information, please see the Wouldwork user manual.
Dave Brown, davypough@gmail.com
