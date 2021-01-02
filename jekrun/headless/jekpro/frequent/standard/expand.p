/**
 * Clauses and goals are automatically expanded for the Prolog session
 * queries, for the Prolog text clauses and for the Prolog text
 * directives. The effect can be seen by the following example. The
 * first goal_expansion/2 fact defines a new expansion for the predicate
 * writeln/2. The next rule for the predicate hello/0 already makes
 * use of the expansion in the body:
 *
 * Example:
 * ?- [user].
 * goal_expansion(writeln(X), (write(X), nl)).
 * hello :- writeln('Hello World!').
 * ^D
 * ?- listing(hello/0).
 * hello :-
 *     write('Hello World!'), nl.
 *
 * The clauses are expanded with the help of the system predicate
 * expand_term/2, which in turn expands the goals of the bodies via the
 * system predicate expand_goal/2. The two system predicates are
 * customizable by the end-user via additional rules for the multi-file
 * predicates term_expansion/2 and goal_expansion/2.
 *
 * The predicate property sys_noexpand/0 allows excluding a meta-predicate
 * from the goal or term traversal. Closures are currently not expanded.
 * If the term or goal expansion steps into the colon notation (:)/2 or
 * the double colon notation (::)/2 with a sufficiently instantiated first
 * argument it will look up the meta-declarations for the qualified
 * predicate name.
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- module(user, []).
:- use_module(library(experiment/simp)).
:- use_module(library(runtime/quali)).
:- use_module(library(advanced/abstract)).
:- use_module(library(basic/lists)).

/*******************************************************/
/* Term Expand                                         */
/*******************************************************/

/**
 * term_expansion(C, D):
 * This predicate can be used to define custom term
 * expansion rules.
 */
% term_expansion(+Clause, -Clause)
:- public term_expansion/2.
:- multifile term_expansion/2.
:- meta_predicate term_expansion(-1, -1).
:- set_predicate_property(term_expansion/2, sys_noexpand).
:- static term_expansion/2.

/**
 * expand_term(C, D):
 * The system predicate succeeds if the expansion of
 * the term C unifies with D.
 */
% expand_term(+Clause, -Clause)
:- public expand_term/2.
:- meta_predicate expand_term(-1, -1).
:- set_predicate_property(expand_term/2, sys_noexpand).
expand_term(B, C) :-
   sys_expand_term(B, C, _).

% sys_expand_term(+Clause, -Clause, -Integer)
:- private sys_expand_term/3.
:- meta_predicate sys_expand_term(-1, -1, ?).
:- set_predicate_property(sys_expand_term/3, sys_noexpand).
sys_expand_term(T, T, 0) :- var(T), !.
sys_expand_term(A, C, 1) :- term_expansion(A, B), !, sys_expand_term(B, C, _).
sys_expand_term(T, N, R) :-
   callable(T), !,
   functor(T, J, A),
   sys_make_indicator(J, A, I),
   sys_expand_term_callable(T, I, H),
   simplify_term(H, N, R).
sys_expand_term(T, T, 0).

% sys_expand_term_callable(+Callable, +Indicator, -Callable)
:- private sys_expand_term_callable/3.
sys_expand_term_callable(T, I, T) :-
   predicate_property(I, sys_noexpand), !.
sys_expand_term_callable(T, I, H) :-
   predicate_property(I, meta_predicate(M)), !,
   T =.. [K|L],
   sys_expand_term_args(M, L, S),
   H =.. [K|S].
sys_expand_term_callable(T, _, H) :-
   T =.. [K|L],
   sys_expand_term_args(_, L, S),
   H =.. [K|S].

% sys_expand_term_args(+Modes, +Args, -Args)
:- private sys_expand_term_args/3.
sys_expand_term_args([], [], []).
sys_expand_term_args([M|T], [A|L], [B|S]) :-
   sys_expand_term_arg(M, A, B),
   sys_expand_term_args(T, L, S).

% sys_expand_term_arg(+Mode, +Arg, -Arg)
:- private sys_expand_term_arg/3.
sys_expand_term_arg(?, X, X) :- !.
sys_expand_term_arg(0, X, Y) :- !, sys_expand_term(X, Y, _).
sys_expand_term_arg(-1, X, Y) :- !, sys_expand_goal(X, Y, _).
sys_expand_term_arg(N, X, Y) :- integer(N), N > 0, !,
   sys_expand_term_closure(X, N, Y).
sys_expand_term_arg(N, X, Y) :- integer(N), N < -1, !,
   M is -N-1,
   sys_expand_goal_closure(X, M, Y).
sys_expand_term_arg(_, X, X).

% sys_expand_term_closure(+Closure, +Integer, -Closure)
:- private sys_expand_term_closure/3.
sys_expand_term_closure(X, N, Y) :- callable(X), !,
   length(A, N),
   sys_extend_term(X, A, H),
   sys_expand_term(H, J, R),
   (  R == 0
   -> sys_shrink_term(J, N, Y, A)
   ;  sys_goal_globals(H^J, L),
      sys_make_locals(L, J, K),
      sys_make_lambda(A, K, Y)).
sys_expand_term_closure(D, _, D).

/*******************************************************/
/* Goal Expand                                         */
/*******************************************************/

/**
 * goal_expansion(C, D):
 * This predicate can be used to define custom goal
 * expansion rules.
 */
% goal_expansion(+Goal, -Goal)
:- public goal_expansion/2.
:- multifile goal_expansion/2.
:- meta_predicate goal_expansion(0, 0).
:- set_predicate_property(goal_expansion/2, sys_noexpand).
:- static goal_expansion/2.

/**
 * expand_goal(C, D):
 * The system predicate succeeds if the expansion of
 * the goal C unifies with D.
 */
% expand_goal(+Goal, -Goal)
:- public expand_goal/2.
:- meta_predicate expand_goal(0, 0).
:- set_predicate_property(expand_goal/2, sys_noexpand).
expand_goal(B, C) :-
   sys_expand_goal(B, C, _).

% sys_expand_goal(+Goal, -Goal, -Integer)
:- private sys_expand_goal/3.
:- meta_predicate sys_expand_goal(0, 0, ?).
:- set_predicate_property(sys_expand_goal/3, sys_noexpand).
sys_expand_goal(G, G, 0) :- var(G), !.
sys_expand_goal(A, C, 1) :- goal_expansion(A, B), !, sys_expand_goal(B, C, _).
sys_expand_goal(G, N, R) :-
   callable(G), !,
   functor(G, J, A),
   sys_make_indicator(J, A, I),
   sys_expand_goal_callable(G, I, H),
   simplify_goal(H, N, R).
sys_expand_goal(G, G, 0).

% sys_expand_goal_callable(+Callable, +Indicator, -Callable)
:- private sys_expand_goal_callable/3.
sys_expand_goal_callable(G, I, G) :-
   predicate_property(I, sys_noexpand), !.
sys_expand_goal_callable(G, I, H) :-
   predicate_property(I, meta_predicate(M)), !,
   G =.. [K|L],
   sys_expand_goal_args(M, L, S),
   H =.. [K|S].
sys_expand_goal_callable(G, _, H) :-
   G =.. [K|L],
   sys_expand_goal_args(_, L, S),
   H =.. [K|S].

% sys_expand_goal_args(+Modes, +Args, -Args)
:- private sys_expand_goal_args/3.
sys_expand_goal_args([], [], []).
sys_expand_goal_args([M|T], [A|L], [B|S]) :-
   sys_expand_goal_arg(M, A, B),
   sys_expand_goal_args(T, L, S).

% sys_expand_goal_arg(+Mode, +Arg, -Arg)
:- private sys_expand_goal_arg/3.
sys_expand_goal_arg(?, X, X) :- !.
sys_expand_goal_arg(0, X, Y) :- !, sys_expand_goal(X, Y, _).
sys_expand_goal_arg(-1, X, Y) :- !, sys_expand_term(X, Y, _).
sys_expand_goal_arg(N, X, Y) :- integer(N), N > 0, !,
   sys_expand_goal_closure(X, N, Y).
sys_expand_goal_arg(N, X, Y) :- integer(N), N < -1, !,
   M is -N-1,
   sys_expand_term_closure(X, M, Y).
sys_expand_goal_arg(_, X, X).

% sys_expand_goal_closure(+Closure, +Integer, -Closure)
:- private sys_expand_goal_closure/3.
sys_expand_goal_closure(X, N, Y) :- callable(X), !,
   length(A, N),
   sys_extend_term(X, A, H),
   sys_expand_goal(H, J, R),
   (  R == 0
   -> sys_shrink_term(J, N, Y, A)
   ;  sys_goal_globals(H^J, L),
      sys_make_locals(L, J, K),
      sys_make_lambda(A, K, Y)).
sys_expand_goal_closure(C, _, C).

/******************************************************************/
/* Helper                                                         */
/******************************************************************/

/**
 * sys_make_locals(L, G, Q):
 * The predicate succeeds in Q with the goal G quatified
 * by the variables L.
 */
% sys_make_locals(+List, +Goal, -QuantGoal)
:- private sys_make_locals/3.
sys_make_locals([X|Y], Z, X^T) :-
   sys_make_locals(Y, Z, T).
sys_make_locals([], X, X).

/**
 * sys_make_lambda(L, G, Q):
 * The predicate succeeds in Q with the goal G lambdaified
 * by the variables L.
 */
% sys_make_lambda(+List, +Goal, -QuantGoal)
:- private sys_make_lambda/3.
sys_make_lambda([X|Y], Z, X\T) :-
   sys_make_lambda(Y, Z, T).
sys_make_lambda([], X, X).

