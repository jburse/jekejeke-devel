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
   predicate_property(I, meta_predicate(P)), !,
   P =.. [_|M],
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
sys_expand_term_arg(1, X, Y) :- !, sys_expand_term_closure(X, Y, _).
sys_expand_term_arg(0, X, Y) :- !, sys_expand_term(X, Y, _).
sys_expand_term_arg(-1, X, Y) :- !, sys_expand_goal(X, Y, _).
sys_expand_term_arg(-2, X, Y) :- !, sys_expand_goal_closure(X, Y, _).
sys_expand_term_arg(_, X, X).

% sys_expand_term_closure(+Closure, -Closure, -Integer)
sys_expand_term_closure(X, Y, R) :- callable(X), !,
   sys_callable_apply(X, V, H),
   sys_expand_term(H, J, R),
   (  R == 0
   -> sys_callable_apply(Y, V, J)
   ;  sys_goal_globals(H^J, L),
      sys_kernel_goal(L, J, K),
      Y = V\K).
sys_expand_term_closure(T, T, 0).

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
   predicate_property(I, meta_predicate(P)), !,
   P =.. [_|M],
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
sys_expand_goal_arg(1, X, Y) :- !, sys_expand_goal_closure(X, Y, _).
sys_expand_goal_arg(0, X, Y) :- !, sys_expand_goal(X, Y, _).
sys_expand_goal_arg(-1, X, Y) :- !, sys_expand_term(X, Y, _).
sys_expand_goal_arg(-2, X, Y) :- !, sys_expand_term_closure(X, Y, _).
sys_expand_goal_arg(_, X, X).

% sys_expand_goal_closure(+Closure, -Closure, -Integer)
sys_expand_goal_closure(X, Y, R) :- callable(X), !,
   sys_callable_apply(X, V, H),
   sys_expand_goal(H, J, R),
   (  R == 0
   -> sys_callable_apply(Y, V, J)
   ;  sys_goal_globals(H^J, L),
      sys_kernel_goal(L, J, K),
      Y = V\K).
sys_expand_goal_closure(G, G, 0).

/******************************************************************/
/* Helper                                                         */
/******************************************************************/

/**
 * sys_kernel_goal(L, G, Q):
 * The predicate succeeds in Q with the goal G quatified
 * by the variables L.
 */
% sys_kernel_goal(+List, +Goal, -QuantGoal)
:- private sys_kernel_goal/3.
sys_kernel_goal([X|Y], Z, X^T) :-
   sys_kernel_goal(Y, Z, T).
sys_kernel_goal([], X, X).

/**
 * sys_callable_apply(G, V, H):
 * The predicate succeeds in H with the callable G extended
 * by the argument V.
 */
:- private sys_callable_apply/3.
sys_callable_apply(G, V, H) :- var(G), !,
   H =.. [F|R],
   last(R, V, S),
   G =.. [F|S].
sys_callable_apply(G, V, H) :-
   G =.. [F|S],
   last(R, V, S),
   H =.. [F|R].

