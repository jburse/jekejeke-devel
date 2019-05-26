/**
 * The body conversion only caters for wrapping variables into call/1.
 * It is possible to implement further heuristics be explicitly calling
 * expand_term/2 respectively expand_goal/2 during asserts or calls.
 * By this module it is arranged that simplify_term/2 respectively
 * simplify_goal/2 are called after the expansion. The predicates are
 * customizable by the end-user via term_simplification/2 respectively
 * goal_simplification/2.
 *
 * Example:
 * ?- [user].
 * goal_expansion((X is E), true) :- ground(E), X is E.
 * test(Y) :- X is 1+2, Y is 4*X.
 * ^D
 * ?- listing(test/1).
 * test(12).
 *
 * There are situations where the compile-time heuristics have to be
 * undone to make them trans-parent. For example when listing clauses
 * or debugging goals. The predicates rebuild_term/2 respectively
 * rebuild_goal/2 are responsible for undoing expansions and
 * simplifications. The rebuilding uses the same flags as the expansion
 * and as well customizable via term_rebuilding/2 respectively
 * goal_rebuilding/2.
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

:- package(library(jekpro/frequent/experiment)).

:- module(simp, []).

/*******************************************************/
/* Term Simplify                                       */
/*******************************************************/

/**
 * term_simplification(C, D):
 * This predicate can be used to define custom term
 * simplification rules.
 */
% term_simplification(+Clause, -Clause)
:- public term_simplification/2.
:- multifile term_simplification/2.
:- meta_predicate term_simplification(-1,-1).
:- set_predicate_property(term_simplification/2, sys_noexpand).

/**
 * simplify_term(C, D):
 * The system predicate succeeds if the simplification of
 * the term C unifies with D.
 */
% simplify_term(+Clause, -Clause)
:- public simplify_term/2.
:- meta_predicate simplify_term(-1,-1).
:- set_predicate_property(simplify_term/2, sys_noexpand).
simplify_term(A, B) :-
   term_simplification(A, B), !.
simplify_term(T, T).

/* Predefined term simplifications */
/* (/\)/2 flattening */
simp:term_simplification(_ /\ A, _) :-
   var(A), !, fail.
simp:term_simplification(C /\ unit, C).
simp:term_simplification(A /\
                         (  B /\ C), J) :-
   simplify_term(A /\ B, H),
   simplify_term(H /\ C, J).
simp:term_simplification(A /\ _, _) :-
   var(A), !, fail.
simp:term_simplification(unit /\ C, C).

/* (:-)/2 flattening */
term_simplification((A :- _), _) :-
   var(A), !, fail.
term_simplification(((A :- B) :- C), (A :- H)) :-
   simplify_goal((  C, B), H).
term_simplification((_ :- A), _) :-
   var(A), !, fail.
term_simplification((C :- true), C).

/*******************************************************/
/* Goal Simplify                                       */
/*******************************************************/

/**
 * goal_simplification(C, D):
 * This predicate can be used to define custom goal
 * simplification rules.
 */
% goal_simplification(+Goal, -Goal)
:- public goal_simplification/2.
:- multifile goal_simplification/2.
:- meta_predicate goal_simplification(0,0).
:- set_predicate_property(goal_simplification/2, sys_noexpand).

/**
 * simplify_goal(C, D):
 * The system predicate succeeds if the simplification of
 * the goal C unifies with D.
 */
% simplify_goal(+Goal, -Goal)
:- public simplify_goal/2.
:- meta_predicate simplify_goal(0,0).
:- set_predicate_property(simplify_goal/2, sys_noexpand).
simplify_goal(A, B) :-
   goal_simplification(A, B), !.
simplify_goal(G, G).

/* Predefined goal implifications */
/* (,)/2 flattening */
goal_simplification((  A, _), _) :-
   var(A), !, fail.
goal_simplification((  true, C), C).
goal_simplification((  _, A), _) :-
   var(A), !, fail.
goal_simplification((  C, true), C).
goal_simplification((  U, C), J) :-
   U = (A,B),
   sys_replace_site(P, U, (B,C)),
   simplify_goal(P, H),
   sys_replace_site(Q, U, (A,H)),
   simplify_goal(Q, J).

/* (;)/2 flattening */
goal_simplification((  A; _), _) :-
   var(A), !, fail.
goal_simplification((  U; C), J) :-
   U = (A;B),
   sys_replace_site(P, U, (B;C)),
   simplify_goal(P, H),
   sys_replace_site(Q, U, (A;H)),
   simplify_goal(Q, J).

/*******************************************************/
/* Rest Simplify                                       */
/*******************************************************/

/**
 * rest_simplification(C, D):
 * This predicate can be used to define custom rest
 * simplification rules.
 */
% rest_simplification(+Goal, -Goal)
:- public rest_simplification/2.
:- multifile rest_simplification/2.
:- set_predicate_property(rest_simplification/2, sys_noexpand).
:- static rest_simplification/2.

/**
 * simplify_rest(C, D):
 * The system predicate succeeds if the simplification of
 * the rest C unifies with D.
 */
% simplify_rest(+Goal, -Goal)
:- public simplify_rest/2.
:- set_predicate_property(simplify_rest/2, sys_noexpand).
simplify_rest(A, B) :-
   rest_simplification(A, B), !.
simplify_rest(G, G).

/*******************************************************/
/* Term Rebuild                                        */
/*******************************************************/

/**
 * term_rebuilding(C, D):
 * This predicate can be used to define custom term
 * rebuilding rules.
 */
% term_rebuilding(+Clause, -Clause)
:- public term_rebuilding/2.
:- multifile term_rebuilding/2.
:- meta_predicate term_rebuilding(-1,-1).
:- set_predicate_property(term_rebuilding/2, sys_noexpand).
:- static term_rebuilding/2.

/**
 * rebuild_term(C, D):
 * The system predicate succeeds if the rebuild of
 * the term C unifies with D
 */
% rebuild_term(+Clause, -Clause)
:- public rebuild_term/2.
:- meta_predicate rebuild_term(-1,-1).
:- set_predicate_property(rebuild_term/2, sys_noexpand).
rebuild_term(P, P) :-
   sys_var(P), !.
rebuild_term(A, C) :-
   term_rebuilding(A, B), !,
   rebuild_term(B, C).
rebuild_term(G, H) :-
   sys_callable(G),
   sys_functor(G, J, A),
   sys_make_indicator(J, A, I),
   \+ predicate_property(I, sys_noexpand), !,
   rebuild_term_callable(G, I, H).
rebuild_term(G, G).

% rebuild_term_callable(+Callable, +Indicator, -Callable)
:- private rebuild_term_callable/3.
rebuild_term_callable(G, I, H) :-
   predicate_property(I, meta_predicate(P)), !,
   P =.. [_|R],
   sys_univ(G, [K|L]),
   rebuild_term_args(R, L, S),
   sys_univ(H, [K|S]).
rebuild_term_callable(G, _, H) :-
   sys_univ(G, [K|L]),
   rebuild_rest_args(L, S),
   sys_univ(H, [K|S]).

% rebuild_term_args(+Modes, +Args, -Args)
:- private rebuild_term_args/3.
rebuild_term_args([], [], []).
rebuild_term_args([M|R], [A|L], [B|S]) :-
   rebuild_term_arg(M, A, B),
   rebuild_term_args(R, L, S).

% rebuild_term_arg(+Mode, +Arg, -Arg)
:- private rebuild_term_arg/3.
rebuild_term_arg(0, X, Y) :- !,
   rebuild_term(X, Y).
rebuild_term_arg(-1, X, Y) :- !,
   rebuild_goal(X, Y).
rebuild_term_arg(_, X, Y) :-
   rebuild_rest(X, Y).

/*******************************************************/
/* Goal Rebuild                                        */
/*******************************************************/

/**
 * goal_rebuilding(C, D):
 * This predicate can be used to define custom goal
 * rebuilding rules.
 */
% goal_rebuilding(+Goal, -Goal)
:- public goal_rebuilding/2.
:- multifile goal_rebuilding/2.
:- meta_predicate goal_rebuilding(0,0).
:- set_predicate_property(goal_rebuilding/2, sys_noexpand).
:- static goal_rebuilding/2.

/**
 * rebuild_goal(C, D):
 * The system predicate succeeds if the rebuild of
 * the goal C unifies with D.
 */
% rebuild_goal(+Goal, -Goal)
:- public rebuild_goal/2.
:- meta_predicate rebuild_goal(0,0).
:- set_predicate_property(rebuild_goal/2, sys_noexpand).
rebuild_goal(P, P) :-
   sys_var(P), !.
rebuild_goal(A, C) :-
   goal_rebuilding(A, B), !,
   rebuild_goal(B, C).
rebuild_goal(G, H) :-
   sys_callable(G),
   sys_functor(G, J, A),
   sys_make_indicator(J, A, I),
   \+ predicate_property(I, sys_noexpand), !,
   rebuild_goal_callable(G, I, H).
rebuild_goal(G, G).

% rebuild_goal_callable(+Callable, +Indicator, -Callable)
:- private rebuild_goal_callable/3.
rebuild_goal_callable(G, I, H) :-
   predicate_property(I, meta_predicate(P)), !,
   P =.. [_|R],
   sys_univ(G, [K|L]),
   rebuild_goal_args(R, L, S),
   sys_univ(H, [K|S]).
rebuild_goal_callable(G, _, H) :-
   sys_univ(G, [K|L]),
   rebuild_rest_args(L, S),
   sys_univ(H, [K|S]).

% rebuild_goal_args(+Modes, +Args, -Args)
:- private rebuild_goal_args/3.
rebuild_goal_args([], [], []).
rebuild_goal_args([M|R], [A|L], [B|S]) :-
   rebuild_goal_arg(M, A, B),
   rebuild_goal_args(R, L, S).

% rebuild_goal_arg(+Mode, +Arg, -Arg)
:- public rebuild_goal_arg/3.
rebuild_goal_arg(0, X, Y) :- !,
   rebuild_goal(X, Y).
rebuild_goal_arg(-1, X, Y) :- !,
   rebuild_term(X, Y).
rebuild_goal_arg(_, X, Y) :-
   rebuild_rest(X, Y).

/*******************************************************/
/* Rest Rebuild                                        */
/*******************************************************/

/**
 * rest_rebuilding(C, D):
 * This predicate can be used to define custom rest
 * rebuilding rules.
 */
% rest_rebuilding(+Goal, -Goal)
:- public rest_rebuilding/2.
:- multifile rest_rebuilding/2.
:- set_predicate_property(rest_rebuilding/2, sys_noexpand).
:- static rest_rebuilding/2.

/**
 * rebuild_rest(C, D):
 * The system predicate succeeds if the rebuild of
 * the rest C unifies with D.
 */
% rebuild_rest(+Goal, -Goal)
:- public rebuild_rest/2.
:- set_predicate_property(rebuild_rest/2, sys_noexpand).
rebuild_rest(P, P) :-
   var(P), !.
rebuild_rest(A, C) :-
   rest_rebuilding(A, B), !,
   rebuild_rest(B, C).
rebuild_rest(G, H) :-
   callable(G),
   functor(G, J, A),
   J/A = I,
   \+ predicate_property(I, sys_nomacro), !,
   rebuild_rest_callable(G, I, H).
rebuild_rest(G, G).

% rebuild_rest_callable(+Callable, +Indicator, -Callable)
:- private rebuild_rest_callable/3.
rebuild_rest_callable(G, I, H) :-
   predicate_property(I, meta_function(P)), !,
   P =.. [_|R],
   G =.. [K|L],
   rebuild_goal_args(R, L, S),
   H =.. [K|S].
rebuild_rest_callable(G, _, H) :-
   G =.. [K|L],
   rebuild_rest_args(L, S),
   H =.. [K|S].

% rebuild_rest_args(+Args, -Args)
:- private rebuild_rest_args/2.
rebuild_rest_args([], []).
rebuild_rest_args([A|L], [B|S]) :-
   rebuild_rest(A, B),
   rebuild_rest_args(L, S).
