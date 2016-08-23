/**
 * The body conversion only caters for wrapping variables into call/1.
 * It is possible to implement further heuristics be explicitly calling
 * expand_term/2 respectively expand_goal/2 during asserts or calls. By
 * this module it is arranged that sys_simplify_term/2 respectively
 * sys_simplify_goal/2 are called after the expansion. The predicates
 * are customizable by the end-user via sys_term_simplification/2
 * respectively sys_goal_simplification/2.
 *
 * Example:
 * ?- use_module(library(expand)).
 *
 * ?- [user].
 * :- multifile simp:sys_goal_simplification/2.
 * simp:sys_goal_simplification((X is E), (X=V)) :- ground(E), V is E.
 *
 * test(X) :- X is 1+2*3.
 * ^D
 *
 * ?- listing(test/1).
 * test(X) :-
 *    X = 7.
 *
 * There are situations where the compile-time heuristics have to be undone
 * to make them transparent. For example when listing clauses or debugging
 * goals. The predicates sys_rebuild_term/2 respectively sys_rebuild_goal/2
 * are responsible for undoing expansions and simplifications. The rebuilding
 * uses the same flags as the expansion and as well customizable via
 * sys_term_rebuilding/2 respectively sys_goal_rebuilding/2.
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- package(library(jekpro/frequent/experiment)).

:- module(simp, []).

/*******************************************************/
/* Term/Goal Simplify                                  */
/*******************************************************/

/**
 * sys_term_simplification(C, D):
 * This predicate can be used to define custom term
 * simplification rules.
 */
% sys_term_simplification(+Clause, -Clause)
:- public sys_term_simplification/2.
:- multifile sys_term_simplification/2.
:- meta_predicate sys_term_simplification(-1,-1).
:- set_predicate_property(sys_term_simplification/2, sys_noexpand).

/**
 * sys_goal_simplification(C, D):
 * This predicate can be used to define custom goal
 * simplification rules.
 */
% sys_goal_simplification(+Goal, -Goal)
:- public sys_goal_simplification/2.
:- multifile sys_goal_simplification/2.
:- meta_predicate sys_goal_simplification(0,0).
:- set_predicate_property(sys_goal_simplification/2, sys_noexpand).

/**
 * sys_simplify_term(C, D):
 * The system predicate succeeds if the simplification of
 * the term C unifies with D.
 */
% sys_simplify_term(+Clause, -Clause)
:- public sys_simplify_term/2.
:- meta_predicate sys_simplify_term(-1,-1).
:- set_predicate_property(sys_simplify_term/2, sys_noexpand).
sys_simplify_term(A, B) :-
   sys_term_simplification(A, B), !.
sys_simplify_term(T, T).

/**
 * sys_simplify_goal(C, D):
 * The system predicate succeeds if the simplification of
 * the goal C unifies with D.
 */
% sys_simplify_goal(+Goal, -Goal)
:- public sys_simplify_goal/2.
:- meta_predicate sys_simplify_goal(0,0).
:- set_predicate_property(sys_simplify_goal/2, sys_noexpand).
sys_simplify_goal(A, B) :-
   sys_goal_simplification(A, B), !.
sys_simplify_goal(G, G).

/*******************************************************/
/* Term/Goal Rebuild                                   */
/*******************************************************/

/**
 * sys_term_rebuilding(C, D):
 * This predicate can be used to define custom term
 * rebuilding rules.
 */
% sys_term_rebuilding(+Clause, -Clause)
:- public sys_term_rebuilding/2.
:- multifile sys_term_rebuilding/2.
:- meta_predicate sys_term_rebuilding(-1,-1).
:- set_predicate_property(sys_term_rebuilding/2, sys_noexpand).
:- static sys_term_rebuilding/2.

/**
 * sys_goal_rebuilding(C, D):
 * This predicate can be used to define custom goal
 * rebuilding rules.
 */
% sys_goal_rebuilding(+Goal, -Goal)
:- public sys_goal_rebuilding/2.
:- multifile sys_goal_rebuilding/2.
:- meta_predicate sys_goal_rebuilding(0,0).
:- set_predicate_property(sys_goal_rebuilding/2, sys_noexpand).
:- static sys_goal_rebuilding/2.

/**
 * sys_rebuild_term(C, D):
 * The system predicate succeeds if the rebuild of
 * the term C unifies with D
 */
% sys_rebuild_term(+Clause, -Clause)
:- public sys_rebuild_term/2.
:- meta_predicate sys_rebuild_term(-1,-1).
:- set_predicate_property(sys_rebuild_term/2, sys_noexpand).
sys_rebuild_term(P, P) :-
   sys_var(P), !.
sys_rebuild_term(A, C) :-
   sys_term_rebuilding(A, B), !,
   sys_rebuild_term(B, C).
sys_rebuild_term(G, H) :-
   sys_callable(G),
   sys_functor(G, J, A),
   sys_make_indicator(J, A, I),
   predicate_property(I, (meta_predicate M)),
   \+ predicate_property(I, sys_noexpand), !,
   M =.. [_|R],
   sys_univ(G, [F|L]),
   sys_rebuild_term_shift(R, F, U, V),
   sys_rebuild_term_args(V, L, S),
   sys_univ(H, [U|S]).
sys_rebuild_term(G, G).

% sys_rebuild_term_shift(+Modes, +Funs, -Funs, -Modes)
:- private sys_rebuild_term_shift/4.
sys_rebuild_term_shift(M, A, B, N) :-
   A = P:Q, !,
   sys_rebuild_term_shift(M, Q, R, N),
   sys_replace_site(B, A, P:R).
sys_rebuild_term_shift([M|L], A, B, N) :-
   A = P::Q, !,
   sys_rebuild_term_arg(M, P, R),
   sys_rebuild_term_shift(L, Q, S, N),
   sys_replace_site(B, A, R::S).
sys_rebuild_term_shift(M, P, P, M).

% sys_rebuild_term_args(+Modes, +Args, -Args)
:- private sys_rebuild_term_args/3.
sys_rebuild_term_args([], [], []).
sys_rebuild_term_args([M|R], [A|L], [B|S]) :-
   sys_rebuild_term_arg(M, A, B),
   sys_rebuild_term_args(R, L, S).

% sys_rebuild_term_arg(+Mode, +Arg, -Arg)
:- private sys_rebuild_term_arg/3.
sys_rebuild_term_arg(0, X, Y) :- !,
   sys_rebuild_term(X, Y).
sys_rebuild_term_arg(-1, X, Y) :- !,
   sys_rebuild_goal(X, Y).
sys_rebuild_term_arg(_, X, X).

/**
 * sys_rebuild_goal(C, D):
 * The system predicate succeeds if the rebuild of
 * the goal C unifies with D.
 */
% sys_rebuild_goal(+Goal, -Goal)
:- public sys_rebuild_goal/2.
:- meta_predicate sys_rebuild_goal(0,0).
:- set_predicate_property(sys_rebuild_goal/2, sys_noexpand).
sys_rebuild_goal(P, P) :-
   sys_var(P), !.
sys_rebuild_goal(A, C) :-
   sys_goal_rebuilding(A, B), !,
   sys_rebuild_goal(B, C).
sys_rebuild_goal(G, H) :-
   sys_callable(G),
   sys_functor(G, J, A),
   sys_make_indicator(J, A, I),
   predicate_property(I, (meta_predicate M)),
   \+ predicate_property(I, sys_noexpand), !,
   M =.. [_|R],
   sys_univ(G, [F|L]),
   sys_rebuild_goal_shift(R, F, U, V),
   sys_rebuild_goal_args(V, L, S),
   sys_univ(H, [U|S]).
sys_rebuild_goal(G, G).

% sys_rebuild_goal_shift(+Modes, +Funs, -Funs, -Modes)
:- private sys_rebuild_goal_shift/4.
sys_rebuild_goal_shift(M, A, B, N) :-
   A = P:Q, !,
   sys_rebuild_goal_shift(M, Q, R, N),
   sys_replace_site(B, A, P:R).
sys_rebuild_goal_shift([M|L], A, B, N) :-
   A = P::Q, !,
   sys_rebuild_goal_arg(M, P, R),
   sys_rebuild_goal_shift(L, Q, S, N),
   sys_replace_site(B, A, R::S).
sys_rebuild_goal_shift(M, P, P, M).

% sys_rebuild_goal_args(+Modes, +Args, -Args)
:- private sys_rebuild_goal_args/3.
sys_rebuild_goal_args([], [], []).
sys_rebuild_goal_args([M|R], [A|L], [B|S]) :-
   sys_rebuild_goal_arg(M, A, B),
   sys_rebuild_goal_args(R, L, S).

% sys_rebuild_goal_arg(+Mode, +Arg, -Arg)
:- public sys_rebuild_goal_arg/3.
sys_rebuild_goal_arg(0, X, Y) :- !,
   sys_rebuild_goal(X, Y).
sys_rebuild_goal_arg(-1, X, Y) :- !,
   sys_rebuild_term(X, Y).
sys_rebuild_goal_arg(_, X, X).

/********************************************************************/
/* Predefined Simplifications                                       */
/********************************************************************/

/* (,)/2 and (;)/2 flattening */
sys_goal_simplification((  A, _), _) :-
   var(A), !, fail.
sys_goal_simplification((  true, C), C).
sys_goal_simplification((  _, A), _) :-
   var(A), !, fail.
sys_goal_simplification((  C, true), C).
sys_goal_simplification((  (  A, B), C), J) :-
   sys_simplify_goal((  B, C), H),
   sys_simplify_goal((  A, H), J).
sys_goal_simplification((  A; _), _) :-
   var(A), !, fail.
sys_goal_simplification((  (  A; B); C), J) :-
   sys_simplify_goal((  B; C), H),
   sys_simplify_goal((  A; H), J).

/* (/\)/2 flattening */
sys_term_simplification(A /\ _, _) :-
   var(A), !, fail.
sys_term_simplification(unit /\ C, C).
sys_term_simplification(_ /\ A, _) :-
   var(A), !, fail.
sys_term_simplification(C /\ unit, C).
sys_term_simplification(A /\
                        (  B /\ C), J) :-
   sys_simplify_term(A /\ B, H),
   sys_simplify_term(H /\ C, J).
