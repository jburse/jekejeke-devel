/**
 * Clauses and goals are automatically expanded for the Prolog session
 * queries, for the Prolog text clauses and for the Prolog text directives.
 * The effect can be seen by the following example. The first
 * goal_expansion/2 fact defines a new expansion for the predicate
 * writeln/2. The next rule for the predicate hello/0 already makes
 * use of the expansion in the body:
 *
 * Example:
 * ?- use_module(library(expand)).
 *
 * ?- [user].
 * :- multifile expand:goal_expansion/2.
 * expand:goal_expansion(writeln(X), (write(X), nl)).
 *
 * hello :- writeln('Hello World!').
 * ^D
 *
 * ?- listing(hello/0).
 * hello :-
 *     write('Hello World!'), nl.
 *
 * The clauses are expanded with the help of the system predicate
 * expand_term/2, which in turn expands the goals of the bodies
 * via the system predicate expand_goal/2. The two system predicates
 * are customizable by the end-user via additional rules for the
 * multi-file predicates term_expansion/2 and goal_expansion/2. If
 * the additional multi-file rules fail, the system predicates will
 * simply leave the term respectively the goal unchanged.
 *
 * The clause expansion is table driven. The predicate property
 * sys_noexpand/allows excluding a meta-predicate from the goal
 * traversal. The predicate property sys_traverse/0 allows including
 * a meta-predicate in the term traversal. Closures are currently not
 * expanded. If the term or goal expansion steps into the colon
 * notation (:)/2 it will look up the meta-declarations for the
 * qualified functor and do the expansion for the rest of the arguments.
 *
 * The result of the expansion can be no clause, a single clause or
 * multiple clauses. No clause is indicated by unit/0 as a result.
 * A single clause is simply returned by itself. Multiple clauses
 * can be conjoined by the operator (/\)/2 and returned this way.
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

:- module(user, []).
:- use_module(library(experiment/simp)).

:- public /\ /2.
:- meta_predicate 0/\0.
_ /\ _ :-
   throw(error(existence_error(body,/\ /2),_)).
:- set_predicate_property(/\ /2, sys_rule).

:- public unit/0.
unit :-
   throw(error(existence_error(body,unit/0),_)).

:- public ^ /2.
:- meta_predicate ? ^0.
_ ^ _ :-
   throw(error(existence_error(body,^ /2),_)).
:- set_predicate_property(^ /2, sys_body).

/*******************************************************/
/* Term/Goal Expand                                    */
/*******************************************************/

/**
 * term_expansion(C, D):
 * This predicate can be used to define custom term
 * expansion rules.
 */
% term_expansion(+Clause, -Clause)
:- public term_expansion/2.
:- multifile term_expansion/2.
:- meta_predicate term_expansion(-1,-1).
:- set_predicate_property(term_expansion/2, sys_noexpand).
:- static term_expansion/2.

/**
 * goal_expansion(C, D):
 * This predicate can be used to define custom goal
 * expansion rules.
 */
% goal_expansion(+Goal, -Goal)
:- public goal_expansion/2.
:- multifile goal_expansion/2.
:- meta_predicate goal_expansion(0,0).
:- set_predicate_property(goal_expansion/2, sys_noexpand).
:- static goal_expansion/2.

/**
 * expand_term(C, D):
 * The system predicate succeeds if the expansion of
 * the term C unifies with D.
 */
% expand_term(+Clause, -Clause)
:- public expand_term/2.
:- meta_predicate expand_term(-1,-1).
:- set_predicate_property(expand_term/2, sys_noexpand).
expand_term(P, P) :-
   sys_var(P), !.
expand_term(A, C) :-
   term_expansion(A, B), !,
   expand_term(B, C).
expand_term(G, N) :-
   sys_callable(G),
   sys_functor(G, J, A),
   sys_make_indicator(J, A, I),
   predicate_property(I, (meta_predicate P)),
   \+ predicate_property(I, sys_noexpand), !,
   P =.. [_|R],
   sys_univ(G, [F|L]),
   sys_expand_term_shift(R, F, U, V),
   sys_expand_term_args(V, L, S),
   sys_univ(H, [U|S]),
   sys_simplify_term(H, N).
expand_term(T, U) :-
   sys_simplify_term(T, U).

% sys_expand_term_shift(+Modes, +Funs, -Funs, -Modes)
:- private sys_expand_term_shift/4.
sys_expand_term_shift(M, A, B, N) :-
   A = P:Q, !,
   sys_expand_term_shift(M, Q, R, N),
   sys_replace_site(B, A, P:R).
sys_expand_term_shift([M|L], A, B, N) :-
   A = P::Q, !,
   sys_expand_term_arg(M, P, R),
   sys_expand_term_shift(L, Q, S, N),
   sys_replace_site(B, A, R::S).
sys_expand_term_shift(M, P, P, M).

% sys_expand_term_args(+Modes, +Args, -Args)
:- private sys_expand_term_args/3.
sys_expand_term_args([], [], []).
sys_expand_term_args([M|R], [A|L], [B|S]) :-
   sys_expand_term_arg(M, A, B),
   sys_expand_term_args(R, L, S).

% sys_expand_term_arg(+Mode, +Arg, -Arg)
:- private sys_expand_term_arg/3.
sys_expand_term_arg(0, X, Y) :- !,
   expand_term(X, Y).
sys_expand_term_arg(-1, X, Y) :- !,
   expand_goal(X, Y).
sys_expand_term_arg(_, X, X).

/**
 * expand_goal(C, D):
 * The system predicate succeeds if the expansion of
 * the goal C unifies with D.
 */
% expand_goal(+Goal, -Goal)
:- public expand_goal/2.
:- meta_predicate expand_goal(0,0).
:- set_predicate_property(expand_goal/2, sys_noexpand).
expand_goal(P, P) :-
   sys_var(P), !.
expand_goal(A, C) :-
   goal_expansion(A, B), !,
   expand_goal(B, C).
expand_goal(G, N) :-
   sys_callable(G),
   sys_functor(G, J, A),
   sys_make_indicator(J, A, I),
   predicate_property(I, (meta_predicate P)),
   \+ predicate_property(I, sys_noexpand), !,
   P =.. [_|R],
   sys_univ(G, [F|L]),
   sys_expand_goal_shift(R, F, U, V),
   sys_expand_goal_args(V, L, S),
   sys_univ(H, [U|S]),
   sys_simplify_goal(H, N).
expand_goal(G, H) :-
   sys_simplify_goal(G, H).

% sys_expand_goal_shift(+Modes, +Funs, -Funs, -Modes)
:- private sys_expand_goal_shift/4.
sys_expand_goal_shift(M, A, B, N) :-
   A = P:Q, !,
   sys_expand_goal_shift(M, Q, R, N),
   sys_replace_site(B, A, P:R).
sys_expand_goal_shift([M|L], A, B, N) :-
   A = P::Q, !,
   sys_expand_goal_arg(M, P, R),
   sys_expand_goal_shift(L, Q, S, N),
   sys_replace_site(B, A, R::S).
sys_expand_goal_shift(M, P, P, M).

% sys_expand_goal_args(+Modes, +Args, -Args)
:- private sys_expand_goal_args/3.
sys_expand_goal_args([], [], []).
sys_expand_goal_args([M|R], [A|L], [B|S]) :-
   sys_expand_goal_arg(M, A, B),
   sys_expand_goal_args(R, L, S).

% sys_expand_goal_arg(+Mode, +Arg, -Arg)
:- private sys_expand_goal_arg/3.
sys_expand_goal_arg(0, X, Y) :- !,
   expand_goal(X, Y).
sys_expand_goal_arg(-1, X, Y) :- !,
   expand_term(X, Y).
sys_expand_goal_arg(_, X, X).
