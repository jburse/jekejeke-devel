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
 * The result of the expansion can be no clause, a single clause or
 * multiple clauses. No clause is indicated by unit/0 as a result. A
 * single clause is simply returned by itself. Multiple clauses can be
 * conjoined by the operator (/\)/2 and returned this way. Expansion is
 * also performed along the existential quantifier (^)/2 second argument.
 *
 * Example:
 * ?- op(500,yfx,++).
 * Yes
 * ?- [user].
 * rest_expansion(X++Y,sys_cond(Z,append(X,Y,Z))).
 * ^D
 * ?- Y = [1,2]++[3].
 * Y = [1,2,3]
 *
 * It is also possible to define rest expansion via the predicate
 * rest_expansion/2 and to invoke rest expansion via the predicate
 * expand_rest/2. Rest expansion is applied to goal or term arguments
 * that are not goals or terms. Rest expansion is driven by meta function
 * declarations and can be block by the predicate property sys_nomacro.
 *
 * Rest expansion might return a result of the form sys_cond(R, C)
 * where C is the so-called side condition. In the context of rest
 * arguments, the side conditions are merged via conjunction to give a
 * new side condition. In the context of a goal G, the condition C is
 * prepended as (C,G), in the context of a term T, the condition C is
 * appended as (T:-C).
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

:- public sys_cond/2.
:- meta_function sys_cond(?,0).
sys_cond(_, _) :-
   throw(error(existence_error(body,sys_cond/2),_)).

:- public unit/0.
unit :-
   throw(error(existence_error(body,unit/0),_)).

:- public /\ /2.
:- meta_predicate /\(0,0).
/\(_, _) :-
   throw(error(existence_error(body,/\ /2),_)).

/*******************************************************/
/* Unpack & Pack                                       */
/*******************************************************/

% sys_unpack_cond(+Rest, -Rest, -Goal)
:- private sys_unpack_cond/3.
sys_unpack_cond(X, X, true) :-
   var(X), !.
sys_unpack_cond(sys_cond(X,G), X, G) :- !.
sys_unpack_cond(X, X, true).

% sys_pack_cond(+Rest, +Goal, -Rest)
:- private sys_pack_cond/3.
sys_pack_cond(X, G, sys_cond(X,G)) :-
   var(G), !.
sys_pack_cond(X, true, X) :- !.
sys_pack_cond(X, G, sys_cond(X,G)).

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
:- meta_predicate term_expansion(-1,-1).
:- set_predicate_property(term_expansion/2, sys_noexpand).
:- static term_expansion/2.

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
   \+ predicate_property(I, sys_noexpand), !,
   expand_term_callable(G, I, H, U),
   simplify_term(H, K),
   simplify_term((K :- U), N).
expand_term(T, U) :-
   simplify_term(T, U).

% expand_term_callable(+Callable, +Indicator, -Callable, -Goal)
:- private expand_term_callable/4.
expand_term_callable(G, I, H, U) :-
   predicate_property(I, meta_predicate(P)), !,
   P =.. [_|R],
   sys_univ(G, [K|L]),
   sys_expand_term_args(R, L, S, U),
   sys_univ(H, [K|S]).
expand_term_callable(G, _, H, U) :-
   sys_univ(G, [K|L]),
   sys_expand_rest_args(L, S, U),
   sys_univ(H, [K|S]).

% sys_expand_term_args(+Modes, +Args, -Args, -Goal)
:- private sys_expand_term_args/4.
sys_expand_term_args([], [], [], true).
sys_expand_term_args([M|R], [A|L], [B|S], T) :-
   sys_expand_term_arg(M, A, B, P),
   sys_expand_term_args(R, L, S, Q),
   simplify_goal((  P, Q), T).

% sys_expand_term_arg(+Mode, +Arg, -Arg)
:- private sys_expand_term_arg/4.
sys_expand_term_arg(0, X, Y, true) :- !,
   expand_term(X, Y).
sys_expand_term_arg(-1, X, Y, true) :- !,
   expand_goal(X, Y).
sys_expand_term_arg(_, X, Y, G) :-
   expand_rest(X, H),
   sys_unpack_cond(H, Y, G).

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
:- meta_predicate goal_expansion(0,0).
:- set_predicate_property(goal_expansion/2, sys_noexpand).
:- static goal_expansion/2.

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
   \+ predicate_property(I, sys_noexpand), !,
   expand_goal_callable(G, I, H, U),
   simplify_goal(H, K),
   simplify_goal((  U, K), N).
expand_goal(G, H) :-
   simplify_goal(G, H).

% expand_goal_callable(+Callable, +Indicator, -Callable, -Goal)
:- private expand_goal_callable/4.
expand_goal_callable(G, I, H, U) :-
   predicate_property(I, meta_predicate(P)), !,
   P =.. [_|R],
   sys_univ(G, [K|L]),
   sys_expand_goal_args(R, L, S, U),
   sys_univ(H, [K|S]).
expand_goal_callable(G, _, H, U) :-
   sys_univ(G, [K|L]),
   sys_expand_rest_args(L, S, U),
   sys_univ(H, [K|S]).

% sys_expand_goal_args(+Modes, +Args, -Args, -Goal)
:- private sys_expand_goal_args/4.
sys_expand_goal_args([], [], [], true).
sys_expand_goal_args([M|R], [A|L], [B|S], T) :-
   sys_expand_goal_arg(M, A, B, P),
   sys_expand_goal_args(R, L, S, Q),
   simplify_goal((  P, Q), T).

% sys_expand_goal_arg(+Mode, +Arg, -Arg, -Goal)
:- private sys_expand_goal_arg/4.
sys_expand_goal_arg(0, X, Y, true) :- !,
   expand_goal(X, Y).
sys_expand_goal_arg(-1, X, Y, true) :- !,
   expand_term(X, Y).
sys_expand_goal_arg(_, X, Y, G) :-
   expand_rest(X, H),
   sys_unpack_cond(H, Y, G).

/*******************************************************/
/* Rest Expand                                         */
/*******************************************************/

/**
 * rest_expansion(C, D):
 * This predicate can be used to define custom rest
 * expansion rules.
 */
% rest_expansion(+Clause, -Clause)
:- public rest_expansion/2.
:- multifile rest_expansion/2.
:- set_predicate_property(rest_expansion/2, sys_noexpand).
:- static rest_expansion/2.

/**
 * expand_rest(C, D):
 * The system predicate succeeds if the expansion of
 * the rest C unifies with D.
 */
% expand_rest(+Goal, -Goal)
:- public expand_rest/2.
:- set_predicate_property(expand_rest/2, sys_noexpand).
expand_rest(P, P) :-
   var(P), !.
expand_rest(A, C) :-
   rest_expansion(A, B), !,
   expand_rest(B, C).
expand_rest(G, N) :-
   callable(G),
   functor(G, J, A),
   J/A = I,
   \+ predicate_property(I, sys_nomacro), !,
   expand_rest_callable(G, I, H, U),
   simplify_rest(H, K),
   sys_pack_cond(K, U, N).
expand_rest(G, H) :-
   simplify_rest(G, H).

% expand_rest_callable(+Callable, +Indicator, -Callable, -Goal)
:- private expand_rest_callable/4.
expand_rest_callable(G, I, H, U) :-
   predicate_property(I, meta_function(P)), !,
   P =.. [_|R],
   G =.. [K|L],
   sys_expand_goal_args(R, L, S, U),
   H =.. [K|S].
expand_rest_callable(G, _, H, U) :-
   G =.. [K|L],
   sys_expand_rest_args(L, S, U),
   H =.. [K|S].

% sys_expand_rest_args(+Args, -Args, -Goal)
:- private sys_expand_rest_args/3.
sys_expand_rest_args([], [], true).
sys_expand_rest_args([A|L], [B|S], T) :-
   expand_rest(A, H),
   sys_unpack_cond(H, B, P),
   sys_expand_rest_args(L, S, Q),
   simplify_goal((  P, Q), T).
