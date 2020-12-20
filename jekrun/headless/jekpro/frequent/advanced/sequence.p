/**
 * This module is inspired by SQL query options such as TOP. Providing
 * such a module was recently pioneered by SWI-Prolog. Currently predicates
 * limit/2, offset/2, call_nth/2, distinct/1 and order_by/2 are provided.
 * The predicates solely work tuple oriented and it is possible to
 * cascade these predicates:
 *
 * Example:
 * ?- limit(5, offset(3, between(1, 10, X))).
 * X = 4 ;
 * X = 5 ;
 * X = 6 ;
 * X = 7 ;
 * X = 8
 *
 * The current implementation of limit/2 and offset/2 is based on
 * call_nth/2. The predicate call_nth/2 is in turn implemented with pivots,
 * an alternative to nb_setarg/3 which does not destruct a Prolog term, but
 * instead a Java object. The implementation of distinct/1 and order_by/2
 * use a custom Java object as well.
 *
 * Examples:
 * ?- order_by([asc(X)], member(X, [red, green, blue])).
 * X = blue ;
 * X = green ;
 * X = red
 * ?- order_by([desc(X)], member(X, [red, green, blue])).
 * X = red ;
 * X = green ;
 * X = blue
 *
 * The order_by/2 order functions are currently limited to ASC and DESC,
 * both using the ISO core standard compare/3 under the hood. The order_by/2
 * predicate can be combined with a goal wrapped in distinct/1 and it will
 * then directly de-duplicate the unsorted part of the solutions
 * in an efficient way.
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

:- package(library(jekpro/frequent/advanced)).
:- use_package(foreign(jekpro/frequent/advanced)).
:- use_package(foreign(jekpro/reference/structure)).
:- use_package(foreign(matula/util/data)).
:- use_package(foreign(jekpro/tools/call)).

:- module(sequence, []).
:- use_module(library(advanced/variant)).
:- use_module(library(advanced/pivot)).
:- use_module(library(advanced/revolve)).
:- use_module(library(runtime/collector)).
:- use_module(library(advanced/abstract)).

/**
 * limit(C, G):
 * The predicate succeeds whenever the goal G succeeds but limits the
 * number of solutions to C.
 */
% limit(+Integer, +Goal)
:- public limit/2.
:- meta_predicate limit(?, 0).
limit(C, G) :-
   C > 0,
   call_nth2(G, N),
   (N < C -> true; !).

/**
 * offset(C, G):
 * The predicate succeeds whenever the goal G succeeds except for the
 * first C solutions which are suppressed.
 */
% offset(+Integer, +Goal)
:- public offset/2.
:- meta_predicate offset(?, 0).
offset(C, G) :-
   call_nth2(G, N),
   N > C.

/**
 * call_nth(G, C):
 * The predicate succeeds whenever G succeeds and unifies C with
 * the numbering of the successes.
 */
% call_nth(+Goal, -Integer)
:- public call_nth/2.
:- meta_predicate call_nth(0, ?).
call_nth(G, C) :- var(C), !,
   call_nth2(G, N),
   C = N.
call_nth(G, C) :-
   C > 0,
   call_nth2(G, N),
   (C =:= N -> !; fail).

:- private call_nth2/2.
:- meta_predicate call_nth2(0, ?).
call_nth2(G, N) :-
   sys_pivot_new(P),
   sys_pivot_set(P, 0),
   G,
   sys_pivot_get(P, M),
   N is M+1,
   sys_pivot_set(P, N).

/**
 * distinct(G):
 * The predicates succeeds eagerly with
 * only the first solutions of G.
 */
% distinct(+Goal)
:- public distinct/1.
:- meta_predicate distinct(0).
distinct(Goal) :-
   term_variables(Goal, W),
   sys_variant_comparator([type(hash)], C),
   sys_pivot_new(P),
   sys_pivot_distinct(Goal, W, P, C).

% sys_pivot_distinct(+Goal, +List, +Ref, +Comparator)
:- private sys_pivot_distinct/4.
:- meta_predicate sys_pivot_distinct(0, ?, ?, ?).
sys_pivot_distinct(Goal, W, P, C) :-
   Goal,
   sys_pivot_put(P, C, W).

/**
 * order_by(W, G):
 * The predicates succeeds lazily and sorted with the
 * solutions of G. Ordering is done according to the list
 * of order function W. For the supported order functions see
 * the API documentation.
 */
% order_by(+Term, +Goal)
:- public order_by/2.
:- meta_predicate order_by(?, 0).
order_by(_, V) :- var(V),
   throw(error(instantiation_error, _)).
order_by(S, distinct(Goal)) :- !,
   sys_order_template(S, W),
   sys_goal_globals(W^Goal, J),
   sys_order_comparator(S, C),
   sys_variant_comparator([type(callback), comparator(C)], K),
   sys_revolve_new(K, R),
   sys_variant_comparator([type(hash)], D),
   (sys_revolve_order(Goal, W, R, J, D), fail; true),
   sys_revolve_pair(R, W-P),
   sys_pivot_enum(P, W-J).
order_by(S, Goal) :-
   sys_order_template(S, W),
   sys_goal_globals(W^Goal, J),
   sys_order_comparator(S, C),
   sys_variant_comparator([type(callback), comparator(C)], K),
   sys_revolve_new(K, R),
   (sys_revolve_order(Goal, W, R, J), fail; true),
   sys_revolve_pair(R, W-P),
   sys_pivot_list(P, W-J).

% sys_revolve_order(+Goal, +List, +Ref, +Term, +Comparator)
:- private sys_revolve_order/5.
:- meta_predicate sys_revolve_order(0, ?, ?, ?, ?).
sys_revolve_order(Goal, W, R, J, C) :-
   sys_goal_kernel(Goal, B),
   B,
   sys_revolve_lookup(R, W, P),
   sys_pivot_put(P, C, W-J).

% sys_revolve_order(+Goal, +List, +Ref, +Term)
:- private sys_revolve_order/4.
:- meta_predicate sys_revolve_order(0, ?, ?, ?).
sys_revolve_order(G, W, R, J) :-
   sys_goal_kernel(G, B),
   B,
   sys_revolve_lookup(R, W, P),
   sys_pivot_add(P, W-J).

/**
 * sys_order_template(L, R):
 * The predicate succeeds in R with the template for the order function L.
 */
% sys_order_template(+List, -List)
:- private sys_order_template/2.
sys_order_template(V, _) :- var(V),
   throw(error(instantiation_error, _)).
sys_order_template([], []) :- !.
sys_order_template([X|Y], [Z|T]) :- !,
   sys_order_template_arg(X, Z),
   sys_order_template(Y, T).
sys_order_template(X, _) :-
   throw(error(type_error(list, X), _)).

% sys_order_template_arg(+Term, -Term)
:- private sys_order_template_arg/2.
sys_order_template_arg(V, _) :- var(V),
   throw(error(instantiation_error, _)).
sys_order_template_arg(asc(X), X) :- !.
sys_order_template_arg(desc(X), X) :- !.
sys_order_template_arg(X, _) :-
   throw(error(domain_error(order, X), _)).

/**
 * sys_order_comparator(L, R):
 * The predicate succeeds in R with the comparator for the order function L.
 */
% sys_order_comparator(+List, -List)
:- private sys_order_comparator/2.
sys_order_comparator(V, _) :- var(V),
   throw(error(instantiation_error, _)).
sys_order_comparator([], []) :- !.
sys_order_comparator([X|Y], [Z|T]) :- !,
   sys_order_comparator_arg(X, Z),
   sys_order_comparator(Y, T).
sys_order_comparator(X, _) :-
   throw(error(type_error(list, X), _)).

% sys_order_comparator_arg(+Term, -Term)
:- private sys_order_comparator_arg/2.
sys_order_comparator_arg(V, _) :- var(V),
   throw(error(instantiation_error, _)).
sys_order_comparator_arg(asc(_), compare) :- !.
sys_order_comparator_arg(desc(_), reversed(compare)) :- !.
sys_order_comparator_arg(X, _) :-
   throw(error(domain_error(order, X), _)).

/************************************************************/
/* Comparator DSL                                           */
/************************************************************/

/**
 * reversed(C, F, X, Y):
 * The reversed comparator of C.
 */
% reversed(+Comparator, -Atom, +Term, +Term)
:- private reversed/4.
:- meta_predicate reversed(3, ?, ?, ?).
reversed(C, F, X, Y) :-
   call(C, F, Y, X).

/**
 * '.'(C, D, F, L, R):
 * The lexical combination of the comparator C and D.
 */
% '.'(+Comparator, +Comparator, -Atom, +Term, +Term)
:- private '.'/5.
:- meta_predicate '.'(3, 3, ?, ?, ?).
'.'(C, D, F, [X|Y], [Z|T]) :-
   call(C, H, X, Z),
   (  H == =
   -> call(D, F, Y, T)
   ;  F = H).

/**
 * [](F, X, Y):
 * The nil comparator.
 */
% [](-Atom, +Term, +Term)
:- private []/3.
[](=, [], []).
