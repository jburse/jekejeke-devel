/**
 * This module is inspired by SQL query options such as TOP. Providing
 * such a module was recently pioneered by SWI-Prolog. Currently predicates
 * limit/2, offset/2, call_nth/2, distinct/2 and order_by/2 are provided.
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
 * instead a Java object. The implementation of distinct/2 and order_by/2
 * use a custom Java object as well.
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
:- use_package(foreign(matula/util/data)).
:- use_package(foreign(jekpro/tools/call)).

:- module(sequence, []).

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
   pivot_new(P),
   pivot_set(P, 0),
   G,
   pivot_get(P, M),
   N is M+1,
   pivot_set(P, N).

/**
 * distinct(W, G):
 * The predicates succeeds eagerly with only the first solutions
 * of G according to the witnesses W.
 */
% distinct(+Term, +Goal)
:- public distinct/2.
:- meta_predicate distinct(?, 0).
distinct(W, Goal) :-
   revolve_new(R),
   sys_revolve_run(Goal, W, R, nil).

/**
 * order_by(W, G):
 * The predicates succeeds lazily and sorted with only the first
 * solutions of G according to the witnesses W.
 */
% order_by(+Term, +Goal)
:- public order_by/2.
:- meta_predicate order_by(?, 0).
order_by(W, Goal) :-
   sys_goal_globals(W^Goal, J),
   revolve_new(R),
   (sys_revolve_run(Goal, W, R, J), fail; true),
   revolve_pair(R, W-P),
   pivot_get(P, J).

% sys_revolve_run(+Goal, +List, +Ref, +Term)
:- private sys_revolve_run/4.
:- meta_predicate sys_revolve_run(0, ?, ?, ?).
sys_revolve_run(Goal, W, R, J) :-
   Goal,
   revolve_lookup(R, W, P),
   \+ pivot_get(P, _),
   pivot_set(P, J).

/*************************************************************/
/* Pivot Datatype                                            */
/*************************************************************/

/**
 * pivot_new(P):
 * The predicate succeeds in P with a new pivot.
 */
% pivot_new(-Pivot)
:- foreign_constructor(pivot_new/1, 'SetEntry', new).

/**
 * pivot_set(P, O):
 * The predicate succeeds setting the pivot P to O.
 */
% pivot_set(+Pivot, +Term)
:- foreign(pivot_set/2, 'ForeignSequence',
      sysPivotSet('Interpreter', 'SetEntry', 'Object')).

/**
 * pivot_get(P, O):
 * The predicate succeeds in O with a copy of the pivot P.
 */
% pivot_get(+Pivot, -Term)
:- foreign(pivot_get/2, 'ForeignSequence', sysPivotGet('SetEntry')).

/**
 * revolve_new(R):
 * Thre predicate succeeds in R with a new revolve.
 */
% revolve_new(-Revolve)
:- foreign(revolve_new/1, 'ForeignSequence', sysRevolveNew).

/**
 * revolve_lookup(R, K, P):
 * The predicate succeeds in P with the old or new pivot
 * for a copy of the key K in the revolve R.
 */
% revolve_lookup(+Revolve, +Term, -Pivot)
:- foreign(revolve_lookup/3, 'ForeignSequence',
      sysRevolveLookup('Interpreter', 'AbstractMap', 'Object')).

/**
 * revolve_pair(R, U):
 * The predicate succeeds in U with the key value pairs
 * of the revolve R.
 */
% revolve_pair(+Revolve, -Pair)
:- foreign(revolve_pair/2, 'ForeignSequence',
      sysRevolvePair('CallOut', 'AbstractMap')).
