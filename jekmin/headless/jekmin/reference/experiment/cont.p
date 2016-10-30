/**
 * This module allows accessing the continuation queue and the verify flag.
 * New goals can be pushed and popped from the continuation queue via the
 * predicates cont_push/1 and cont_pop/0. The predicate sys_ripple/1
 * allows disabling the verify flag during the execution of a goal.
 *
 * It is recommended to push a delayed goal by the predicate
 * sys_assume_cont/1 from the module assume. This predicate will
 * also place an undo handler on the trail that will pop the
 * delayed goal upon backtracking.
 *
 * When the verify flag allows it the whole continuation queue is executed
 * either on the next exit port of a builtin or the next unification port of
 * a defined predicate. The verify flag is automatically disabled during
 * execution of an attribute variable hook.
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

:- package(library(jekmin/reference/experiment)).
:- use_package(foreign(jekmin/reference/experiment)).

:- module(cont, []).

/**
 * cont_push(G):
 * The predicate pushes the goal G on the continuation queue.
 */
% cont_push(+Term)
:- public cont_push/1.
:- meta_predicate cont_push(0).
:- special(cont_push/1, 'SpecialCont', 0).

/**
 * cont_pop:
 * The predicate pops a goal from the continuation queue.
 */
% cont_pop
:- public cont_pop/0.
:- special(cont_pop/0, 'SpecialCont', 1).

/**
 * sys_ripple(A):
 * The predicate succeeds whenever A succeeds. The goal A is
 * invoked with the verify flag temporarily set to off.
 */
% sys_ripple(+Goal)
:- public sys_ripple/1.
:- meta_predicate sys_ripple(0).
:- set_predicate_property(sys_ripple/1, sys_notrace).
:- special(sys_ripple/1, 'SpecialCont', 2).
