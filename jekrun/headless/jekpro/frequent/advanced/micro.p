/**
 * This module provides an interface to micro engines. Micro engines
 * share the same controller with its parent. A new micro engine can be
 * created by the predicate micro_new/2. A micro engine can then be
 * controlled by the predicates micro_cont/1 and micro_close/1.
 *
 * To exchanged data with a micro engine, pivots are provided.
 * Pivots work like unsynchronized queues of length one. A new pivot
 * can be created by the predicate pivot_new/1. A pivot can then
 * be modified by the predicates pivot_put/2 and pivot_take/2.
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
:- use_package(foreign(matula/util/misc)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/tools/term)).

:- module(micro, []).

/**
 * micro_new(G, M):
 * The predicate succeeds in M with a new micro engine for the goal G.
 */
% micro_new(+Goal, -Micro)
:- public micro_new/2.
:- foreign(micro_new/2, 'ForeignMicro', sysMicroNew('Interpreter','AbstractTerm')).

/**
 * micro_cont(M):
 * The predicate succeeds if the micro engine could continue to a success.
 * Othewise the predicate fails.
 */
% micro_cont(+Micro)
:- public micro_cont/1.
:- foreign(micro_cont/1, 'ForeignMicro', sysMicroCont('CallIn')).

/**
 * micro_close(M):
 * The predicate succeeds in closing the micro engine M.
 */
% micro_close(+Micro)
:- public micro_close/1.
:- virtual micro_close/1.
:- foreign(micro_close/1, 'CallIn', close).

/**
 * pivot_new(P):
 * The predicate succeeds in P with a new pivot.
 */
% pivot_new(-Pivot)
:- public pivot_new/1.
:- foreign_constructor(pivot_new/1, 'Pivot', new).

/**
 * pivot_put(P, O):
 * The predicate succeeds for sending a copy of the term O
 * to the pivot P.
 */
% pivot_put(+Pivot, +Term)
:- public pivot_put/2.
:- foreign(pivot_put/2, 'ForeignMicro',
      sysPivotPut('Interpreter','Pivot','AbstractTerm')).

/**
 * pivot_take(P, O):
 * The predicate succeeds for getting a term O form the pivot P.
 */
% pivot_take(+Pivot, -Term)
:- public pivot_take/2.
:- virtual pivot_take/2.
:- foreign(pivot_take/2, 'Pivot', take).