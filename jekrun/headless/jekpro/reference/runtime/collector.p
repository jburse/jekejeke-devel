/**
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

:- package(library(jekpro/reference/runtime)).
:- use_package(foreign(jekpro/reference/runtime)).
:- use_package(foreign(matula/util/data)).
:- use_package(foreign(jekpro/tools/call)).

:- module(collector, []).

/**
 * sys_pivot_new(P):
 * The predicate succeeds in P with a new pivot.
 */
% sys_pivot_new(-Pivot)
:- public sys_pivot_new/1.
:- special(sys_pivot_new/1, 'SpecialCollector', 0).

/*****************************************************************/
/* List                                                          */
/*****************************************************************/

/**
 * sys_pivot_add(P, O):
 * The predicate succeeds extending the pivot P by O.
 */
% sys_pivot_add(+Pivot, +Term)
:- public sys_pivot_add/2.
:- special(sys_pivot_add/2, 'SpecialCollector', 1).

/**
 * sys_pivot_collect(P, E, L):
 * The predicate succceeds in L with the elements of P ending in E.
 */
% sys_pivot_collect(+Pivot, +List, -List)
:- public sys_pivot_collect/3.
:- special(sys_pivot_collect/3, 'SpecialCollector', 2).

/*****************************************************************/
/* Tree                                                          */
/*****************************************************************/

/**
 * sys_pivot_put(P, O):
 * The predicate succeeds extending the pivot P by O. The
 * predicate fails if O is already present in P.
 */
% sys_pivot_put(+Pivot, +Term)
:- public sys_pivot_put/2.
:- special(sys_pivot_put/2, 'SpecialCollector', 3).

/**
 * sys_pivot_gather(P, E, L):
 * The predicate succceeds in L with the elements of P ending in E.
 */
% sys_pivot_gather(+Pivot, +List, -List)
:- public sys_pivot_gather/3.
:- special(sys_pivot_gather/3, 'SpecialCollector', 4).




