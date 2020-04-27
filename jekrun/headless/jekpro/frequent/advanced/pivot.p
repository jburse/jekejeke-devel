/**
 * This module provides access to the pivot data type. The data type
 * is a collection of term skeletons. During modificaton a term skeletons
 * copy of the given term is created. And upon access an instance of the
 * term skeletons is created.
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
:- use_package(foreign(jekpro/reference/structure)).

:- module(pivot, []).

/**
 * sys_pivot_new(P):
 * The predicate succeeds in P with a new pivot.
 */
% sys_pivot_new(-Pivot)
:- foreign_constructor(sys_pivot_new/1, 'SetEntry', new).

/**
 * sys_pivot_set(P, O):
 * The predicate succeeds setting the pivot P to O.
 */
% sys_pivot_set(+Pivot, +Term)
:- foreign(sys_pivot_set/2, 'ForeignPivot',
      sysPivotSet('Interpreter', 'SetEntry', 'Object')).

/**
 * sys_pivot_get(P, O):
 * The predicate succeeds in O with a copy of the pivot P.
 */
% sys_pivot_get(+Pivot, -Term)
:- foreign(sys_pivot_get/2, 'ForeignPivot',
      sysPivotGet('SetEntry')).

/**
 * sys_pivot_add(P, O):
 * The predicate succeeds extending the pivot P by O.
 */
% sys_pivot_add(+Pivot, +Term)
:- foreign(sys_pivot_add/2, 'ForeignPivot',
      sysPivotAdd('Interpreter', 'SetEntry', 'Object')).

/**
 * sys_pivot_list(R, U):
 * The predicate succeeds in U with the values
 * of the pivot R.
 */
% sys_pivot_list(+Pivot, -Term)
:- foreign(sys_pivot_list/2, 'ForeignPivot',
      sysPivotList('CallOut', 'SetEntry')).

/**
 * sys_pivot_put(P, O):
 * The predicate succeeds extending the pivot P by O. The
 * predicate fails if O is already present in P.
 */
% sys_pivot_put(+Pivot, +Term)
:- foreign(sys_pivot_put/2, 'ForeignPivot',
      sysPivotPut('Interpreter', 'SetEntry', 'Object')).

/**
 * sys_pivot_put(P, C, O):
 * The predicate succeeds extending the pivot P by O
 * for the variant comparator C. The predicate fails if O
 * is already present in P.
 */
% sys_pivot_put(+Pivot, +Comparator, +Term)
:- foreign(sys_pivot_put/3, 'ForeignPivot',
      sysPivotPut('Interpreter', 'SetEntry', 'AbstractLexical', 'Object')).

/**
 * sys_pivot_enum(R, U):
 * The predicate succeeds in U with the values
 * of the pivot R.
 */
% sys_pivot_enum(+Pivot, -Term)
:- foreign(sys_pivot_enum/2, 'ForeignPivot',
      sysPivotEnum('CallOut', 'SetEntry')).

/**
 * sys_pivot_enum(R, C, U):
 * The predicate succeeds in U with the values
 * of the pivot R for the variant comparator C.
 */
% sys_pivot_enum(+Pivot, +Comparator, -Term)
:- foreign(sys_pivot_enum/3, 'ForeignPivot',
      sysPivotEnum('CallOut', 'SetEntry', 'AbstractLexical')).
