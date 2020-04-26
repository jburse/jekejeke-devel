/**
 * This module provides access to the revolve data type. The data type
 * is a map of term skeletons to pivots. During modificaton a term skeletons
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

:- module(revolve, []).

/**
 * sys_revolve_new(R):
 * Thre predicate succeeds in R with a new revolve.
 */
% sys_revolve_new(-Revolve)
:- foreign(sys_revolve_new/1, 'ForeignRevolve', sysRevolveNew).

/**
 * sys_revolve_new(C, R):
 * The predicate succeeds in R with a new revolve
 * for the variant comparator C.
 */
% sys_revolve_new(+Comparator, -Revolve)
:- foreign(sys_revolve_new/2, 'ForeignRevolve',
      sysRevolveNew('AbstractLexical')).

/**
 * sys_revolve_lookup(R, K, P):
 * The predicate succeeds in P with the old or new pivot
 * for a copy of the key K in the revolve R.
 */
% sys_revolve_lookup(+Revolve, +Term, -Pivot)
:- foreign(sys_revolve_lookup/3, 'ForeignRevolve',
      sysRevolveLookup('Interpreter', 'AbstractMap', 'Object')).

/**
 * sys_revolve_pair(R, U):
 * The predicate succeeds in U with the key value pairs
 * of the revolve R.
 */
% sys_revolve_pair(+Revolve, -Pair)
:- foreign(sys_revolve_pair/2, 'ForeignRevolve',
      sysRevolvePair('CallOut', 'AbstractMap')).

/**
 * sys_revolve_pair(R, C, U):
 * The predicate succeeds in U with the key value pairs
 * of the revolve R for the variant comparator C.
 */
% sys_revolve_pair(+Revolve, +Comparator, -Pair)
:- foreign(sys_revolve_pair/3, 'ForeignRevolve',
      sysRevolvePair('CallOut', 'AbstractMap', 'AbstractLexical')).
