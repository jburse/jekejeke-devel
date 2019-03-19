/**
 * This module provides additional access to sources. The predicate
 * sys_first_location/2 and sys_location/2 allow quick lookup of indicators
 * from positions. The predicates also work for local modules.
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

:- package(library(jekdev/reference/inspection)).
:- use_package(foreign(jekdev/reference/inspection)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/tools/term)).

:- module(base, []).

/**
 * sys_first_location(S, O, L, I):
 * The predicate succeeds in I with the predicate indicators of the
 * declarations in source S at file path O and line number L.
 */
% sys_first_location(+Atom, +Atom, +Integer, -Indicator)
:- public sys_first_location/4.
:- foreign(sys_first_location/4, 'ForeignBase',
      sysFirstLocation('Interpreter','CallOut','TermAtomic',
         'String',int)).

/**
 * sys_location(S, O, L, I):
 * The predicate succeeds in I with the predicate indicators of the
 * static clauses in source S at file path O and line number L.
 */
% sys_location(+Atom, +Atom, +Integer, -Indicator)
:- public sys_location/4.
:- foreign(sys_location/4, 'ForeignBase',
      sysLocation('Interpreter','CallOut','TermAtomic',
         'String',int)).

/**
 * sys_provable_hash(I, S, J):
 * The predicate succeeds in J with the predicate indicator which
 * is the short form of I in the source S.
 */
% sys_provable_hash(+Indicator, +Atom, -Indicator)
:- public sys_provable_hash/3.
:- foreign(sys_provable_hash/3, 'ForeignBase',
      sysProvableHash('Interpreter','Object','TermAtomic')).
