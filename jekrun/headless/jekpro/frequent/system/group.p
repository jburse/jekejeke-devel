/**
 * A Prolog thread group is simply a Java thread group. A Prolog thread
 * group might contain Prolog threads and otherwise threads.
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

:- package(library(jekpro/frequent/system)).
:- use_package(foreign(jekpro/frequent/system)).
:- use_package(foreign(jekpro/tools/call)).

:- module(group, []).

/****************************************************************/
/* Group Inspection                                             */
/****************************************************************/

/**
 * current_group_flag(G, K, V):
 * The predicate succeeds for the values V of the keys K concerning the
 * group G. The following keys are returned by the predicate. For
 * a list of keys see the API documentation.
 */
% current_group_flag(+Group, +Atom, -Atomic)
:- public current_group_flag/3.
current_group_flag(T, K, V) :-
   var(K), !,
   sys_current_group_flag(K),
   sys_get_group_flag(T, K, V).
current_group_flag(T, K, V) :-
   sys_get_group_flag(T, K, V).

% sys_current_group_flag(-Atom)
:- private sys_current_group_flag/1.
:- foreign(sys_current_group_flag/1, 'ForeignGroup',
      sysCurrentGroupFlag('CallOut')).

% sys_get_group_flag(+Group, +Atom, -Atomic)
:- private sys_get_group_flag/3.
:- foreign(sys_get_group_flag/3, 'ForeignGroup',
      sysGetGroupFlag('ThreadGroup','String')).
