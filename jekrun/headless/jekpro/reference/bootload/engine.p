/**
 * Some interpreter state can be queried and updated. The predicate
 * current_prolog_flag/2 allows accessing an interpreter attribute. The
 * predicate set_prolog_flag/2 allows updating an interpreter attribute.
 * The predicates halt/[0,1] allow exiting the current process.
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

:- use_package(foreign(jekpro/reference/bootload)).
:- use_package(foreign(jekpro/tools/call)).

:- module(user, []).

/**
 * current_prolog_flag(F, V): [ISO 8.17.2]
 * The predicate succeeds for the value V of the flag F.
 */
% current_prolog_flag(-Atom, -Term)
:- public current_prolog_flag/2.
current_prolog_flag(Flag, Value) :-
   ground(Flag), !,
   sys_get_flag(Flag, Value).
current_prolog_flag(Flag, Value) :-
   sys_list_flags(List),
   sys_member(Flag, List),
   sys_get_flag(Flag, Value).

:- private sys_list_flags/1.
:- foreign(sys_list_flags/1, 'ForeignEngine',
      sysListFlags('Interpreter')).

:- private sys_get_flag/2.
:- foreign(sys_get_flag/2, 'ForeignEngine',
      sysGetFlag('Interpreter','String')).

/**
 * set_prolog_flag(F, V): [ISO]
 * The predicate sets the flag F to the value V.
 */
% set_prolog_flag(+Atom, +Term)
:- public set_prolog_flag/2.
:- foreign(set_prolog_flag/2, 'ForeignEngine',
      sysSetFlag('Interpreter','String','Object')).

/**
 * halt: [ISO 8.17.3]
 * halt(N): [ISO 8.17.4]
 * The predicate without arguments terminates the interpreter with
 * exit value zero. The unary predicate terminates the interpreter
 * with exit value N.
 */
% halt
:- public halt/0.
halt :-
   halt(0).

% halt(+Integer)
:- public halt/1.
:- foreign(halt/1, 'ForeignEngine',
      sysHalt('Object')).
