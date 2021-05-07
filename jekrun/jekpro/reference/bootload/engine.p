/**
 * Some interpreter state can be queried and updated. The predicate
 * current_prolog_flag/2 allows accessing an interpreter attribute. The
 * predicate set_prolog_flag/2 allows updating an interpreter attribute.
 * The predicates halt/[0,1] allow exiting the current process.
 *
 * The predicates begin_module/1 and end_module/0 can be used to open
 * respectively close a local module. For a consulted file the predicate
 * begin_module/1 will also do first a clear of the local module, and
 * the predicate end_module/0 will do a style check of the local module.
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

:- use_package(foreign(jekpro/reference/bootload)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/tools/term)).

:- module(user, []).
:- use_module(library(stream/console)).

/**
 * current_prolog_flag(F, V): [ISO 8.17.2]
 * The predicate succeeds for the value V of the flag F.
 */
% current_prolog_flag(-Atom, -Term)
:- public current_prolog_flag/2.
current_prolog_flag(Flag, Value) :- var(Flag), !,
   sys_current_flag(Flag),
   sys_get_flag(Flag, Value).
current_prolog_flag(Flag, Value) :-
   sys_get_flag(Flag, Value).

:- private sys_current_flag/1.
:- foreign(sys_current_flag/1, 'ForeignEngine',
      sysCurrentFlag('Interpreter', 'CallOut')).

:- private sys_get_flag/2.
:- foreign(sys_get_flag/2, 'ForeignEngine',
      sysGetFlag('Interpreter', 'String')).

/**
 * set_prolog_flag(F, V): [ISO]
 * The predicate sets the flag F to the value V.
 */
% set_prolog_flag(+Atom, +Term)
:- public set_prolog_flag/2.
:- foreign(set_prolog_flag/2, 'ForeignEngine',
      sysSetFlag('Interpreter', 'String', 'Object')).

/**
 * halt: [ISO 8.17.3]
 * halt(N): [ISO 8.17.4]
 * The predicate without arguments terminates the interpreter with
 * exit value zero. The unary predicate terminates the interpreter
 * with exit value N.
 */
% halt
:- public halt/0.
halt :- halt(0).

% halt(+Integer)
:- public halt/1.
:- foreign(halt/1, 'ForeignEngine', sysHalt('Integer')).

/********************************************************/
/* Prolog Data                                          */
/********************************************************/

/**
 * welcome:
 * version:
 * The predicate displays a version banner.
 */
% welcome
:- public welcome/0.
welcome :- version.
:- set_predicate_property(welcome/0, sys_notrace).

:- public version/0.
version :-
   sys_prolog_version(V),
   write(V), nl,
   sys_prolog_vendor(W),
   write(W), nl.
:- set_predicate_property(version/0, sys_notrace).

:- private sys_prolog_version/1.
:- foreign(sys_prolog_version/1, 'ForeignEngine',
      sysPrologVersion('Interpreter')).

:- private sys_prolog_vendor/1.
:- foreign(sys_prolog_vendor/1, 'ForeignEngine',
      sysPrologVendor('Interpreter')).

/********************************************************/
/* Locale Modules                                       */
/********************************************************/

/**
 * begin_module(N):
 * The predicate begins a new typein module N.
 */
% begin_module(+Atom)
:- public begin_module/1.
begin_module(N) :-
   absolute_file_name(verbatim(N), D),
   sys_module_action(D, [action(begin_module), sys_link(sys_auto_load)]),
   set_prolog_flag(sys_last_pred, null).

/**
 * end_module:
 * The predicate ends the current typein module.
 */
% end_module
:- public end_module/0.
end_module :-
   sys_peek_stack(D),
   sys_module_action(D, [action(end_module), sys_link(sys_auto_load)]),
   set_prolog_flag(sys_last_pred, null).

/**
 * end_all_modules:
 * The predicates ends all current typein modules.
 */
% end_all_modules
:- public end_all_modules/0.
end_all_modules :-
   sys_count_stack(C), >=(C, 2), !,
   end_module,
   end_all_modules.
end_all_modules.

/**
 * top_module(M):
 * The predicate succeeds in M with the top module.
 */
:- public top_module/1.
top_module(N) :-
   sys_peek_stack(D),
   absolute_file_name(verbatim(N), D).

:- private sys_module_action/2.
:- foreign(sys_module_action/2, 'ForeignEngine',
      sysModuleAction('Interpreter', 'TermAtomic', 'Object')).

:- private sys_peek_stack/1.
:- foreign(sys_peek_stack/1, 'ForeignEngine',
      sysPeekStack('Interpreter')).

:- private sys_count_stack/1.
:- foreign(sys_count_stack/1, 'ForeignEngine',
      sysCountStack('Interpreter')).