/**
 * Predicates are brought into intermediate form before execution. The
 * intermediate form determines how the head of a clause is unified and
 * how the goals of the body of a clause are invoked. The intermediate
 * form can be listed by the directives vm_list/[0,1] and vm_hooked/[0,1].
 * The intermediate form consists of instructions of the following form:
 *
 * instruction        --> integer [ level ] name [ operand { "," operand } ]
 * level              --> { "   " }
 * operand            --> "_" integer
 *                        | term.
 *
 * An operand that is an argument of the currently invoked goal is denoted
 * by an underscore ('_') followed by an integer, indicating the argument
 * index starting from zero (0). Other operands are simply Prolog terms
 * from within the Prolog clause. The predicate vm_hooked/[0,1] will
 * also list the debugger instrumentation of the clause:
 *
 * Examples:
 * ?- vm_list(hello/1).
 * hello(X) :-
 *    format('Hello %s\n', [X]).
 * 0  unify_var _0, X
 * 1  last_goal format('Hello %s\n', [X])
 *
 * ?- vm_hooked(hello/1).
 * hello(X) :-
 *    format('Hello %s\n', [X]).
 * 0  unify_var _0, X
 * 1  call_goal sys_at
 * 2  call_goal sys_in
 * 3  last_goal format('Hello %s\n', [X])
 * 4  call_goal sys_out
 *
 * Our instruction set is not derived from the WAM architecture [5] since
 * terms are represented by a display and a skeleton. Therefore, during
 * unification in write mode we do not need to allocate compounds or
 * lists. Instead, the space effort is bound by the number of variable
 * placeholders that are created.
 *
 * Exanples:
 * ?- ensure_loaded('<path>/benchmark/harness/jekejeke.p').
 * % 15 consults and 0 unloads in 93 ms.
 * Yes
 *
 * ?- vm_summary.
 *  cut_then        1
 *  flow_trust      1
 *  flow_try        1
 *  goal_call       228
 *  goal_last       110
 *  unify_combo     14
 *  unify_linear    388
 *  unify_mixed     8
 *  unify_term      41
 *
 * The permanently used optimizations are extra variables and disjunction
 * inlining. The predicate vm_summary/[0,1] produces an overall statistic
 * of the Intermediate code instruction in the user space. The predicate
 * vm_report/[0,1] produces a statistic of the intermediate code in the
 * user space on a per predicate basis.
 *
 * With the introduction of the occurs check flag we have introduced head
 * structure analysis which determines whether an argument is linear and/or
 * whether an argument has first occurrence. Depending on this analysis
 * different head argument unification are used and these head unifications
 * can spare an occurs check.
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

:- package(library(jekdev/reference/debug)).
:- use_package(foreign(jekdev/reference/debug)).

:- module(friendly, []).
:- use_module(library(inspection/provable)).
:- use_module(library(basic/lists)).

/****************************************************************/
/* List & Hooked                                                */
/****************************************************************/

/**
 * vm_list:
 * vm_list(P):
 * The predicate lists the intermediate form of the clauses of the
 * user predicates. The unary predicate allows specifying a predicate
 * indicator P. For a list of the intermediate instructions see
 * the full API documentation.
 */
% vm_list
:- public vm_list/0.
:- sys_notrace vm_list/0.
vm_list :-
   vm_disassemble(_, 0).

% vm_list(+Indicator)
:- public vm_list/1.
:- sys_notrace vm_list/1.
vm_list(I) :-
   vm_disassemble(I, 0).

/**
 * vm_hooked:
 * vm_hooked(P):
 * Works like the predicates vm_list/[0,1] except that the debugger
 * instrumented variants of the clauses are shown.
 */
% vm_hooked
:- public vm_hooked/0.
:- sys_notrace vm_hooked/0.
vm_hooked :-
   vm_disassemble(_, 1).

% vm_hooked(+Pattern)
:- public vm_hooked/1.
:- sys_notrace vm_hooked/1.
vm_hooked(I) :-
   vm_disassemble(I, 1).

% vm_disassemble(+Pattern, +Integer)
:- private vm_disassemble/2.
vm_disassemble(I, F) :- ground(I), !,
   vm_disassemble2(I, F).
vm_disassemble(I, F) :-
   bagof(I, (sys_listing_user(U),
      sys_intermediate_item_idx(U, I),
      sys_has_clause(I, U)), B),
   sys_show_base(U),
   sys_show_import(U), nl,
   member(I, B),
   sys_vm_disassemble(I, U, F),
   fail.
vm_disassemble(_, _).

% vm_disassemble2(+Indicator, +Integer)
:- private vm_disassemble2/2.
vm_disassemble2(I, F) :-
   sys_intermediate_item_chk(I, U),
   sys_listing_user_chk(U),
   sys_has_clause(I, U),
   sys_show_base(U), nl,
   sys_vm_disassemble(I, U, F),
   fail.
vm_disassemble2(_, _).

% sys_vm_disassemble(+Indicator, +Source, +Integer)
:- private sys_vm_disassemble/3.
:- special(sys_vm_disassemble/3, 'SpecialFriendly', 0).

/****************************************************************/
/* Summary & Report                                             */
/****************************************************************/

/**
 * vm_summary:
 * vm_summary(P):
 * Works like the predicates vm_list/[0,1] except that an overall statistics
 * about the intermediate codes is reported.
 */
% vm_summary
:- public vm_summary/0.
:- sys_notrace vm_summary/0.
vm_summary :-
   vm_summary(_).

% vm_summary(+Pattern)
:- public vm_summary/1.
:- sys_notrace vm_summary/1.
vm_summary(I) :-
   sys_historgram_new(M),
   vm_summary(I, M),
   sys_historgram_show(M).

% vm_summary(+Pattern, +Map)
:- private vm_summary/2.
vm_summary(I, M) :- ground(I), !,
   vm_summary2(I, M).
vm_summary(I, M) :-
   bagof(I, (sys_listing_user(U),
      sys_intermediate_item_idx(U, I),
      sys_has_clause(I, U)), B),
   member(I, B),
   sys_vm_collect(I, U, M),
   fail.
vm_summary(_, _).

% vm_summary2(+Indicator, , +Map)
:- private vm_summary2/2.
vm_summary2(I, M) :-
   sys_intermediate_item_chk(I, U),
   sys_listing_user_chk(U),
   sys_has_clause(I, U),
   sys_vm_collect(I, U, M),
   fail.
vm_summary2(_, _).

/**
 * vm_report:
 * vm_report(P):
 * Works like the predicates vm_list/[0,1] except that a statistics
 * per predicate about the intermediate codes is reported.
 */
% vm_report
:- public vm_report/0.
vm_report :-
   vm_report(_).
:- set_predicate_property(vm_report/0, sys_notrace).

% vm_report(+Indicator)
:- public vm_report/1.
vm_report(I) :- ground(I), !,
   vm_report2(I).
vm_report(I) :-
   bagof(I, (sys_listing_user(U),
      sys_intermediate_item_idx(U, I),
      sys_has_clause(I, U)), B),
   sys_show_base(U),
   sys_show_import(U), nl,
   member(I, B),
   sys_historgram_new(M),
   sys_vm_collect(I, U, M),
   sys_intermediate_item_sep(I),
   sys_historgram_show(M), nl,
   fail.
vm_report(_).
:- set_predicate_property(vm_report/1, sys_notrace).

% vm_report2(+Indicator)
:- private vm_report2/1.
vm_report2(I) :-
   sys_intermediate_item_chk(I, U),
   sys_listing_user_chk(U),
   sys_has_clause(I, U),
   sys_show_base(U), nl,
   sys_historgram_new(M),
   sys_vm_collect(I, U, M),
   sys_intermediate_item_sep(I),
   sys_historgram_show(M), nl,
   fail.
vm_report2(_).

% sys_vm_collect(+Indicator, +Source, +Map)
:- private sys_vm_collect/3.
:- special(sys_vm_collect/3, 'SpecialFriendly', 1).

/**************************************************************/
/* Helpers                                                    */
/**************************************************************/

/**
 * sys_intermediate_item_chk(I, U):
 * If I is a intermediate indicator then the predicate
 * succeeds for each usage source U.
 */
% sys_intermediate_item_chk(+Indicator, -Source)
sys_intermediate_item_chk(I, U) :-
   \+ provable_property(I, built_in),
   provable_property(I, sys_usage(U)).

/**
 * sys_intermediate_item_idx(I, U):
 * If U is a usage source then the predicate succceeds
 * for each intermediate indicator I.
 */
% sys_intermediate_item_idx(+Source, -Indicator)
sys_intermediate_item_idx(U, I) :-
   provable_property(I, sys_usage(U)),
   \+ provable_property(I, built_in).

/**
 * sys_intermediate_item_sep(I):
 * The predicate shows a separator for the indicator I.
 */
sys_intermediate_item_sep(I) :-
   write('-------- '), writeq(I), write(' ---------'), nl.

% sys_historgram_new(-Map)
:- private sys_historgram_new/1.
:- special(sys_historgram_new/1, 'SpecialFriendly', 2).

% sys_historgram_show(+Map)
:- private sys_historgram_show/1.
:- special(sys_historgram_show/1, 'SpecialFriendly', 3).
