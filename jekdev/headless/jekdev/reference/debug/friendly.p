/**
 * Predicates are brought into intermediate form before execution. The
 * intermediate form determines how the head of a clause is unified and
 * how the goals of the body of a clause are invoked. The intermediate
 * form can be listed by the directives friendly/[0,1] and instrumented/[0,1].
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
 * from within the Prolog clause. The predicate instrumented/[0,1] will
 * also list the debugger instrumentation of the clause:
 *
 * Here is a simple example of a clause and its intermediate forms:
 *
 * ?- friendly(hello/1).
 * hello(X) :-
 *    format('Hello %s\n', [X]).
 * 0  unify_var _0, X
 * 1  last_goal format('Hello %s\n', [X])
 *
 * ?- instrumented(hello/1).
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
 * The optimization we implemented therefore tend to reduce the number
 * of placeholder allocations or to provide the Java virtual machine an
 * opportunity to reuse placeholders. The optimizations are not based on
 * n-grams [6]. Instead we analyse the clause head for extra variables
 * and inline disjunctions in the clause body.
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

/**
 * firendly:
 * The predicate lists the intermediate form of the clauses of the
 * user predicates. For a list of the intermediate instructions see
 * the full API documentation.
 */
% friendly
:- public friendly/0.
friendly :-
   friendly(_).
:- set_predicate_property(friendly/0, sys_notrace).

/**
 * friendly(P):
 * The predicate lists the intermediate form of the clauses of the
 * user predicate P. For a list of the intermediate instructions see
 * the full API documentation.
 */
% friendly(+Indicator)
:- public friendly/1.
friendly(I) :-
   ground(I), !,
   friendly2(I).
friendly(I) :-
   bagof(I, (  sys_listing_user(U),
               sys_intermediate_item_idx(U, I)), B),
   sys_show_base(U),
   sys_member(I, B),
   sys_friendly(I, U), fail.
friendly(_).
:- set_predicate_property(friendly/1, sys_notrace).

% friendly2(+Indicator)
:- private friendly2/1.
friendly2(I) :-
   sys_intermediate_item_chk(I, U),
   sys_listing_user_chk(U),
   sys_listing_has_clause(I, U),
   sys_short_base(U),
   sys_friendly(I, U), fail.
friendly2(_).

:- private sys_friendly/2.
:- special(sys_friendly/2, 'SpecialFriendly', 0).

/**
 * instrumented:
 * Works like the predicate friendly/0 except that the debugger
 * instrumented variant of the clause is shown.
 */
% instrumented
:- public instrumented/0.
instrumented :-
   instrumented(_).
:- set_predicate_property(instrumented/0, sys_notrace).

/**
 * instrumented(P):
 * Works like the predicate friendly/1 except that the debugger
 * instrumented variant of the clause is shown.
 */
% instrumented(+Indicator)
:- public instrumented/1.
instrumented(I) :-
   ground(I), !,
   instrumented2(I).
instrumented(I) :-
   bagof(I, (  sys_listing_user(U),
               sys_intermediate_item_idx(U, I)), B),
   sys_show_base(U),
   sys_member(I, B),
   sys_instrumented(I, U), fail.
instrumented(_).
:- set_predicate_property(instrumented/1, sys_notrace).

% instrumented2(+Indicator)
:- private instrumented2/1.
instrumented2(I) :-
   sys_intermediate_item_chk(I, U),
   sys_listing_user_chk(U),
   sys_listing_has_clause(I, U),
   sys_short_base(U),
   sys_instrumented(I, U), fail.
instrumented2(_).

:- private sys_instrumented/2.
:- special(sys_instrumented/2, 'SpecialFriendly', 1).

/**
 * sys_intermediate_item_chk(I, U):
 * If I is a intermediate indicator then the predicate
 * succeeds for each usage source U.
 */
% sys_intermediate_item_chk(+Indicator, -Source)
:- private sys_intermediate_item_chk/2.
sys_intermediate_item_chk(I, U) :-
   \+ provable_property(I, built_in),
   provable_property(I, sys_usage(U)).

/**
 * sys_intermediate_item_idx(I, U):
 * If U is a usage source then the predicate succceeds
 * for each intermediate indicator I.
 */
% sys_intermediate_item_idx(+Source, -Indicator)
:- private sys_intermediate_item_idx/2.
sys_intermediate_item_idx(U, I) :-
   provable_property(I, sys_usage(U)),
   \+ provable_property(I, built_in).
