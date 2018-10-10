/**
 * Predicates are brought into intermediate form before execution.
 * The intermediate form de-termines how the head of a clause is
 * unified and how the goals of the body of a clause are invoked.
 * The intermediate form is not necessarily a linear sequence of
 * instructions. Never-theless a thread working on a clause will
 * only be able to execute one block of code at a time. We identify
 * these blocks as instructions and show them in linear form. Some
 * optimization techniques might alter the execution order at runtime.
 *
 * When the intermediate form of a clause is listed, first the Prolog
 * text representation of the clause is given. Then the instructions
 * of the clause are listed in their default linear order. An instruction
 * is listed by showing its name and its operands. Our instruction set
 * only demands two forms of operands, goal arguments and clause
 * skeletons. The clause skeleton operands will be displayed by
 * respecting the operator definitions, list notation and set notation.
 * For better readability the instructions are numbered:
 *
 * instruction	--> integer name [ operand { "," operand } ]
 * operand		--> "_" integer
 * 		            | term.
 *
 * We do not find any branching instructions in our instruction set.
 * Therefore instructions need not be prefix by a label and labels
 * cannot appear in the operands of an instruction. Labels are also
 * not needed to invoke a goal. The called predicate is simply
 * identified by the functor and arity of the goal. Further we do
 * not find any arithmetic or bit operation. These are handled by
 * invoking the corresponding built-ins. Most instructions are executed
 * multiple times since choice points might succeed again and thus
 * continuations might be re-executed.
 *
 * Here is a simple example of a clause and its intermediate form:
 *
 * ?- friendly(hello/1).
 * hello(X) :-
 *     write('Hello '),
 *     write(X),
 *     nl.
 *    0 init_display
 *    1 call_goal write('Hello ')
 *    2 new_bind X
 *    3 unify_var _0, X
 *    4 last_goal write(X)
 *    5 dispose_bind X
 *    6 last_goal nl
 *    7 last_cont
 *
 * Our instruction set is not derived from the WAM architecture [5]
 * since terms are represented by a display and a skeleton. Therefore
 * during unification in write mode we do not need to allocate
 * compounds or lists. Instead our space effort is bound by the
 * number of variable place holders that need to be created.
 *
 * The optimization we implemented therefore tend to reduce the
 * number of place holder allo-cations or to provide the Java
 * virtual machine an opportunity to reuse place holders. The local
 * optimizations are not based on n-grams [6]. Instead we do a variable
 * range analysis with far reaching code movements.
 *
 * The same intermediate code is used for a clause independent of the
 * mode, type or multiplicity of the called goal. We do not take
 * into account the ground-ness, data-type or determinacy of predicate
 * arguments. Subsequently there is no global or local optimization that
 * tries to infer this information [7]. The single independent code
 * idea is also epitomized in that we do not pursue global or local
 * code specialization [8] [9].
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
   sys_intermediate_base(U),
   sys_member(I, B),
   sys_friendly(I, U), fail.
friendly(_).
:- set_predicate_property(friendly/1, sys_notrace).

% friendly2(+Indicator)
:- private friendly2/1.
friendly2(I) :-
   sys_intermediate_item_chk(I, U),
   sys_listing_user_chk(U),
   sys_intermediate_base(U),
   sys_friendly(I, U), fail.
friendly2(_).

:- private sys_intermediate_base/1.
:- special(sys_intermediate_base/1, 'SpecialFriendly', 0).

:- private sys_friendly/2.
:- special(sys_friendly/2, 'SpecialFriendly', 1).

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
   sys_intermediate_base(U),
   sys_member(I, B),
   sys_instrumented(I, U), fail.
instrumented(_).
:- set_predicate_property(instrumented/1, sys_notrace).

% instrumented2(+Indicator)
:- private instrumented2/1.
instrumented2(I) :-
   sys_intermediate_item_chk(I, U),
   sys_listing_user_chk(U),
   sys_intermediate_base(U),
   sys_instrumented(I, U), fail.
instrumented2(_).

:- private sys_instrumented/2.
:- special(sys_instrumented/2, 'SpecialFriendly', 2).

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
