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
:- static friendly/0.
:- set_predicate_property(friendly/0, sys_nostack).
friendly :-
   sys_parent_goal(G),
   sys_friendly_site(G).
:- set_predicate_property(friendly/0, sys_notrace).

% sys_friendly_site(+Term):
:- private sys_friendly_site/1.
:- special(sys_friendly_site/1, 'SpecialFriendly', 0).

/**
 * friendly(P):
 * The predicate lists the intermediate form of the clauses of the
 * user predicate P. For a list of the intermediate instructions see
 * the full API documentation.
 */
% friendly(+Pattern)
:- public friendly/1.
friendly(I) :-
   ground(I), !,
   sys_friendly(I).
friendly(I) :-
   current_provable(I),
   \+ provable_property(I, built_in),
   sys_friendly(I), fail.
friendly(_).
:- set_predicate_property(friendly/1, sys_notrace).

:- private sys_friendly/1.
:- special(sys_friendly/1, 'SpecialFriendly', 1).

/**
 * instrumented:
 * Works like the predicate friendly/0 except that the debugger
 * instrumented variant of the clause is shown.
 */
% instrumented
:- public instrumented/0.
:- static instrumented/0.
:- set_predicate_property(instrumented/0, sys_nostack).
instrumented :-
   sys_parent_goal(G),
   sys_instrumented_site(G).
:- set_predicate_property(instrumented/0, sys_notrace).

% sys_instrumented_site(+Term):
:- private sys_instrumented_site/1.
:- special(sys_instrumented_site/1, 'SpecialFriendly', 2).

/**
 * instrumented(P):
 * Works like the predicate friendly/1 except that the debugger
 * instrumented variant of the clause is shown.
 */
% instrumented(+Pattern)
:- public instrumented/1.
instrumented(I) :-
   ground(I), !,
   sys_instrumented(I).
instrumented(I) :-
   current_provable(I),
   \+ provable_property(I, built_in),
   sys_instrumented(I), fail.
instrumented(_).
:- set_predicate_property(instrumented/1, sys_notrace).

:- private sys_instrumented/1.
:- special(sys_instrumented/1, 'SpecialFriendly', 3).