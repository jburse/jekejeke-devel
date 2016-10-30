/**
 * We provide some predicates that allow listing the Java auto generated
 * members. The auto loader has two functions. Firstly it allows loading
 * a Prolog text when a module has been specified in a (:)/2 invocation.
 * Secondly it allows the automatic generation of interface members
 * for a Java class when class name has been specified in a (:)/2
 * invocation.
 *
 * The programming interface documentation of the Jekejeke runtime
 * describes the generated interface members in more detail. Basically
 * we generate a public foreign function for an evaluable function if
 * all arguments are numbers otherwise we generate a public foreign
 * function for a predicate. If the name is overloaded we generate
 * branching Prolog code and if needed a bridge that checks the
 * argument types.
 *
 * Example:
 * ?- X is 'Math':'PI'.
 * X = 3.141592653589793
 *
 * ?- system/automatic:generated(eval(_:_/0)).
 * % Math.class
 * :- module(java/lang/'Math', []).
 * :- reexport(foreign('Object')).
 *
 * :- public foreign_constant('E'/0, 'Math', 'E').
 *
 * :- public foreign_constant('PI'/0, 'Math', 'PI').
 *
 * :- public foreign_function(random/0, 'Math', random).
 *
 * The usual listing commands listing/0 and listing/1 suppress the
 * automatically generated interface members. The commands generated/0
 * and generated /1 on the other hand allow listing the automatically
 * generated interface members. Only evaluable functions or predicates
 * that match the given pattern are listed. The listing profits from
 * our compact representation by stacked modifiers, shortened import
 * specifiers and shorted class specifiers.
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

:- package(library(jekdev/reference/system)).

:- module(automatic, []).

/***************************************************************/
/* Listing Automatic Members                                   */
/***************************************************************/

/**
 * generated:
 * The predicate lists the user clauses of evaluable functions and
 * predicates. Only automatic evaluable functions and predicates
 * are listed.
 */
% generated
:- public generated/0.
:- set_predicate_property(generated/0, sys_notrace).
generated :-
   generated(_).

/**
 * generated(I):
 * The predicate lists the user clauses of evaluable functions and
 * predicates that match the pattern I. Only automatic evaluable
 * functions and predicates are listed.
 */
% generated(-Indicator)
:- public generated/1.
:- set_predicate_property(generated/1, sys_notrace).
generated(I) :-
   ground(I),
   sys_automatic_check(I, U),
   sys_show_base(U),
   sys_automatic_show(I, U), fail.
generated(I) :-
   sys_not(ground(I)),
   bagof(I, sys_automatic_match(I, U), B),
   sys_show_base(U),
   sys_member(I, B),
   sys_automatic_show(I, U), fail.
generated(_).

% sys_automatic_check(+Indicator, -Source)
:- private sys_automatic_check/2.
sys_automatic_check(I, U) :-
   sys_provable_property_chk(I, automatic/0, [automatic]),
   sys_provable_property_chk(I, sys_accessible_usage/1, R),
   sys_member(sys_accessible_usage(U), R).

% sys_automatic_show(+Indicator, +Source)
:- private sys_automatic_show/2.
sys_automatic_show(I, U) :-
   sys_show_provable_source(I, U).

% sys_automatic_match(-Indicator, -Source)
:- private sys_automatic_match/2.
sys_automatic_match(I, U) :-
   sys_current_provable(L),
   sys_member(I, L),
   sys_provable_property_chk(I, automatic/0, [automatic]),
   sys_provable_property_chk(I, sys_accessible_usage/1, R),
   sys_member(sys_accessible_usage(U), R).