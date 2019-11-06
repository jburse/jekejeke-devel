/**
 * We provide some predicates that allow listing the Java auto generated
 * members. The auto loader has two functions. Firstly it allows loading
 * a Prolog text when a module or class has been specified in a (:)/2
 * respectively (::)/2 invocation. Secondly it allows the automatic
 * generation of interface members for a Java class when class name
 * has been specified in a (:)/2 respectively (::)/2 invocation.
 *
 * The programming interface documentation of the Jekejeke runtime
 * describes the generated interface members in more detail. Basically
 * we generate a public foreign function for an evaluable function if all
 * arguments are numbers otherwise we generate a public foreign function
 * for a predicate. If the name is overloaded we generate branching Prolog
 * code. Bridging and tunnelling is automatically provided by the
 * Prolog interpreter.
 *
 * Example:
 * ?- X is 'Math':'PI'.
 * X = 3.141592653589793
 *
 * ?- system/automatic:generated(_:_/1).
 * % Math.class
 * :- package(foreign(java/lang)).
 * :- module('Math', []).
 * :- reexport(foreign('Object')).
 * :- sys_auto_load(foreign('Math')).
 *
 * :- public foreign_const('E'/1,'Math','E').
 *
 * :- public foreign_const('PI'/1,'Math','PI').
 *
 * :- public foreign_fun(random/1,'Math',random).
 *
 * The usual listing commands listing/0 and listing/1 suppress the
 * automatically generated interface members. The commands generated/0
 * and generated /1 on the other hand allow listing the automatically
 * generated interface members. Only predicates that match the given
 * pattern are listed. The listing profits from our compact representation
 * by stacked modifiers, shortened import specifiers and shortened
 * class specifiers.
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

:- package(library(jekdev/reference/system)).

:- module(automatic, []).
:- use_module(library(inspection/provable)).

/***************************************************************/
/* Listing Automatic Members                                   */
/***************************************************************/

/**
 * generated:
 * The predicate lists the user clauses of automatic predicates.
 */
% generated
:- public generated/0.
:- set_predicate_property(generated/0, sys_notrace).
generated :-
   generated(_).

/**
 * generated(I):
 * The predicate lists the user clauses of automatic predicates
 * match the pattern I.
 */
% generated(-Indicator)
:- public generated/1.
generated(I) :- ground(I), !,
   generated2(I).
generated(I) :-
   bagof(I, (sys_listing_user(U), sys_automatic_item_idx(U, I)), B),
   sys_show_base(U),
   sys_member(I, B),
   sys_show_provable_source(I, U),
   fail.
generated(_).
:- set_predicate_property(generated/1, sys_notrace).

:- private generated2/1.
generated2(I) :-
   sys_automatic_item_chk(I, U),
   sys_listing_user_chk(U),
   sys_short_base(U),
   sys_show_provable_source(I, U),
   fail.
generated2(_).

/**
 * sys_automatic_item_chk(I, U):
 * If I is a automatic indicator then the predicate
 * succeeds for each usage source U.
 */
% sys_automatic_item_chk(+Indicator, -Source)
:- private sys_automatic_item_chk/2.
sys_automatic_item_chk(I, U) :-
   provable_property(I, automatic),
   provable_property(I, sys_usage(U)).

/**
 * sys_automatic_item_idx(I, U):
 * If U is a usage source then the predicate succceeds
 * for each automatic indicator I.
 */
% sys_automatic_item_idx(+Source, -Indicator)
:- private sys_automatic_item_idx/2.
sys_automatic_item_idx(U, I) :-
   provable_property(I, sys_usage(U)),
   provable_property(I, automatic).
