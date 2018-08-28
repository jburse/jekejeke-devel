/**
 * Tihs module provides delegates that allow to access and modify Java
 * arrays. Foreign predicates can be registered by one of the directives
 * foreign_dimension/2, foreign_element/2 and foreign_update/2. Foreign
 * evaluable functions can be registered by one of the directives
 * foreign_length/2 or foreign_member/2.
 *
 * directive --> "foreign_dimension(" indicator "," module ")"
 *             | "foreign_element(" indicator "," module ")"
 *             | "foreign_update(" indicator "," module ")"
 *             | "foreign_length(" indicator "," module ")"
 *             | "foreign_member(" indicator "," module ")".
 *
 * Example:
 * :- foreign_dimension(new/2, int[]).
 *
 * As a convenience we have defined the postfix operator [] and the path
 * resolution understands this syntax to find Java array classes. When
 * accessing or modifying array elements the delegates will see to it
 * that the values are automatically normalized or de-normalized Prolog
 * terms. The supported data types are the same as in the ordinary foreign
 * function interface.
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

:- package(library(jekpro/frequent/basic)).
:- use_package(foreign(jekpro/frequent/basic)).

:- module(array, []).

/**
 * foreign_dimension(I, C):
 * Succeeds with registering the predicate indicator I as a foreign
 * array constructor for the array class C.
 */
% foreign_dimension(+IndicatorColon, +Class)
:- public foreign_dimension/2.
foreign_dimension(I, C) :-
   sys_foreign_dimension(I, C),
   sys_check_style_predicate(I).

:- private sys_foreign_dimension/2.
:- special(sys_foreign_dimension/2, 'SpecialArray', 0).

/**
 * foreign_element(I, C):
 * Succeeds with registering the predicate indicator I as a foreign
 * array element getter for the array class C.
 */
% foreign_element(+IndicatorColon, +Class)
:- public foreign_element/2.
foreign_element(I, C) :-
   sys_foreign_element(I, C),
   sys_check_style_predicate(I).

:- private sys_foreign_element/2.
:- special(sys_foreign_element/2, 'SpecialArray', 1).

/**
 * foreign_update(I, C):
 * Succeeds with registering the predicate indicator I as a foreign
 * array element setter for the array class C.
 */
% foreign_update(+IndicatorColon, +Class)
:- public foreign_update/2.
foreign_update(I, C) :-
   sys_foreign_update(I, C),
   sys_check_style_predicate(I).

:- private sys_foreign_update/2.
:- special(sys_foreign_update/2, 'SpecialArray', 2).

/**
 * foreign_length(I, C):
 * Succeeds with registering the predicate indicator I as a foreign
 * array length getter for the array class C.
 */
% foreign_length(+IndicatorColon, +Class)
:- public foreign_length/2.
foreign_length(I, C) :-
   sys_foreign_length(I, C),
   sys_check_style_predicate(I).

:- private sys_foreign_length/2.
:- special(sys_foreign_length/2, 'SpecialArray', 3).

/**
 * foreign_member(I, C):
 * Succeeds with registering the predicate indicator I as a foreign
 * array numeric element getter for the array class C.
 */
% foreign_member(+IndicatorColon, +Class)
:- public foreign_member/2.
foreign_member(I, C) :-
   sys_foreign_member(I, C),
   sys_check_style_predicate(I).

:- private sys_foreign_member/2.
:- special(sys_foreign_member/2, 'SpecialArray', 4).
