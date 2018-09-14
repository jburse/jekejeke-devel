/**
 * This module provides functions on tagged structures without the
 * sins of SWI-Prolog 7. Like already with the tagged structures
 * itself, no new Prolog term category is introduced and we stay
 * complete in the data model of the ISO core standard. Further, the
 * translation is such that head side conditions are added to the
 * end of a Prolog clause.
 *
 * Examples:
 * ?- P = point{x:1,y:2}, X = P.x, Y = P.y.
 * X = 1, Y = 2
 * ?- P = point{x:1,y:2}, V = P.K.
 * V = 1, K = x ;
 * V = 2, K = y
 *
 * After importing the module a dot notation by the operator (.)/2 will
 * be available to the importing module. The operator can be used to
 * access tagged structure fields anywhere inside the head or the body
 * of a Prolog clause. The operator will be replaced by (.)/3 side
 * conditions through the function expansion framework and by
 * a rest expansion.
 *
 * Examples:
 * ?- D = point{x:1,y:2}.dist().
 * D = 2.23606797749979
 * ?- D = point{x:1,y:2}.offset(3,4).dist().
 * D = 7.211102550927978
 *
 * The operator ('.')/2 can be also used to invoke arbitrary arity
 * functions. To disambiguate be-tween a field access and a zero argument
 * function invocation the module a unit notation by the operator (())/1.
 * Arbitrary arity function definitions can be done by the further
 * operator (:=)/2. Our translation is Pythonesk, the self is placed
 * in the first argument.
 *
 * Finally the operator ('.')/2 can be also used to perform a couple
 * of field operations. Among the field operations we find get(K), which
 * will return the field value. For a non-existing field the field
 * operation will not throw an exception and fail instead. Further
 * field operations include put(K,V) and put(D) to set a single
 * respectively multiple key values at once.
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

/**
 * Obtained rights, copyright notice of SWI-Prolog 7.7.18 the Prolog
 * text boot/dict.pl when we adopted the API specification.
 *
 *    Author:        Jan Wielemaker
 *    E-mail:        J.Wielemaker@vu.nl
 *    WWW:           www.swi-prolog.org
 *    Copyright (c)  2013-2015, University of Amsterdam
 *                              VU University Amsterdam
 *    All rights reserved.
 *
 *    Redistribution and use in source and binary forms, with or without
 *    modification, are permitted provided that the following conditions
 *    are met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 *  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 *  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 *  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE
 */
:- package(library(jekpro/frequent/advanced)).

:- module(func, []).
:- use_module(library(advanced/dict)).
:- use_module(library(basic/lists)).

:- public infix('.').
:- op(100, yfx, '.').
:- set_oper_property(infix('.'), sys_alias(sys_dot)).

:- public infix(sys_dot).
:- op(100, yfx, sys_dot).
:- set_oper_property(infix(sys_dot), sys_portray('.')).

:- public infix(:=).
:- op(800, xfx, :=).

:- public postfix(()).
:- op(50, xf, ()).

:- public := /2.
_ := _ :-
   throw(error(existence_error(body,:= /2),_)).

/**
 * D.F:
 * This rest expansion replaces a dot notation D.F by a
 * side condition that calls ('.')/3.
 */
% user:rest_expansion(+Rest, -Rest)
:- public user:rest_expansion/2.
:- multifile user:rest_expansion/2.
user:rest_expansion(D.F, sys_cond(X,'.'(D, F, X))).

/**
 * D.F := X:
 * This term expansion replaces a dot notation D.F by a
 * clause head with result X.
 */
% user:term_expansion(+Term, -Term)
:- public user:term_expansion/2.
:- multifile user:term_expansion/2.
:- meta_predicate user:term_expansion(-1,-1).
user:term_expansion(A := _, _) :-
   var(A),
   throw(error(instantiation_error,_)).
user:term_expansion(_.A := _, _) :-
   var(A),
   throw(error(instantiation_error,_)).
user:term_expansion(D.F() := X, H) :- !,
   func_def(D, F, X, H).
user:term_expansion(D.F := X, H) :-
   func_def(D, F, X, H).

:- private func_def/4.
func_def(D, F, X, H) :-
   callable(F), !,
   F =.. [G|L],
   append(L, [X], R),
   H =.. [G,D|R].
func_def(_, A, _, _) :-
   throw(error(type_error(callable,A),_)).

/**
 * '.'(D, F, X):
 * The predicate succeeds whenever the field access, field operation
 * or arbitrary arity function F applied to the argument D succeeds
 * with a value X.
 */
:- public '.'/3.
'.'(D, F, X) :-
   var(F), !,
   get_dict(F, D, X).
'.'(D, K, V) :-
   atomic(K), !,
   get_dict_ex(K, D, V).
'.'(D, get(K), V) :- !,
   get_dict(K, D, V).
'.'(D, put(K,V), E) :- !,
   put_dict(K, D, V, E).
'.'(D, put(E), F) :- !,
   put_dict(E, D, F).
'.'(D, F(), X) :- !,
   func_call(D, F, X).
'.'(D, F, X) :-
   func_call(D, F, X).

:- private func_call/3.
func_call(D, F, X) :-
   is_dict(D, T), !,
   F =.. [G|L],
   append(L, [X], R),
   H =.. [G,D|R],
   T:H.
func_call(T, _, _) :-
   throw(error(type_error(dict,T),_)).

:- private get_dict_ex/3.
get_dict_ex(F, D, X) :-
   get_dict(F, D, Y), !,
   X = Y.
get_dict_ex(F, _, _) :-
   throw(error(existence_error(key,F),_)).
