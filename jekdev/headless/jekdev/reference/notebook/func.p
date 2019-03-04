/**
 * This module provides functions on tagged structures and otherwise
 * Prolog terms. Like already with the tagged structures itself, no
 * new Prolog term category is introduced and we stay complete in the
 * data model of the ISO core standard. Further, the translation is such
 * that head side conditions are added to the end of a Prolog clause
 *
 * Examples:
 * ?- P = point{x:1,y:2}, X = P.x, Y = P.y.
 * X = 1, Y = 2
 * ?- P = [1,2], V = P.K.
 * V = 1, K = 0 ;
 * V = 2, K = 1
 *
 * After importing the module a dot notation by the operator ('.')/2
 * will be available to the importing module. The operator can be used to
 * access tagged structure fields, JSON object fields and JSON array
 * elements anywhere inside the head or the body of a Prolog clause.
 * The operator will be replaced through the function expansion framework.
 *
 * Examples:
 * ?- D = {"x":1,"y":2}.dist().
 * D = 2.23606797749979
 * ?- D = point{x:1,y:2}.offset(3,4).dist().
 * D = 7.211102550927978
 *
 * The operator ('.')/2 can be also used to perform a couple of field
 * operations. Among the field operations we find get(K), which will
 * return the field value. For a non-existing field the field operation
 * will not throw an exception and fail instead. Further field operations
 * include put(D) and put(K,V) to set multiple respectively a single
 * key values at once.
 *
 * Finally the operator ('.')/2 can be also used to invoke arbitrary arity
 * functions. To disambiguate between a field access and a zero argument
 * function invocation the module a unit notation by the operator (())/1.
 * Arbitrary arity function definitions can be done by the further operator
 * (:=)/2. Our translation is Pythonesk, the self is placed in the first argument.
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

:- package(library(jekdev/reference/notebook)).

:- module(func, []).
:- use_module(library(notebook/dict)).
:- use_module(library(notebook/json)).
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
 * This rest expansion replaces a dot notation D.F where F is a
 * variable or atomic by a side condition to access the field F.
 */
% user:rest_expansion(+Rest, -Rest)
:- public user:rest_expansion/2.
:- multifile user:rest_expansion/2.
user:rest_expansion(G, sys_cond(X,H)) :-
   sys_eq(G, D.F),
   var(F),
   sys_replace_site(H, G, sys_get_obj_var(F,D,X)).
user:rest_expansion(G, sys_cond(X,H)) :-
   sys_eq(G, D.F),
   atomic(F),
   sys_replace_site(H, G, sys_get_obj_atomic(F,D,X)).

/**
 * D.get(K):
 * D.put(E):
 * D.put(K, V):
 * This rest expansion replaces a dot notation D.get(K), D.put(E)
 * respectively D.put(K,V) by a side condition to perform a field
 * operation get/3, put/3 respectively put/4.
 */
user:rest_expansion(G, sys_cond(X,H)) :-
   sys_eq(G, D.get(F)),
   sys_replace_site(H, G, sys_get_obj(F,D,X)).
user:rest_expansion(G, sys_cond(X,H)) :-
   sys_eq(G, D.put(F)),
   sys_replace_site(H, G, sys_put_obj(F,D,X)).
user:rest_expansion(G, sys_cond(X,H)) :-
   sys_eq(G, D.put(F,V)),
   sys_replace_site(H, G, sys_put_obj(F,D,V,X)).

/**
 * D.F():
 * D.F(X1, .., Xn):
 * This rest expansion replaces a dot notation D.F respectively
 * D.F(X1, .., Xn) by a side condition to invoke the definition
 * of F/0 respectively F/n.
 */
user:rest_expansion(G, sys_cond(X,H)) :-
   sys_eq(G, D.F()),
   sys_replace_site(H, G, sys_call_obj(F,D,X)).
user:rest_expansion(G, sys_cond(X,H)) :-
   sys_eq(G, D.F),
   sys_replace_site(H, G, sys_call_obj(F,D,X)).

/**
 * D.F() := X:
 * D.F(X1, .., Xn) := X:
 * This term expansion replaces a dot notation D.F() respectively
 * D.F(X1, .., Xn) by a clause head with result X making up a
 * definition for F/0 respectively F/n.
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
   make_def(D, F, X, H).
user:term_expansion(D.F := X, H) :-
   compound(F), !,
   make_def(D, F, X, H).
user:term_expansion(_.A := _, _) :-
   throw(error(type_error(compound,A),_)).

% make_def(+Obj, +Term, +Term, -Term)
:- private make_def/4.
make_def(D, F, X, H) :-
   F =.. [G|L],
   append(L, [X], R),
   H =.. [G,D|R].

/***********************************************************/
/* Runtime Support                                         */
/***********************************************************/

% sys_call_obj(+Term, +Obj, -Term)
:- public sys_call_obj/3.
sys_call_obj(F, D, X) :-
   is_dict(D, T), !,
   make_def(D, F, X, H),
   T:H.
sys_call_obj(F, D, X) :-
   make_def(D, F, X, H), H.

% sys_get_obj_var(+Term, +Obj, -Term)
:- public sys_get_obj_var/3.
sys_get_obj_var(F, D, X) :-
   var(F), !,
   sys_get_obj(F, D, X).
sys_get_obj_var(F, D, X) :-
   sys_get_obj_atomic(F, D, X).

% sys_get_obj_atomic(+Term, +Obj, -Term)
:- public sys_get_obj_atomic/3.
sys_get_obj_atomic(F, D, X) :-
   sys_get_obj(F, D, Y), !,
   X = Y.
sys_get_obj_atomic(F, _, _) :-
   throw(error(existence_error(key,F),_)).

% sys_get_obj(+Term, +Obj, -Term)
:- public sys_get_obj/3.
sys_get_obj(K, D, V) :-
   is_dict(D), !,
   get_dict(K, D, V).
sys_get_obj(K, D, V) :-
   is_json(D), !,
   get_json('$STR'(K), D, V).
sys_get_obj(K, D, V) :-
   nth0(K, D, V).

% sys_put_obj(+Term, +Obj, +Term, -Obj)
:- public sys_put_obj/4.
sys_put_obj(K, D, V, E) :-
   is_dict(D), !,
   put_dict(K, D, V, E).
sys_put_obj(K, D, V, E) :-
   is_json(D), !,
   put_json('$STR'(K), D, V, E).
sys_put_obj(K, D, V, E) :-
   nth0(K, D, _, H),
   nth0(K, E, V, H).

% sys_put_obj(+Obj, +Obj, -Obj)
:- public sys_put_obj/3.
sys_put_obj(E, D, F) :-
   is_dict(D), !,
   put_dict(E, D, F).
sys_put_obj(E, D, F) :-
   put_json(E, D, F).
