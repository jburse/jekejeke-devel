/**
 * This module provides functions on tagged structures without the sins
 * of SWI-Prolog 7. Like already with the tagged structures itself, no
 * new Prolog term category is introduced and we stay complete in the
 * data model of the ISO core standard. Further, the translation is such
 * that head side conditions are added to the end of a Prolog clause.
 *
 * Examples:
 * ?- P = point{x:1,y:2}, X = P.x, Y = P.y.
 * X = 1, Y = 2
 * ?- P = point{x:1,y:2}, V = P.K.
 * V = 1, K = x ;
 * V = 2, K = y
 *
 * After importing the module a dot notation by the operator ('.')/2 will
 * be available to the importing module. The operator can be used to
 * access tagged structure fields anywhere inside the head or the body
 * of a Prolog clause. The operator will be replaced by ('.')/3 side
 * conditions through the function expansion framework and by a
 * rest expansion.
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

% user:rest_expansion(+Rest, -Rest)
:- public user:rest_expansion/2.
:- multifile user:rest_expansion/2.
user:rest_expansion(D.F, sys_cond(X,'.'(D, F, X))).

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
 * The predicate succeeds whenever the function F applied to the
 * tagged structure D succeeds with a value X.
 */
:- public '.'/3.
'.'(D, F, X) :-
   var(F), !,
   get_dict(F, D, X).
'.'(D, F, X) :-
   atomic(F), !,
   get_dict_ex(F, D, X).
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
