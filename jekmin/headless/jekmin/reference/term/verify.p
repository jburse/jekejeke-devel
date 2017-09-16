/**
 * Type 2 interface pre instantiation hook and XXX_atts naming.
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

:- package(library(jekmin/reference/term)).

:- module(verify, []).
:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).
:- use_module(library(minimal/assume)).
:- use_module(library(experiment/ref)).

% put_atts(+Var, +Key, +Value)
:- public put_atts/3.
put_atts(V, K, W) :-
   del_atts(V, K),
   sys_freeze_var(H, F),
   H = wrap(W),
   sys_compile_hook(V, atts(K, F), R),
   set_ref_property(R, sys_verify(sys_type2)),
   sys_assume_ref(R).

% get_atts(+Var, +Key, -Value)
:- public get_atts/3.
get_atts(V, K, W) :-
   sys_clause_hook(V, atts(K, F), _),
   sys_melt_var(F, wrap(W)).

% del_atts(+Var, +Key)
del_atts(V, K) :-
   sys_clause_hook(V, atts(K, _), R), !,
   sys_retire_ref(R).
del_atts(_, _).

% sys_type2(+Slot, +Var, +Term)
:- private sys_type2/3.
sys_type2(atts(K,_), V, T) :-
   K:verify_attributes(V, T, G),
   sys_assume_cont(G).

% ?- [user].
% foo:verify_attributes(V, _, true) :- get_atts(V, foo, L), write('L='), write(L), nl.

% ?- put_atts(X, foo, [X,Y]), put_atts(Y, foo, [X,Y]), [X,Y]=[1,2].
% L=[_A,_B]
% L=[1,_B]
% X = 1,
% Y = 2

% ?- put_atts(X, foo, [X,Y]), put_atts(Y, foo, [X,Y]), X=Y.
% L=[_A,_B]
% Y = X
