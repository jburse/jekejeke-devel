/**
 * For debugging purposes it might be necessary to access syntax
 * operators that are not accessible from the top-level by the
 * module system visibility rules. We provide predicates that
 * allow direct access.
 *
 * The directly accessible syntax operators can be tested and
 * enumerated by the predicate current_syntax/1. The predicates
 * syntax_property/2, set_syntax_property/2 and reset_syntax_property/2
 * are responsible for accessing and modifying properties of
 * directly accessible syntax operators.
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

:- package(library(jekdev/reference/inspection)).
:- use_package(foreign(jekdev/reference/inspection)).

:- module(syntax, []).

/**
 * current_syntax(P):
 * The predicate succeeds for each directly accessible syntax operator P.
 */
% current_syntax(-Indicator)
:- public current_syntax/1.
current_syntax(I) :-
   ground(I), !,
   sys_current_syntax_chk(I).
current_syntax(I) :-
   sys_current_syntax(L),
   sys_member(I, L).

% already defined in body.p of runtime
% :- private sys_current_syntax/1.
% :- special(sys_current_syntax/1, 'SpecialSyntax', 0).

:- private sys_current_syntax_chk/1.
:- special(sys_current_syntax_chk/1, 'SpecialSyntax', 1).

/**
 * syntax_property(O, Q):
 * The predicate succeeds for the properties Q of the syntax operator O. The
 * predicate will also try to access invisible syntax operators.
 */
% syntax_property(+-Oper, -+Property)
:- public syntax_property/2.
syntax_property(I, R) :-
   ground(I), !,
   syntax_property2(I, R).
syntax_property(I, R) :-
   var(R), !,
   sys_current_syntax(L),
   sys_member(I, L),
   sys_syntax_property(I, P),
   sys_member(R, P).
syntax_property(I, R) :-
   sys_syntax_property_idx(R, P),
   sys_member(I, P).

% syntax_property2(+Oper, -Property)
:- private syntax_property2/2.
syntax_property2(I, R) :-
   var(R), !,
   sys_syntax_property(I, P),
   sys_member(R, P).
syntax_property2(I, R) :-
   functor(R, F, A),
   sys_syntax_property_chk(I, F/A, P),
   sys_member(R, P).

:- private sys_syntax_property/2.
:- special(sys_syntax_property/2, 'SpecialSyntax', 2).

% already defined in body.p of runtime
% :- private sys_syntax_property_chk/3.
% :- special(sys_syntax_property_chk/3, 'SpecialSyntax', 3).

% already defined in body.p of runtime
% :- private sys_syntax_property_idx/2.
% :- special(sys_syntax_property_idx/2, 'SpecialSyntax', 4).

/**
 * set_syntax_property(O, Q):
 * The predicate assigns the property Q to the syntax operator O. The
 * predicate will also try to access invisible syntax operators.
 */
% set_syntax_property(+Oper, +Property)
:- public set_syntax_property/2.
:- special(set_syntax_property/2, 'SpecialSyntax', 5).

/**
 * reset_syntax_property(O, Q):
 * The predicate de-assigns the property Q from the syntax operator O. The
 * predicate will also try to access invisible syntax operators.
 */
% reset_syntax_property(+Oper, +Property)
:- public reset_syntax_property/2.
:- special(reset_syntax_property/2, 'SpecialSyntax', 6).
