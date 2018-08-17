/**
 * We provide function application via the predicates call/n. This predicate
 * takes as a first argument a term which plays the role of a closure, then
 * extends it by the remaining n-1 arguments and calls the resulting goal.
 * Since the closure need not necessarily be an atom but can also be a compound,
 * it is possible to create closures that carry around data.
 *
 * Example:
 * ?- [user].
 * writeln(X) :- write(X), nl.
 * ^D
 * ?- call(writeln, hello).
 * hello
 *
 * The call/n predicates also work for qualified closures. A qualified closure
 * is similarly extended to an unqualified closure. The arguments will be added
 * to the inner unqualified closure and the qualification will be preserved.
 * So extending a:b(X) by Y results in a:b(X,Y). The predicates
 * sys_modext_args/[3..9] allow to call the argument extension without further
 * invoking the result.
 *
 * Example:
 * ?- X is call(sin,0.2).
 * X = 0.19866933079506122
 * ?- Y = sin, X is call(Y,0.2).
 * Y = sin,
 * X = 0.19866933079506122
 *
 * Thanks to  bridging it is also possible to use the call/n predicates
 * inside arithmetic expressions. The bridge will turn the call/n evaluable
 * function into a call/n+1 predicate with the desired effect.
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

:- use_package(foreign(jekpro/frequent/standard)).

:- module(user, []).

% already defined in quali.p
% :- public (:)/2.
% :- virtual (:)/2.
% :- meta_predicate :(?,0).
% :(_,_) :- throw(error(existence_error(body, (:)/2), _)).

% already defined in quali.p
% :- public (:)/3.
% :- virtual (:)/3.
% :- meta_predicate :(?,1,?).
% :(_,_,_) :- throw(error(existence_error(body, (:)/3), _)).

:- public : /4.
:- virtual : /4.
:- meta_predicate :(?,2,?,?).
:(_, _, _, _) :-
   throw(error(existence_error(body,: /4),_)).

:- public : /5.
:- virtual : /5.
:- meta_predicate :(?,3,?,?,?).
:(_, _, _, _, _) :-
   throw(error(existence_error(body,: /5),_)).

:- public : /6.
:- virtual : /6.
:- meta_predicate :(?,4,?,?,?,?).
:(_, _, _, _, _, _) :-
   throw(error(existence_error(body,: /6),_)).

:- public : /7.
:- virtual : /7.
:- meta_predicate :(?,5,?,?,?,?,?).
:(_, _, _, _, _, _, _) :-
   throw(error(existence_error(body,: /7),_)).

:- public : /8.
:- virtual : /8.
:- meta_predicate :(?,6,?,?,?,?,?,?).
:(_, _, _, _, _, _, _, _) :-
   throw(error(existence_error(body,: /8),_)).

:- public : /9.
:- virtual : /9.
:- meta_predicate :(?,7,?,?,?,?,?,?,?).
:(_, _, _, _, _, _, _, _, _) :-
   throw(error(existence_error(body,: /9),_)).

% already defined in quali.p
% :- public (::)/2.
% :- virtual (::)/2.
% :- meta_predicate ::(?,::(0)).
% ::(_,_) :- throw(error(existence_error(body, (::)/2), _)).

% already defined in quali.p
% :- public (::)/3.
% :- virtual (::)/3.
% :- meta_predicate ::(?,::(1),?).
% ::(_,_,_) :- throw(error(existence_error(body, (::)/3), _)).

:- public :: /4.
:- virtual :: /4.
:- meta_predicate ::(?,::(2),?,?).
::(_, _, _, _) :-
   throw(error(existence_error(body,:: /4),_)).

:- public :: /5.
:- virtual :: /5.
:- meta_predicate ::(?,::(3),?,?,?).
::(_, _, _, _, _) :-
   throw(error(existence_error(body,:: /5),_)).

:- public :: /6.
:- virtual :: /6.
:- meta_predicate ::(?,::(4),?,?,?,?).
::(_, _, _, _, _, _) :-
   throw(error(existence_error(body,:: /6),_)).

:- public :: /7.
:- virtual :: /7.
:- meta_predicate ::(?,::(5),?,?,?,?,?).
::(_, _, _, _, _, _, _) :-
   throw(error(existence_error(body,:: /7),_)).

:- public :: /8.
:- virtual :: /8.
:- meta_predicate ::(?,::(6),?,?,?,?,?,?).
::(_, _, _, _, _, _, _, _) :-
   throw(error(existence_error(body,:: /8),_)).

:- public :: /9.
:- virtual :: /9.
:- meta_predicate ::(?,::(7),?,?,?,?,?,?,?).
::(_, _, _, _, _, _, _, _, _) :-
   throw(error(existence_error(body,:: /9),_)).

/**
 * sys_modext_args(P, Y1, .., Yn, Q):
 * The predicate adds the arguments Y1, .., Yn to the
 * callable P and unifies the result with Q. The result Q will have
 * the same call-site information and the same colon and double
 * colon notation as the callable P. The predicate is currently
 * defined for 1 ≤ n ≤ 7.
 */
:- public sys_modext_args/3.
:- special(sys_modext_args/3, 'SpecialApply', 0).

:- public sys_modext_args/4.
:- special(sys_modext_args/4, 'SpecialApply', 0).

:- public sys_modext_args/5.
:- special(sys_modext_args/5, 'SpecialApply', 0).

:- public sys_modext_args/6.
:- special(sys_modext_args/6, 'SpecialApply', 0).

:- public sys_modext_args/7.
:- special(sys_modext_args/7, 'SpecialApply', 0).

:- public sys_modext_args/8.
:- special(sys_modext_args/8, 'SpecialApply', 0).

:- public sys_modext_args/9.
:- special(sys_modext_args/9, 'SpecialApply', 0).

/**
 * call(P, Y1, .., Yn): [TC2 8.15.4]
 * The goal call(p(X1, .., Xm), Y1, .., Yn) succeeds whenever
 * the goal p(X1, .., Xm, Y1, .., Yn) succeeds. The predicate is
 * currently defined for 1 ≤ n ≤ 7.
 */
% call(+Goal, +Term, ..)
:- public call/2.
:- virtual call/2.
:- meta_predicate call(1,?).
:- set_predicate_property(call/2, sys_notrace).
:- special(call/2, 'SpecialApply', 1).

:- public call/3.
:- virtual call/3.
:- meta_predicate call(2,?,?).
:- set_predicate_property(call/3, sys_notrace).
:- special(call/3, 'SpecialApply', 1).

:- public call/4.
:- virtual call/4.
:- meta_predicate call(3,?,?,?).
:- set_predicate_property(call/4, sys_notrace).
:- special(call/4, 'SpecialApply', 1).

:- public call/5.
:- virtual call/5.
:- meta_predicate call(4,?,?,?,?).
:- set_predicate_property(call/5, sys_notrace).
:- special(call/5, 'SpecialApply', 1).

:- public call/6.
:- virtual call/6.
:- meta_predicate call(5,?,?,?,?,?).
:- set_predicate_property(call/6, sys_notrace).
:- special(call/6, 'SpecialApply', 1).

:- public call/7.
:- virtual call/7.
:- meta_predicate call(6,?,?,?,?,?,?).
:- set_predicate_property(call/7, sys_notrace).
:- special(call/7, 'SpecialApply', 1).

:- public call/8.
:- virtual call/8.
:- meta_predicate call(7,?,?,?,?,?,?,?).
:- set_predicate_property(call/8, sys_notrace).
:- special(call/8, 'SpecialApply', 1).
