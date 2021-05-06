/**
 * This module provides predicates to automatically turn a Prolog module
 * into a Java class. The Java class will be a Java proxy class generated
 * for a set of interfaces. The set of interfaces is collected from the
 * reexported auto loaded Java classes of the given Prolog text. The Java
 * proxy class is generated when an instance of the Java proxy class is
 * requested by the predicate sys_new_instance/2:
 *
 * Example:
 * :- module(mycomparator, []).
 * :- reexport(foreign(java/util/'Comparator')).
 *
 * :- public new/1.
 * new(X) :-
 *     sys_new_instance(mycomparator, X).  % create Java instance
 *
 * :- override compare/4.
 * :- public compare/4.
 * compare(_, X, Y, R) :- ... % define the method
 *
 * The predicate and evaluable functions of the Prolog text will be used
 * for the execution of the methods on the Java proxy instance. Only
 * methods that belong to the set of interfaces can be invoked directly
 * from Java on the Java proxy instances. If the set of interfaces
 * contains directly or indirectly the Java interface InterfacePivot
 * the proxy instances will be state-full and understand the predicates
 * value/2 and set_value/2.
 *
 * Example:
 * ?- sys_assignable_from(java/util/'Comparator', mycomparator).
 * Yes
 *
 * ?- mycomparator:new(X), sys_instance_of(X, java/util/'Comparator').
 * X = 0r709d5f9e
 *
 * ?- mycomparator:new(X), X::compare(7,7,Y).
 * X = 0r43dd69,
 * Y = 0
 *
 * The re-export chain of Prolog modules and auto loaded Java classes
 * defines a module taxonomy. The module taxonomy can be tested by
 * the predicate sys_assignable_from/2, which checks whether one module
 * is derived from another module. Further Java Prolog proxy instances,
 * instances directly created from within Java and Prolog callables can
 * be tested with the predicate sys_instance_of/2.
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

:- package(library(jekpro/frequent/basic)).
:- use_package(foreign(jekpro/frequent/basic)).

:- module(proxy, []).

/***************************************************************/
/* Proxy Objects                                               */
/***************************************************************/

/**
 * sys_new_instance(M, R):
 * The predicate succeeds for an instance R of the
 * Java proxy class for the Prolog module M.
 */
% sys_new_instance(+Slash, -Ref)
:- public sys_new_instance/2.
:- special(sys_new_instance/2, 'SpecialProxy', 0).

/***************************************************************/
/* Module Taxonomy                                             */
/***************************************************************/

/**
 * sys_assignable_from(N, M):
 * The predicate succeeds when the class M is a subclass
 * of the class N.
 */
% sys_assignable_from(+Atom, +Atom)
:- public sys_assignable_from/2.
:- special(sys_assignable_from/2, 'SpecialProxy', 2).

/**
 * sys_get_class(O, C):
 * The predicate succeeds in C with the class of
 * the receiver object O.
 */
% sys_get_class(+Term, -Package)
:- public sys_get_class/2.
sys_get_class(O, _) :- var(O),
   throw(error(instantiation_error, _)).
sys_get_class(K, F) :- K = R/O, !,
   sys_get_class(O, H),
   sys_replace_site(F, K, R/H).
sys_get_class(O, F) :- reference(O), !,
   sys_sys_get_class(O, F).
sys_get_class(O, F) :-
   :(user, functor(O, F, _)).

% sys_sys_get_class(+Ref, -Ref)
:- private sys_sys_get_class/2.
:- special(sys_sys_get_class/2, 'SpecialProxy', 3).

/**
 * sys_instance_of(O, N):
 * The predicate succeeds when the receiver object O
 * is an instance of the class N.
 */
% sys_instance_of(+Term, +Atom)
:- public sys_instance_of/2.
sys_instance_of(O, N) :-
   sys_get_class(O, M),
   sys_assignable_from(N, M).

/******************************************************************/
/* Improved Arg & SetArg                                          */
/******************************************************************/

/**
 * arg(N, O, A):
 * The predicate succeeds in A with the N-th argument
 * of receiver object O.
 */
% arg(+Integer, +Term, -Term)
:- public arg/3.
:- override arg/3.
arg(_, O, _) :- :(user, var(O)),
   throw(error(instantiation_error, _)).
arg(N, _/O, A) :- !,
   arg(N, O, A).
arg(N, O, A) :-
   :(user, arg(N, O, A)).

/**
 * set_arg(N, O, A, P):
 * The predicate succeeds in P with a new receiver object with
 * the N-th argument of the receiver object O replaced by A.
 */
% set_arg(+Integer, +Term, +Term, -Term)
:- public set_arg/4.
:- override set_arg/4.
set_arg(_, O, _, _) :- :(user, var(O)),
   throw(error(instantiation_error, _)).
set_arg(N, K, A, P) :- K = R/O, !,
   set_arg(N, O, A, H),
   sys_replace_site(P, K, R/H).
set_arg(N, O, A, P) :-
   :(user, set_arg(N, O, A, P)).
