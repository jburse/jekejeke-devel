/**
 * This module provides predicates to automatically turn a Prolog text
 * into a Java class. The Java class will be a Java proxy class generated
 * for a set of interfaces. The set of interfaces is collected from the
 * re-exported auto loaded Java classes of the current Prolog text. The Java
 * proxy class is generated when an instance of the Java proxy class is
 * requested by the predicates sys_instance/1 or sys_instance/2:
 *
 * Example:
 * :- module(mycomparator, []).
 * :- reexport(foreign(java/util/'Comparator')).
 *
 * :- public new/1.
 * new(X) :- sys_instance(X). % define the constructor
 *
 * :- override compare/4.
 * :- public compare/4.
 * compare(_, X, Y, R) :- ... % define the method
 * :- new(X), ... % use the instance
 *
 * The predicate and evaluable functions of the Prolog text will be used
 * for the execution of the methods on the instance. Only methods that belong
 * to the set of interfaces can be invoked directly from Java on proxy
 * instances. If the set of interfaces contains the Slots interface proxy
 * instances should be created with the sys_instance_size/2 or
 * sys_instance_size/3 predicates.
 *
 * Example:
 * ?- current_error(X), X::getClass(Y).
 * X = 0r254e36fe,
 * Y = 0r2172deb1
 *
 * ?- current_error(X), sys_receiver_class(X, Y).
 * X = 0r254e36fe,
 * Y = 0r2172deb1
 *
 * ?- sys_receiver_class(point(1,2), X).
 * X = point
 *
 * The module and arity of a term object can be queried by the predicate
 * sys_term_object/3. The module of a term object or reference type can
 * be queried by the predicate sys_receiver_class/2. The module of an ordinary
 * object is the class name. The module of a proxy object is the originating
 * Prolog module. The predicates sys_subclass_of/2 and sys_instance_of/2
 * check for containment in the re-export chain.
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

:- use_package(foreign(jekpro/frequent/basic)).

:- module(user, []).

/***************************************************************/
/* Proxy Objects                                               */
/***************************************************************/

/**
 * sys_instance(R):
 * sys_instance(P, R):
 * The predicate succeeds for an instance R for the Java proxy class
 * of the current Prolog text. The binary variant allows specifying
 * the module P for the Java proxy class.
 */
% sys_instance(-Ref)
:- public sys_instance/1.
:- static sys_instance/1.
:- set_predicate_property(sys_instance/1, sys_nostack).
sys_instance(R) :-
   sys_parent_goal(G),
   sys_instance_site(G, R).
% sys_instance(+Slash, -Ref)
:- public sys_instance/2.
sys_instance(P, R) :-
   sys_module_site(P, G),
   sys_instance_site(G, R).

% sys_instance_site(+Term, -Ref):
:- private sys_instance_site/2.
:- special(sys_instance_site/2, 'SpecialProxy', 0).

/**
 * sys_instance_size(S, R):
 * sys_instance_size(P, S, R):
 * The predicate succeeds for an instance R of size S for the Java proxy
 * class for the current Prolog text. The ternary variant allows specifying
 * the module P for the Java proxy class.
 */
% sys_instance_size(+Integer, -Ref)
:- public sys_instance_size/2.
:- static sys_instance_size/2.
:- set_predicate_property(sys_instance_size/2, sys_nostack).
sys_instance_size(S, R) :-
   sys_parent_goal(G),
   sys_instance_site_size(G, S, R).
% sys_instance_size(+Slash, +Integer, -Ref)
:- public sys_instance_size/3.
sys_instance_size(P, S, R) :-
   sys_module_site(P, G),
   sys_instance_site_size(G, S, R).

% sys_instance_site_size(+Term, +Integer, -Ref):
:- private sys_instance_site_size/3.
:- special(sys_instance_site_size/3, 'SpecialProxy', 1).

/***************************************************************/
/* Term Objects                                                */
/***************************************************************/

/**
 * sys_term_object(T, F, A):
 * The predicate unifies F with the possibly structured functor
 * of T and unifies A with the arity of T.
 */
% sys_term_object(+-Term, -+Term, -+Integer)
:- public sys_term_object/3.
sys_term_object(T, F, A) :-
   var(T), !,
   sys_term_object2(F, A, T).
sys_term_object(K, J, A) :-
   K = R/T, !,
   functor(T, F, A),
   sys_replace_site(J, K, R/F).
sys_term_object(T, F, A) :-
   functor(T, F, A).

% sys_term_object2(+Term, +Integer, -Term)
:- private sys_term_object2/3.
sys_term_object2(F, _, _) :-
   var(F),
   throw(error(instantiation_error,_)).
sys_term_object2(J, A, K) :-
   J = M/F, !,
   functor(T, F, A),
   sys_replace_site(K, J, M/T).
sys_term_object2(F, A, T) :-
   functor(T, F, A).

/***************************************************************/
/* Module Names                                                */
/***************************************************************/

/**
 * sys_receiver_class(O, M):
 * The predicate succeeds when O is reference type or term object
 * and M unifies with the class reference or module name of O.
 */
% sys_receiver_class(+Term, -Slash)
:- public sys_receiver_class/2.
sys_receiver_class(R, M) :-
   reference(R), !,
   sys_reference_class(R, M).
sys_receiver_class(T, M) :-
   sys_term_object(T, M, _).

% sys_reference_class(+Ref, -Ref)
:- private sys_reference_class/2.
:- special(sys_reference_class/2, 'SpecialProxy', 2).

/**
 * sys_subclass_of(M, N):
 * The predicate succeeds when M is a subclass of N, meaning
 * that N is found in the re-export chain of M.
 */
% sys_subclass_of(+Atom, +Atom)
:- public sys_subclass_of/2.
:- special(sys_subclass_of/2, 'SpecialProxy', 3).

/**
 * sys_instance_of(O, N):
 * The predicate succeeds when O is an instance of N, meaning
 * that N is found in the re-export chain of the class reference
 * or module name of O.
 */
% sys_instance_of(+Term, +Atom)
:- public sys_instance_of/2.
sys_instance_of(O, N) :-
   sys_receiver_class(O, M),
   sys_subclass_of(M, N).
