/**
 * This module provides predicates to automatically turn a Prolog text
 * into a Java class. The Java class will be a Java proxy class generated
 * for a set of interfaces. The set of interfaces is collected from the
 * re-exported auto loaded Java classes of the current Prolog text. The Java
 * proxy class is generated when an instance of the Java proxy class is
 * requested by the predicates sys_new_instance/2:
 *
 * Examples:
 * :- module(mycomparator, []).
 * :- reexport(foreign(java/util/'Comparator')).
 *
 * :- public new/1.
 * new(X) :- sys_new_instance(X). % define the constructor
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
 * instances should be created with the sys_new_instance/3 predicate.
 *
 * Examples:
 * ?- current_error(X), X::getClass(Y).
 * X = 0r254e36fe,
 * Y = 0r2172deb1
 *
 * ?- current_error(X), sys_get_module(X, Y).
 * X = 0r254e36fe,
 * Y = 0r2172deb1
 *
 * ?- sys_get_module(point(1,2), X).
 * X = point
 *
 * The module and arity of a term object can be queried by the predicate
 * sys_term_object/3. The module of a term object or reference type can
 * be queried by the predicate sys_get_module/2. The module of an ordinary
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

:- package(library(jekpro/frequent/basic)).
:- use_package(foreign(jekpro/frequent/basic)).

:- module(proxy, []).

/***************************************************************/
/* Proxy Objects                                               */
/***************************************************************/

/**
 * sys_new_instance(M, R):
 * The predicate succeeds for a stateless instance R for the
 * Java proxy class of the Prolog module M.
 */
% sys_new_instance(+Slash, -Ref)
:- public sys_new_instance/2.
:- special(sys_new_instance/2, 'SpecialProxy', 0).

/**
 * sys_new_instance(M, S, R):
 * The predicate succeeds for a statefull instance R of size S
 * for the Java proxy class of the Prolog module M.
 */
% sys_new_instance(+Slash, +Integer, -Ref)
:- public sys_new_instance/3.
:- special(sys_new_instance/3, 'SpecialProxy', 1).

/***************************************************************/
/* Module Taxonomy                                             */
/***************************************************************/

/**
 * sys_subclass_of(M, N):
 * The predicate succeeds when M is a subclass of N, meaning
 * that N is found in the re-export chain of M.
 */
% sys_subclass_of(+Atom, +Atom)
:- public sys_subclass_of/2.
:- special(sys_subclass_of/2, 'SpecialProxy', 2).

/**
 * sys_instance_of(O, N):
 * The predicate succeeds when O is an instance of N, meaning
 * that N is found in the re-export chain of the class reference
 * or module name of O.
 */
% sys_instance_of(+Term, +Atom)
:- public sys_instance_of/2.
sys_instance_of(O, N) :-
   sys_get_module(O, M),
   sys_subclass_of(M, N).

