/**
 * This module is inspired by SQL query options such as TOP. Providing
 * such a module was recently pioneered by SWI-Prolog. Currently predicates
 * limit/2 and offset/2 are provided. The predicates solely work tuple
 * oriented and it is possible to cascade these predicates:
 *
 * Example:
 * ?- limit(5, offset(3, between(1, 10, X))).
 * X = 4 ;
 * X = 5 ;
 * X = 6 ;
 * X = 7 ;
 * X = 8
 *
 * The predicates are implemented with thread local annonymous state
 * variables and setup_call_cleanup/3, they thus work without
 * synchronization and are interrupt safe. Predicates for ORDER BY,
 * DISTINCT and GROUP BY are currently not yet implemented.
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

:- module(sequence, []).
:- use_module(library(experiment/surrogate)).
:- use_module(library(misc/struc)).

/**
 * limit(C, G):
 * The predicate succeeds whenever the goal G succeeds but limits the
 * number of solutions to C.
 */
% limit(+Integer, +Goal)
:- public limit/2.
:- meta_predicate limit(?,0).
limit(C, _) :-
   var(C),
   throw(error(instantiation_error,_)).
limit(C, _) :-
   \+ integer(C),
   throw(error(type_error(integer,C),_)).
limit(C, G) :-
   setup_call_cleanup(
      new_local(K, 0),
      (  call(G),
         get_local(K, M),
         N is M + 1,
         (  N < C
         -> set_local(K, N); !)),
      free_local(K)).

/**
 * offset(C, G):
 * The predicate succeeds whenever the goal G succeeds except for the
 * first C solutions which are suppressed.
 */
% offset(+Integer, +Goal)
:- public offset/2.
:- meta_predicate offset(?,0).
offset(C, _) :-
   var(C),
   throw(error(instantiation_error,_)).
offset(C, _) :-
   \+ integer(C),
   throw(error(type_error(integer,C),_)).
offset(C, G) :-
   setup_call_cleanup(
      new_local(K, 0),
      (  call(G),
         get_local(K, M),
         (  M < C
         -> N is M + 1,
            set_local(K, N), fail; true)),
      free_local(K)).
