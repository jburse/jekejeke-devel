/**
 * For debugging purpose it might be necessary to access sources
 * that are not accessible from the top-level. We provide predicates
 * to allow direct access. For sources there is no need of extra
 * predicates. Access and modification is available through the
 * runtime predicate current_source/1, set_source_property/2 and
 * reset_source_property/2.
 *
 * Examples:
 * ?- sys_atom_slash(X, a/b/c).
 * X = 'a.b.c'
 *
 * ?- sys_atom_slash('c[][]', X).
 * X = {{c}}
 *
 * The predicate sys_atom_slash/2 can be used to explicitly invoke a
 * slash (/)/2 notation conversion for an atom. The notation can be
 * used to denote Prolog text modules and Java Classes. We additionally
 * support conversion for the {}/1 notation as well, which can be used
 * to denote Java Array Classes.
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

:- package(library(jekdev/reference/inspection)).
:- use_package(foreign(jekdev/reference/inspection)).

:- module(notation, []).

/**
 * sys_atom_slash(S, T):
 * The predicate succeeds when S is an atom of the form ‘s1. .. .sn’
 * and T is a slash notation atom of the form s1/../sn, for 1 ≤ n. Besides
 * the (/)/2 operator the {}/1 operator is supported as well.
 */
% sys_atom_slash(+-Atom, -+Term)
:- public sys_atom_slash/2.
:- special(sys_atom_slash/2, 'SpecialNotation', 0).

/**
 * sys_callable_colon(S, T):
 * The predicate succeeds when S is a callable of the form
 * ‘pk-1%pk’(X1, .., Xm) and T is a colon notation callable of the
 * form p1:..:pk(X1, .., Xm), for 1 ≤ k and 0 ≤ m.
 */
% sys_callable_colon(+-Callable, -+Term):
:- special(sys_callable_colon/2, 'SpecialNotation', 1).
:- set_predicate_property(sys_callable_colon/2, visible(public)).

/**
 * sys_indicator_colon(S, T):
 * The predicate succeeds when S is an indicator of the form
 * ‘pk-1%pk’/m and T is a colon notation indicator of the form
 * p1:..:pk/m, for 1 ≤ k and 0 ≤ m.
 */
% sys_indicator_colon(+-Indicator, -+Term):
:- special(sys_indicator_colon/2, 'SpecialNotation', 2).
:- set_predicate_property(sys_indicator_colon/2, visible(public)).
