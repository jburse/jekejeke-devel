/**
 * For debugging purpose it might be necessary to have direct access
 * to our Prolog module notation. Our Prolog module notations are
 * ordinary Prolog terms that are converted to interpreter objects.
 * The slash notation combines a package name and a module name
 * into a structured module name. The colon notation separates combines
 * module name and a predi-cate name into a qualified predicate name.
 *
 * Examples:
 * ?- sys_atom_slash(X, foo/bar).
 * X = 'user$foo$bar'
 * ?- sys_atom_slash(X, basic/lists).
 * X = 'jekpro.frequent.basic.lists'
 *
 * The predicate sys_atom_slash/2 can be used to explicitly invoke
 * the slash (/)/2 compound notation conversion, yielding a period (.)
 * respectively dollar ($) characters in the resulting atom. The
 * notation can be used to denote Prolog text modules and Java
 * Classes. We additionally support for the {}/1 compound notation
 * conversion as well, which can be used to denote Java Array
 * Classes and yielding ([]) characters.
 *
 * Examples:
 * ?- sys_callable_colon(X, basic/lists:member(A,B)).
 * X = 'jekpro.frequent.basic.lists\bmember'(A,B)
 * ?- sys_indicator_colon(X, basic/lists:member/2).
 * X = 'jekpro.frequent.basic.lists\bmember'/2
 *
 * The predicates sys_callable_colon/2 can be used to explicitly invoke
 * the colon (:)/2 and double colon (::)/2 notation conversion for a
 * callable. The double colon notation combines the receiver module
 * name by additionally prepending the receiver itself to the callable
 * similar to the Python dynamic invocation. The predicate
 * sys_indicator_colon/2 can be used to explicitly invoke to colon
 * notation conversion for a predicate indicator.
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

:- module(notation, []).

/**
 * sys_atom_slash(S, T):
 * The predicate succeeds when S is an atom of the form ‘s1. .. .sn’
 * and T is a slash notation atom of the form s1/../sn, for 1 ≤ n. Besides
 * the (/)/2 operator the {}/1 operator is supported as well.
 */
% sys_atom_slash(+-Atom, -+Term)
:- public sys_atom_slash/2.
sys_atom_slash(A, T) :-
   var(A), !,
   sys_slash_to_module(T, A).
sys_atom_slash(A, T) :-
   sys_module_to_slash(A, T).

:- private sys_slash_to_module/2.
:- special(sys_slash_to_module/2, 'SpecialNotation', 0).

:- private sys_module_to_slash/2.
:- special(sys_module_to_slash/2, 'SpecialNotation', 1).

/**
 * sys_callable_colon(S, T):
 * The predicate succeeds when S is a callable of the form
 * ‘pk-1%pk’(X1, .., Xm) and T is a colon notation callable of the
 * form p1:..:pk(X1, .., Xm), for 1 ≤ k and 0 ≤ m.
 */
% sys_callable_colon(+-Callable, -+Term):
:- public sys_callable_colon/2.
sys_callable_colon(A, T) :-
   var(A), !,
   sys_colon_to_callable(T, A).
sys_callable_colon(A, T) :-
   sys_callable_to_colon(A, T).

:- private sys_colon_to_callable/2.
:- special(sys_colon_to_callable/2, 'SpecialNotation', 2).

:- private sys_callable_to_colon/2.
:- special(sys_callable_to_colon/2, 'SpecialNotation', 3).

/**
 * sys_indicator_colon(S, T):
 * The predicate succeeds when S is an indicator of the form
 * ‘pk-1%pk’/m and T is a colon notation indicator of the form
 * p1:..:pk/m, for 1 ≤ k and 0 ≤ m.
 */
% sys_indicator_colon(+-Indicator, -+Term):
:- public sys_indicator_colon/2.
sys_indicator_colon(A, T) :-
   var(A), !,
   sys_colon_to_indicator(T, A).
sys_indicator_colon(A, T) :-
   sys_indicator_to_colon(A, T).

:- private sys_colon_to_indicator/2.
:- special(sys_colon_to_indicator/2, 'SpecialNotation', 4).

:- private sys_indicator_to_colon/2.
:- special(sys_indicator_to_colon/2, 'SpecialNotation', 5).
