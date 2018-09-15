/**
 * Foreign predicates can be automatically generated when a Java class
 * is loaded. The module loader will proceed in that it will analyse the
 * Java class and automatically collect all the Java methods and Java
 * fields and generate foreign predicates for them. For Java methods
 * that were overloaded the module loader will generate branching code.
 *
 * Example:
 * ?- system/automatic:generated('String':valueOf/2).
 * % String.class
 * :- package(foreign(java/lang)).
 * :- module('String', []).
 * ..
 * :- public valueOf/2.
 * valueOf(A, B) :-
 *    sys_boolean(A), !,
 *    valueOf_var0(A, B).
 * valueOf(A, B) :-
 *    sys_char16(A), !,
 *    valueOf_var1(A, B).
 * ..
 *
 * The branching code uses a type check and then a cut. The type checks go
 * beyond what the core standard defines as type checks. Since Java uses
 * expression infered types and we have only value manifest types, we pursue
 * the strategy that we look at the magnitude of a value. This explains
 * for example the variety of test predicates such as sys_integer8/1 and
 * sys_integer16_and_not_integer8/1 defined here.
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

:- module(score, []).

/**
 * sys_boolean(X):
 * The predicate succeeds when X is an atom from the set {true, false}.
 */
:- public sys_boolean/1.
sys_boolean(true).
sys_boolean(false).

/**
 * sys_integer8(X):
 * The predicate succeeds when X is an integer and X is in the
 * range -2^7 to 2^7-1.
 */
:- public sys_integer8/1.
sys_integer8(X) :-
   integer(X),
   -128 =< X,
   X =< 127.

/**
 * sys_char16(X):
 * The predicate succeeds when X is a character and X is in the
 * range of 0 to 2^16-1.
 */
:- public sys_char16/1.
sys_char16(X) :-
   atom(X),
   atom_length(X, 1),
   atom_codes(X, [Y]),
   Y =< 65535.

/**
 * sys_integer16(X):
 * The predicate succeeds when X is an integer and X is in the
 * range -2^15 to 2^15-1.
 */
:- public sys_integer16/1.
sys_integer16(X) :-
   integer(X),
   -32768 =< X,
   X =< 32767.

/**
 * sys_integer32(X):
 * The predicate succeeds when X is an integer and X is in the
 * range -2^31 to 2^31-1.
 */
:- public sys_integer32/1.
sys_integer32(X) :-
   integer(X),
   -2147483648 =< X,
   X =< 2147483647.

/**
 * sys_integer64(X):
 * The predicate succeeds when X is an integer and X is in the
 * range -2^63 to 2^63-1.
 */
:- public sys_integer64/1.
sys_integer64(X) :-
   integer(X),
   -9223372036854775808 =< X,
   X =< -9223372036854775807.

/**
 * sys_integer32_or_float32(X):
 * The predicate succeeds when X is an 32-bit integer or a 32-bit float.
 */
:- public sys_integer32_or_float32/1.
sys_integer32_or_float32(X) :-
   sys_integer32(X), !.
sys_integer32_or_float32(X) :-
   float32(X).

/**
 * sys_integer64_or_float(X):
 * The predicate succeeds when X is an 64-bit integer or a float.
 */
:- public sys_integer64_or_float/1.
sys_integer64_or_float(X) :-
   sys_integer64(X), !.
sys_integer64_or_float(X) :-
   float(X).

/**
 * sys_integer16_and_not_integer8(X):
 * The predicate succeeds when X is an 16-bit integer but not an 8-bit integer.
 */
:- public sys_integer16_and_not_integer8/1.
sys_integer16_and_not_integer8(X) :-
   sys_integer8(X), !, fail.
sys_integer16_and_not_integer8(X) :-
   sys_integer16(X).

/**
 * sys_integer32_and_not_integer16(X):
 * The predicate succeeds when X is an 32-bit integer but not an 16-bit integer.
 */
:- public sys_integer32_and_not_integer16/1.
sys_integer32_and_not_integer16(X) :-
   sys_integer16(X), !, fail.
sys_integer32_and_not_integer16(X) :-
   sys_integer32(X).

/**
 * sys_integer64_and_not_integer32(X):
 * The predicate succeeds when X is an 64-bit integer but not an 32-bit integer.
 */
:- public sys_integer64_and_not_integer32/1.
sys_integer64_and_not_integer32(X) :-
   sys_integer32(X), !, fail.
sys_integer64_and_not_integer32(X) :-
   sys_integer64(X).

/**
 * sys_integer_and_not_integer64(X):
 * The predicate succeeds when X is an integer but not an 64-bit integer.
 */
:- public sys_integer_and_not_integer64/1.
sys_integer_and_not_integer64(X) :-
   sys_integer64(X), !, fail.
sys_integer_and_not_integer64(X) :-
   integer(X).

/**
 * sys_atom_or_type_of(C, X):
 * The predicate succeeds when X is an atom or an instance of C.
 */
:- public sys_atom_or_type_of/2.
sys_atom_or_type_of(_, X) :-
   atom(X), !.
sys_atom_or_type_of(C, X) :-
   sys_type_of(C, X).

/**
 * sys_type_of(C, X):
 * The predicate succeeds when X is an instance of C.
 */
:- public sys_type_of/2.
:- special(sys_type_of/2, 'SpecialScore', 0).
