/**
 * The predicates code_type/2 and char_type/2 can be used to classify
 * a character. The character classifier is configurable and plug-able.
 * To use a custom character classifier the predicates code_type/3
 * and char_type/3 can be used.
 *
 * Example:
 * ?- char_type(a, X).
 * X = lower
 * ?- char_type('A', X).
 * X = upper
 *
 * The default character classifier uses the Prolog ISO core standard
 * classification for the ASCII range plus the Jekejeke Prolog specific
 * extension for Unicode that extends the range to the Unicode range.
 * Codes outside of the range are classified as invalid.
 *
 * The predicates code_lower/2 and code_upper/2 can be used for case
 * conversion of code points. The predicates atom_lower/2 and atom_upper/2
 * can be used for case conversion of atoms.
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

:- package(library(jekpro/frequent/misc)).
:- use_package(foreign(jekpro/frequent/misc)).
:- use_package(foreign(matula/util/misc)).

:- module(text, []).

/**
 * char_type(C, N):
 * char_type(H, C, N):
 * The predicate succeeds for the name N of the Prolog classification
 * of the character C. The ternary predicate allows specifying a custom
 * classifier H. For a description of the classification names see the
 * API documentation.
 */
:- public char_type/2.
char_type(Char, Name) :-
   sys_get_iso_classifier(Handle),
   sys_char_type(Handle, Char, Type),
   sys_type_name(Type, Name).
:- public char_type/3.
char_type(Handle, Char, Name) :-
   sys_char_type(Handle, Char, Type),
   sys_type_name(Type, Name).

/**
 * code_type(C, N):
 * code_type(H, C, N):
 * The predicate succeeds for the name N of the Prolog classification
 * of the code point C. The ternary predicate allows specifying a custom
 * classifier H.
 */
:- public code_type/2.
code_type(CodePoint, Name) :-
   sys_get_iso_classifier(Handle),
   sys_code_type(Handle, CodePoint, Type),
   sys_type_name(Type, Name).
:- public code_type/3.
code_type(Handle, CodePoint, Name) :-
   sys_code_type(Handle, CodePoint, Type),
   sys_type_name(Type, Name).

:- private sys_type_name/2.
sys_type_name(0, space).
sys_type_name(1, layout).
sys_type_name(2, inval).
sys_type_name(3, solo).
sys_type_name(4, score).
sys_type_name(5, upper).
sys_type_name(6, lower).
sys_type_name(7, anum).
sys_type_name(8, digit).
sys_type_name(9, graph).

:- private sys_get_iso_classifier/1.
:- foreign(sys_get_iso_classifier/1, 'ForeignText', sysGetISOClassifier).

:- private sys_char_type/3.
:- foreign(sys_char_type/3, 'ForeignText', sysCharType('CodeType','String')).

:- private sys_code_type/3.
:- foreign(sys_code_type/3, 'ForeignText', sysCodeType('CodeType',int)).

/**
 * code_lower(C, D):
 * The predicate succeeds in D for the lower case of C.
 */
:- public code_lower/2.
:- foreign(code_lower/2, 'Character', toLowerCase(int)).

/**
 * code_upper(C, D):
 * The predicate succeeds in D for the upper case of C.
 */
:- public code_upper/2.
:- foreign(code_upper/2, 'Character', toUpperCase(int)).

/**
 * atom_lower(A, B):
 * The predicate succeeds in A for the lower case of B.
 */
:- public atom_lower/2.
:- virtual foreign(atom_lower/2,'String',toLowerCase).

/**
 * atom_upper(A, B):
 * The predicate succeeds in A for the upper case of B.
 */
:- public atom_upper/2.
:- virtual foreign(atom_upper/2,'String',toUpperCase).
