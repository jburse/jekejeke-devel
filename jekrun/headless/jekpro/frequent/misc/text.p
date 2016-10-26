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
:- use_package(foreign(matula/util/regex)).

:- module(text, []).

/******************************************************************/
/* Code Type Classification                                       */
/******************************************************************/

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
   char_code(Char, Code),
   sys_code_type(Handle, Code, Type),
   sys_type_name(Type, Name).
:- public char_type/3.
char_type(Handle, Char, Name) :-
   char_code(Char, Code),
   sys_code_type(Handle, Code, Type),
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
sys_type_name(0, white).
sys_type_name(1, control).
sys_type_name(2, inval).
sys_type_name(3, solo).
sys_type_name(4, score).
sys_type_name(5, upper).
sys_type_name(6, lower).
sys_type_name(7, other).
sys_type_name(8, digit).
sys_type_name(9, graph).

:- private sys_get_iso_classifier/1.
:- foreign_getter(sys_get_iso_classifier/1, 'CodeType', 'ISO_CODETYPE').

:- private sys_code_type/3.
:- virtual sys_code_type/3.
:- foreign(sys_code_type/3, 'CodeType', classOf(int)).

/******************************************************************/
/* Upper/Lower Case Conversion                                    */
/******************************************************************/

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
:- virtual atom_lower/2.
:- foreign(atom_lower/2, 'String', toLowerCase).

/**
 * atom_upper(A, B):
 * The predicate succeeds in A for the upper case of B.
 */
:- public atom_upper/2.
:- virtual atom_upper/2.
:- foreign(atom_upper/2, 'String', toUpperCase).

/******************************************************************/
/* Shell Pattern Matching                                         */
/******************************************************************/

/**
 * match(S, P):
 * match(S, P, O):
 * The predicate succeeds when the atom S matches the shell pattern P.
 * The ternary predicate allows specifying match options O. For a list
 * of match options see the API documentation.
 */
:- public match/2.
match(S, P) :-
   sys_get_iso_compiler(C),
   sys_create_specimen(C, P, H),
   sys_match_pattern(H, 0, S, 0).

:- public match/3.
match(S, P, O) :-
   sys_get_iso_compiler(C),
   sys_pattern_options(O, Q),
   sys_create_specimen(C, P, Q, H),
   sys_match_pattern(H, 0, S, 0).

/**
 * replace(S, P, R, T):
 * replace(S, P, R, T, O):
 * The predicate succeeds when the atom S matches the shell pattern P,
 * and when replacing the matched pattern by R yields the atom T. The
 * quinary predicate allows specifying match and replace options O.
 * for a list of match and replace options see the API documentation.
 */
:- public replace/4.
replace(S, P, R, T) :-
   sys_get_iso_compiler(C),
   sys_create_specimen(C, P, H),
   sys_match_pattern(H, 0, S, 0),
   sys_set_target(H, R),
   sys_pattern_replace(H, true, T).

:- public replace/5.
replace(S, P, R, T, O) :-
   sys_get_iso_compiler(C),
   sys_pattern_options(O, Q),
   sys_create_specimen(C, P, Q, H),
   sys_match_pattern(H, 0, S, 0),
   sys_set_target(H, R),
   sys_pattern_replace(H, true, T).

:- private sys_pattern_options/2.
sys_pattern_options(O, Q) :-
   sys_get_match_sensitive(M),
   sys_pattern_options(O, M, Q).

:- private sys_pattern_options/3.
sys_pattern_options(V, _, _) :-
   var(V),
   throw(error(instantiation_error,_)).
sys_pattern_options([], P, P) :- !.
sys_pattern_options([X|Y], P, Q) :- !,
   sys_pattern_option(X, P, H),
   sys_pattern_options(Y, H, Q).
sys_pattern_options(L, _, _) :-
   throw(error(type_error(list,L),_)).

:- private sys_pattern_option/3.
sys_pattern_option(V, _, _) :-
   var(V),
   throw(error(instantiation_error,_)).
sys_pattern_option(ignore_case(V), _, _) :-
   var(V),
   throw(error(instantiation_error,_)).
sys_pattern_option(ignore_case(true), P, Q) :- !,
   sys_get_match_sensitive(M),
   Q is P/\ \M.
sys_pattern_option(ignore_case(false), P, Q) :- !,
   sys_get_match_sensitive(M),
   Q is P\/M.
sys_pattern_option(O, _, _) :-
   throw(error(type_error(option,O),_)).

:- private sys_get_iso_compiler/1.
:- foreign_getter(sys_get_iso_compiler/1, 'CompilerSimple', 'ISO_COMPILERSIMPLE').

:- private sys_create_specimen/3.
:- virtual sys_create_specimen/3.
:- foreign(sys_create_specimen/3, 'AbstractCompiler', createSpecimen('String')).

:- private sys_create_specimen/4.
:- virtual sys_create_specimen/4.
:- foreign(sys_create_specimen/4, 'AbstractCompiler', createSpecimen('String',int)).

:- private sys_match_pattern/4.
:- virtual sys_match_pattern/4.
:- foreign(sys_match_pattern/4, 'AbstractPattern', matchPattern(int,'String',int)).

:- private sys_set_target/2.
:- virtual sys_set_target/2.
:- foreign(sys_set_target/2, 'AbstractSpecimen', setTarget('String')).

:- private sys_pattern_replace/3.
:- virtual sys_pattern_replace/3.
:- foreign(sys_pattern_replace/3, 'AbstractPattern', patternReplace(boolean)).

:- private sys_get_match_sensitive/1.
:- foreign_getter(sys_get_match_sensitive/1, 'AbstractSpecimen', 'MATCH_SENSITIV').


