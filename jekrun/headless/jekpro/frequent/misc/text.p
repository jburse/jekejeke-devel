/**
 * The predicates code_class/2 and char_class/2 can be used to classify
 * a character. The char-acter classifier is configurable and plug-able.
 * To use a custom character classifier the predi-cates code_class/3 and
 * char_class/3 can be used. The predicates code_digit/3 and char_digit/3
 * can be used to classify and decode digits in a given base.
 *
 * Examples:
 * ?- char_class(a, X).
 * X = lower
 * ?- char_class('A', X).
 * X = upper
 *
 * The default character classifier uses the Prolog ISO core standard
 * classification for the ASCII range plus the Jekejeke Prolog specific
 * extension for Unicode. The predicates code_lower/2 and code_upper/2
 * can be used for case conversion of code points. The predicates
 * downcase_atom/2 and upcase_atom/2 can be used for case conversion of atoms.
 *
 * Examples:
 * ?- pattern_match('foobarfoo','bar').
 * No
 * ?- pattern_match('foobarfoo','*bar*').
 * Yes
 *
 * We also provide predicates for pattern matching. The predicate
 * match/2 takes an atom and matches it against a pattern. The predicate
 * replace/4 takes a further pattern and produces a new atom. The pattern
 * language provided by us is inspired by the NEBIS library system.
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

:- package(library(jekpro/frequent/misc)).
:- use_package(foreign(jekpro/frequent/misc)).
:- use_package(foreign(matula/util/regex)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(library(matula/util/regex)).

:- module(text, []).

:- sys_load_resource(resource(scanner)).
:- sys_add_resource(resource(scanner)).

/******************************************************************/
/* Code Type Classification                                       */
/******************************************************************/

/**
 * char_class(C, N):
 * char_class(H, C, N):
 * The predicate succeeds for the name N of the Prolog classification
 * of the character C. The ternary predicate allows specifying a custom
 * classifier H. For a description of the classification names see the
 * API documentation.
 */
% code_class(+-Atom, -+Atom)
:- public char_class/2.
char_class(Char, Name) :-
   sys_get_iso_classifier(Handle),
   char_code(Char, CodePoint),
   sys_code_class(Handle, CodePoint, Class),
   sys_class_name(Class, Name).

% code_class(+Handle, +-Atom, -+Atom)
:- public char_class/3.
char_class(Handle, Char, Name) :-
   char_code(Char, CodePoint),
   sys_code_class(Handle, CodePoint, Class),
   sys_class_name(Class, Name).

/**
 * code_class(C, N):
 * code_class(H, C, N):
 * The predicate succeeds for the name N of the Prolog classification
 * of the code point C. The ternary predicate allows specifying a custom
 * classifier H.
 */
% code_class(+-Integer, -+Atom)
:- public code_class/2.
code_class(CodePoint, Name) :-
   sys_get_iso_classifier(Handle),
   sys_code_class(Handle, CodePoint, Class),
   sys_class_name(Class, Name).

% code_class(+Handle, +-Integer, -+Atom)
:- public code_class/3.
code_class(Handle, CodePoint, Name) :-
   sys_code_class(Handle, CodePoint, Class),
   sys_class_name(Class, Name).

:- private sys_class_name/2.
sys_class_name(0, blank).
sys_class_name(1, cntrl).
sys_class_name(2, inval).
sys_class_name(3, solo).
sys_class_name(4, score).
sys_class_name(5, upper).
sys_class_name(6, lower).
sys_class_name(7, other).
sys_class_name(8, digit).
sys_class_name(9, symbol).

:- private sys_get_iso_classifier/1.
:- foreign_getter(sys_get_iso_classifier/1, 'CodeType', 'ISO_CODETYPE').

:- private sys_code_class/3.
:- virtual sys_code_class/3.
:- foreign(sys_code_class/3, 'CodeType', classOf(int)).

/**
 * char_digit(C, R, V):
 * The predicate succeeds in V with the numerical value of
 * the digit C in the radix R.
 */
% char_digit(+Integer, +Integer, -Integer)
:- public char_digit/3.
char_digit(Char, Radix, Value) :-
   char_code(Char, CodePoint),
   sys_code_digit(CodePoint, Radix, W),
   W \== -1,
   Value = W.

/**
 * code_digit(C, R, V):
 * The predicate succeeds in V with the numerical value of
 * the digit C in the radix R.
 */
% code_digit(+Integer, +Integer, -Integer)
:- public code_digit/3.
code_digit(CodePoint, Radix, Value) :-
   sys_code_digit(CodePoint, Radix, W),
   W \== -1,
   Value = W.

:- private sys_code_digit/3.
:- foreign(sys_code_digit/3, 'Character', digit(int, int)).

/******************************************************************/
/* Upper/Lower Case Conversion                                    */
/******************************************************************/

/**
 * code_lower(C, D):
 * The predicate succeeds in D for the lower case of C.
 */
% code_lower(+Integer, -Integer)
:- public code_lower/2.
:- foreign(code_lower/2, 'Character', toLowerCase(int)).

/**
 * code_upper(C, D):
 * The predicate succeeds in D for the upper case of C.
 */
% code_upper(+Integer, -Integer)
:- public code_upper/2.
:- foreign(code_upper/2, 'Character', toUpperCase(int)).

/**
 * downcase_atom(A, B): [Prolog Commons Atom Utilities]
 * The predicate succeeds in B for the lower case of A.
 */
% downcase_atom(+Atom, -Atom)
:- public downcase_atom/2.
:- virtual downcase_atom/2.
:- foreign(downcase_atom/2, 'String', toLowerCase).

/**
 * upcase_atom(A, B): [Prolog Commons Atom Utilities]
 * The predicate succeeds in B for the upper case of A.
 */
% upcase_atom(+Atom, -Atom)
:- public upcase_atom/2.
:- virtual upcase_atom/2.
:- foreign(upcase_atom/2, 'String', toUpperCase).

/******************************************************************/
/* Matching & Replace                                             */
/******************************************************************/

/**
 * pattern_match(S, P):
 * pattern_match(S, P, O):
 * The predicate succeeds when the atom S matches the shell pattern P.
 * The ternary predicate allows specifying match options O. For a list
 * of match options see the API documentation.
 */
% pattern_match(+Atom, +Pattern)
:- public pattern_match/2.
pattern_match(S, P) :-
   pattern_compile(P, H),
   compiled_match(H, S).

% pattern_match(+Atom, +Pattern, +Options)
:- public pattern_match/3.
pattern_match(S, P, O) :-
   pattern_compile(P, O, H),
   compiled_match(H, S).

/**
 * pattern_replace(S, P, R, T):
 * pattern_replace(S, P, R, T, O):
 * The predicate succeeds when the atom S matches the shell pattern P,
 * and when replacing the matched pattern by R yields the atom T. The
 * quinary predicate allows specifying match and replaces options O.
 * For a list of match and replace options see the API documentation.
 */
% pattern_replace(+Atom, +Pattern, +Pattern, -Atom)
:- public pattern_replace/4.
pattern_replace(S, P, R, T) :-
   pattern_compile(P, H),
   pattern_compile(R, J),
   sys_replace_to(H, J),
   sys_pattern_replace(H, S, T).

% pattern_replace(+Atom, +Pattern, +Pattern, -Atom, +Options)
:- public pattern_replace/5.
pattern_replace(S, P, R, T, O) :-
   pattern_compile(P, O, H),
   pattern_compile(R, O, J),
   sys_replace_to(H, J),
   sys_pattern_replace(H, S, T).

/**
 * last_pattern_replace(S, P, R, T):
 * last_pattern_replacee(S, P, R, T, O):
 * These predicates work similar to the predicates replace/4 and
 * replace/5 except that they search backwards.
 */
% last_pattern_replace(+Atom, +Pattern, +Pattern, -Atom)
:- public last_pattern_replace/4.
last_pattern_replace(S, P, R, T) :-
   pattern_compile(P, H),
   pattern_compile(R, J),
   sys_replace_to(H, J),
   sys_pattern_last_replace(H, S, T).

% last_pattern_replace(+Atom, +Pattern, +Pattern, -Atom, +Options)
:- public last_pattern_replace/5.
last_pattern_replace(S, P, R, T, O) :-
   pattern_compile(P, O, H),
   pattern_compile(R, O, J),
   sys_replace_to(H, J),
   sys_pattern_last_replace(H, S, T).

:- private sys_replace_to/2.
:- virtual sys_replace_to/2.
:- foreign(sys_replace_to/2, 'AbstractPattern', replaceTo('AbstractPattern')).

:- private sys_pattern_replace/3.
:- virtual sys_pattern_replace/3.
:- foreign(sys_pattern_replace/3, 'AbstractPattern', patternReplace('String')).

:- private sys_pattern_last_replace/3.
:- virtual sys_pattern_last_replace/3.
:- foreign(sys_pattern_last_replace/3, 'AbstractPattern', patternLastReplace('String')).

/******************************************************************/
/* Compiled Patterns                                              */
/******************************************************************/

/**
 * pattern_compile(P, H):
 * pattern_compile(P, O, H):
 * The predicate succeeds in H with the compiled shell pattern P.
 * The ternary predicate allows specifying match options O. 
 */
% pattern_compile(+Atom, -Compiled)
:- public pattern_compile/2.
pattern_compile(P, H) :-
   sys_get_iso_compiler(C),
   sys_create_specimen(C, P, H).

% pattern_compile(+Atom, -Options, -Compiled)
:- public pattern_compile/3.
pattern_compile(P, O, H) :-
   sys_get_iso_compiler(C),
   sys_pattern_options(O, Q),
   sys_make_pattern(C, P, Q, H).

% sys_get_iso_compiler(-Compiler)
:- private sys_get_iso_compiler/1.
:- foreign_getter(sys_get_iso_compiler/1, 'CompilerSimple', 'ISO_COMPILERSIMPLE').

% sys_create_specimen(+Compiler, +Atom, -Compiled)
:- private sys_create_specimen/3.
:- foreign(sys_create_specimen/3, 'ForeignText',
      sysCreateSpecimen('Interpreter', 'AbstractCompiler', 'String')).

% sys_make_pattern(+Compiler, +Atom, +Integer, -Compiled)
:- private sys_make_pattern/4.
:- foreign(sys_make_pattern/4, 'ForeignText',
      sysMakePattern('Interpreter', 'AbstractCompiler', 'String', int)).

/**
 * compiled_match(H, S):
 * pattern_match(S, P, O):
 * The predicate succeeds when the atom S matches the compiled pattern H.
 */
% compiled_match(+Compiled, +Atom)
:- public compiled_match/2.
:- virtual compiled_match/2.
:- foreign(compiled_match/2, 'AbstractPattern', matchPattern('String')).

/******************************************************************/
/* Match Options                                                  */
/******************************************************************/

/**
 * sys_pattern_options(O, Q):
 * The predicate succeeds in Q for the value of the options O.
 */
% sys_pattern_options(+Options, -Integer)
:- private sys_pattern_options/2.
sys_pattern_options(O, Q) :-
   sys_get_match_whole(M),
   sys_pattern_options(O, M, Q).

/**
 * sys_pattern_options(O, P, Q):
 * The predicate succeeds in Q for the value of the
 * options O starting with the default value P.
 */
% sys_pattern_options(+Options, +Integer, -Integer)
:- private sys_pattern_options/3.
sys_pattern_options(V, _, _) :- var(V),
   throw(error(instantiation_error, _)).
sys_pattern_options([], P, P) :- !.
sys_pattern_options([X|Y], P, Q) :- !,
   sys_pattern_option(X, P, H),
   sys_pattern_options(Y, H, Q).
sys_pattern_options(L, _, _) :-
   throw(error(type_error(list, L), _)).

/**
 * sys_pattern_option(O, P, Q):
 * The predicate succeeds in Q for the value of the
 * option O starting with the default value P.
 */
% sys_pattern_option(+Option, +Integer, -Integer)
:- private sys_pattern_option/3.
sys_pattern_option(V, _, _) :- var(V),
   throw(error(instantiation_error, _)).
sys_pattern_option(boundary(O), P, Q) :- !,
   sys_option_boundary(O, P, Q).
sys_pattern_option(ignore_case(O), P, Q) :- !,
   sys_option_ignore_case(O, P, Q).
sys_pattern_option(style(O), P, Q) :- !,
   sys_option_style(O, P, Q).
sys_pattern_option(O, _, _) :-
   throw(error(type_error(option, O), _)).

/**
 * sys_option_boundary(O, P, Q):
 * The predicate succeeds in Q for the value of the
 * boundary option O starting with the default value P.
 */
% sys_option_boundary(+BoundaryOption, +Integer, -Integer)
:- private sys_option_boundary/3.
sys_option_boundary(V, _, _) :- var(V),
   throw(error(instantiation_error, _)).
sys_option_boundary(whole, P, Q) :- !,
   sys_get_match_boundary(M),
   sys_get_match_whole(N),
   Q is P/\ \M\/N.
sys_option_boundary(part, P, Q) :- !,
   sys_get_match_boundary(M),
   sys_get_match_part(N),
   Q is P/\ \M\/N.
sys_option_boundary(word, P, Q) :- !,
   sys_get_match_boundary(M),
   sys_get_match_word(N),
   Q is P/\ \M\/N.
sys_option_boundary(O, _, _) :-
   throw(error(type_error(option, boundary(O)), _)).

:- private sys_get_match_boundary/1.
:- foreign_getter(sys_get_match_boundary/1, 'AbstractSpecimen', 'MATCH_BDRY').

:- private sys_get_match_word/1.
:- foreign_getter(sys_get_match_word/1, 'AbstractSpecimen', 'MATCH_WORD').

:- private sys_get_match_whole/1.
:- foreign_getter(sys_get_match_whole/1, 'AbstractSpecimen', 'MATCH_WHLE').

:- private sys_get_match_part/1.
:- foreign_getter(sys_get_match_part/1, 'AbstractSpecimen', 'MATCH_PART').

/**
 * sys_option_ignore_case(O, P, Q):
 * The predicate succeeds in Q for the value of the
 * ignore case option O starting with the default value P.
 */
% sys_option_ignore_case(+IgnoreCaseFlag, +Integer, -Integer)
:- private sys_option_ignore_case/3.
sys_option_ignore_case(V, _, _) :- var(V),
   throw(error(instantiation_error, _)).
sys_option_ignore_case(true, P, Q) :- !,
   sys_get_match_ignore_case(M),
   Q is P\/M.
sys_option_ignore_case(false, P, Q) :- !,
   sys_get_match_ignore_case(M),
   Q is P/\ \M.
sys_option_ignore_case(O, _, _) :-
   throw(error(type_error(option, ignore_case(O)), _)).

:- private sys_get_match_ignore_case/1.
:- foreign_getter(sys_get_match_ignore_case/1, 'AbstractSpecimen', 'MATCH_IGCS').

/**
 * sys_option_style(O, P, Q):
 * The predicate succeeds in Q for the value of the
 * style option O starting with the default value P.
 */
% sys_option_style(+StyleOption, +Integer, -Integer)
:- private sys_option_style/3.
sys_option_style(V, _, _) :- var(V),
   throw(error(instantiation_error, _)).
sys_option_style(create, P, Q) :- !,
   sys_get_match_style(M),
   sys_get_match_create(N),
   Q is P/\ \M\/N.
sys_option_style(parse, P, Q) :- !,
   sys_get_match_style(M),
   sys_get_match_parse(N),
   Q is P/\ \M\/N.
sys_option_style(O, _, _) :-
   throw(error(type_error(option, style(O)), _)).

:- private sys_get_match_style/1.
:- foreign_getter(sys_get_match_style/1, 'AbstractSpecimen', 'MATCH_STLE').

:- private sys_get_match_create/1.
:- foreign_getter(sys_get_match_create/1, 'AbstractSpecimen', 'MATCH_CRTE').

:- private sys_get_match_parse/1.
:- foreign_getter(sys_get_match_parse/1, 'AbstractSpecimen', 'MATCH_PRSE').

