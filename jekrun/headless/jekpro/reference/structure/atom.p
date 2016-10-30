/**
 * Characters and codes are not genuine data types in Jekejeke Prolog.
 * Characters are simply atoms of code length one and codes are integer
 * values in the range 0 to 0x10FFFF. Atoms are internally represented
 * as sequences of 16-bit words. Codes in the range above 16-bit are
 * represented as surrogate pairs. It is permitted to have single
 * standing surrogates in an atom. It is also permitted to have
 * surrogates in escape sequences.
 *
 * Examples:
 * ?- char_code('\xDBFF\\xDFFF\',X).
 * X = 1114111
 * ?- char_code(X,1114111).
 * X = '\x10FFFF\'
 * ?- char_code('\x10FFFF\',X).
 * X = 1114111
 *
 * Besides the ISO core standard inspired atom related predicates we also
 * provide SWI-Prolog inspired atom related predicates. The predicate
 * atom_list_concat/3 allows concatenating and splitting atom lists to
 * and from atoms.
 *
 * Examples:
 * ?- atom_list_concat([a,b,c],'_',X).
 * X = a_b_c
 * ?- atom_list_concat(X,'_',a_b_c).
 * X = [a,b,c]
 *
 * The arguments of the below string predicates do not work with 16-bit
 * word units. The arguments are measured in code units. This results
 * in a certain performance penalty. For example the length of an atom
 * is not anymore a one shot operation, but instead the whole atom has
 * to be scanned to compute the length. Similar conversions apply
 * to offsets.
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

:- use_package(foreign(jekpro/reference/structure)).
:- use_package(foreign(jekpro/tools/call)).

:- module(user, []).

/****************************************************************/
/* String Ops                                                   */
/****************************************************************/

/**
 * atom_length(X, Y): [ISO 8.16.1]
 * The predicate succeeds when Y is the length of the atom X.
 */
% atom_length(+Atom, -Integer)
:- public atom_length/2.
atom_length(Str, Len) :-
   sys_atom_word_len(Str, Count),
   sys_atom_word_count(Str, 0, Count, Len).

/**
 * atom_concat(X, Y, Z): [ISO 8.16.2]
 * The predicate succeeds whenever the atom Z is the concatenation
 * of the atom X and the atom Y.
 */
% atom_concat(+-Atom, +-Atom, -+Atom)
:- public atom_concat/3.
atom_concat(Str1, Str2, Str) :-
   var(Str1),
   var(Str2), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_pos(Str, 0, Count, Pos),
   sys_atom_word_substring(Str, 0, Pos, Str1),
   sys_atom_word_substring(Str, Pos, Count, Str2).
atom_concat(Str1, Str2, Str) :-
   var(Str2), !,
   sys_atom_word_len(Str1, Count1),
   sys_atom_word_match(Str1, 0, Str, 0, Count1),
   sys_atom_word_len(Str, Count),
   sys_atom_word_substring(Str, Count1, Count, Str2).
atom_concat(Str1, Str2, Str) :-
   var(Str1), !,
   sys_atom_word_len(Str2, Count2),
   sys_atom_word_len(Str, Count),
   Pos is Count-Count2,
   sys_atom_word_match(Str2, 0, Str, Pos, Count2),
   sys_atom_word_substring(Str, 0, Pos, Str1).
atom_concat(Str1, Str2, Str) :-
   sys_atom_concat(Str1, Str2, Str).

/**
 * sub_atom(X, Y, Z, U):
 * sub_atom(X, Y, Z, T, U): [ISO 8.16.3]
 * The predicate succeeds whenever the atom U is the sub atom of
 * the atom X starting at position Y with length Z and ending T
 * characters before.
 */
% sub_atom(+Atom, +-Integer, +-Integer, -+Atom)
:- public sub_atom/4.
sub_atom(Str, Off, Len, Sub) :-
   var(Off),
   var(Len),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_pos(Str, 0, Count, Pos2),
   sys_atom_word_count(Str, 0, Pos2, Off),
   sys_atom_word_pos(Str, Pos2, Count, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   sys_atom_word_count(Str, Pos2, Pos, Len).
sub_atom(Str, Off, Len, Sub) :-
   var(Off),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Len2 is -Len,
   sys_atom_word_offset(Str, Count, Len2, Count2),
   sys_atom_word_pos(Str, 0, Count2, Pos),
   sys_atom_word_offset(Str, Pos, Len, Pos2),
   sys_atom_word_substring(Str, Pos, Pos2, Sub),
   sys_atom_word_count(Str, 0, Pos, Off).
sub_atom(Str, Off, Len, Sub) :-
   var(Off), !,
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   sys_atom_word_len(Str, Count2),
   Count is Count2-Count1,
   sys_atom_word_pos(Str, 0, Count, Pos),
   sys_atom_word_match(Str, Pos, Sub, 0, Count1),
   sys_atom_word_count(Str, 0, Pos, Off).
sub_atom(Str, Off, Len, Sub) :-
   var(Len),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_offset(Str, 0, Off, Pos2),
   sys_atom_word_pos(Str, Pos2, Count, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   sys_atom_word_count(Str, Pos2, Pos, Len).
sub_atom(Str, Off, Len, Sub) :-
   var(Sub), !,
   sys_atom_word_offset(Str, 0, Off, Pos1),
   sys_atom_word_offset(Str, Pos1, Len, Pos2),
   sys_atom_word_substring(Str, Pos1, Pos2, Sub).
sub_atom(Str, Off, Len, Sub) :-
   sys_atom_word_len(Sub, Count),
   sys_atom_word_count(Sub, 0, Count, Len),
   sys_atom_word_offset(Str, 0, Off, Pos),
   sys_atom_word_match(Str, Pos, Sub, 0, Count).

% sub_atom(+Atom, +-Integer, +-Integer, +-Integer, -+Atom)
:- public sub_atom/5.
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Len),
   var(Off2),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_pos(Str, 0, Count, Pos2),
   sys_atom_word_count(Str, 0, Pos2, Off),
   sys_atom_word_pos(Str, Pos2, Count, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   sys_atom_word_count(Str, Pos2, Pos, Len),
   sys_atom_word_count(Str, Pos, Count, Off2).
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Off2),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Len2 is -Len,
   sys_atom_word_offset(Str, Count, Len2, Count2),
   sys_atom_word_pos(Str, 0, Count2, Pos),
   sys_atom_word_offset(Str, Pos, Len, Pos2),
   sys_atom_word_substring(Str, Pos, Pos2, Sub),
   sys_atom_word_count(Str, 0, Pos, Off),
   sys_atom_word_count(Str, Pos2, Count, Off2).
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Off2), !,
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   sys_atom_word_len(Str, Count2),
   Count is Count2-Count1,
   sys_atom_word_pos(Str, 0, Count, Pos),
   sys_atom_word_match(Str, Pos, Sub, 0, Count1),
   sys_atom_word_count(Str, 0, Pos, Off),
   Pos1 is Pos+Count1,
   sys_atom_word_count(Str, Pos1, Count2, Off2).
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Len),
   var(Sub), !,                                             % not in sub_atom/4.
   sys_atom_word_len(Str, Count),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos2),
   sys_atom_word_pos(Str, 0, Pos2, Pos),
   sys_atom_word_substring(Str, Pos, Pos2, Sub),
   sys_atom_word_count(Str, Pos, Pos2, Len),
   sys_atom_word_count(Str, 0, Pos, Off).
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Sub), !,                                             % not in sub_atom/4.
   sys_atom_word_len(Str, Count),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos2),
   Len1 is -Len,
   sys_atom_word_offset(Str, Pos2, Len1, Pos1),
   sys_atom_word_substring(Str, Pos1, Pos2, Sub),
   sys_atom_word_count(Str, 0, Pos1, Off).
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off), !,                                             % not in sub_atom/4.
   sys_atom_word_len(Sub, Count),
   sys_atom_word_count(Sub, 0, Count, Len),
   sys_atom_word_len(Str, Count1),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count1, Off1, Pos),
   Pos1 is Pos-Count,
   sys_atom_word_match(Str, Pos1, Sub, 0, Count),
   sys_atom_word_count(Str, 0, Pos1, Off).
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Len),
   var(Off2),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_offset(Str, 0, Off, Pos2),
   sys_atom_word_pos(Str, Pos2, Count, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   sys_atom_word_count(Str, Pos2, Pos, Len),
   sys_atom_word_count(Str, Pos, Count, Off2).
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off2),
   var(Sub), !,
   sys_atom_word_offset(Str, 0, Off, Pos1),
   sys_atom_word_offset(Str, Pos1, Len, Pos2),
   sys_atom_word_substring(Str, Pos1, Pos2, Sub),
   sys_atom_word_len(Str, Count),
   sys_atom_word_count(Str, Pos2, Count, Off2).
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Len),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_offset(Str, 0, Off, Pos1),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos2),
   sys_atom_word_substring(Str, Pos1, Pos2, Sub),
   sys_atom_word_count(Str, Pos1, Pos2, Len).
sub_atom(Str, Off, Len, Off2, Sub) :-
   sys_atom_word_len(Sub, Count),
   sys_atom_word_count(Sub, 0, Count, Len),
   sys_atom_word_offset(Str, 0, Off, Pos),
   sys_atom_word_match(Str, Pos, Sub, 0, Count),
   Pos1 is Pos+Count,
   sys_atom_word_len(Str, Count1),
   sys_atom_word_count(Str, Pos1, Count1, Off2).

/****************************************************************/
/* Codes & Chars Conversion                                     */
/****************************************************************/

/**
 * atom_chars(X, Y): [ISO 8.16.4]
 * If Y is ground and Y is a character list then the predicate
 * succeeds when X unifies with the corresponding atom. If Y is
 * not ground and X is a atom then the predicate succeeds when Y
 * unifies with the corresponding character list.
 */
% atom_chars(+-Atom, -+Chars)
:- public atom_chars/2.
atom_chars(Atom, Chars) :-
   ground(Chars), !,
   sys_list_to_atom(Chars, 0, Atom).
atom_chars(Atom, Chars) :-
   sys_atom_to_list(Atom, 0, Chars).

/**
 * atom_codes(X, Y): [ISO 8.16.5]
 * If Y is ground and Y is a code list then the predicate succeeds
 * when X unifies with the corresponding atom. If Y is not ground
 * and X is a atom then the predicate succeeds when Y unifies with
 * the corresponding code list.
 */
% atom_codes(+-Atom, -+Codes)
:- public atom_codes/2.
atom_codes(Atom, Codes) :-
   ground(Codes), !,
   sys_list_to_atom(Codes, 1, Atom).
atom_codes(Atom, Codes) :-
   sys_atom_to_list(Atom, 1, Codes).

:- private sys_atom_to_list/3.
:- foreign(sys_atom_to_list/3, 'ForeignAtom',
      sysAtomToList('String',int)).

:- private sys_list_to_atom/3.
:- foreign(sys_list_to_atom/3, 'ForeignAtom',
      sysListToAtom('Object',int)).

/**
 * char_code(X, Y): [ISO 8.16.6]
 * If Y is ground and if Y is a code then the predicate succeeds
 * when X unifies with the corresponding character. Otherwise if X
 * is a character then the predicate succeeds when Y unifies with
 * the corresponding code.
 */
% char_code(+-Char, -+Code)
:- public char_code/2.
char_code(Char, Code) :-
   var(Char), !,
   sys_code_to_char(Code, Char).
char_code(Char, Code) :-
   sys_char_to_code(Char, Code).

:- private sys_char_to_code/2.
:- foreign(sys_char_to_code/2, 'ForeignAtom',
      sysCharToCode('String')).

:- private sys_code_to_char/2.
:- foreign(sys_code_to_char/2, 'ForeignAtom',
      sysCodeToChar(int)).

/**
 * number_chars(X, Y): [ISO 8.16.7]
 * If Y is ground and Y is a character list then the predicate
 * succeeds when X unifies with the corresponding number. If Y
 * is not ground and X is a number then the predicate succeeds
 * when Y unifies with the corresponding character list.
 */
% number_chars(+-Number, -+Chars)
:- public number_chars/2.
number_chars(Number, Chars) :-
   ground(Chars), !,
   sys_list_to_atom(Chars, 0, Atom),
   sys_atom_to_number(Atom, Number).
number_chars(Number, Chars) :-
   sys_number_to_atom(Number, Atom),
   sys_atom_to_list(Atom, 0, Chars).

/**
 * number_codes(X, Y): [ISO 8.16.8]
 * If Y is ground and Y is a code list then the predicate succeeds
 * when X unifies with the corresponding number. If Y is not ground
 * and X is a number then the predicate suc-ceeds when Y unifies
 * with the corresponding code list.
 */
% number_codes(+-Number, -+Codes)
:- public number_codes/2.
number_codes(Number, Codes) :-
   ground(Codes), !,
   sys_list_to_atom(Codes, 1, Atom),
   sys_atom_to_number(Atom, Number).
number_codes(Number, Codes) :-
   sys_number_to_atom(Number, Atom),
   sys_atom_to_list(Atom, 1, Codes).

:- private sys_atom_to_number/2.
:- foreign(sys_atom_to_number/2, 'ForeignAtom',
      sysAtomToNumber('Interpreter','CallOut','String')).

:- private sys_number_to_atom/2.
:- foreign(sys_number_to_atom/2, 'ForeignAtom',
      sysNumberToAtom('Number')).

/****************************************************************/
/* 16-bit Word Helpers                                          */
/****************************************************************/

:- private sys_atom_word_len/2.
:- virtual sys_atom_word_len/2.
:- foreign(sys_atom_word_len/2, 'String', length).

:- private sys_atom_word_substring/4.
:- virtual sys_atom_word_substring/4.
:- foreign(sys_atom_word_substring/4, 'String', substring(int,int)).

:- private sys_atom_word_count/4.
:- virtual sys_atom_word_count/4.
:- foreign(sys_atom_word_count/4, 'String', codePointCount(int,int)).

:- private sys_atom_word_pos/4.
:- foreign(sys_atom_word_pos/4, 'ForeignAtom',
      sysAtomWordPos('CallOut','String',int,int)).

:- private sys_atom_word_offset/4.
:- virtual sys_atom_word_offset/4.
:- foreign(sys_atom_word_offset/4, 'String', offsetByCodePoints(int,int)).

:- private sys_atom_word_match/5.
:- virtual sys_atom_word_match/5.
:- foreign(sys_atom_word_match/5, 'String', regionMatches(int,'String',int,int)).

/****************************************************************/
/* SWI-Prolog Inspired                                          */
/****************************************************************/

/**
 * atom_list_concat(L, S, R):
 * If L is ground the predicate succeeds when R unifies with the
 * concatenation of each atom from the list L separated by the
 * atom S. Otherwise the predicate splits the atom R into a list L
 * of atoms that are separated by the atom S.
 */
% atom_list_concat(+-List, +Atom, -+Atom)
:- public atom_list_concat/3.
atom_list_concat(L, S, R) :-
   ground(L), !,
   atom_list_concat1(L, S, R).
atom_list_concat(L, S, R) :-
   atom_list_concat2(R, S, L).

% atom_list_concat1(+List, +Atom, -Atom)
:- private atom_list_concat1/3.
atom_list_concat1([X], _, X) :- !.
atom_list_concat1([X,Y|Z], S, R) :-
   atom_list_concat1([Y|Z], S, H),
   atom_concat(S, H, J),
   atom_concat(X, J, R).

% atom_list_concat2(+Atom, +Atom, -List)
:- private atom_list_concat2/3.
atom_list_concat2(R, S, [X|L]) :-
   sub_atom(R, Before, _, After, S), !,
   sub_atom(R, 0, Before, _, X),
   sub_atom(R, _, After, 0, H),
   atom_list_concat2(H, S, L).
atom_list_concat2(R, _, [R]).

/****************************************************************/
/* String Reverse Ops                                           */
/****************************************************************/

/**
 * last_atom_concat(X, Y, Z):
 * The predicate succeeds whenever the atom Z is the concatenation
 * of the atom X and the atom Y. Works like the ISO predicate
 * atom_concat/3 except that non-deterministic results are enumerated
 * in reverse order.
 */
% last_atom_concat(+-Atom, +-Atom, -+Atom)
:- public last_atom_concat/3.
last_atom_concat(Str1, Str2, Str) :-
   var(Str1),
   var(Str2), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_pos(Str, Count, 0, Pos),
   sys_atom_word_substring(Str, 0, Pos, Str1),
   sys_atom_word_substring(Str, Pos, Count, Str2).
last_atom_concat(Str1, Str2, Str) :-
   var(Str2), !,
   sys_atom_word_len(Str1, Count1),
   sys_atom_word_match(Str1, 0, Str, 0, Count1),
   sys_atom_word_len(Str, Count),
   sys_atom_word_substring(Str, Count1, Count, Str2).
last_atom_concat(Str1, Str2, Str) :-
   var(Str1), !,
   sys_atom_word_len(Str2, Count2),
   sys_atom_word_len(Str, Count),
   Pos is Count-Count2,
   sys_atom_word_match(Str2, 0, Str, Pos, Count2),
   sys_atom_word_substring(Str, 0, Pos, Str1).
last_atom_concat(Str1, Str2, Str) :-
   sys_atom_concat(Str1, Str2, Str).

/**
 * last_sub_atom(X, Z, T, U):
 * last_sub_atom(X, Y, Z, T, U):
 * The predicate succeeds whenever the atom U is the sub atom of
 * the atom X starting at position Y with length Z and ending T
 * characters before. Works like the ISO predicate sub_atom/5
 * except that non-deterministic results are enumerated in
 * reverse order.
 */
% last_sub_atom(+Atom, +-Integer, +-Integer, -+Atom)
:- public last_sub_atom/4.
last_sub_atom(Str, Len, Off2, Sub) :-
   var(Len),
   var(Off2),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_pos(Str, Count, 0, Pos2),
   sys_atom_word_pos(Str, Count, Pos2, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   sys_atom_word_count(Str, Pos2, Pos, Len),
   sys_atom_word_count(Str, Pos, Count, Off2).
last_sub_atom(Str, Len, Off2, Sub) :-
   var(Off2),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Len2 is -Len,
   sys_atom_word_offset(Str, Count, Len2, Count2),
   sys_atom_word_pos(Str, Count2, 0, Pos),
   sys_atom_word_offset(Str, Pos, Len, Pos2),
   sys_atom_word_substring(Str, Pos, Pos2, Sub),
   sys_atom_word_count(Str, Pos2, Count, Off2).
last_sub_atom(Str, Len, Off2, Sub) :-
   var(Off2), !,
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   sys_atom_word_len(Str, Count2),
   Count is Count2-Count1,
   sys_atom_word_pos(Str, Count, 0, Pos),
   sys_atom_word_match(Str, Pos, Sub, 0, Count1),
   Pos1 is Pos+Count1,
   sys_atom_word_count(Str, Pos1, Count2, Off2).
last_sub_atom(Str, Len, Off2, Sub) :-
   var(Len),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos2),
   sys_atom_word_pos(Str, Pos2, 0, Pos),
   sys_atom_word_substring(Str, Pos, Pos2, Sub),
   sys_atom_word_count(Str, Pos, Pos2, Len).
last_sub_atom(Str, Len, Off2, Sub) :-
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos2),
   Len1 is -Len,
   sys_atom_word_offset(Str, Pos2, Len1, Pos1),
   sys_atom_word_substring(Str, Pos1, Pos2, Sub).
last_sub_atom(Str, Len, Off2, Sub) :-
   sys_atom_word_len(Sub, Count),
   sys_atom_word_count(Sub, 0, Count, Len),
   sys_atom_word_len(Str, Count1),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count1, Off1, Pos),
   Pos1 is Pos-Count,
   sys_atom_word_match(Str, Pos1, Sub, 0, Count).

% last_sub_atom(+Atom, +-Integer, +-Integer, +-Integer, -+Atom)
:- public last_sub_atom/5.
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Len),
   var(Off2),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_pos(Str, Count, 0, Pos2),
   sys_atom_word_count(Str, 0, Pos2, Off),
   sys_atom_word_pos(Str, Count, Pos2, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   sys_atom_word_count(Str, Pos2, Pos, Len),
   sys_atom_word_count(Str, Pos, Count, Off2).
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Off2),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Len2 is -Len,
   sys_atom_word_offset(Str, Count, Len2, Count2),
   sys_atom_word_pos(Str, Count2, 0, Pos),
   sys_atom_word_offset(Str, Pos, Len, Pos2),
   sys_atom_word_substring(Str, Pos, Pos2, Sub),
   sys_atom_word_count(Str, 0, Pos, Off),
   sys_atom_word_count(Str, Pos2, Count, Off2).
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Off2), !,
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   sys_atom_word_len(Str, Count2),
   Count is Count2-Count1,
   sys_atom_word_pos(Str, Count, 0, Pos),
   sys_atom_word_match(Str, Pos, Sub, 0, Count1),
   sys_atom_word_count(Str, 0, Pos, Off),
   Pos1 is Pos+Count1,
   sys_atom_word_count(Str, Pos1, Count2, Off2).
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Len),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos2),
   sys_atom_word_pos(Str, Pos2, 0, Pos),
   sys_atom_word_substring(Str, Pos, Pos2, Sub),
   sys_atom_word_count(Str, Pos, Pos2, Len),
   sys_atom_word_count(Str, 0, Pos, Off).
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos2),
   Len1 is -Len,
   sys_atom_word_offset(Str, Pos2, Len1, Pos1),
   sys_atom_word_substring(Str, Pos1, Pos2, Sub),
   sys_atom_word_count(Str, 0, Pos1, Off).
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off), !,
   sys_atom_word_len(Sub, Count),
   sys_atom_word_count(Sub, 0, Count, Len),
   sys_atom_word_len(Str, Count1),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count1, Off1, Pos),
   Pos1 is Pos-Count,
   sys_atom_word_match(Str, Pos1, Sub, 0, Count),
   sys_atom_word_count(Str, 0, Pos1, Off).
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Len),
   var(Off2),
   var(Sub), !,                                             % not in last_sub_atom/4
   sys_atom_word_len(Str, Count),
   sys_atom_word_offset(Str, 0, Off, Pos2),
   sys_atom_word_pos(Str, Count, Pos2, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   sys_atom_word_count(Str, Pos2, Pos, Len),
   sys_atom_word_count(Str, Pos, Count, Off2).
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off2),
   var(Sub), !,                                             % not in last_sub_atom/4
   sys_atom_word_offset(Str, 0, Off, Pos1),
   sys_atom_word_offset(Str, Pos1, Len, Pos2),
   sys_atom_word_substring(Str, Pos1, Pos2, Sub),
   sys_atom_word_len(Str, Count),
   sys_atom_word_count(Str, Pos2, Count, Off2).
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Len),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_offset(Str, 0, Off, Pos1),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos2),
   sys_atom_word_substring(Str, Pos1, Pos2, Sub),
   sys_atom_word_count(Str, Pos1, Pos2, Len).
last_sub_atom(Str, Off, Len, Off2, Sub) :-        % not in last_sub_atom/4
   sys_atom_word_len(Sub, Count),
   sys_atom_word_count(Sub, 0, Count, Len),
   sys_atom_word_offset(Str, 0, Off, Pos),
   sys_atom_word_match(Str, Pos, Sub, 0, Count),
   Pos1 is Pos+Count,
   sys_atom_word_len(Str, Count1),
   sys_atom_word_count(Str, Pos1, Count1, Off2).
