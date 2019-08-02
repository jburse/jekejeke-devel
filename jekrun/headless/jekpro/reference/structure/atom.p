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
 * provide Prolog commons inspired atom related predicates. The predicate
 * atom_split/3 allows concatenating and splitting atom lists to and from atoms.
 * The predicate atom_number/2 allows converting between atoms and numbers.
 *
 * Examples:
 * ?- atom_split(X,'_', [a,b,c]).
 * X = a_b_c
 * ?- atom_split(a_b_c, '_', X).
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- use_package(foreign(jekpro/reference/structure)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/tools/term)).

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
   sys_atom_word_pos(Str, 0, 0, Count, Off, Pos2),
   sys_atom_word_pos(Str, Off, Pos2, Count, Help, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   Len is Help-Off.
sub_atom(Str, Off, Len, Sub) :-
   var(Off),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Len2 is -Len,
   sys_atom_word_offset(Str, Count, Len2, Count2),
   sys_atom_word_pos(Str, 0, 0, Count2, Off, Pos2),
   sys_atom_word_offset(Str, Pos2, Len, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub).
sub_atom(Str, Off, Len, Sub) :-
   var(Off), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   Len2 is -Len,
   sys_atom_word_offset(Str, Count, Len2, Count2),
   sys_atom_word_pos(Str, 0, 0, Count2, Off, Pos2),
   sys_atom_word_match(Str, Pos2, Sub, 0, Count1).
sub_atom(Str, Off, Len, Sub) :-
   var(Len),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_offset(Str, 0, Off, Pos2),
   sys_atom_word_pos(Str, Off, Pos2, Count, Help, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   Len is Help-Off.
sub_atom(Str, Off, Len, Sub) :-
   var(Sub), !,
   sys_atom_word_offset(Str, 0, Off, Pos2),
   sys_atom_word_offset(Str, Pos2, Len, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub).
sub_atom(Str, Off, Len, Sub) :-
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   sys_atom_word_offset(Str, 0, Off, Pos),
   sys_atom_word_match(Str, Pos, Sub, 0, Count1).

% sub_atom(+Atom, +-Integer, +-Integer, +-Integer, -+Atom)
:- public sub_atom/5.
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Len),
   var(Off2),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_count(Str, 0, Count, Temp),
   sys_atom_word_pos(Str, 0, 0, Count, Off, Pos2),
   sys_atom_word_pos(Str, Off, Pos2, Count, Help, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   Len is Help-Off,
   Off2 is Temp-Help.
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Off2),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Len2 is -Len,
   sys_atom_word_offset(Str, Count, Len2, Count2),
   sys_atom_word_count(Str, 0, Count2, Temp2),
   sys_atom_word_pos(Str, 0, 0, Count2, Off, Pos2),
   sys_atom_word_offset(Str, Pos2, Len, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   Off2 is Temp2-Off.
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Off2), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   Len2 is -Len,
   sys_atom_word_offset(Str, Count, Len2, Count2),
   sys_atom_word_count(Str, 0, Count2, Temp2),
   sys_atom_word_pos(Str, 0, 0, Count2, Off, Pos2),
   sys_atom_word_match(Str, Pos2, Sub, 0, Count1),
   Off2 is Temp2-Off.
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Len),
   var(Sub), !,                                   % not in sub_atom/4.
   sys_atom_word_len(Str, Count),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos),
   sys_atom_word_count(Str, 0, Pos, Temp2),
   sys_atom_word_pos(Str, 0, 0, Pos, Off, Pos2),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   Len is Temp2-Off.
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Sub), !,                                   % not in sub_atom/4.
   sys_atom_word_len(Str, Count),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos),
   Len1 is -Len,
   sys_atom_word_offset(Str, Pos, Len1, Pos2),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   sys_atom_word_count(Str, 0, Pos2, Off).
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off), !,                                   % not in sub_atom/4.
   sys_atom_word_len(Str, Count),
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos),
   Pos2 is Pos-Count1,
   sys_atom_word_match(Str, Pos2, Sub, 0, Count1),
   sys_atom_word_count(Str, 0, Pos2, Off).
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Len),
   var(Off2),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_offset(Str, 0, Off, Pos2),
   sys_atom_word_count(Str, Pos2, Count, Temp2),
   sys_atom_word_pos(Str, Off, Pos2, Count, Help, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   Len is Help-Off,
   Off2 is Temp2-Len.
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off2),
   var(Sub), !,
   sys_atom_word_offset(Str, 0, Off, Pos2),
   sys_atom_word_offset(Str, Pos2, Len, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   sys_atom_word_len(Str, Count),
   sys_atom_word_count(Str, Pos, Count, Off2).
sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Len),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_offset(Str, 0, Off, Pos2),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   sys_atom_word_count(Str, Pos2, Pos, Len).
sub_atom(Str, Off, Len, Off2, Sub) :-
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   sys_atom_word_offset(Str, 0, Off, Pos),
   sys_atom_word_match(Str, Pos, Sub, 0, Count1),
   Pos2 is Pos+Count1,
   sys_atom_word_len(Str, Count),
   sys_atom_word_count(Str, Pos2, Count, Off2).

/****************************************************************/
/* Codes & Chars Conversion                                     */
/****************************************************************/

/**
 * atom_chars(X, Y): [ISO 8.16.4]
 * If X is a variable and Y is a character list then the predicate
 * succeeds when X unifies with the corresponding atom. Otherwise if
 * X is an atom then the predicate succeeds when Y unifies with the
 * corresponding character list.
 */
% atom_chars(+-Atom, -+Chars)
:- public atom_chars/2.
atom_chars(Atom, Chars) :-
   var(Atom), !,
   sys_list_to_atom(Chars, 0, Atom).
atom_chars(Atom, Chars) :-
   sys_atom_to_list(Atom, 0, Chars).

/**
 * atom_codes(X, Y): [ISO 8.16.5]
 * If X is a variable and Y is a code list then the predicate succeeds
 * when X unifies with the corresponding atom. Otherwise if X is
 * an atom then the predicate succeeds when Y unifies with the
 * corresponding code list.
 */
% atom_codes(+-Atom, -+Codes)
:- public atom_codes/2.
atom_codes(Atom, Codes) :-
   var(Atom), !,
   sys_list_to_atom(Codes, 1, Atom).
atom_codes(Atom, Codes) :-
   sys_atom_to_list(Atom, 1, Codes).

:- private sys_atom_to_list/3.
:- foreign(sys_atom_to_list/3, 'ForeignAtom', 
      sysAtomToList('Interpreter', 'String', int)).

:- private sys_list_to_atom/3.
:- foreign(sys_list_to_atom/3, 'ForeignAtom', 
      sysListToAtom('Object', int)).

/**
 * char_code(X, Y): [ISO 8.16.6]
 * If X is a variable and if Y is a code then the predicate succeeds
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
      sysCodeToChar('Integer')).

/**
 * number_chars(X, Y): [ISO 8.16.7]
 * If Y is ground and Y is a character list then the predicate
 * succeeds when X unifies with the corresponding number. Otherwise
 * if X is a number then the predicate succeeds when Y unifies with
 * the corresponding character list.
 */
% number_chars(+-Number, -+Chars)
:- public number_chars/2.
number_chars(Number, Chars) :-
   ground(Chars), !,
   sys_list_to_atom(Chars, 0, Atom),
   sys_atom_to_number(Atom, Number).
number_chars(Number, Chars) :-
   sys_number_to_atom(Number, 1, Atom),
   sys_atom_to_list(Atom, 0, Chars).

/**
 * number_codes(X, Y): [ISO 8.16.8]
 * If Y is ground and Y is a code list then the predicate
 * succeeds when X unifies with the corresponding number. Otherwise
 * if X is a number then the predicate suceeds when Y unifies
 * with the corresponding code list.
 */
% number_codes(+-Number, -+Codes)
:- public number_codes/2.
number_codes(Number, Codes) :-
   ground(Codes), !,
   sys_list_to_atom(Codes, 1, Atom),
   sys_atom_to_number(Atom, Number).
number_codes(Number, Codes) :-
   sys_number_to_atom(Number, 1, Atom),
   sys_atom_to_list(Atom, 1, Codes).

:- private sys_atom_to_number/2.
:- foreign(sys_atom_to_number/2, 'ForeignAtom', 
      sysAtomToNumber('Interpreter', 'String')).

:- private sys_number_to_atom/3.
:- foreign(sys_number_to_atom/3, 'ForeignAtom', 
      sysNumberToAtom('Number', int)).

/**
 * integer_chars(X, R, Y):
 * If Y is ground and Y is a character list then the predicate
 * succeeds when X unifies with corresponding integer in
 * base R. Otherwise if X is an integer then the predicate
 * succeeds when Y unifies with the corresponding character list
 * in base R.
 */
% integer_chars(+-Integer, +Integer, -+Chars)
:- public integer_chars/3.
integer_chars(Integer, Radix, Chars) :-
   ground(Chars), !,
   sys_list_to_atom(Chars, 0, Atom),
   sys_atom_to_integer(Atom, Radix, Integer).
integer_chars(Integer, Radix, Chars) :-
   sys_integer_to_atom(Integer, Radix, Atom),
   sys_atom_to_list(Atom, 0, Chars).

/**
 * integer_codes(X, R, Y):
 * If Y is ground and Y is a code list then the predicate
 * succeeds when X unifies with corresponding integer in
 * base R. Otherwise if X is an integer then the predicate
 * succeeds when Y unifies with the corresponding code list
 * in base R.
 */
% integer_codes(+-Integer, +Integer, -+Codes)
:- public integer_codes/3.
integer_codes(Integer, Radix, Codes) :-
   ground(Codes), !,
   sys_list_to_atom(Codes, 1, Atom),
   sys_atom_to_integer(Atom, Radix, Integer).
integer_codes(Integer, Radix, Codes) :-
   sys_integer_to_atom(Integer, Radix, Atom),
   sys_atom_to_list(Atom, 1, Codes).

:- private sys_atom_to_integer/3.
:- foreign(sys_atom_to_integer/3, 'ForeignAtom', 
      sysAtomToInteger('Interpreter', 'String', int)).

:- private sys_integer_to_atom/3.
:- foreign(sys_integer_to_atom/3, 'ForeignAtom', 
      sysIntegerToAtom('Number', int)).

/****************************************************************/
/* 16-bit Word Helpers                                          */
/****************************************************************/

:- private sys_atom_word_len/2.
:- virtual sys_atom_word_len/2.
:- foreign(sys_atom_word_len/2, 'String', length).

:- private sys_atom_word_substring/4.
:- virtual sys_atom_word_substring/4.
:- foreign(sys_atom_word_substring/4, 'String', substring(int, int)).

:- private sys_atom_word_count/4.
:- virtual sys_atom_word_count/4.
:- foreign(sys_atom_word_count/4, 'String', codePointCount(int, int)).

:- private sys_atom_word_pos/4.
:- foreign(sys_atom_word_pos/4, 'ForeignAtom', 
      sysAtomWordPos('CallOut', 'String', int, int)).

:- private sys_atom_word_offset/4.
:- foreign(sys_atom_word_offset/4, 'ForeignAtom', 
      sysOffsetByCodePoints('String', int, int)).

:- private sys_atom_word_match/5.
:- virtual sys_atom_word_match/5.
:- foreign(sys_atom_word_match/5, 'String', regionMatches(int, 'String', int, int)).

:- private sys_atom_word_pos/6.
:- foreign(sys_atom_word_pos/6, 'ForeignAtom', 
      sysAtomWordPos('Interpreter', 'CallOut', 'String', int, int, int, 'AbstractTerm')).

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
   sys_atom_word_count(Str, 0, Count, Temp),
   sys_atom_word_pos(Str, Temp, Count, 0, Off, Pos2),
   sys_atom_word_pos(Str, Temp, Count, Pos2, Help, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   Len is Help-Off,
   Off2 is Temp-Help.
last_sub_atom(Str, Len, Off2, Sub) :-
   var(Off2),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Len2 is -Len,
   sys_atom_word_offset(Str, Count, Len2, Count2),
   sys_atom_word_count(Str, 0, Count2, Temp2),
   sys_atom_word_pos(Str, Temp2, Count2, 0, Off, Pos2),
   sys_atom_word_offset(Str, Pos2, Len, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   Off2 is Temp2-Off.
last_sub_atom(Str, Len, Off2, Sub) :-
   var(Off2), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   Len2 is -Len,
   sys_atom_word_offset(Str, Count, Len2, Count2),
   sys_atom_word_count(Str, 0, Count2, Temp2),
   sys_atom_word_pos(Str, Temp2, Count2, 0, Off, Pos2),
   sys_atom_word_match(Str, Pos2, Sub, 0, Count1),
   Off2 is Temp2-Off.
last_sub_atom(Str, Len, Off2, Sub) :-
   var(Len),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos),
   sys_atom_word_count(Str, 0, Pos, Temp2),
   sys_atom_word_pos(Str, Temp2, Pos, 0, Off, Pos2),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   Len is Temp2-Off.
last_sub_atom(Str, Len, Off2, Sub) :-
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos),
   Len1 is -Len,
   sys_atom_word_offset(Str, Pos, Len1, Pos2),
   sys_atom_word_substring(Str, Pos2, Pos, Sub).
last_sub_atom(Str, Len, Off2, Sub) :-
   sys_atom_word_len(Str, Count),
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos),
   Pos2 is Pos-Count1,
   sys_atom_word_match(Str, Pos2, Sub, 0, Count1).

% last_sub_atom(+Atom, +-Integer, +-Integer, +-Integer, -+Atom)
:- public last_sub_atom/5.
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Len),
   var(Off2),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_count(Str, 0, Count, Temp),
   sys_atom_word_pos(Str, Temp, Count, 0, Off, Pos2),
   sys_atom_word_pos(Str, Temp, Count, Pos2, Help, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   Len is Help-Off,
   Off2 is Temp-Help.
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Off2),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Len2 is -Len,
   sys_atom_word_offset(Str, Count, Len2, Count2),
   sys_atom_word_count(Str, 0, Count2, Temp2),
   sys_atom_word_pos(Str, Temp2, Count2, 0, Off, Pos2),
   sys_atom_word_offset(Str, Pos2, Len, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   Off2 is Temp2-Off.
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Off2), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   Len2 is -Len,
   sys_atom_word_offset(Str, Count, Len2, Count2),
   sys_atom_word_count(Str, 0, Count2, Temp2),
   sys_atom_word_pos(Str, Temp2, Count2, 0, Off, Pos2),
   sys_atom_word_match(Str, Pos2, Sub, 0, Count1),
   Off2 is Temp2-Off.
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Len),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos),
   sys_atom_word_count(Str, 0, Pos, Temp2),
   sys_atom_word_pos(Str, Temp2, Pos, 0, Off, Pos2),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   Len is Temp2-Off.
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos),
   Len1 is -Len,
   sys_atom_word_offset(Str, Pos, Len1, Pos2),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   sys_atom_word_count(Str, 0, Pos2, Off).
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos),
   Pos2 is Pos-Count1,
   sys_atom_word_match(Str, Pos2, Sub, 0, Count1),
   sys_atom_word_count(Str, 0, Pos2, Off).
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Len),
   var(Off2),
   var(Sub), !,                                   % not in last_sub_atom/4
   sys_atom_word_len(Str, Count),
   sys_atom_word_count(Str, 0, Count, Temp),
   sys_atom_word_offset(Str, 0, Off, Pos2),
   sys_atom_word_pos(Str, Temp, Count, Pos2, Help, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   Len is Help-Off,
   Off2 is Temp-Help.
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Off2),
   var(Sub), !,                                   % not in last_sub_atom/4
   sys_atom_word_offset(Str, 0, Off, Pos2),
   sys_atom_word_offset(Str, Pos2, Len, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   sys_atom_word_len(Str, Count),
   sys_atom_word_count(Str, Pos, Count, Off2).
last_sub_atom(Str, Off, Len, Off2, Sub) :-
   var(Len),
   var(Sub), !,
   sys_atom_word_len(Str, Count),
   sys_atom_word_offset(Str, 0, Off, Pos2),
   Off1 is -Off2,
   sys_atom_word_offset(Str, Count, Off1, Pos),
   sys_atom_word_substring(Str, Pos2, Pos, Sub),
   sys_atom_word_count(Str, Pos2, Pos, Len).
last_sub_atom(Str, Off, Len, Off2, Sub) :-        % not in last_sub_atom/4
   sys_atom_word_len(Sub, Count1),
   sys_atom_word_count(Sub, 0, Count1, Len),
   sys_atom_word_offset(Str, 0, Off, Pos),
   sys_atom_word_match(Str, Pos, Sub, 0, Count1),
   Pos2 is Pos+Count1,
   sys_atom_word_len(Str, Count),
   sys_atom_word_count(Str, Pos2, Count, Off2).

/****************************************************************/
/* Prolog Commons                                               */
/****************************************************************/

/**
 * atom_split(R, S, L): [Prolog Commons Atom Utilities]
 * If R is a variable the predicate succeeds when R unifies with the
 * concatenation of each atom from the non-empty list L separated by
 * the atom S. Otherwise the predicate splits the atom R into a list L
 * of atoms that are separated by the atom S.
 */
% atom_split(+Atom, -+Atom, +-List)
:- public atom_split/3.
atom_split(Atom, Sep, List) :-
   var(Atom), !,
   atom_split1(List, Sep, Atom).
atom_split(Atom, Sep, List) :-
   atom_split2(Atom, Sep, List).

% atom_split1(+List, +Atom, -Atom)
:- private atom_split1/3.
atom_split1(List, _, _) :-
   var(List),
   throw(error(instantiation_error, _)).
atom_split1([X], _, X) :- !.
atom_split1([X, Y|Z], Sep, R) :- !,
   atom_split1([Y|Z], Sep, H),
   atom_concat(X, Sep, J),
   atom_concat(J, H, R).
atom_split1(List, _, _) :-
   throw(error(type_error(list, List), _)).

% atom_split2(+Atom, +Atom, -List)
:- private atom_split2/3.
atom_split2(Atom, Sep, [X|L]) :-
   sub_atom(Atom, Before, _, After, Sep), !,
   sub_atom(Atom, 0, Before, X),
   last_sub_atom(Atom, After, 0, H),
   atom_split2(H, Sep, L).
atom_split2(Atom, _, [Atom]).

/**
 * atom_number(A, N): [Prolog Commons Atom Utilities]
 * If A is a variable, then the predicate succeeds in A
 * with the number unparsing of N. Otherwise the predicate
 * succeeds in N with the number parsing of A.
 */
% atom_number(+-Atom, -+Number)
:- public atom_number/2.
atom_number(Atom, Number) :-
   var(Atom), !,
   sys_number_to_atom(Number, 1, Atom).
atom_number(Atom, Number) :-
   sys_atom_to_number(Atom, Number).

/**
 * atom_integer(A, R, N):
 * If A is a variable, then the predicate succeeds in A
 * with the integer unparsing of N in base R. Otherwise
 * the predicate succeeds in N with the integer parsing
 * of A in base R.
 */
% atom_integer(+-Atom, +Integer, -+Integer)
:- public atom_integer/3.
atom_integer(Atom, Radix, Integer) :-
   var(Atom), !,
   sys_integer_to_atom(Integer, Radix, Atom).
atom_integer(Atom, Radix, Integer) :-
   sys_atom_to_integer(Atom, Radix, Integer).

/**
 * atom_block(A, B):
 * If A is a variable, then the predicate succeeds in A
 * with the atom for the block B. Otherwise the predicate
 * succeeds in B with the atom for the block B.
 */
% atom_block(+-Atom, -+Bytes)
:- public atom_block/2.
atom_block(A, B) :-
   var(A), !,
   sys_block_to_atom(B, A).
atom_block(A, B) :-
   sys_atom_to_block(A, B).

:- private sys_block_to_atom/2.
:- foreign(sys_block_to_atom/2, 'ForeignAtom', 
      sysBlockToAtom({byte})).

:- private sys_atom_to_block/2.
:- foreign(sys_atom_to_block/2, 'ForeignAtom', 
      sysAtomToBlock('String')).

/****************************************************************/
/* Term Conversion                                              */
/****************************************************************/

/**
 * term_atom(T, A):
 * term_atom(T, A, O):
 * The predicate succeeds when the atom A is the serialization
 * of the term T. The ternary predicate accepts read respectively
 * write options O.
 */
% term_atom(+-Term, -+Atom)
:- public term_atom/2.
term_atom(T, A) :-
   term_atom(T, A, []).

% term_atom(+-Term, -+Atom, +List)
:- public term_atom/3.
term_atom(T, A, O) :-
   var(A), !,
   sys_unparse_term(T, O, A).
term_atom(T, A, O) :-
   sys_parse_term(A, O, T).

:- private sys_parse_term/3.
:- foreign(sys_parse_term/3, 'ForeignAtom', 
      sysParseTerm('Interpreter', 'String', 'Object')).

:- private sys_unparse_term/3.
:- foreign(sys_unparse_term/3, 'ForeignAtom', 
      sysUnparseTerm('Interpreter', 'AbstractTerm', 'Object')).
