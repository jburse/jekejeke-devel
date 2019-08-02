/**
 * Characters can be written to text streams or read from text
 * streams. Text streams can be obtained by the to open/3 and
 * open/4 system predicates documented in the stream control
 * section. The standard output and/or the standard input might
 * also point to text streams, but this is not guaranteed.
 *
 * Text streams are automatically flushed when the new line primitive
 * is invoked. Additionally the text stream of the console window
 * is automatically flushed when more than 1024 characters have
 * been written.
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

:- use_package(foreign(jekpro/frequent/stream)).
:- use_package(foreign(java/io)).
:- use_package(foreign(matula/util/regex)).

:- module(user, []).

/****************************************************************/
/* Char I/O                                                     */
/****************************************************************/

/**
 * nl: [ISO 8.12.3]
 * nl(T): [ISO 8.12.3]
 * The predicate without arguments writes the system end of line
 * sequence to the standard output and flushes it. The unary
 * predicate takes an additional text stream sink as argument.
 */
% nl
:- public nl/0.
nl :-
   current_output(Stream),
   sys_nl(Stream).

% nl(+AliasOrStream)
:- public nl/1.
nl(Alias) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_nl(Stream).
nl(Stream) :-
   sys_nl(Stream).

:- foreign(sys_nl/1, 'ForeignChar',
      sysNl('Writer')).

/**
 * put_char(C): [ISO 8.12.3]
 * put_char(T, C): [ISO 8.12.3]
 * The unary predicate writes the character C to the standard output.
 * The binary predicate takes an additional text stream sink
 * as argument.
 */
% put_char(+Char)
:- public put_char/1.
put_char(Char) :-
   current_output(Stream),
   sys_put_char(Stream, Char).

% put_char(+AliasOrStream, +Char)
:- public put_char/2.
put_char(Alias, Char) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_put_char(Stream, Char).
put_char(Stream, Char) :-
   sys_put_char(Stream, Char).

:- private sys_put_char/2.
:- foreign(sys_put_char/2, 'ForeignChar',
      sysPutChar('Writer', 'String')).

/**
 * put_code(C): [ISO 8.12.3]
 * put_code(T, C): [ISO 8.12.3]
 * The unary predicate writes the code C to the standard output.
 * The binary predicate takes an additional text stream sink
 * as argument.
 */
% put_code(+Code)
:- public put_code/1.
put_code(Code) :-
   current_output(Stream),
   sys_put_code(Stream, Code).

% put_code(+AliasOrStream, +Code)
:- public put_code/2.
put_code(Alias, Code) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_put_code(Stream, Code).
put_code(Stream, Code) :-
   sys_put_code(Stream, Code).

:- private sys_put_code/2.
:- foreign(sys_put_code/2, 'ForeignChar',
      sysPutCode('Writer', 'Integer')).

/**
 * get_char(C): [ISO 8.12.1]
 * get_char(T, C): [ISO 8.12.1]
 * The predicate reads a character from the standard input. The predicate
 * succeeds when C unifies with the read character or the atom end_of_file
 * when the end of the stream has been reached. The binary predicate takes
 * an additional text stream source as argument.
 */
% get_char(-Char)
:- public get_char/1.
get_char(Char) :-
   current_input(Stream),
   sys_get_char(Stream, Char).

% get_char(+AliasOrStream, -Char)
:- public get_char/2.
get_char(Alias, Char) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_get_char(Stream, Char).
get_char(Stream, Char) :-
   sys_get_char(Stream, Char).

:- private sys_get_char/2.
:- foreign(sys_get_char/2, 'ForeignChar',
      sysGetChar('Reader')).

/**
 * get_code(C): [ISO 8.12.1]
 * get_code(T, C): [ISO 8.12.1]
 * The predicate reads a code from the standard input. The predicate
 * succeeds when C unifies with the read code or the integer -1 when the
 * end of the stream has been reached. The binary predicate takes an additional
 * text stream source as argument.
 */
% get_code(-Code)
:- public get_code/1.
get_code(Code) :-
   current_input(Stream),
   sys_get_code(Stream, Code).

% get_code(+AliasOrStream, -Code)
:- public get_code/2.
get_code(Alias, Code) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_get_code(Stream, Code).
get_code(Stream, Code) :-
   sys_get_code(Stream, Code).

:- private sys_get_code/2.
:- foreign(sys_get_code/2, 'ScannerToken',
      sysGetCode('Reader')).

/**
 * peek_char(C): [ISO 8.12.2]
 * peek_char(T, C): [ISO 8.12.2]
 * The unary predicate reads a character from the standard input and
 * puts it back. The predicate succeeds when C unifies with the peeked
 * character or the atom end_of_file when the end of the stream has been
 * reached. The binary predicate takes an additional text stream source
 * as argument.
 */
% peek_char(-Char)
:- public peek_char/1.
peek_char(Char) :-
   current_input(Stream),
   sys_peek_char(Stream, Char).

% peek_char(+AliasOrStream, -Char)
:- public peek_char/2.
peek_char(Alias, Char) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_peek_char(Stream, Char).
peek_char(Stream, Char) :-
   sys_peek_char(Stream, Char).

:- private sys_peek_char/2.
:- foreign(sys_peek_char/2, 'ForeignChar',
      sysPeekChar('Reader')).

/**
 * peek_code(C): [ISO 8.12.2]
 * peek_code(T, C): [ISO 8.12.2]
 * The predicate reads a code from the standard input and puts it back. The
 * predicate succeeds when C unifies with the read code or the integer -1 when
 * the end of the stream has been reached. The binary predicate takes an
 * additional text stream source as argument.
 */
% peek_code(-Code)
:- public peek_code/1.
peek_code(Code) :-
   current_input(Stream),
   sys_peek_code(Stream, Code).

% peek_code(+AliasOrStream, -Code)
:- public peek_code/2.
peek_code(Alias, Code) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_peek_code(Stream, Code).
peek_code(Stream, Code) :-
   sys_peek_code(Stream, Code).

:- private sys_peek_code/2.
:- foreign(sys_peek_code/2, 'ScannerToken',
      sysPeekCode('Reader')).
