/**
 * Bytes can be written to binary streams or read from binary streams.
 * Binary streams can be obtained by the to open/3 and open/4 system
 * predicates documented in the stream control section. The standard
 * output and/or the standard input might also point to binary streams,
 * but this is not guaranteed.
 *
 * Text streams and binary streams share the notion of flushing and
 * end of stream. Therefore the system predicates flush_output/[0,1]
 * and at_end_of_stream/[0,1] apply to both text streams and
 * binary streams.
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

:- use_package(foreign(jekpro/frequent/stream)).
:- use_package(foreign(java/io)).

:- module(user, []).

/****************************************************************/
/* Byte I/O                                                     */
/****************************************************************/

/**
 * put_byte(B): [ISO 8.13.3]
 * put_byte(H, B): [ISO 8.13.3]
 * The unary predicate writes the byte B to the standard output. The
 * binary predicate takes an additional binary stream sink
 * as argument.
 */
% put_byte(+Byte)
:- public put_byte/1.
put_byte(Byte) :-
   current_output(Stream),
   sys_put_byte(Stream, Byte).

% put_byte(+AliasOrStream, +Byte)
:- public put_byte/2.
put_byte(Alias, Byte) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_put_byte(Stream, Byte).
put_byte(Stream, Byte) :-
   sys_put_byte(Stream, Byte).

:- private sys_put_byte/2.
:- foreign(sys_put_byte/2, 'ForeignByte',
      sysPutByte('OutputStream',int)).

/**
 * get_byte(B): [ISO 8.13.1]
 * get_byte(H, B): [ISO 8.13.1]
 * The predicate reads a byte from the standard input. The predicate
 * succeeds when B unifies with the read byte or the integer -1 when
 * the end of the stream has been reached. The binary predicate takes
 * an additional binary stream source as argument.
 */
% get_byte(-Byte)
:- public get_byte/1.
get_byte(Byte) :-
   current_input(Stream),
   sys_get_byte(Stream, Byte).

% get_byte(+AliasOrStream, -Byte)
:- public get_byte/2.
get_byte(Alias, Byte) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_get_byte(Stream, Byte).
get_byte(Stream, Byte) :-
   sys_get_byte(Stream, Byte).

:- private sys_get_byte/2.
:- foreign(sys_get_byte/2, 'ForeignByte',
      sysGetByte('InputStream')).

/**
 * peek_byte(B): [ISO 8.13.2]
 * peek_byte(H, B): [ISO 8.13.2]
 * The predicate reads a byte from the standard input and puts it
 * back. The predicate succeeds when B unifies with the read byte or
 * the integer -1 when the end of the stream has been reached. The
 * binary predicate takes an additional binary stream
 * source as argument.
 */
% peek_byte(-Byte)
:- public peek_byte/1.
peek_byte(Byte) :-
   current_input(Stream),
   sys_peek_byte(Stream, Byte).

% peek_byte(+AliasOrStream, -Byte)
:- public peek_byte/2.
peek_byte(Alias, Byte) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_peek_byte(Stream, Byte).
peek_byte(Stream, Byte) :-
   sys_peek_byte(Stream, Byte).

:- private sys_peek_byte/2.
:- foreign(sys_peek_byte/2, 'ForeignByte',
      sysPeekByte('InputStream')).

/**
 * flush_output: [ISO 8.11.7]
 * flush_output(S): [ISO 8.11.7]
 * The predicate without arguments flushes the standard output. The
 * unary predicate takes an additional text or binary stream
 * sink as argument.
 */
% flush_output
:- public flush_output/0.
flush_output :-
   current_output(Stream),
   sys_flush_output(Stream).

% flush_output(+AliasOrPath)
:- public flush_output/1.
flush_output(Alias) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_flush_output(Stream).
flush_output(Stream) :-
   sys_flush_output(Stream).

:- foreign(sys_flush_output/1, 'ForeignByte',
      sysFlushOutput('Object')).

/**
 * at_end_of_stream: [ISO 8.11.8]
 * at_end_of_stream(S): [ISO 8.11.8]
 * The predicate without arguments checks whether we are at the end
 * of the standard input. The unary predicate takes an additional
 * text or binary stream sink as argument.
 */
% at_end_of_stream
:- public at_end_of_stream/0.
at_end_of_stream :-
   current_input(Stream),
   sys_at_end_of_stream(Stream).

% at_end_of_stream(+AliasOrPath)
:- public at_end_of_stream/1.
at_end_of_stream(Alias) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_at_end_of_stream(Stream).
at_end_of_stream(Stream) :-
   sys_at_end_of_stream(Stream).

:- private sys_at_end_of_stream/1.
:- foreign(sys_at_end_of_stream/1, 'ForeignByte',
      sysAtEndOfStream('Object')).
