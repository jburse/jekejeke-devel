/**
 * Streams can have a couple of properties. The if-modified-since date
 * property can be supplied by the open options. It causes the open
 * primitive to throw a not-modified exception when the source has not
 * been modified since the given date. The last-modified and expiration
 * date properties can be retrieved via the open options.
 *
 * Text streams have further the character set encoding property. For
 * remote sources with a mime type this property defaults to the character
 * set property of the mime type. Otherwise the property defaults to UTF-8.
 * Incoming CR LF sequences or CR characters are automatically compressed
 * respectively translated to LF characters.
 *
 * If a text stream belongs to the file schema and is opened for read
 * with the byte order mark detection on, this detection will try to
 * determine the default encoding. The detection can currently detect
 * UTF-8, UTF-16LE and UTF-16BE. If a mark is detected the initial read
 * position is placed after the mark.
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
:- use_package(foreign(jekpro/tools/call)).

:- module(user, []).

/****************************************************************/
/* Stream Control                                               */
/****************************************************************/

/**
 * current_input(S): [ISO 8.11.1]
 * The predicate succeeds when S unifies with the standard input stream.
 */
% current_input(-Stream)
:- public current_input/1.
current_input(Stream) :-
   current_prolog_flag(sys_cur_input, Stream).

/**
 * current_output(S): [ISO 8.11.2]
 * The predicate succeeds when S unifies with the standard output stream.
 */
% current_output(-Stream)
:- public current_output/1.
current_output(Stream) :-
   current_prolog_flag(sys_cur_output, Stream).

/**
 * current_error(S):
 * The predicate succeeds when S unifies with the standard error stream.
 */
% current_error(-Stream)
:- public current_error/1.
current_error(Stream) :-
   current_prolog_flag(sys_cur_error, Stream).

/**
 * set_input(S): [ISO 8.11.3]
 * The predicate sets the standard input stream to the source S.
 */
% set_input(+AliasOrPath)
:- public set_input/1.
set_input(Alias) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   set_prolog_flag(sys_cur_input, Stream).
set_input(Stream) :-
   set_prolog_flag(sys_cur_input, Stream).

/**
 * set_output(S): [ISO 8.11.4]
 * The predicate sets the standard output stream to the sink S.
 */
% set_output(+AliasOrPath)
:- public set_output/1.
set_output(Alias) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   set_prolog_flag(sys_cur_output, Stream).
set_output(Stream) :-
   set_prolog_flag(sys_cur_output, Stream).

/**
 * set_error(S):
 * The predicate sets the standard error stream to the sink S.
 */
% set_error(+AliasOrPath)
:- public set_error/1.
set_error(Alias) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   set_prolog_flag(sys_cur_error, Stream).
set_error(Stream) :-
   set_prolog_flag(sys_cur_error, Stream).

/**
 * open(P, M, S): [ISO 8.11.5.4]
 * open(P, M, S, O): [ISO 8.11.5.4]
 * The ternary predicate succeeds when S unifies with the new stream
 * associated with the path P and the access mode M (read, write or
 * append). The quaternary predicate additionally recognizes the following
 * open options. For a list of options see the API documentation.
 */
% open(+Path, +Mode, -Stream, +Opt)
:- public open/4.
open(Path, Mode, Stream, Opt) :-
   sys_oneof(Opt, alias(Alias), Opt2), !,
   absolute_file_name(Path, Pin, [access(Mode)]),
   sys_open(Pin, Mode, Opt2, Stream),
   sys_put_alias(Alias, Stream).
open(Path, Mode, Stream, Opt) :-
   absolute_file_name(Path, Pin, [access(Mode)]),
   sys_open(Pin, Mode, Opt, Stream).

% open(+Path, +Mode, -Stream)
:- public open/3.
open(Path, Mode, Stream) :-
   open(Path, Mode, Stream, []).

:- private sys_open/4.
:- foreign(sys_open/4, 'ForeignStream',
      sysOpen('Interpreter','String','String','Object')).

/**
 * close(S): [ISO 8.11.6]
 * close(S, O): [ISO 8.11.6]
 * The unary predicate closes the stream S. The binary predicate
 * additionally recognizes the following close options. For a list
 * of options see the API documentation.
 */
% close(+AliasOrPath, +Opt)
:- public close/2.
close(Alias, Opt) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_close(Stream, Opt),
   sys_remove_alias(Alias, Stream).
close(Stream, Opt) :-
   sys_close(Stream, Opt).

% close(+AliasOrPath)
:- public close/1.
close(Stream) :-
   close(Stream, []).

:- private sys_close/2.
:- foreign(sys_close/2, 'ForeignStream',
      sysClose('Object','Object')).

/**
 * stream_property(S, P): [ISO 8.11.8]
 * The predicate succeeds with all the properties of the stream
 * S that unify with P. The following stream properties are
 * supported. For a list of options see the API documentation.
 */
% stream_property(+AliasOrPath, -Prop)
:- public stream_property/2.
stream_property(Alias, Prop) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_stream_properties(Stream, Props),
   sys_member(Prop, Props).
stream_property(Stream, Prop) :-
   sys_stream_properties(Stream, Props),
   sys_member(Prop, Props).

:- private sys_stream_properties/2.
:- foreign(sys_stream_properties/2, 'ForeignStream',
      sysStreamProperties('Object')).

/**
 * set_stream_position(S, P): [ISO 8.11.9]
 * The predicate sets the file position of the stream S to P.
 */
% set_stream_position(+AliasOrPath, +Pos)
:- public set_stream_position/2.
set_stream_position(Alias, Pos) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_set_stream_position(Stream, Pos).
set_stream_position(Stream, Pos) :-
   sys_set_stream_position(Stream, Pos).

:- private sys_set_stream_position/2.
:- foreign(sys_set_stream_position/2, 'ForeignStream',
      sysSetStreamPosition('Object',long)).

/**
 * set_stream_length(S, L):
 * The predicate sets the file length of the stream S to L.
 */
% set_stream_length(+AliasOrPath, +Pos)
:- public set_stream_length/2.
set_stream_length(Alias, Len) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_set_stream_length(Stream, Len).
set_stream_length(Stream, Len) :-
   sys_set_stream_length(Stream, Len).

:- private sys_set_stream_length/2.
:- foreign(sys_set_stream_length/2, 'ForeignStream',
      sysSetStreamLength('Object',long)).

/*************************************************************************/
/* Alias handling                                                        */
/*************************************************************************/

:- private sys_alias/2.
:- dynamic sys_alias/2.

% sys_get_alias(+Alias, -Stream)
sys_get_alias(Alias, Stream) :-
   sys_alias(Alias, Stream), !.
sys_get_alias(Alias, _) :-
   throw(error(existence_error(stream,Alias),_)).

% sys_put_alias(+Alias, +Stream)
:- private sys_put_alias/2.
sys_put_alias(Alias, _) :-
   sys_alias(Alias, _), !,
   throw(error(permission_error(open,source_sink,alias(Alias)),_)).
sys_put_alias(Alias, Stream) :-
   assertz(sys_alias(Alias, Stream)).

% sys_remove_alias(+Alias)
:- private sys_remove_alias/2.
sys_remove_alias(Alias, Stream) :-
   retract(sys_alias(Alias, Stream)).
