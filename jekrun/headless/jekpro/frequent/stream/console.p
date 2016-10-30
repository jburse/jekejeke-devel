/**
 * This module supports access to the console. The first part
 * consists of read utilities. Among the read utilities there
 * are currently predicates read_line/[1,2].
 *
 * The second part consist Quintus Prolog inspired formatted
 * output predicates. Among the formatted output there are
 * currently the predicates format/[2,3], print_message/[1,2]
 * print_error/[1,2] and print_stack_trace/[1,2]. The formatting
 * is based on the Java Formatter class.
 *
 * Example:
 * ?- format('res=%20d',[123123123]), nl.
 * res=           123123123
 * Yes
 * ?- format('res=%25.4f',[123123123.123]), nl.
 * res=           123123123.1230
 * Yes
 *
 * The third part consist Edinburgh Prolog inspired terminal
 * input/output. Among the terminal input/output there are
 * currently the predicates ttynl/0, ttywrite/1, ttywrite_term/2,
 * ttyflush_output/0 and ttyread_line/1.
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

:- package(library(jekpro/frequent/stream)).
:- use_package(foreign(jekpro/frequent/stream)).
:- use_package(foreign(java/util)).
:- use_package(foreign(java/io)).
:- use_package(foreign(jekpro/tools/call)).

:- module(console, []).
:- use_module(library(system/locale)).

/****************************************************************/
/* Read Utils                                                   */
/****************************************************************/

/**
 * read_line(C):
 * read_line(T, C):
 * The predicate reads a character line from the standard input.
 * The predicate succeeds when C unifies with the read characters
 * or fails when the end of the stream has been reached. The binary
 * predicate takes an additional text stream T as argument.
 */
% read_line(-Atom)
:- public read_line/1.
read_line(Atom) :-
   current_input(Stream),
   sys_read_line(Stream, Atom).

% read_line(+AliasOrStream, -Atom)
:- public read_line/2.
read_line(Alias, Atom) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_read_line(Stream, Atom).
read_line(Stream, Atom) :-
   sys_read_line(Stream, Atom).

:- private sys_read_line/2.
:- foreign(sys_read_line/2, 'ForeignConsole',
      sysReadLine('Reader')).

/****************************************************************/
/* Formatted Output                                             */
/****************************************************************/

/**
 * format(F, A):
 * format(T, F, A):
 * The predicate formats the list of arguments A according to
 * the format F using the current locale and writes it to the
 * current output. The ternary predicate allows specifying
 * a text stream T.
 */
% format(+Atom, +List)
:- public format/2.
format(Format, Arguments) :-
   atom_format(Format, Arguments, Atom),
   write(Atom).

% format(+AliasOrStream, +Atom, +List)
:- public format/3.
format(AliasOrStream, Format, Arguments) :-
   atom_format(Format, Arguments, Atom),
   write(AliasOrStream, Atom).

/**
 * print_message(M):
 * print_message(T, M):
 * The predicate formats the message term M according to the
 * error properties of the knowledge base and the current locale,
 * and writes it to the current output. The binary predicate
 * allows specifying a text stream T.
 */
% print_message(+Term)
:- public print_message/1.
print_message(Message) :-
   get_error_properties(Props),
   message_make(Props, Message, Atom),
   write(Atom).

% print_message(+AliasOrStream, +Term)
:- public print_message/2.
print_message(AliasOrStream, Message) :-
   get_error_properties(Props),
   message_make(Props, Message, Atom),
   write(AliasOrStream, Atom).

/**
 * print_error(E):
 * print_error(T, E):
 * The predicate formats the error term E without its context
 * according to the error properties of the knowledge base and
 * the current locale, and writes it to the current error. The
 * binary predicate allows specifying a text stream T.
 */
% print_error(+Term)
:- public print_error/1.
print_error(Error) :-
   current_prolog_flag(sys_disp_error, Stream),
   get_error_properties(Props),
   error_make(Props, Error, Atom),
   write(Stream, Atom),
   nl(Stream).

% print_error(+AliasOrStream, +Term)
:- public print_error/2.
print_error(AliasOrStream, Error) :-
   get_error_properties(Props),
   error_make(Props, Error, Atom),
   write(AliasOrStream, Atom),
   nl(AliasOrStream).

/**
 * print_stack_trace(E):
 * print_stack_trace(T, E):
 * The predicate formats the error term E with its context according
 * to the error properties of the knowledge base and the current
 * locale, and writes it to the current error. The binary predicate
 * allows specifying a text stream T.
 */
% print_stack_trace(+Term)
:- public print_stack_trace/1.
print_stack_trace(Error) :-
   current_prolog_flag(sys_disp_error, Stream),
   current_prolog_flag(sys_locale, Locale),
   get_error_properties(Props),
   sys_print_stack_trace(Stream, Error, Locale, Props).

% print_stack_trace(+AliasOrStream, +Term)
:- public print_stack_trace/2.
print_stack_trace(Alias, Error) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   current_prolog_flag(sys_locale, Locale),
   get_error_properties(Props),
   sys_print_stack_trace(Stream, Error, Locale, Props).
print_stack_trace(Stream, Error) :-
   current_prolog_flag(sys_locale, Locale),
   get_error_properties(Props),
   sys_print_stack_trace(Stream, Error, Locale, Props).

:- private sys_print_stack_trace/4.
:- foreign(sys_print_stack_trace/4, 'ForeignConsole',
      sysPrintStackTrace('Interpreter','Writer','Object',
         'String','Properties')).

/****************************************************************/
/* Edinburgh Style                                              */
/****************************************************************/

/**
 * ttynl:
 * The predicate writes the system end of line sequence to the
 * terminal and flushes it.
 */
% ttynl
:- public ttynl/0.
ttynl :-
   current_prolog_flag(sys_disp_output, Stream),
   sys_nl(Stream).

/**
 * ttywrite(E):
 * The predicate writes the term E to the terminal whereby
 * numbering variables.
 */
% ttywrite(+Term)
:- public ttywrite/1.
ttywrite(Term) :-
   current_prolog_flag(sys_disp_output, Stream),
   sys_write(Stream, Term).

/**
 * ttywrite_term(E, O):
 * The predicate writes the term E to the terminal taking
 * into account the write options O.
 */
% ttywrite_term(+Term, +Term)
:- public ttywrite_term/2.
ttywrite_term(Term, Opt) :-
   current_prolog_flag(sys_disp_output, Stream),
   sys_write_term(Stream, Term, Opt).

/**
 * ttyflush_output:
 * The predicate flushes the terminal.
 */
% ttyflush_output
:- public ttyflush_output/0.
ttyflush_output :-
   current_prolog_flag(sys_disp_output, Stream),
   sys_flush_output(Stream).

/**
 * ttyread_line(C):
 * The predicate reads a character line from the terminal.
 * The predicate succeeds when C unifies with the read characters
 * or fails when the end of the stream has been reached.
 */
% ttyread_line(-Atom)
:- public ttyread_line/1.
ttyread_line(Atom) :-
   current_prolog_flag(sys_disp_input, Stream),
   sys_read_line(Stream, Atom).