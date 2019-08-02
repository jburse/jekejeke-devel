/**
 * This module supports access to the console. The first part
 * consists of read utilities. Among the read utilities there
 * are currently the predicates read_line/[1,2] and
 * read_line_max/[2,3]. The former does an unbounded read
 * the later does a bounded read.
 *
 * Since the line read predicates work with a text stream and
 * does not fully consume a line, so that it can cater for different
 * line formats with CR LF, LF or CR termination, we introduced further
 * predicates read_punch/[1,2] and read_punch_max/[2,3].
 *
 * Example:
 * ?- format('res=%20d',[123123123]), nl.
 * res=           123123123
 * Yes
 * ?- format('res=%25.4f',[123123123.123]), nl.
 * res=           123123123.1230
 * Yes
 *
 * The second part consist Quintus Prolog inspired formatted output
 * predicates. Among the for-matted output there are currently the
 * predicates format/[2,3] with the difference that it currently
 * only supports the Java formatting format.
 *
 * Finally the predicates error_make/[2,3] can be used to retrieve
 * the message of a Prolog excep-tion term. Further the predicates
 * print_stack_trace/[2,3] allows sending the message and the context
 * of a Prolog exception term to a stream.
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

:- package(library(jekpro/frequent/stream)).
:- use_package(foreign(jekpro/frequent/stream)).
:- use_package(foreign(java/util)).
:- use_package(foreign(java/io)).
:- use_package(foreign(jekpro/tools/call)).

:- module(console, []).
:- use_module(library(system/locale)).

/****************************************************************/
/* Read Line                                                    */
/****************************************************************/

/**
 * read_line(C):
 * read_line(T, C):
 * The predicate succeeds in C in reading a line. The predicate fails
 * upon an empty line and an end of file. The binary predicate
 * allows specifying a text stream T
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
      readLine('Reader')).

/**
 * read_line_max(L, C):
 * read_line_max(T, L, C):
 * The predicate succeeds in C in reading a line with maximally L
 * characters. The predicate fails upon an empty line and an end of file.
 * The ternary predicate allows specifying a text stream T.
 */
% read_line_max(+Integer, -Atom)
:- public read_line_max/2.
read_line_max(Length, Atom) :-
   current_input(Stream),
   sys_read_line_max(Stream, Length, Atom).

% read_line_max(+AliasOrStream, +Integer, -Atom)
:- public read_line_max/3.
read_line_max(Alias, Length, Atom) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_read_line_max(Stream, Length, Atom).
read_line_max(Stream, Length, Atom) :-
   sys_read_line_max(Stream, Length, Atom).

:- private sys_read_line_max/3.
:- foreign(sys_read_line_max/3, 'ForeignConsole',
      readLineMax('Reader', 'Integer')).

/****************************************************************/
/* Read Punch                                                   */
/****************************************************************/

/**
 * read_punch(C):
 * read_punch(T, C):
 * The predicate succeeds in C in reading a punch. The predicate fails
 * upon end of file. The punch must end in CR LF, otherwise an exception
 * is thrown. The binary predicate allows specifying a binary stream T.
 */
% read_punch(-Block)
:- public read_punch/1.
read_punch(Block) :-
   current_input(Stream),
   sys_read_punch(Stream, Block).

% read_punch(+AliasOrStream, -Block)
:- public read_punch/2.
read_punch(Alias, Block) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_read_punch(Stream, Block).
read_punch(Stream, Block) :-
   sys_read_punch(Stream, Block).

:- private sys_read_punch/2.
:- foreign(sys_read_punch/2, 'ForeignConsole',
      readPunch('InputStream')).

/**
 * read_punch_max(L, C):
 * read_punch_max(T, L, C):
 * The predicate succeeds in C in reading a punch with maximally L
 * bytes. The predicate fails upon end of file. If less than L
 * bytes different from CR are read, the punch must end in CR LF
 * otherwise an exception is thrown. The ternary predicate allows
 * specifying a binary stream T.
 */
% read_punch_max(+Integer, -Block)
:- public read_punch_max/2.
read_punch_max(Length, Block) :-
   current_input(Stream),
   sys_read_punch_max(Stream, Length, Block).

% read_punch_max(+AliasOrStream, +Integer, -Block)
:- public read_punch_max/3.
read_punch_max(Alias, Length, Block) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_read_punch_max(Stream, Length, Block).
read_punch_max(Stream, Length, Block) :-
   sys_read_punch_max(Stream, Length, Block).

:- private sys_read_punch_max/3.
:- foreign(sys_read_punch_max/3, 'ForeignConsole',
      readPunchMax('InputStream', 'Integer')).

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
   format_atom(Format, Arguments, Atom),
   write(Atom).

% format(+AliasOrStream, +Atom, +List)
:- public format/3.
format(AliasOrStream, Format, Arguments) :-
   format_atom(Format, Arguments, Atom),
   write(AliasOrStream, Atom).

/**
 * error_make(E, S):
 * error_make(L, E, S):
 * The predicate formats the error term E without its context
 * unifies the result with S. The quaternary predicate allows
 * specifying a locale L.
 */
% error_make(+Term, -Atom)
:- public error_make/2.
error_make(Term, Atom) :-
   current_prolog_flag(sys_locale, Locale),
   error_make(Locale, Term, Atom).

% error_make(+Atom, +Term, -Atom)
:- public error_make/3.
error_make(Locale, Term, Atom) :-
   get_error_properties(Locale, Props),
   sys_error_make(Term, Locale, Props, Atom).

% sys_error_make(+Term, +Atom, +Props, -Atom)
:- private sys_error_make/4.
:- foreign(sys_error_make/4, 'ForeignConsole',
      sysErrorMake('Interpreter', 'Object', 'String', 'Properties')).

/**
 * print_stack_trace(T, E):
 * print_stack_trace(L, T, E):
 * The predicate formats the error term E with the context to
 * the output stream T. The quaternary predicate allows
 * specifying a locale L.
 */
% print_stack_trace(+AliasOrStream, +Term)
:- public print_stack_trace/2.
print_stack_trace(Alias, Error) :-
   current_prolog_flag(sys_locale, Locale),
   print_stack_trace(Locale, Alias, Error).

% print_stack_trace(+Atom, +AliasOrStream, +Term)
:- public print_stack_trace/3.
print_stack_trace(Locale, Alias, Error) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   get_error_properties(Locale, Props),
   sys_print_stack_trace(Stream, Error, Locale, Props).
print_stack_trace(Locale, Stream, Error) :-
   get_error_properties(Locale, Props),
   sys_print_stack_trace(Stream, Error, Locale, Props).

% sys_print_stack_trace(+Writer, +Term, +Atom, +Props)
:- private sys_print_stack_trace/4.
:- foreign(sys_print_stack_trace/4, 'ForeignConsole',
      sysPrintStackTrace('Interpreter', 'Writer', 'Object',
         'String', 'Properties')).
