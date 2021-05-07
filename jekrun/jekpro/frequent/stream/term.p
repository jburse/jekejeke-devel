/**
 * Terms can be written to text streams or read from text streams
 * depending on the current operator definitions. It is possible
 * to switch off operator usage by the write option ignore_ops/1.
 * Further during writing variables and atoms are put into quotes
 * when necessary. It is possible to switch on quoting by the
 * write option quoted/1.
 *
 * Finally terms of the form ‘$VAR’(<number>) are usually recognized
 * and written out as <var>. It is possible to switch on the variable
 * numbering by the write option numbervars/1. The Prolog system also
 * supports '$STR'(<atom>) terms to represent strings and can read or
 * write them as quoted strings.
 *
 * Table 13: Predefined Write Predicates
 * Predicate	numbervars	quoted	ignore_ops
 * write	Yes	No	No
 * writeq	Yes	Yes	No
 * write_canonical	No	Yes	Yes
 *
 * The spacing around operators determined by the operator properties
 * sys_nspl/0 and sys_nspr/0. By default, the operators above level 699
 * have spacing around them, and the operators below and including level
 * 699 do not have spacing around them. With the exception that the
 * operators (';') and (',') do not have a space in the front of them.
 *
 * Examples:
 * ?- X = (a->b;c).
 * X = (a -> b; c)
 *
 * ?- X = (1+2,3+4).
 * X = (1+2, 3+4)
 *
 * If the format option is newline the operator properties sys_tabr/0
 * and sys_newr/0 are also taken into consideration so that the output
 * matches the Prolog coding guidelines as published in [9]. Further the
 * priority option determines whether parentheses are needed around an
 * operator expressions depending on the level of the operator.
 *
 * When double quotes or back quotes are set to ‘variable’ and quote is
 * true, then variable names are automatically set into the corresponding
 * quotes when necessary. If neither double quotes nor back quotes are set
 * to ‘variable’, then the predicates write_term/[2,3] throw an error if a
 * variable name would need quotes.
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
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/tools/term)).

:- module(user, []).

/****************************************************************/
/* Term I/O                                                     */
/****************************************************************/

/**
 * write(E): [ISO 8.14.2]
 * write(T, E): [ISO 8.14.2]
 * The unary predicate writes the term E to the standard output
 * whereby numbering variables. The binary predicate takes an
 * additional text stream sink as argument.
 */
% write(+Term)
:- public write/1.
write(Term) :-
   current_output(Stream),
   sys_write_term(Stream, Term).

% write(+AliasOrStream, +Term)
:- public write/2.
write(Alias, Term) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_write_term(Stream, Term).
write(Stream, Term) :-
   sys_write_term(Stream, Term).

:- private sys_write_term/2.
:- foreign(sys_write_term/2, 'ForeignTerm',
      sysWriteTerm('Interpreter', 'Writer', 'AbstractTerm')).

/**
 * writeq(E): [ISO 8.14.2]
 * writeq(T, E): [ISO 8.14.2]
 * The predicate writes the term E to the standard output whereby
 * quoting atoms and variables if necessary. The binary predicate
 * takes an additional text stream sink as argument.
 */
% writeq(+Term)
:- public writeq/1.
writeq(Term) :-
   current_output(Stream),
   sys_write_term(Stream, Term, [numbervars(true), quoted(true)]).

% writeq(+AliasOrStream, +Term)
:- public writeq/2.
writeq(Alias, Term) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_write_term(Stream, Term, [numbervars(true), quoted(true)]).
writeq(Stream, Term) :-
   sys_write_term(Stream, Term, [numbervars(true), quoted(true)]).

/**
 * write_canonical(E): [ISO 8.14.2]
 * write_canonical(T, E): [ISO 8.14.2]
 * The predicate writes the term E to the standard output whereby
 * quoting atoms and variables if necessary and ignoring operator
 * declarations. The binary predicate takes an additional text stream
 * sink as argument.
 */
% write_canonical(+Term)
:- public write_canonical/1.
write_canonical(Term) :-
   current_output(Stream),
   sys_write_term(Stream, Term, [quoted(true), ignore_ops(true)]).

% write_canonical(+AliasOrStream, +Term)
:- public write_canonical/2.
write_canonical(Alias, Term) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_write_term(Stream, Term, [quoted(true), ignore_ops(true)]).
write_canonical(Stream, Term) :-
   sys_write_term(Stream, Term, [quoted(true), ignore_ops(true)]).

/**
 * write_term(E, O): [ISO 8.14.2]
 * write_term(T, E, O): [ISO 8.14.2]
 * The predicate writes the term E to the standard output taking
 * into account the write options O. The ternary predicate takes
 * an additional text stream sink as argument. For a list of options
 * see the API documentation.
 */
% write_term(+Term, +Term)
:- public write_term/2.
write_term(Term, Opt) :-
   current_output(Stream),
   sys_write_term(Stream, Term, Opt).

% write_term(+AliasOrStream, +Term, +Term)
:- public write_term/3.
write_term(Alias, Term, Opt) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_write_term(Stream, Term, Opt).
write_term(Stream, Term, Opt) :-
   sys_write_term(Stream, Term, Opt).

:- private sys_write_term/3.
:- foreign(sys_write_term/3, 'ForeignTerm',
      sysWriteTerm('Interpreter', 'Writer', 'AbstractTerm', 'Object')).

/**
 * read(E): [ISO 8.14.1]
 * read(T, E): [ISO 8.14.1]
 * The unary predicate reads a sentence from the standard input and parses
 * it into a Prolog term. The sentence consists of the tokens up to the first
 * terminating period (“.”). When the sentence only contains filler without a
 * terminating period (“.”) then the predicate succeeds when E unifies with the
 * end_of_file atom. Otherwise the predicate succeeds when E unifies with the
 * parsed term. The binary predicate takes an additional text stream source
 * as argument.
 */
% read(-Term)
:- public read/1.
read(Term) :-
   current_input(Stream),
   sys_read_term(Stream, Term).

% read(+AliasOrStream, -Term)
:- public read/2.
read(Alias, Term) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_read_term(Stream, Term).
read(Stream, Term) :-
   sys_read_term(Stream, Term).

:- private sys_read_term/2.
:- foreign(sys_read_term/2, 'ForeignTerm',
      sysReadTerm('Interpreter', 'Reader')).

/**
 * read_term(E, O): [ISO 8.14.1]
 * read_term(T, E, O): [ISO 8.14.1]
 * The predicate reads the term E from the standard input taking into account
 * the read options O. The ternary predicate takes an additional text stream
 * source as argument. For a list of options see the API documentation.
 */
% read_term(-Term, +Term)
:- public read_term/2.
read_term(Term, Opt) :-
   current_input(Stream),
   sys_read_term(Stream, Opt, Term).

% read_term(+AliasOrStream, -Term, +Term)
:- public read_term/3.
read_term(Alias, Term, Opt) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_read_term(Stream, Opt, Term).
read_term(Stream, Term, Opt) :-
   sys_read_term(Stream, Opt, Term).

:- private sys_read_term/3.
:- foreign(sys_read_term/3, 'ForeignTerm',
      sysReadTerm('Interpreter', 'Reader', 'Object')).