/**
 * Terms can be written to text streams or read from text streams
 * depending on the current operator definitions. It is possible to
 * switch off using the current operator definitions. Further during
 * writing we might want to put atoms and variables back into quotes
 * when necessary. It is possible to switch on quoting of atoms and
 * variables. Finally terms of the form ‘$VAR’(&lt;number&gt;) are usually
 * recognized and written out as &lt;var&gt;. It is also possible to switch
 * off this variable numbering:
 *
 * Table 13: Predefined Write Predicates
 * Predicate	numbervars	quoted	ignore_ops
 * write	Yes	No	No
 * writeq	No	Yes	No
 * write_canonical	No	Yes	Yes
 *
 * The spacing is determined by the context type option. The context type
 * ‘?’ minimizes the spacing. The other context types use spacing for the
 * current compound and they also determine which meta-declaration should
 * be looked up in case of a closure. Here are some examples whereby
 * we assume a meta_predicate declaration solve(0):
 *
 * Table 14: Context Dependent Spacing
 * Context	Example 1
 * ?	solve((_A,_B)):-solve(_A),solve(_B)
 * 0 or -1	solve((_A, _B)) :- solve(_A), solve(_B)
 *
 * If the format option is newline then the spacing is enhanced by new
 * lines and further spaces so that the output matches the Prolog coding
 * guidelines as published in [9]. Further the priority option determines
 * whether parentheses are set around operator expressions. Finally the
 * operand option determines whether parentheses are set around single
 * standing operators and some pathological left associative cases
 * already mentioned in the ISO standard.
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
   sys_write_term(Stream, Term, [quoted(false)]).

% write(+AliasOrStream, +Term)
:- public write/2.
write(Alias, Term) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_write_term(Stream, Term, [quoted(false)]).
write(Stream, Term) :-
   sys_write_term(Stream, Term, [quoted(false)]).

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
   sys_write_term(Stream, Term, []).

% writeq(+AliasOrStream, +Term)
:- public writeq/2.
writeq(Alias, Term) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_write_term(Stream, Term, []).
writeq(Stream, Term) :-
   sys_write_term(Stream, Term, []).

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
   sys_write_term(Stream, Term, [numbervars(false),ignore_ops(true)]).

% write_canonical(+AliasOrStream, +Term)
:- public write_canonical/2.
write_canonical(Alias, Term) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_write_term(Stream, Term, [numbervars(false),ignore_ops(true)]).
write_canonical(Stream, Term) :-
   sys_write_term(Stream, Term, [numbervars(false),ignore_ops(true)]).

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

:- foreign(sys_write_term/3, 'ForeignTerm',
      sysWriteTerm('Interpreter','Writer','AbstractTerm','Object')).

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
   sys_read_term(Stream, [], Term).

% read(+AliasOrStream, -Term)
:- public read/2.
read(Alias, Term) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_read_term(Stream, [], Term).
read(Stream, Term) :-
   sys_read_term(Stream, [], Term).

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
      sysReadTerm('Interpreter','Reader','Object')).
