/*
 * This module provides help utilities. The predicate apropos/1
 * allows listing the advertised predicates. The lists are available
 * independent whether a module is loaded or not. The lists are taken
 * from the successfully activated and still registered capabilities.
 *
 * Example:
 * ?- apropos(time).
 * Indicator               Module
 * get_time/1              system/shell
 * get_time/2              system/shell
 * get_time_file/2         system/file
 * set_time_file/2         system/file
 * time_out/2              misc/time
 * time/1                  swing/stats
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

:- use_package(foreign(jekpro/frequent/basic)).

:- module(user, []).
:- use_module(library(stream/console)).
:- use_module(library(system/locale)).
:- use_module(library(misc/text)).
:- sys_load_resource(info).

/***********************************************************/
/* Apropos Utility                                         */
/***********************************************************/

/**
 * apropos(P):
 * The predicate succeeds in listing the public predicates on the
 * terminal that are advertised by the loaded capabilities and that
 * contain the given atom P in their name.
 */
% apropos(+Atom)
:- public apropos/1.
apropos(Pattern) :-
   sys_apropos_compile(Pattern, Compiled), sys_apropos_keys,
   sys_apropos_table(Table),
   sys_enum_apropos(Table, Row),
   sys_match_row(Compiled, Row),
   sys_apropos_values(Row), fail.
apropos(_).
:- set_predicate_property(apropos/1, sys_notrace).

% sys_apropos_keys
:- private sys_apropos_keys/0.
sys_apropos_keys :-
   sys_get_lang(info, P),
   sys_apropos_key(K),
   message_make(P, apropos_key(K), M),
   ttywrite(M), fail.
sys_apropos_keys :- ttynl.

% sys_apropos_values(+Row)
:- private sys_apropos_values/1.
sys_apropos_values(A) :-
   sys_get_lang(info, P),
   sys_apropos_key(K),
   sys_apropos_value(A, K, V),
   message_make(P, apropos_value(K,V), M),
   ttywrite(M), fail.
sys_apropos_values(_) :- ttynl.

% sys_apropos_key(-Atom)
:- private sys_apropos_key/1.
sys_apropos_key(pred).
sys_apropos_key(arith).
sys_apropos_key(path).

% sys_apropos_value(+Row, +Atom, -Term)
:- private sys_apropos_value/3.
sys_apropos_value(row(I,_,_), pred, I).
sys_apropos_value(row(_,E,_), arith, E).
sys_apropos_value(row(_,_,M), path, M).

/***********************************************************/
/* Apropos Search                                          */
/***********************************************************/

/**
 * sys_apropos_table(T):
 * The predicate succeeds in T with the file name of a apropos table.
 */
:- multifile sys_apropos_table/1.
:- public sys_apropos_table/1.
sys_apropos_table(library(bootload/reference)).
sys_apropos_table(library(stream/frequent)).

% sys_enum_apropos(+Atom, -Row)
:- private sys_enum_apropos/2.
sys_enum_apropos(N, row(I,E,M)) :-
   setup_call_cleanup(
      open_resource(N, S),
      (  repeat,
         (  read_line(S, L)
         -> atom_list_concat(U, '\t', L),
            sys_split_line(U, H, E, T),
            term_atom(I, H),
            term_atom(M, T); !, fail)),
      close(S)).

% sys_split_line(+List, -Atom, -Atom)
:- private sys_split_line/4.
% old apropos format
sys_split_line([H,T], H, undef, T).
% new apropos format
sys_split_line([H,E,T], H, E, T).

% sys_match_row(+Compiled, +Row)
:- private sys_match_row/2.
sys_match_row(Compiled, Row) :-
   sys_apropos_value(Row, pred, Indicator),
   sys_indicator_fun(Indicator, Fun, Arity),
   sys_apropos_match(Compiled, Fun/Arity).

% sys_indicator_fun(+Indicator, -Atom, -Integer)
:- private sys_indicator_fun/3.
sys_indicator_fun(Fun/Arity, Fun, Arity).
sys_indicator_fun(_:Fun/Arity, Fun, Arity).

/***********************************************************/
/* Compile and Match                                       */
/***********************************************************/

% sys_apropos_compile(+Pattern, -Compiled)
:- private sys_apropos_compile/2.
sys_apropos_compile(Pattern/Arity, Compiled/Arity) :- !,
   sys_apropos_compile2(Pattern, Compiled).
sys_apropos_compile(Pattern, Compiled) :-
   pattern_compile(Pattern, [boundary(part)], Compiled).

% sys_apropos_compile2(+Pattern, -Compiled)
:- private sys_apropos_compile2/2.
sys_apropos_compile2(Pattern, _) :-
   var(Pattern), !.
sys_apropos_compile2(Pattern, Compiled) :-
   pattern_compile(Pattern, [boundary(part)], Compiled).

% sys_apropos_match(+Compiled, +Instance)
:- private sys_apropos_match/2.
sys_apropos_match(Compiled/Arity1, Fun/Arity2) :- !,
   sys_apropos_match2(Compiled, Fun),
   Arity1 = Arity2.
sys_apropos_match(Compiled, Fun/_) :- !,
   compiled_match(Compiled, Fun).

% sys_apropos_match2(+Compiled, +Instance)
:- private sys_apropos_match2/2.
sys_apropos_match2(Compiled, _) :-
   var(Compiled), !.
sys_apropos_match2(Compiled, Fun) :-
   compiled_match(Compiled, Fun).
