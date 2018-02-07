/*
 * This module provides help utilities.
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
:- use_module(library(misc/text)).

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
apropos(P) :-
   pattern_compile(P, [boundary(part)], H),
   sys_apropos_table(N),
   sys_enum_apropos(N, I, M),
   sys_get_functor(I, F),
   compiled_match(H, F),
   ttywriteq(I),
   ttywrite('\t'),
   ttywrite(M), ttynl, fail.
apropos(_).
:- set_predicate_property(apropos/1, sys_notrace).

/**
 * sys_apropos_table(T):
 * The predicate succeeds in T with the file name of a apropos table.
 */
:- multifile sys_apropos_table/1.
:- public sys_apropos_table/1.
:- static sys_apropos_table/1.

% sys_get_functor(+Indicator, -Functor)
:- private sys_get_functor/2.
sys_get_functor(F/_, F).
sys_get_functor(_:F/_, F).

% sys_enum_apropos(+Atom, -Indicator, -Module)
:- private sys_enum_apropos/3.
sys_enum_apropos(N, I, M) :-
   setup_call_cleanup(
      open_resource(N, S),
      (  repeat,
         (  read_line(S, L)
         -> sys_split_line(L, H, T),
            term_atom(I, H),
            term_atom(M, T); !, fail)),
      close(S)).

% sys_split_line(+Atom, -Atom, -Atom)
:- private sys_split_line/3.
sys_split_line(L, H, T) :-
   sub_atom(L, P, 1, '\t'),
   sub_atom(L, 0, P, H),
   atom_length(L, N),
   Q is P+1,
   M is N-Q,
   sub_atom(L, Q, M, T).
