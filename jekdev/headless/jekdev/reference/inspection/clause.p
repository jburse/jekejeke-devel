/**
 * A Prolog clause consists of a head and a body. Practical Prolog
 * systems store additional information in a clause. We provide
 * access to clause information via additional option lists. The
 * predicates asserta_opt/2 and assertz_opt/2 allow asserting
 * static clauses with clause information.
 *
 * If the predicate of the clause is undefined it will be turned
 * into a static predicate by default. If a dynamic or thread local
 * predicate is desired one has to explicitly call dynamic/1 or
 * thread_local/1 before asserting.
 *
 * A reference to a clause can be retrieved from a
 * frame reference via the predicate frame_property/2.
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

:- package(library(jekdev/reference/inspection)).
:- use_package(foreign(jekdev/reference/inspection)).

:- module(clause, []).

/**
 * asserta_opt(C, O):
 * The predicate inserts the clause C at the top with assertion
 * options O. The following assertion options O are available. For
 * list of options see the API documentation.
 */
% asserta_opt(+Clause, +Opts)
:- public asserta_opt/2.
:- meta_predicate asserta_opt(-1,?).
:- special(asserta_opt/2, 'SpecialClause', 0).

/**
 * assertz_opt(C, O):
 * The predicate inserts the clause C at the bottom with assertion
 * options O.
 */
% assertz_opt(+Clause, +Opts)
:- public assertz_opt/2.
:- meta_predicate assertz_opt(-1,?).
:- special(assertz_opt/2, 'SpecialClause', 1).
