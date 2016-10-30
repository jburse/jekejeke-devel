/**
 * The predicates assertable_ref/2 and assumable_ref/2 allow the
 * compilation of a clause without any thread contention. The clause
 * can be associated and de-associated with the head predicate via
 * the predicates recorda_ref/1, recordz_ref/1 and erase_ref/1 whereby
 * only one thread will win. A new instance of the original clause
 * can be retrieved again by the predicate compiled_ref/2.
 *
 * The predicate clause_ref/3 can be used to find a clause in the
 * knowledge base. This predicate respects the logical view approach
 * form the ISO core Prolog standard. The predicate will further filter
 * and only return those clauses that are visible from the head
 * predicate that is used in the search. The predicate additionally
 * returns clauses that can be used with the other predicates here.
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

:- package(library(jekpro/frequent/experiment)).
:- use_package(foreign(jekpro/frequent/experiment)).

:- module(ref, []).

/**
 * assertable_ref(C, R):
 * The predicate compiles the term C into a new clause reference R.
 * An undefined or unimplemented head predicate will be turned into a
 * dynamic predicate. Otherwise the head predicate must be dynamic
 * or thread local.
 */
:- public assertable_ref/2.
:- meta_predicate assertable_ref(-1,?).
:- special(assertable_ref/2, 'SpecialRef', 0).

/**
 * assumable_ref(C, R):
 * The predicate compiles the term C into a new clause reference R.
 * An undefined or unimplemented head predicate will be turned into a
 * thread local predicate. Otherwise the head must be dynamic
 * or thread local.
 */
:- public assumable_ref/2.
:- meta_predicate assumable_ref(-1,?).
:- special(assumable_ref/2, 'SpecialRef', 1).

/**
 * recorda_ref(R):
 * The predicate inserts the clause referenced by R at the top. The
 * predicate fails when the clause has already been recorded.
 */
:- public recorda_ref/1.
:- special(recorda_ref/1, 'SpecialRef', 2).

/**
 * recordz_ref(R):
 * The predicate inserts the clause referenced by R at the bottom. The
 * predicate fails when the clause has already been recorded.
 */
:- public recordz_ref/1.
:- special(recordz_ref/1, 'SpecialRef', 3).

/**
 * erase_ref(R):
 * The predicate removes the clause referenced by R. The predicate
 * fails when the clause has already been erased.
 */
:- public erase_ref/1.
:- special(erase_ref/1, 'SpecialRef', 4).

/**
 * compiled_ref(R, C):
 * The predicate returns a copy of the term C that was compiled
 * into the clause reference R.
 */
:- public compiled_ref/2.
:- meta_predicate compiled_ref(?,-1).
:- set_predicate_property(compiled_ref/2, sys_noexpand).
:- special(compiled_ref/2, 'SpecialRef', 5).

/**
 * clause_ref(H, B, R):
 * The predicate succeeds with the user clauses that match
 * H :- B and the clause reference R of the user clause. The
 * head predicate must be dynamic or thread local.
 */
% clause_ref(-Term, -Goal, -Ref)
:- public clause_ref/3.
:- meta_predicate clause_ref(-1,0,?).
:- set_predicate_property(clause_ref/3, sys_noexpand).
:- special(clause_ref/3, 'SpecialRef', 6).