/**
 * The shape of the clause index depends on the call pattern history
 * of the predicate. We do not provide a programming interface to
 * selectively inspect the clause index. Instead the end-user can
 * dump the clause index for predicates in one go. Locking is used
 * so that no other thread can query or updated a shared predicate
 * during the dump. The following syntax is used to dump the
 * clause index:
 *
 * length=<len>: Gives the size of indexed clause set.
 * arg=<pos>: Gives the argument position that is indexed.
 * map=<size>: Gives the hash table size of the argument position.
 * <key>=: Gives the key and corresponding sub index.
 * hash=<index>: Gives the hash code module hash table size of the key.
 * nonguard: Gives the nonguard hash table miss fallback index.
 * guard: Gives the guard hash table miss fallback index.
 *
 * The detected call patterns can be read off from the detected argument
 * positions. The clause index need not follow a simple collection of
 * call patterns. Sub-indexes can have individual call patterns. Let’s
 * give a simple example:
 *
 * ?- [user].
 * p(7, a).
 * p(7, b).
 * p(9, c).
 * ^D
 * Yes
 * ?- p(7, a).
 * Yes
 *
 * The query will deterministically succeed. This is an indicative that
 * a clause index has been built that covers multiple arguments. Clause
 * indexing based on first argument indexing only would not be able
 * to detect this determinism. Although the clause index is multi
 * argument, it does so only for the key “7”:
 *
 * ?- dump(p/2).
 * -------- p/2 ---------
 * length=3
 * arg=0, map=4
 *   key=9, hash=1, length=1
 *   key=7, hash=3, length=2
 *     arg=1, map=4
 *       key=a, hash=1, length=1
 *       key=b, hash=2, length=1
 * Yes
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

:- package(library(jekdev/reference/debug)).
:- use_package(foreign(jekdev/reference/debug)).

:- module(dump, []).
:- use_module(library(inspection/provable)).

/**
 * dump:
 * The predicate dumps the clause index of the clauses of the
 * user predicates.
 */
% dump
:- public dump/0.
:- static dump/0.
:- set_predicate_property(dump/0, sys_nostack).
dump :-
   sys_parent_goal(G),
   sys_dump_site(G).
:- set_predicate_property(dump/0, sys_notrace).

% sys_dump_site(+Term):
:- private sys_dump_site/1.
:- special(sys_dump_site/1, 'SpecialDump', 0).

/**
 * dump(P):
 * The predicate dumps the clause index of the clauses of the
 * user predicate P.
 */
% dump(+Pattern)
:- public dump/1.
dump(I) :-
   ground(I), !,
   sys_dump(I).
dump(I) :-
   current_provable(I),
   \+ provable_property(I, built_in),
   sys_dump(I), fail.
dump(_).
:- set_predicate_property(dump/1, sys_notrace).

:- private sys_dump/1.
:- special(sys_dump/1, 'SpecialDump', 1).
