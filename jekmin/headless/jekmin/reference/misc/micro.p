/**
 * We provide a couple of additional variables predicates. Prolog already
 * provides an existential quantification operator in the form of (^)/2.
 * This operator is only needed in certain circumstance such as the
 * frequent predicates from the module abstract or the module bags.
 *
 * Example:
 * ?- sys_term_kernel(X#p(X,Y),K).
 * K = p(X,Y)
 * ?- sys_term_globals(X#p(X,Y),L).
 * L = [Y]
 *
 * We introduce a further operator. We use the form (#)/2 for the
 * universal quantification opera-tor. For performance reasons we
 * have introduced the predicates sys_term_kernel/2 and sys_term_globals/2
 * which are the analogues to the corresponding goal predicates.
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

:- package(library(jekmin/reference/misc)).
:- use_package(foreign(jekmin/reference/misc)).

:- module(struc, []).

/**
 * sys_term_kernel(G, K):
 * The predicate succeeds when K unifies with the kernel of the goal G.
 */
:- public sys_term_kernel/2.
:- special(sys_term_kernel/2, 'SpecialStruc', 0).

/**
 * sys_term_globals(G, L):
 * The predicate succeeds when L unifies with the global variables of the goal G.
 */
:- public sys_term_globals/2.
:- special(sys_term_globals/2, 'SpecialStruc', 1).
