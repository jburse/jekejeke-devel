/**
 * Attribute variables trigger during unification. From the viewpoint of
 * the interpreter attribute variables are simply variables. The main
 * functionality is that binding an attribute variable triggers the hooks
 * that are associated with the attribute variable. If a hook fails the
 * unification fails. If a hook succeeds the binding effects of the hook
 * are kept.
 *
 * Example:
 * hook(V, T) :-
 *    write('bind '), write(V),
 *    write(' to '), write(T), nl.
 *
 * A hook is just a closure that takes the attribute variable and the
 * term that is attempted to unify with the attribute variable. It can be
 * compiled with the predicate sys_compile_hook/3. The resulting reference
 * can be recorded and erased like a clause reference. The hooks of a
 * variable can be enumerated via the predicate sys_clause_hook/3.
 *
 * Example:
 * ?- sys_compile_hook(V, hook, R),
 *    recordz_ref(R), V = 99.
 * bind _A to 99
 * V = 99.
 *
 * In contrast to ordinary variables, attribute variables are reordered
 * during unification so that two attribute variables are only unified as
 * a last resort. As a result attribute variables are less often instantiated
 * than ordinary variables. The predicate sys_ensure_serno/1 can be used to
 * force the assignment of a serial number. The serial number of a variable
 * is used in writing and lexical comparison.
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

:- package(library(jekmin/reference/experiment)).
:- use_package(foreign(jekmin/reference/experiment)).

:- module(attr, []).

/**
 * sys_ensure_serno(V):
 * The predicate promotes the first argument to an attribute variable
 * when V is an ordinary variable. The predicate then ensures that
 * the first argument has a serial number.
 */
:- public sys_ensure_serno/1.
:- special(sys_ensure_serno/1, 'SpecialAttr', 0).

/**
 * sys_compile_hook(V, H, R):
 * The predicate promotes the first argument to an attribute variable
 * when V is an ordinary variable. The predicate then succeeds when
 * the compiled reference of the hook H unifies with R.
 */
:- public sys_compile_hook/3.
:- meta_predicate sys_compile_hook(?,2,?).
:- special(sys_compile_hook/3, 'SpecialAttr', 1).

/**
 * sys_clause_hook(V, H, R):
 * The predicate fails when V is an ordinary variable. Otherwise the
 * predicate succeeds for each hook H and reference R that unifies
 * with the hooks and references of the attribute variable V.
 */
:- public sys_clause_hook/3.
:- meta_predicate sys_clause_hook(?,2,?).
:- special(sys_clause_hook/3, 'SpecialAttr', 2).
