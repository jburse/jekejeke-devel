/**
 * By default the top-level shows the current unification equations.
 * An extension can show arbitrary constraints. It can do so by
 * defining further clauses for the multi-file predicates
 * sys_current_eq/2 and sys_unwrap_eq/3.
 *
 * The constraints that are related directly or indirectly to a term
 * can be retrieved by the predicate sys_term_eq_list/2. As a further
 * convenience the predicate call_residue/2 allows calling a goal and
 * retrieving the related constraints for each success.
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

:- package(library(jekpro/frequent/misc)).
:- use_package(foreign(jekpro/frequent/misc)).
:- use_package(foreign(jekpro/tools/call)).

:- module(residue, []).
:- use_module(library(advanced/sets)).
:- use_module(library(misc/residue)).

/***********************************************************/
/* New Constraint Display API                              */
/***********************************************************/

/**
 * sys_current_eq(V, H):
 * The predicate succeeds for each equation H with variables
 * wrapped that listens on the variable V. Constraint solvers
 * should extend this multi-file predicate.
 */
% sys_current_eq(+Var, -Handle)
:- public sys_current_eq/2.
:- multifile sys_current_eq/2.
:- static sys_current_eq/2.

/**
 * sys_unwrap_eq(H, I, O):
 * The predicate converts equation H with variables wrapped into
 * equations I with variables unwrapped. The list uses the end O.
 * Constraint solvers should extend this multi-file predicate.
 */
% sys_unwrap_eq(+Handle, -Goals, +Goals)
:- public sys_unwrap_eq/3.
:- multifile sys_unwrap_eq/3.
:- static sys_unwrap_eq/3.

/**
 * sys_eq_list(L):
 * The predicate unifies L with the list of constraints.
 */
% sys_eq_list(-Goals)
:- public sys_eq_list/1.
sys_eq_list(L) :-
   findall(E, (  sys_residue_attr(V),
                 sys_current_eq(V, E)), H),
   sys_distinct(H, J),
   sys_unwrap_eqs(J, L, []).

% sys_unwrap_eqs(+Goals, -Goals, +Goals)
:- private sys_unwrap_eqs/3.
sys_unwrap_eqs([G|L], I, O) :-
   sys_unwrap_eq(G, I, H),
   sys_unwrap_eqs(L, H, O).
sys_unwrap_eqs([], L, L).

/**
 * call_residue(G, L):
 * The predicate succeeds whenever the goal G succeeds. The predicate
 * unifies L with the list of constraints, both shown and hidden.
 */
:- public call_residue/2.
:- meta_predicate call_residue(0,?).
call_residue(G, L) :-
   call(G),
   sys_eq_list(L).

/***********************************************************/
/* CAS Display Hook                                        */
/***********************************************************/

/**
 * printable(F, G):
 * The predicate succeeds in G with a custom form of F.
 */
% printable(+Term, -Term)
:- public printable/2.
printable(E, F) :-
   sys_printable_value(E, H), !,
   F = H.
printable(E, E).

/**
 * sys_printable_value(F, G):
 * The predicate succeeds in G with a custom form of F. The
 * predicate should be extended for custom forms.
 */
% sys_printable_value(+Term, -Term)
:- public sys_printable_value/2.
:- multifile sys_printable_value/2.
:- static sys_printable_value/2.

/**
 * surrogate_new(K):
 * Creates a new surrogate key K.
 */
% surrogate_new(-Ref)
:- public surrogate_new/1.
:- foreign_constructor(surrogate_new/1, 'Object', new).

/**
 * sys_residue_attr(V):
 * The predicate succeeds in V with the residue attributed variables.
 */
:- public sys_residue_attr/1.
:- foreign(sys_residue_attr/1, 'ForeignResidue',
      sysResidueAttr('CallOut','Interpreter')).
