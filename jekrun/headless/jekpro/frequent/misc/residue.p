/**
 * By default the top-level shows the current unification equations.
 * An extension can show arbi-trary constraints. It can do so by
 * efining further clauses for the multi-file predicates sys_current_eq/2
 * and sys_unwrap_eq/3.
 *
 * The predicate call_residue_var/2 can be used to determine the attributed
 * variables that were freshly introduced while executing a goal. As
 * a convenience, the predicate call_residue/2 will return the constraints
 * of these variables. The later predicate is useful for writing test
 * cases for uses of attributed variables.
 *
 * Terms that are directly instantiated to a variable can be customized
 * by the multi-file predicate sys_printable_value/2 and queried by the
 * predicate printable/2. The former predicate should fail if there is
 * no custom form and the later predicate will then return the original.
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
:- use_package(foreign(jekpro/model/molec)).

:- module(residue, []).
:- use_module(library(advanced/sets)).
:- use_module(library(misc/residue)).

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

/***********************************************************/
/* Constraint Retrieval API                                */
/***********************************************************/

/**
 * call_residue_vars(G, L):
 * The predicate succeeds whenever the goal G succeeds and unifies L
 * with the newly introduced attributed variables.
 */
:- public call_residue_vars/2.
:- meta_predicate call_residue_vars(0, ?).
call_residue_vars(G, L) :-
   sys_current_mark(M),
   call(G),
   sys_mark_attrs(M, L).

% sys_current_mark(-Undo)
:- public sys_current_mark/1.
:- foreign(sys_current_mark/1, 'ForeignResidue', 
      sysCurrentMark('Interpreter')).

% sys_mark_attrs(+Undo, -List)
:- public sys_mark_attrs/2.
:- foreign(sys_mark_attrs/2, 'ForeignResidue', 
      sysMarkAttrs('Interpreter', 'AbstractUndo')).

/**
 * call_residue(G, L):
 * The predicate succeeds whenever the goal G succeeds and unifies L
 * with the constraints of the newly introduced attributed variables.
 */
:- public call_residue/2.
:- meta_predicate call_residue(0, ?).
call_residue(G, L) :-
   call_residue_vars(G, K),
   sys_eq_list(K, L).

/**
 * sys_eq_list(K, L):
 * The predicate unifies L with the list of constraints
 * for the attributed variables K.
 */
% sys_eq_list(+List, -Goals)
:- private sys_eq_list/2.
sys_eq_list(K, L) :-
   findall(E, (  sys_member(V, K),
                 sys_current_eq(V, E)), H),
   sys_distinct(H, J),
   sys_unwrap_eqs(J, L, []).

% sys_unwrap_eqs(+Goals, -Goals, +Goals)
:- private sys_unwrap_eqs/3.
sys_unwrap_eqs([G|L], I, O) :-
   sys_unwrap_eq(G, I, H),
   sys_unwrap_eqs(L, H, O).
sys_unwrap_eqs([], L, L).

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

