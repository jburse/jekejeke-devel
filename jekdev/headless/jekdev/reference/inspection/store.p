/**
 * Since recently we have introduce hierarchical knowledge bases.
 * They are already used in the Swing GUI, but not in the Android GUI.
 * Every Swing console window runs in its own sub knowledge base which
 * provides a separate class loader. The current knowledge base stack
 * can be listed by the store/0 command:
 *
 * Example, in Swing GUI:
 * ?- stores.
 * Store-1
 * Store-0
 *
 * Example, in Android GUI:
 * ?- stores.
 * Store-0
 *
 * Knowledge base properties can be query by the predicate
 * store_property/2. The predicates set_store_property/2 and
 * reset_store_property/2 serve updating knowledge base properties.
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
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/tools/term)).

:- module(store, []).
:- use_module(library(system/thread)).

/**
 * store_property(F, P):
 * The predicate succeeds for the properties P of the store F.
 */
% store_property(+Frame, -Property)
:- public store_property/2.
store_property(I, R) :-
   var(R), !,
   sys_store_property(I, P),
   sys_member(R, P).
store_property(I, R) :-
   functor(R, F, A),
   sys_store_property_chk(I, F/A, P),
   sys_member(R, P).

:- private sys_store_property/2.
:- foreign(sys_store_property/2, 'ForeignStore',
      sysStoreProperty('Interpreter','Knowledgebase')).

:- private sys_store_property_chk/3.
:- foreign(sys_store_property_chk/3, 'ForeignStore',
      sysStorePropertyChk('Interpreter','Knowledgebase','Object')).

/**
 * set_store_property(S, Q):
 * The predicate assigns the property Q to the store S.
 */
% set_store_property(+Oper, +Property)
:- public set_store_property/2.
:- foreign(set_store_property/2, 'ForeignStore',
      sysSetStoreProperty('Interpreter','Knowledgebase','Object')).

/**
 * reset_store_property(S, Q):
 * The predicate de-assigns the property Q from the store S.
 */
% reset_store_property(+Oper, +Property)
:- public reset_store_property/2.
:- foreign(reset_store_property/2, 'ForeignStore',
      sysResetStoreProperty('Interpreter','Knowledgebase','Object')).

/**
 * stores:
 * The predicate lists the store chain of the current thread.
 */
% stores
:- public stores/0.
stores :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_store, Store),
   stores(Store).

% stores(+Store)
:- private stores/1.
stores(null) :- !.
stores(Store) :-
   store_property(Store, sys_name(N)),
   write(N), nl,
   store_property(Store, sys_parent(Store2)),
   stores(Store2).


