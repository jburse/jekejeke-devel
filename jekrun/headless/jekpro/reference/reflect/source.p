/**
 * The Prolog system maintains a table of consulted source files
 * in each knowledge base. The entries in this table have a longer
 * life cycle than the entries in the stream alias table. When a
 * named stream is closed, the alias is removed from the alias table
 * and thus the stream properties are not anymore accessible via
 * the alias. On the other hand when a source file is closed after
 * consult, the entry in the source file table is not removed.
 *
 * For a verbose consult, ensure loaded or unload the source also
 * keeps a property with the timing of the operation. The timing is
 * suspended when a source consults or ensure loads another source,
 * so that the property only shows the time spent in the given source.
 * Further a source records the other sources that this source has
 * been consulted or ensure loaded. This way a graph of the sources
 * and their dependency is built.
 *
 * Each source has an individual lock. This lock serializes the consult,
 * ensure loaded and unload of source by multiple threads. If multiple
 * threads access sources that depend on each other this locking might
 * cause a deadlock, since we do not the release the lock inbetween
 * when a source consults or ensure loadeds an other source. Deadlock
 * detection can be controlled by a timeout parameter.
 *
 * The source dependency graph is allowed to have cycles. During
 * consult or ensure loaded a thread only processes each source once,
 * so that there is no danger of duplicate imports or an infinite
 * import loop. The dependency graph is also used by a mark and sweep
 * algorithm to unload unused sources. The user source does play an
 * important role here, it determines the root for the marking of
 * the sources.
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

:- sys_get_context(here, C),
   set_source_property(C, use_package(foreign(jekpro/reference/reflect))).
:- sys_get_context(here, C),
   reset_source_property(C, sys_source_visible(public)).

/****************************************************************/
/* Source Handling                                              */
/****************************************************************/

/**
 * current_source(S):
 * The predicate succeeds for the user absolute source paths S.
 */
% current_source(-+Pin)
:- static current_source/1.
current_source(X) :-
   ground(X), !,
   sys_current_source_chk(X).
current_source(X) :-
   sys_current_source(L),
   sys_member(X, L).
:- set_predicate_property(current_source/1, visible(public)).

:- special(sys_current_source/1, 'SpecialSource', 0).
:- set_predicate_property(sys_current_source/1, visible(private)).

:- special(sys_current_source_chk/1, 'SpecialSource', 1).
:- set_predicate_property(sys_current_source_chk/1, visible(private)).

/**
 * source_property(S, P):
 * The predicate succeeds for the properties P of the source path S.
 */
% source_property(+-Pin, -Property)
source_property(I, R) :-
   var(R), !,
   sys_source_property(I, P),
   sys_member(R, P).
source_property(I, R) :-
   functor(R, F, A),
   sys_source_property_chk(I, F/A, P),
   sys_member(R, P).
:- set_predicate_property(source_property/2, visible(public)).

:- special(sys_source_property/2, 'SpecialSource', 2).
:- set_predicate_property(sys_source_property/2, visible(private)).

:- special(sys_source_property_chk/3, 'SpecialSource', 3).
:- set_predicate_property(sys_source_property_chk/3, visible(private)).

/**
 * set_source_property(S, P):
 * The predicate assigns the property P to the source path S.
 */
% set_source_property(+Pin, +Property)
% already defined in special
% :- special(set_source_property/2, 'SpecialSource', 4).

/**
 * reset_source_property(S, P):
 * The predicate deassigns the property P from the source path S.
 */
% reset_source_property(+Pin, +Property)
% already defined in special
% :- special(reset_source_property/2, 'SpecialSource', 5).

/****************************************************************/
/* Resource Handling                                            */
/****************************************************************/

/**
 * current_resource(S):
 * The predicate succeeds for the user absolute resource paths S.
 */
% current_resource(-+Pin)
current_resource(X) :-
   sys_current_resource(L),
   sys_member(X, L).
:- set_predicate_property(current_resource/1, visible(public)).

:- special(sys_current_resource/1, 'SpecialSource', 6).
:- set_predicate_property(sys_current_resource/1, visible(private)).
