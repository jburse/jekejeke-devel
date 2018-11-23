/**
 * This module provides a couple of simple utilities to deal with
 * the generation and parsing of XML texts. The predicate text_escape/2
 * can be used to escape and un-escape texts. The predicate text_escape/2
 * will escape the characters '"<>&' and the character 0xA0. It is
 * suitable for attribute values in double quotes and for texts.
 *
 * Examples:
 * ?- text_escape('&lt;abc&gt;', X).
 * X = '&amp;lt;abc&amp;gt;'
 * ?- text_escape(X, '&amp;lt;abc&amp;gt;').
 * X = '&lt;abc&gt;'
 *
 * The rest of the predicates deal with reading/writing a DOM model.
 * The predicate node_load/[2,3] can be used to load a DOM model from a
 * stream. The loading requires an already existing DOM node, which
 * is then overwritten. The predicate node_store/[2,3] can be used to store
 * a DOM model to a stream.
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

:- package(library(jekpro/frequent/system)).
:- use_package(foreign(matula/util/system)).
:- use_package(foreign(matula/util/format)).
:- use_package(foreign(java/io)).
:- use_package(foreign(jekpro/frequent/system)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(library(matula/util/format)).

:- module(json, []).
:- sys_load_resource(library(dom)).
:- sys_add_resource(library(dom)).

/****************************************************************/
/* Entity Conversion                                            */
/****************************************************************/

/**
 * text_escape(T, E):
 * If T is a variable then the predicate succeeds when T unifies with
 * the text unescape of E. Otherwise the predicate succeeds when E unifies
 * with the text escape of T.
 */
% text_escape(+-Atom, -+Atom)
:- public text_escape/2.
text_escape(X, Y) :-
   var(X), !,
   sys_text_unescape(Y, X).
text_escape(X, Y) :-
   sys_text_escape(X, Y).

:- private sys_text_escape/2.
:- foreign(sys_text_escape/2, 'ForeignXml',
      sysTextEscape('String')).

:- private sys_text_unescape/2.
:- foreign(sys_text_unescape/2, 'ForeignXml',
      sysTextUnescape('String')).

/*******************************************************************/
/* XML Format                                                      */
/*******************************************************************/

/**
 * node_load(S, N):
 * node_load(S, N, O):
 * The predicate succeeds in loading the stream S into the DOM
 * node N with the DOM options O. For a list of options see
 * the API documentation.
 */
% node_load(+Stream, +AbstractDom)
:- public node_load/2.
node_load(S, N) :-
   node_load(S, N, []).

% node_load(+Stream, +AbstractDom, +List)
:- public node_load/3.
:- foreign(node_load/3, 'ForeignJson',
      sysNodeLoad('Interpreter','Reader','AbstractDom','Object')).

/**
 * node_store(S, N):
 * node_store(S, N, O):
 * The predicate succeeds in storing the DOM node N into the
 * stream S and the DOM options O. For a list of
 * options see the API documentation.
 */
% node_store(+Stream, +AbstractDom)
:- public node_store/2.
node_store(S, N) :-
   node_store(S, N, []).

% node_store(+Stream, +AbstractDom, +List)
:- public node_store/3.
:- foreign(node_store/3, 'ForeignJson',
      sysNodeStore('Writer','AbstractDom','Object')).

