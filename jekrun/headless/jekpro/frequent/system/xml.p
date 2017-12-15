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
 * The rest of the predicates deal with reading/writing a DOM models
 * and accessing/modifying a DOM model. When reading/writing a DOM
 * model the retain flag indicates whether only tags (0) or tags and
 * text (1) should be read or written. A DOM model is referenced by a
 * Prolog reference data type and automatically reclaimed by the Java GC.
 *
 * Examples:
 * ?- open('data.html', read, S), elem_new(D),
 *    node_load(D, S, []), close(S), assertz(my_data(D)).
 * S = 0r22126bf,
 * D = 0r682136cf
 * ?- my_data(D), current_output(S), node_store(D, S, '', []).
 * &lt;parent foo=123&gt;
 *     &lt;child bar="alfa"/&gt;
 *     &lt;child bar="beta"/&gt;
 * &lt;/parent&gt;
 * &lt;parent foo=456/&gt;
 * D = 0r682136cf,
 * S = 0r3fc82f6e
 *
 * The predicates elem_attr/2 and elem_child/2 return their results by
 * backtracking and they use a logical cursor. The DOM model is thus
 * similarly accessed as the facts and rules from a Prolog knowledge base.
 * It is also supported that the DOM model is accessed and modified
 * concurrently by multiple threads.
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

:- package(library(jekpro/frequent/system)).
:- use_package(foreign(matula/util/system)).
:- use_package(foreign(matula/util/format)).
:- use_package(foreign(java/io)).
:- use_package(foreign(jekpro/frequent/system)).
:- use_package(foreign(jekpro/tools/call)).

:- module(xml, []).

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
/* Node Access & Modification                                      */
/*******************************************************************/

/**
 * node_load(D, S, O):
 * The predicate succeeds in loading the stream S into the DOM
 * node D with the DOM options O. For a list of options see
 * the API documentation.
 */
% node_load(+DomNode, +Stream, +List)
:- public node_load/3.
:- foreign(node_load/3, 'ForeignDom', sysNodeLoad('DomNode','Reader','Object')).

/**
 * node_store(D, S, C, O):
 * The predicate succeeds in storing the DOM node D into the
 * stream S with comment C and the DOM options O. For a list of
 * options see the API documentation.
 */
% node_store(+DomNode, +Stream, +Atom, +List)
:- public node_store/4.
:- foreign(node_store/4, 'ForeignDom', sysNodeStore('DomNode','Writer','String','Object')).

/**
 * node_get_parent(D, C):
 * The predicate succeeds in C with the parent of the DOM node D.
 */
% node_get_parent(+DomNode, -DomElement)
:- public node_get_parent/2.
:- virtual node_get_parent/2.
:- foreign(node_get_parent/2, 'DomNode', getParent).

/**
 * node_is_elem(D):
 * The predicate succeeds if the DOM node D is a DOM element.
 */
% node_is_elem(+DomNode)
:- public node_is_elem/1.
:- foreign(node_is_elem/1, 'ForeignDom', sysNodeIsElem('DomNode')).

/**
 * node_is_text(D):
 * The predicate succeeds if the DOM node D is a DOM text.
 */
% node_is_text(+DomNode)
:- public node_is_text/1.
:- foreign(node_is_text/1, 'ForeignDom', sysNodeIsText('DomNode')).

/*******************************************************************/
/* Element Access & Modification                                   */
/*******************************************************************/

/**
 * elem_new(D):
 * The predicate succeeds in D with a new DOM element.
 */
% elem_new(-DomElement)
:- public elem_new/1.
:- foreign_constructor(elem_new/1, 'DomElement', new).

/**
 * elem_get_name(D, N):
 * The predicate succeeds in N with the name of the DOM element D.
 */
% elem_get_name(+DomElement, -Atom)
:- public elem_get_name/2.
:- virtual elem_get_name/2.
:- foreign(elem_get_name/2, 'DomElement', getName).

/**
 * elem_set_name(D, N):
 * The predicate succeeds in setting the name of the DOM element D to N.
 */
% elem_set_name(+DomElement, +Atom)
:- public elem_set_name/2.
:- virtual elem_set_name/2.
:- foreign(elem_set_name/2, 'DomElement', setName('String')).

/**
 * elem_get_attr(D, A, V):
 * The predicate succeeds in V with the attribute A of the DOM element D.
 */
% elem_get_attr(+DomElement, +Atom, -Atom)
:- public elem_get_attr/3.
:- foreign(elem_get_attr/3, 'ForeignDom', sysGetElemAttr('DomElement','String')).

/**
 * elem_set_attr(D, A, V):
 * The predicate succeeds in setting the attribute A of the DOM element D to V.
 */
% elem_set_attr(+DomElement, +Atom, +Atom)
:- public elem_set_attr/3.
:- foreign(elem_set_attr/3, 'ForeignDom', sysSetElemAttr('DomElement','String','Object')).

/**
 * elem_remove_attr(D, A):
 * The predicate succeeds in removing the attribute A from the DOM element D.
 */
% elem_remove_attr(+DomElement, +Atom)
:- public elem_remove_attr/2.
:- virtual elem_remove_attr/2.
:- foreign(elem_remove_attr/2, 'DomElement', removeAttr('String')).

/**
 * elem_attr(D, A):
 * The predicate succeeds in A for all the attribute names of the DOM element D.
 */
% elem_attr(+DomElement, -Atom)
:- public elem_attr/2.
:- foreign(elem_attr/2, 'ForeignDom', sysElemAttr('CallOut','DomElement')).

/**
 * elem_add_child(D, C):
 * The predicate succeeds in adding the child C to the DOM element D.
 */
% elem_add_child(+DomElement, +DomNode)
:- public elem_add_child/2.
:- virtual elem_add_child/2.
:- foreign(elem_add_child/2, 'DomElement', addChild('DomNode')).

/**
 * elem_remove_child(D, C):
 * The predicate succeeds in removing the child C from the DOM element D.
 */
% elem_remove_child(+DomElement, +DomNode)
:- public elem_remove_child/2.
:- virtual elem_remove_child/2.
:- foreign(elem_remove_child/2, 'DomElement', removeChild('DomNode')).

/**
 * elem_child(D, C):
 * The predicate succeeds in C for all the children of the DOM element D.
 */
% elem_child(+DomElement, -DomNode)
:- public elem_child/2.
:- foreign(elem_child/2, 'ForeignDom', sysElemChild('CallOut','DomElement')).

/*******************************************************************/
/* Text Access & Modification                                      */
/*******************************************************************/

/**
 * text_new(D):
 * The predicate succeeds in D with a new DOM text.
 */
% text_new(-DomText)
:- public text_new/1.
:- foreign_constructor(text_new/1, 'DomText', new).

/**
 * text_get_data(D, T):
 * The predicate succeeds in T with the data of the DOM text D.
 */
% text_get_data(+DomText, -Atom)
:- public text_get_data/2.
:- virtual text_get_data/2.
:- foreign(text_get_data/2, 'DomText', getData).

/**
 * text_set_data(D, T):
 * The predicate succeeds in setting the data of the DOM text D to T.
 */
% text_set_data(+DomText, +Atom)
:- public text_set_data/2.
:- virtual text_set_data/2.
:- foreign(text_set_data/2, 'DomText', setData('String')).
