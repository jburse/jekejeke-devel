/**
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

:- package(library(jekpro/frequent/experiment)).
:- use_package(foreign(matula/util/format)).
:- use_package(foreign(jekpro/frequent/experiment)).
:- use_package(foreign(jekpro/tools/call)).

:- module(dom, []).

/*******************************************************************/
/* Node Access & Modification                                      */
/*******************************************************************/

/**
 * node_is_elem(D):
 * The predicate succeeds if the DOM node D is a DOM element.
 */
% node_is_elem(+AbstractDom)
:- public node_is_elem/1.
:- foreign(node_is_elem/1, 'ForeignDom', sysNodeIsElem('AbstractDom')).

/**
 * node_is_text(D):
 * The predicate succeeds if the DOM node D is a DOM text.
 */
% node_is_text(+AbstractDom)
:- public node_is_text/1.
:- foreign(node_is_text/1, 'ForeignDom', sysNodeIsText('AbstractDom')).

/**
 * node_get_parent(D, C):
 * The predicate succeeds in C with the parent of the DOM node D.
 */
% node_get_parent(+AbstractDom, -DomElement)
:- public node_get_parent/2.
:- virtual node_get_parent/2.
:- foreign(node_get_parent/2, 'AbstractDom', getParent).

/**
 * node_get_key(D, K):
 * The predicate succeeds in K with the key name of the DOM node D.
 */
:- public node_get_key/2.
:- virtual node_get_key/2.
:- foreign(node_get_key/2, 'AbstractDom', getKey).

/**
 * node_set_key(D, K):
 * The predicate succeeds in setting the key name of the DOM node D to K.
 */
:- public node_set_key/2.
:- virtual node_set_key/2.
:- foreign(node_set_key/2, 'AbstractDom', setKey('String')).

/**
 * node_copy(D, C):
 * The predicate succeeds in C with a copy of the DOM node D.
 */
:- public node_copy/2.
:- foreign(node_copy/2, 'ForeignDom', sysNodeCopy('AbstractDom')).

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
:- foreign(text_get_data/2, 'ForeignDom', sysGetData('DomText')).

/**
 * text_set_data(D, T):
 * The predicate succeeds in setting the data of the DOM text D to T.
 */
% text_set_data(+DomText, +Atom)
:- public text_set_data/2.
:- foreign(text_set_data/2, 'ForeignDom', sysSetData('DomText','Object')).

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
:- virtual elem_get_attr/3.
:- foreign(elem_get_attr/3, 'DomElement', getAttrDom('String')).

/**
 * elem_set_attr(D, A, V):
 * The predicate succeeds in setting the attribute A of the DOM element D to V.
 */
% elem_set_attr(+DomElement, +Atom, +Atom)
:- public elem_set_attr/3.
:- virtual elem_set_attr/3.
:- foreign(elem_set_attr/3, 'DomElement', setAttrDom('String','AbstractDom')).

/**
 * elem_reset_attr(D, A):
 * The predicate succeeds in removing the attribute A from the DOM element D.
 */
% elem_reset_attr(+DomElement, +Atom)
:- public elem_reset_attr/2.
:- virtual elem_reset_attr/2.
:- foreign(elem_reset_attr/2, 'DomElement', resetAttr('String')).

/**
 * elem_attr(D, A):
 * The predicate succeeds in A for all the attribute names of the DOM element D.
 */
% elem_attr(+DomElement, -Atom)
:- public elem_attr/2.
:- foreign(elem_attr/2, 'ForeignDom', sysElemAttr('CallOut','DomElement')).

/**
 * elem_add_node(D, C):
 * The predicate succeeds in adding the DOM node C to the DOM element D.
 */
% elem_add_node(+DomElement, +AbstractDom)
:- public elem_add_node/2.
:- virtual elem_add_node/2.
:- foreign(elem_add_node/2, 'DomElement', addNode('AbstractDom')).

/**
 * elem_remove_node(D, C, I):
 * The predicate succeeds in removing the DOM node C from the DOM element D
 * returning the index I.
 */
% elem_remove_node(+DomElement, +AbstractDom, -Integer)
:- public elem_remove_node/3.
:- virtual elem_remove_node/3.
:- foreign(elem_remove_node/3, 'DomElement', removeNode('AbstractDom')).

/**
 * elem_node(D, C):
 * The predicate succeeds in C for all the DOM nodes of the DOM element D.
 */
% elem_node(+DomElement, -AbstractDom)
:- public elem_node/2.
:- foreign(elem_node/2, 'ForeignDom', sysElemNode('CallOut','DomElement')).
