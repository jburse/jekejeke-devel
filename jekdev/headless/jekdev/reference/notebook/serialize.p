/**
 * The rest of the predicates deal with reading/writing a DOM model.
 * The predicate node_read/[2,3] can be used to load a DOM model from a
 * stream. The loading requires an already existing DOM node, which
 * is then overwritten. The predicate node_write/[2,3] can be used to store
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

:- package(library(jekdev/reference/notebook)).
:- use_package(foreign(jekdev/reference/notebook)).
:- use_package(foreign(matula/util/system)).
:- use_package(foreign(matula/text/format)).
:- use_package(foreign(java/io)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(library(matula/text/format)).

:- module(serialize, []).
:- use_module(library(notebook/model)).
:- sys_load_resource(library(dom)).
:- sys_add_resource(library(dom)).

/*******************************************************************/
/* XML Format                                                      */
/*******************************************************************/

/**
 * node_read(S, N):
 * node_read(S, N, O):
 * The predicate succeeds in loading the stream S into the DOM
 * node N with the DOM options O. For a list of options see
 * the API documentation.
 */
% node_read(+Stream, -AbstractDom)
:- public node_read/2.
node_read(Stream, Node) :-
   node_read(Stream, Node, []).

% node_read(+Stream, -AbstractDom, +List)
:- public node_read/3.
node_read(Alias, Node, Opt) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   elem_new(Node),
   elem_set_name(Node, array),
   sys_node_read(Stream, Opt, Node).
node_read(Stream, Node, Opt) :-
   elem_new(Node),
   elem_set_name(Node, array),
   sys_node_read(Stream, Opt, Node).

% sys_node_read(+Stream, +List, +AbstractDom)
:- private sys_node_read/3.
:- foreign(sys_node_read/3, 'ForeignSerialize',
      sysNodeRead('Interpreter','Reader','Object','AbstractDom')).

/**
 * node_write(S, N):
 * node_write(S, N, O):
 * The predicate succeeds in storing the DOM node N into the
 * stream S and the DOM options O. For a list of
 * options see the API documentation.
 */
% node_write(+Stream, +AbstractDom)
:- public node_write/2.
node_write(Stream, Node) :-
   node_write(Stream, Node, []).

% node_write(+Stream, +AbstractDom, +List)
:- public node_write/3.
node_write(Alias, Node, Opt) :-
   atom(Alias), !,
   sys_get_alias(Alias, Stream),
   sys_node_write(Stream, Opt, Node).
node_write(Stream, Node, Opt) :-
   sys_node_write(Stream, Opt, Node).

% node_write(+Stream, +List, +AbstractDom)
:- private sys_node_write/3.
:- foreign(sys_node_write/3, 'ForeignSerialize',
      sysNodeWrite('Writer','Object','AbstractDom')).

/**
 * node_term(N, T):
 * node_term(N, T, O):
 * If N is a variable then the predicate succeeds when N unifies with
 * the DOM of T. Otherwise the predicate succeeds when T unifies
 * with the term N.
 */
% node_term(-+AbstractDom, +-Term)
:- public node_term/2.
node_term(Node, Term) :-
   node_term(Node, Term, []).

% node_term(-+AbstractDom, +-Term, +List)
:- public node_term/3.
node_term(Node, Term, Opt) :-
   var(Node), !,
   node_term2(Term, Node, Opt).
node_term(Node, Term, Opt) :-
   sys_member(format(xml), Opt), !,
   xml_get_term(Node, Term).
node_term(Node, Term, _) :-
   json_get_term(Node, Term).

% node_term2(+Term, -AbstractDom, +List)
:- private node_term2/3.
node_term2(Term, Node, Opt) :-
   sys_member(format(xml), Opt), !,
   xml_set_term(Term, Node).
node_term2(Term, Node, _) :-
   json_set_term(Term, Node).

/***************************************************************/
/* XML Externalize                                             */
/***************************************************************/

% xml_get_term(+Node, -Value)
:- private xml_get_term/2.
xml_get_term(N, R) :-
   node_is_text(N), !,
   text_get_data(N, X),
   xml_get_data(X, R).
xml_get_term(N, K) :-
   elem_get_name(N, D),
   findall(K, elem_attr(N, K), L),
   xml_get_object(L, N, H),
   findall(M, elem_node(N, M), R),
   xml_get_array(R, J),
   K = element(D,H,J).

% xml_get_object(+List, +Node, -Object)
:- private xml_get_object/3.
xml_get_object([], _, []).
xml_get_object([K|J], N, [P|Q]) :-
   xml_get_pair(N, K, P),
   xml_get_object(J, N, Q).

% xml_get_pair(+Node, +Atom, -Pair)
:- private xml_get_pair/3.
xml_get_pair(N, K, K-V) :-
   elem_get_attr(N, K, M),
   xml_get_attr(M, V).

% xml_get_attr(+Node, -Value)
:- private xml_get_attr/2.
xml_get_attr(N, R) :-
   node_is_text(N), !,
   text_get_data(N, X),
   xml_get_value(X, R).
xml_get_attr(_, _) :-
   throw(error(syntax_error(dom_illegal_value),_)).

% xml_get_value(+Data, -Value)
:- private xml_get_value/2.
xml_get_value(X, R) :-
   atom(X), !,
   R = X.
xml_get_value(X, R) :-
   number(X), !,
   R = X.
xml_get_value(_, _) :-
   throw(error(syntax_error(dom_illegal_value),_)).

% xml_get_array(+List, -Array)
:- private xml_get_array/2.
xml_get_array([], []).
xml_get_array([N|M], [A|B]) :-
   xml_get_term(N, A),
   xml_get_array(M, B).

% xml_get_data(+Data, -Value)
:- private xml_get_data/2.
xml_get_data(X, R) :-
   atom(X), !,
   R = X.
xml_get_data(_, _) :-
   throw(error(syntax_error(dom_missing_text),_)).

/***************************************************************/
/* XML Internalize                                             */
/***************************************************************/

% xml_set_term(+Value, -Node)
:- private xml_set_term/2.
xml_set_term(X, _) :-
   var(X),
   throw(error(instantiation_error,_)).
xml_set_term(X, N) :-
   atom(X), !,
   text_new(N),
   text_set_data(N, X).
xml_set_term(element(D,L,R), N) :- !,
   elem_new(N),
   elem_set_name(N, D),
   xml_set_object(L, N),
   xml_set_array(R, N).
xml_set_term(_, _) :-
   throw(error(syntax_error(dom_missing_elem),_)).

% xml_set_object(+Object, +Node)
:- private xml_set_object/2.
xml_set_object(X, _) :-
   var(X),
   throw(error(instantiation_error,_)).
xml_set_object([], _) :- !.
xml_set_object([A|B], N) :- !,
   xml_set_pair(A, N, H),
   node_get_key(H, K),
   elem_set_attr(N, K, H),
   xml_set_object(B, N).
xml_set_object(_, _) :-
   throw(error(syntax_error(dom_missing_end),_)).

% xml_set_array(+Array, +Node)
:- private xml_set_array/2.
xml_set_array(X, _) :-
   var(X),
   throw(error(instantiation_error,_)).
xml_set_array([], _) :- !.
xml_set_array([A|B], N) :- !,
   xml_set_term(A, H),
   elem_add_node(N, H),
   xml_set_array(B, N).
xml_set_array(_, _) :-
   throw(error(syntax_error(dom_missing_end),_)).

% xml_set_pair(+Pair, +Node, -Node)
:- private xml_set_pair/3.
xml_set_pair(X, _, _) :-
   var(X),
   throw(error(instantiation_error,_)).
xml_set_pair(X-_, H, _) :-
   elem_get_attr(H, X, _),
   throw(error(syntax_error(xml_duplicate_attr),_)).
xml_set_pair(X-Y, _, N) :- !,
   xml_set_attr(Y, N),
   node_set_key(N, X).
xml_set_pair(_, _, _) :-
   throw(error(syntax_error(xml_illegal_attr),_)).

% xml_set_attr(+Value, -Node)
:- private xml_set_attr/2.
xml_set_attr(X, _) :-
   var(X),
   throw(error(instantiation_error,_)).
xml_set_attr(X, N) :-
   atom(X), !,
   text_new(N),
   text_set_data(N, X).
xml_set_attr(X, N) :-
   number(X), !,
   text_new(N),
   text_set_data(N, X).
xml_set_attr(_, _) :-
   throw(error(syntax_error(dom_illegal_value),_)).

/***************************************************************/
/* JSON Externalize                                            */
/***************************************************************/

% json_get_term(+Node, -Value)
:- private json_get_term/2.
json_get_term(N, R) :-
   node_is_text(N), !,
   text_get_data(N, X),
   json_get_data(X, R).
json_get_term(N, R) :-
   elem_get_name(N, D),
   json_get_name(D, N, R).

% json_get_name(+Name, +Node, -Value)
:- private json_get_name/3.
json_get_name(object, N, R) :- !,
   findall(K, elem_attr(N, K), L),
   json_get_object(L, N, H),
   R = object(H).
json_get_name(array, N, R) :- !,
   findall(M, elem_node(N, M), L),
   json_get_array(L, H),
   R = array(H).
json_get_name(_, _, _) :-
   throw(error(syntax_error(json_element_missing),_)).

% json_get_object(+List, +Node, -Object)
:- private json_get_object/3.
json_get_object([], _, []).
json_get_object([K|J], N, [P|Q]) :-
   json_get_pair(N, K, P),
   json_get_object(J, N, Q).

% json_get_pair(+Node, +Atom, -Pair)
:- private json_get_pair/3.
json_get_pair(N, K, K-V) :-
   elem_get_attr(N, K, M),
   json_get_term(M, V).

% json_get_array(+List, -Array)
:- private json_get_array/2.
json_get_array([], []).
json_get_array([N|M], [A|B]) :-
   json_get_term(N, A),
   json_get_array(M, B).

% json_get_data(+Data, -Value)
:- private json_get_data/2.
json_get_data(X, R) :-
   atom(X), !,
   R = X.
json_get_data(X, R) :-
   number(X), !,
   R = X.
json_get_data(_, _) :-
   throw(error(syntax_error(json_element_missing),_)).

/***************************************************************/
/* JSON Internalize                                            */
/***************************************************************/

% json_set_term(+Value, -Node)
:- private json_set_term/2.
json_set_term(X, _) :-
   var(X),
   throw(error(instantiation_error,_)).
json_set_term(X, N) :-
   atom(X), !,
   text_new(N),
   text_set_data(N, X).
json_set_term(X, N) :-
   number(X), !,
   text_new(N),
   text_set_data(N, X).
json_set_term(object(L), N) :- !,
   elem_new(N),
   elem_set_name(N, object),
   json_set_object(L, N).
json_set_term(array(L), N) :- !,
   elem_new(N),
   elem_set_name(N, array),
   json_set_array(L, N).
json_set_term(_, _) :-
   throw(error(syntax_error(json_element_missing),_)).

% json_set_object(+Object, +Node)
:- private json_set_object/2.
json_set_object(X, _) :-
   var(X),
   throw(error(instantiation_error,_)).
json_set_object([], _) :- !.
json_set_object([A|B], N) :- !,
   json_set_pair(A, N, H),
   node_get_key(H, K),
   elem_set_attr(N, K, H),
   json_set_object(B, N).
json_set_object(_, _) :-
   throw(error(syntax_error(json_unblanced_object),_)).

% json_set_array(+Array, +Node)
:- private json_set_array/2.
json_set_array(X, _) :-
   var(X),
   throw(error(instantiation_error,_)).
json_set_array([], _) :- !.
json_set_array([A|B], N) :- !,
   json_set_term(A, H),
   elem_add_node(N, H),
   json_set_array(B, N).
json_set_array(_, _) :-
   throw(error(syntax_error(json_unblanced_array),_)).

% json_set_pair(+Pair, +Node, -Node)
:- private json_set_pair/3.
json_set_pair(X, _, _) :-
   var(X),
   throw(error(instantiation_error,_)).
json_set_pair(X-_, H, _) :-
   elem_get_attr(H, X, _),
   throw(error(syntax_error(json_duplicate_key),_)).
json_set_pair(X-Y, _, N) :- !,
   json_set_term(Y, N),
   node_set_key(N, X).
json_set_pair(_, _, _) :-
   throw(error(syntax_error(json_colon_missing),_)).
