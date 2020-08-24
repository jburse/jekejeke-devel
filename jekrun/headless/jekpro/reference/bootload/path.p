/**
 * A relative path that is not wrapped into a compound is resolved
 * against the current base in write or append mode. The current base
 * is the Prolog flag “base_url”. Otherwise in read mode, if the call-site
 * is not user it is resolved against the path of the call-site itself.
 * In both cases the suffixes of the sys_add_file_extension/2
 * command are used:
 *
 * Example:
 * ?- set_prolog_flag(base_url, '/C:/Users/Jan Burse/Desktop/').
 *
 * ?- sys_add_file_extension('.dcg', [mime('text/prolog')]).
 *
 * ?- absolute_file_name('my folder/my file', X).
 * X = 'file:/C:/Users/Jan Burse/Desktop/my folder/my file.dcg'
 *
 * Paths should not use a system specific directory separator but
 * always use the forward slash (/). For convenience, paths have
 * an automatic prefixing of a schema. Paths starting with a double
 * slash (//) are prefixed by the “http” schema. Paths starting with
 * a single slash (/) are prefixed by the “file” schema. Drive
 * letters are not considered schema.
 *
 * If the path is wrapped into a compound and if the functor of the
 * compound is either library/1, foreign/1, verbatim/1 or resource/1
 * then the path is looked up in the class path. The class path can
 * be updated and queried by the predicates sys_add_path/1 and
 * sys_current_path/1. The prefixes of the package/1 and use_package/1
 * command are also used.
 *
 * Write or append access resolution:
 *   &lt;path&gt;              resolve &lt;path&gt; in base.
 *
 * Read access resolution:
 *   library(&lt;path&gt;)     lookup text &lt;path&gt; in class path.
 *   foreign(&lt;path&gt;)     lookup class &lt;path&gt; in class path.
 *   verbatim(&lt;path&gt;)    like library(&lt;path&gt;) or take as is.
 *   resource(&lt;path&gt;)    lookup resource &lt;path&gt; in class path.
 *   &lt;path&gt;              resolve &lt;path&gt; in scope or base.
 *
 * The predicates absolute_file_name/[2,3] provide file name resolving.
 * The predicate absolute_file_name/2 works bi-directionally. For a
 * given already resolved path it will make a best effort attempt
 * to reconstruct either a compound form foreign/1, library/1, verbatim/1
 * or resource/1. Otherwise, only a relative path is attempt.
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

:- callable_property(here, sys_context(C)),
   set_source_property(C, use_package(foreign(jekpro/reference/bootload))).
:- callable_property(here, sys_context(C)),
   set_source_property(C, use_package(foreign(jekpro/tools/call))).
:- callable_property(here, sys_context(C)),
   set_source_property(C, use_package(foreign(jekpro/tools/term))).
:- callable_property(here, sys_context(C)),
   set_source_property(C, use_package(foreign(matula/util/system))).
:- callable_property(here, sys_context(C)),
   reset_source_property(C, sys_source_visible(public)).

:- sys_neutral_oper(prefix(../)).
:- set_oper_property(prefix(../), op(200, fy)).
:- set_oper_property(prefix(../), sys_nspr).
:- set_oper_property(prefix(../), visible(public)).

/****************************************************************/
/* Class Path Modification & Access                             */
/****************************************************************/

/**
 * sys_add_path(R):
 * The predicate succeeds in adding the relative path R
 * to the current class loader.
 */
% sys_add_path(+Path)
:- foreign(sys_add_path/1, 'ForeignPath',
      sysAddClassdPath('Interpreter', 'String')).
:- set_predicate_property(sys_add_path/1, visible(public)).

/**
 * sys_current_path(A):
 * The predicate succeeds in A with the currently added
 * absolute paths A along the class loaders.
 */
% sys_current_path(-Path)
sys_current_path(Path) :-
   sys_get_class_paths(Paths),
   sys_member(Path, Paths).
:- set_predicate_property(sys_current_path/1, visible(public)).

:- foreign(sys_get_class_paths/1, 'ForeignPath',
      sysGetClassPaths('Interpreter')).
:- set_predicate_property(sys_get_class_paths/1, visible(private)).

/**
 * sys_add_file_extension(E, O):
 * The predicate succeeds in adding the file extension database
 * entry E with type and mime options O to the current knowledge base.
 * For a list of recognized mime options see the API documentation.
 */
:- foreign(sys_add_file_extension/2, 'ForeignPath',
      sysAddFileExtenstion('Interpreter', 'String', 'Object')).
:- set_predicate_property(sys_add_file_extension/2, visible(public)).

/**
 * sys_remove_file_extension(E):
 * The predicate succeeds in removing file extension database entry
 * with the name suffix E from the current knowledge base
 */
:- foreign(sys_remove_file_extension/1, 'ForeignPath',
      sysRemoveFileExtenstion('Interpreter', 'String')).
:- set_predicate_property(sys_remove_file_extension/1, visible(public)).

/**
 * sys_current_file_extension(E, O):
 * The predicate succeeds in E and O with the currently added
 * file extension database entries and their type and mime options
 * along the knowledge bases.
 */
sys_current_file_extension(E, O) :-
   sys_get_file_extensions(-(E, O)).
:- set_predicate_property(sys_current_file_extension/2, visible(public)).

:- foreign(sys_get_file_extensions/1, 'ForeignPath',
      sysGetFileExtenstions('CallOut', 'Interpreter')).
:- set_predicate_property(sys_get_file_extensions/1, visible(private)).

/****************************************************************/
/* File Resolution                                              */
/****************************************************************/

/**
 * absolute_file_name(R, A):
 * absolute_file_name(R, A, O):
 * The binary predicate succeeds when the read path R resolves to an
 * absolute path and this absolute path unifies with S. The ternary
 * predicate additionally recognizes the following path option. The
 * predicates can be also used in a backward manner.
 */
% absolute_file_name(+Slash, -Pin)
absolute_file_name(Slash, Pin) :-
   absolute_file_name(Slash, Pin, []).
:- set_predicate_property(absolute_file_name/2, visible(public)).

% absolute_file_name(+Slash, -Pin, +Opt)
absolute_file_name(Slash, Pin, Opt) :- ground(Slash), !,
   absolute_file_name2(Slash, Pin, Opt).
absolute_file_name(Slash, Pin, Opt) :-
   sys_unsearch_file_name(Pin, Slash, Opt).
:- set_predicate_property(absolute_file_name/3, visible(public)).

% absolute_file_name2(+Slash, -Pin, +Opt)
absolute_file_name2(Slash, Pin, Opt) :-
   sys_search_file_name(Slash, Pin, Opt), !.
absolute_file_name2(library(Slash), _, _) :-
   throw(error(existence_error(library, Slash), _)).
absolute_file_name2(foreign(Slash), _, _) :-
   throw(error(existence_error(class, Slash), _)).
absolute_file_name2(verbatim(Slash), _, _) :-
   throw(error(existence_error(verbatim, Slash), _)).
absolute_file_name2(resource(Slash), _, _) :-
   throw(error(existence_error(resource, Slash), _)).
absolute_file_name2(Slash, _, _) :-
   throw(error(existence_error(source_sink, Slash), _)).
:- set_predicate_property(absolute_file_name2/3, visible(private)).

/****************************************************************/
/* Search File                                                  */
/****************************************************************/

% sys_search_file_name2(+Spec, -Pin, +Integer)
/* library */
sys_search_file_name2(library(Slash), Pin, _) :- !,
   callable_property(Slash, sys_context(C)),
   sys_path_to_atom(Slash, Path),
   sys_search_options([search_path(library), file_type(text), failure(none)], Mask),
   sys_find_prefix(Path, C, Mask, J),
   sys_find_key(J, C, Mask, H),
   set_callable_property(Pin, sys_context(C), H).
/* verbatim */
sys_search_file_name2(verbatim(Slash), Pin, _) :- !,
   callable_property(Slash, sys_context(C)),
   sys_path_to_atom(Slash, Path),
   sys_search_options([search_path(library), file_type(text), failure(child)], Mask),
   sys_find_prefix(Path, C, Mask, J),
   sys_find_key(J, C, Mask, H),
   set_callable_property(Pin, sys_context(C), H).
/* foreign */
sys_search_file_name2(foreign(Slash), Pin, _) :- !,
   callable_property(Slash, sys_context(C)),
   sys_path_to_atom(Slash, Path),
   sys_search_options([search_path(foreign), file_type(binary), failure(none)], Mask),
   sys_find_prefix(Path, C, Mask, J),
   sys_find_key(J, C, Mask, H),
   set_callable_property(Pin, sys_context(C), H).
/* resource */
sys_search_file_name2(resource(Slash), Pin, _) :- !,
   callable_property(Slash, sys_context(C)),
   sys_path_to_atom(Slash, Path),
   sys_search_options([search_path(library), file_type(resource), failure(none)], Mask),
   sys_find_prefix(Path, C, Mask, J),
   sys_find_key(J, C, Mask, H),
   set_callable_property(Pin, sys_context(C), H).
/* absolute and relative */
sys_search_file_name2(Slash, Pin, Mask) :-
   callable_property(Slash, sys_context(C)),
   sys_path_to_atom(Slash, Path),
   sys_find_prefix(Path, C, Mask, J),
   sys_find_key(J, C, Mask, H),
   set_callable_property(Pin, sys_context(C), H).
:- set_predicate_property(sys_search_file_name2/3, visible(private)).

% sys_search_file_name(+Spec, -Pin, +Opt)
sys_search_file_name(Spec, Pin, Opt) :-
   sys_search_options(Opt, Mask),
   sys_search_read(Mask), !,
   sys_search_file_name2(Spec, Pin, Mask).
sys_search_file_name(Slash, Pin, _) :-
   callable_property(Slash, sys_context(C)),
   sys_path_to_atom(Slash, Path),
   sys_find_write(Path, H),
   set_callable_property(Pin, sys_context(C), H).
:- set_predicate_property(sys_search_file_name/3, visible(private)).

% sys_search_options(+List, -Integer)
:- foreign(sys_search_options/2, 'ForeignPath',
      sysSearchOptions('Object')).
:- set_predicate_property(sys_search_options/2, visible(private)).

% sys_search_read(+Integer)
:- foreign(sys_search_read/1, 'ForeignPath',
      sysSearchRead('Integer')).
:- set_predicate_property(sys_search_read/1, visible(private)).

% sys_find_write(+Atom, -Atom)
:- foreign(sys_find_write/2, 'ForeignPath',
      sysFindWrite('Interpreter', 'String')).
:- set_predicate_property(sys_find_write/2, visible(private)).

% sys_find_prefix(+Atom, +Atom, +Integer, -Atom)
:- foreign(sys_find_prefix/4, 'ForeignPath',
      sysFindPrefix('Interpreter', 'String', 'TermAtomic', 'Integer')).
:- set_predicate_property(sys_find_prefix/4, visible(private)).

% sys_find_key(+Atom, +Atom, +Integer, -Atom)
:- foreign(sys_find_key/4, 'ForeignPath',
      sysFindKey('Interpreter', 'String', 'TermAtomic', 'Integer')).
:- set_predicate_property(sys_find_key/4, visible(private)).

/****************************************************************/
/* File Unprobing                                               */
/****************************************************************/

% sys_unsearch_file_name2(+Pin, -Spec)
sys_unsearch_file_name2(Pin, Slash) :-
   callable_property(Pin, sys_context(C)),
   sys_search_options([search_path(all)], Mask),
   sys_unfind_key(Pin, C, Mask, H),
   sys_unsearch_file_name3(H, C, Slash).
:- set_predicate_property(sys_unsearch_file_name2/2, visible(private)).

% sys_unsearch_file_name3(+Spec, +Context, -Spec)
sys_unsearch_file_name3(library(Path), C, Slash) :- !,
   sys_search_options([search_path(library), file_type(text), failure(child)], Mask),
   sys_unfind_prefix(Path, C, Mask, J),
   sys_unsearch_file_name4(J, C, Slash).
sys_unsearch_file_name3(foreign(Path), C, foreign(Slash)) :- !,
   sys_search_options([search_path(foreign), file_type(binary), failure(none)], Mask),
   sys_unfind_prefix(Path, C, Mask, J),
   sys_path_to_atom(H, J),
   set_callable_property(Slash, sys_context(C), H).
sys_unsearch_file_name3(resource(Path), C, resource(Slash)) :- !,
   sys_search_options([search_path(library), file_type(resource), failure(none)], Mask),
   sys_unfind_prefix(Path, C, Mask, J),
   sys_path_to_atom(H, J),
   set_callable_property(Slash, sys_context(C), H).
sys_unsearch_file_name3(Path, C, Slash) :-
   sys_is_relative_uri(Path), !,
   sys_path_to_atom(H, Path),
   set_callable_property(Slash, sys_context(C), H).
sys_unsearch_file_name3(Path, C, Slash) :-
   set_callable_property(Slash, sys_context(C), Path).
:- set_predicate_property(sys_unsearch_file_name3/3, visible(private)).

% sys_unsearch_file_name4(+Spec, +Context, -Spec)
sys_unsearch_file_name4(verbatim(Path), C, verbatim(Slash)) :- !,
   sys_path_to_atom(H, Path),
   set_callable_property(Slash, sys_context(C), H).
sys_unsearch_file_name4(Path, C, library(Slash)) :- !,
   sys_path_to_atom(H, Path),
   set_callable_property(Slash, sys_context(C), H).
:- set_predicate_property(sys_unsearch_file_name4/3, visible(private)).

% sys_unsearch_file_name2(+Pin, -Spec, +Opt)
sys_unsearch_file_name(Pin, Slash, Opt) :-
   sys_search_options(Opt, Mask),
   sys_search_read(Mask), !,
   sys_unsearch_file_name2(Pin, Slash).
sys_unsearch_file_name(Pin, Slash, _) :-
   callable_property(Pin, sys_context(C)),
   sys_unfind_write(Pin, Path),
   sys_path_to_atom(H, Path),
   set_callable_property(Slash, sys_context(C), H).
:- set_predicate_property(sys_unsearch_file_name/3, visible(private)).

% sys_is_relative_uri(+Atom)
:- foreign(sys_is_relative_uri/1, 'ForeignUri',
      sysUriIsRelative('String')).
:- set_predicate_property(sys_is_relative_uri/1, visible(private)).

% sys_unfind_write(+Atom, -Atom)
:- foreign(sys_unfind_write/2, 'ForeignPath',
      sysUnfindWrite('Interpreter', 'String')).
:- set_predicate_property(sys_unfind_write/2, visible(private)).

% sys_unfind_key(+Atom, +Atom, +Integer, -Atom)
:- foreign(sys_unfind_key/4, 'ForeignPath',
      sysUnfindKey('Interpreter', 'String', 'TermAtomic', 'Integer')).
:- set_predicate_property(sys_unfind_key/4, visible(private)).

% sys_unfind_prefix(+Atom, +Atom, +Integer, -Atom)
:- foreign(sys_unfind_prefix/4, 'ForeignPath',
      sysUnfindPrefix('Interpreter', 'String', 'TermAtomic', 'Integer')).
:- set_predicate_property(sys_unfind_prefix/4, visible(private)).

/****************************************************************/
/* Term Representation                                          */
/****************************************************************/

/**
 * sys_path_to_atom(A, B):
 * Succeeds when B unifies with the atom representing the path A.
 */
% sys_path_to_atom(+-Slash, -+Atom)
sys_path_to_atom(Slash, Atom) :- var(Atom), !,
   sys_path_to_atom1(Slash, Atom).
sys_path_to_atom(Slash, Atom) :-
   sys_path_to_atom2(Atom, Slash).
:- set_predicate_property(sys_path_to_atom/2, visible(private)).

% sys_path_to_atom(+Slash, -Atom)
sys_path_to_atom1(Slash, _) :- var(Slash),
   throw(error(instantiation_error, _)).
sys_path_to_atom1({Dir}, Path) :- !,
   sys_path_to_atom1(Dir, Y),
   sys_atom_concat(Y, [], Path).
sys_path_to_atom1(Dir/Name, Path) :- !,
   sys_path_to_atom1(Dir, Y),
   sys_atom_concat(Y, /, H),
   sys_atom_concat(H, Name, Path).
sys_path_to_atom1(../Dir, Path) :- !,
   sys_path_to_atom1(Dir, Y),
   sys_atom_concat(../, Y, Path).
sys_path_to_atom1(X, Path) :- sys_atom(X), !,
   =(X, Path).
sys_path_to_atom1(X, _) :-
   throw(error(type_error(path, X), _)).
:- set_predicate_property(sys_path_to_atom1/2, visible(private)).

% sys_path_to_atom2(+Atom, -Slash)
sys_path_to_atom2(Path, {Dir}) :-
   sub_atom(Path, Before, _, 0, []), !,
   sub_atom(Path, 0, Before, X),
   sys_path_to_atom2(X, Dir).
sys_path_to_atom2(Path, Dir/Name) :-
   last_sub_atom(Path, Before, _, After, /),
   sub_atom(Path, 0, Before, X),
   \+ =(X, ..),
   \+ last_sub_atom(X, _, 0, /..), !,
   last_sub_atom(Path, After, 0, Name),
   sys_path_to_atom2(X, Dir).
sys_path_to_atom2(Path, ../Dir) :-
   sub_atom(Path, 0, _, After, ../),
   last_sub_atom(Path, After, 0, X),
   \+ =(X, ''), !,
   sys_path_to_atom2(X, Dir).
sys_path_to_atom2(Atom, Atom).
:- set_predicate_property(sys_path_to_atom2/2, visible(private)).
