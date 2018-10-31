/**
 * A relative path that is not wrapped into a compound is resolved
 * against the current base in write or append mode. The current base
 * is the Prolog flag “base_url”. Otherwise in read mode, if the call-site
 * is not user it is resolved against the path of the call-site itself.
 * In both cases the suffixes of the sys_add_file_extension/1
 * command are used:
 *
 * Example:
 * ?- absolute_file_name('my folder/my file', X).
 * Error: File 'my folder/my file' not found.
 * 	     absolute_file_name/2
 *
 * ?- set_prolog_flag(base_url, '/C:/Users/Jan Burse/Desktop/').
 * Yes
 *
 * ?- sys_add_file_extension(text('.dcg')).
 * Yes
 *
 * ?- absolute_file_name('my folder/my file', X).
 * X = 'file:/C:/Users/Jan Burse/Desktop/my folder/my file.dcg'
 *
 * Paths should not use a system specific directory separator but always use
 * the forward slash (/). For convenience paths have an automatic prefixing
 * of a schema. Paths starting with a double slash (//) are prefixed by the
 * “http” schema. Paths starting with a single slash (/) are prefixed by
 * the “file” schema. Drive letters are not considered schema.
 *
 * If the path is wrapped into a compound and if the functor of the compound
 * is either library/1, foreign/1 or verbatim/1 then the path is looked up
 * in the class path. The class path can be updated and queried by the
 * predicates sys_add_path/1 and sys_current_path/1. In these cases the
 * prefixes of the package/1 and use_package/1 command are also used.
 *
 * Write or append access resolution:
 *   &lt;path&gt;              resolve &lt;path&gt; in base.
 *
 * Read access resolution:
 *   library(<path>)           lookup resource <path> in class path.
 *   foreign(<path>)           lookup class <path> in class path.
 *   verbatim(<path>)          like library(<path>) or take as is.
 *   <path>                    resolve <path> in scope or base.
 *
 * The predicates absolute_file_name/[2,3] and absolute_resource_name/1
 * provide file name resolving. The predicate absolute_file_name/2 works
 * bi-directionally. For a given already resolved path it will make a best
 * effort attempt to reconstruct either a compound form foreign/1, library/1
 * or verbatim/1 or a relative path.
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

:- sys_context_property(here, C),
   set_source_property(C, use_package(foreign(jekpro/reference/bootload))).
:- sys_context_property(here, C),
   set_source_property(C, use_package(foreign(jekpro/tools/call))).
:- sys_context_property(here, C),
   set_source_property(C, use_package(foreign(jekpro/tools/term))).
:- sys_context_property(here, C),
   set_source_property(C, use_package(foreign(matula/util/system))).
:- sys_context_property(here, C),
   reset_source_property(C, sys_source_visible(public)).

:- sys_op(200, fy, ../).
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
sys_add_path(P) :-
   sys_find_write(P, Q),
   sys_add_class_path(Q).
:- set_predicate_property(sys_add_path/1, visible(public)).

:- foreign(sys_add_class_path/1, 'ForeignPath',
      sysAddClassdPath('Interpreter','String')).
:- set_predicate_property(sys_add_class_path/1, visible(private)).

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
 * sys_add_file_extension(E):
 * The predicate succeeds in adding the file extension E
 * to the current knowledge base.
 */
sys_add_file_extension(text(E)) :- !,
   sys_get_suffix_text(T),
   sys_add_file_extension(E, T).
sys_add_file_extension(binary(E)) :- !,
   sys_get_suffix_binary(T),
   sys_add_file_extension(E, T).
sys_add_file_extension(resource(E)) :- !,
   sys_get_suffix_resource(T),
   sys_add_file_extension(E, T).
sys_add_file_extension(E) :-
   throw(error(domain_error(fix_option,E),_)).
:- set_predicate_property(sys_add_file_extension/1, visible(public)).

:- foreign(sys_add_file_extension/2, 'ForeignPath',
      sysAddFileExtenstion('Interpreter','String',int)).
:- set_predicate_property(sys_add_file_extension/2, visible(private)).

/**
 * sys_current_file_extension(E):
 * The predicate succeeds in E with the currently added
 * file extensions along the knowledge bases.
 */
sys_current_file_extension(E) :-
   sys_get_file_extensions(L),
   sys_map_file_extensions(L, R),
   sys_member(E, R).
:- set_predicate_property(sys_current_file_extension/1, visible(public)).

:- foreign(sys_get_file_extensions/1, 'ForeignPath',
      sysGetFileExtenstions('Interpreter')).
:- set_predicate_property(sys_get_file_extensions/1, visible(private)).

sys_map_file_extensions([E-T|L], R) :-
   sys_map_extension_text(T, E, H, I),
   sys_map_extension_binary(T, E, I, J),
   sys_map_extension_resource(T, E, J, R),
   sys_map_file_extensions(L, H).
sys_map_file_extensions([], []).
:- set_predicate_property(sys_map_file_extensions/2, visible(private)).

sys_map_extension_text(T, E, H, R) :-
   sys_get_suffix_text(M),
   0 =\= T/\M, !,
   R = [text(E)|H].
sys_map_extension_text(_, _, R, R).
:- set_predicate_property(sys_map_extension_text/4, visible(private)).

sys_map_extension_binary(T, E, H, R) :-
   sys_get_suffix_binary(M),
   0 =\= T/\M, !,
   R = [binary(E)|H].
sys_map_extension_binary(_, _, R, R).
:- set_predicate_property(sys_map_extension_binary/4, visible(private)).

sys_map_extension_resource(T, E, H, R) :-
   sys_get_suffix_resource(M),
   0 =\= T/\M, !,
   R = [resource(E)|H].
sys_map_extension_resource(_, _, R, R).
:- set_predicate_property(sys_map_extension_resource/4, visible(private)).

:- foreign_getter(sys_get_suffix_text/1, 'ForeignPath', 'MASK_SUFX_TEXT').
:- set_predicate_property(sys_get_suffix_text/1, visible(private)).

:- foreign_getter(sys_get_suffix_binary/1, 'ForeignPath', 'MASK_SUFX_BNRY').
:- set_predicate_property(sys_get_suffix_binary/1, visible(private)).

:- foreign_getter(sys_get_suffix_resource/1, 'ForeignPath', 'MASK_SUFX_RSCS').
:- set_predicate_property(sys_get_suffix_resource/1, visible(private)).

/****************************************************************/
/* File Resolution                                              */
/****************************************************************/

/**
 * absolute_file_name(R, A):
 * absolute_file_name(R, A, O):
 * The binary predicate succeeds when the read path R resolves to an absolute
 * file path A. The ternary predicate additionally recognizes the following
 * path option. For a list of options see the API documentation.
 */
% absolute_file_name(+Slash, -Pin)
absolute_file_name(Slash, Pin) :-
   var(Slash), !,
   sys_absolute_file_name2(Pin, Slash).
absolute_file_name(Slash, Pin) :-
   sys_absolute_file_name(Slash, Pin), !.
absolute_file_name(library(Slash), _) :-
   throw(error(existence_error(library,Slash),_)).
absolute_file_name(foreign(Slash), _) :-
   throw(error(existence_error(class,Slash),_)).
absolute_file_name(verbatim(Slash), _) :-
   throw(error(existence_error(verbatim,Slash),_)).
absolute_file_name(Slash, _) :-
   throw(error(existence_error(source_sink,Slash),_)).
:- set_predicate_property(absolute_file_name/2, visible(public)).

% absolute_file_name(+Slash, -Pin, +Opt)
absolute_file_name(Slash, _, _) :-
   var(Slash),
   throw(error(instantiation_error,_)).
absolute_file_name(Slash, Pin, Opt) :-
   sys_absolute_file_name(Slash, Pin, Opt), !.
absolute_file_name(library(Slash), _, _) :-
   throw(error(existence_error(library,Slash),_)).
absolute_file_name(foreign(Slash), _, _) :-
   throw(error(existence_error(class,Slash),_)).
absolute_file_name(verbatim(Slash), _, _) :-
   throw(error(existence_error(verbatim,Slash),_)).
absolute_file_name(Slash, _, _) :-
   throw(error(existence_error(source_sink,Slash),_)).
:- set_predicate_property(absolute_file_name/3, visible(public)).

/****************************************************************/
/* Resource Resolution                                          */
/****************************************************************/

/**
 * absolute_resource_name(R, A):
 * The binary predicate succeeds when the read path R resolves to
 * an absolute resource path A.
 */
% absolute_resource_name(+Slash, -Pin)
absolute_resource_name(Slash, Pin) :-
   sys_absolute_resource_name(Slash, Pin), !.
absolute_resource_name(library(Slash), _) :-
   throw(error(existence_error(library,Slash),_)).
absolute_resource_name(Slash, _) :-
   throw(error(existence_error(source_sink,Slash),_)).
:- set_predicate_property(absolute_resource_name/2, visible(public)).

/****************************************************************/
/* File Probing                                                 */
/****************************************************************/

% sys_absolute_file_name(+Spec, -Pin)
/* library */
sys_absolute_file_name(library(Slash), Pin) :- !,
   sys_context_property(Slash, C),
   sys_path_to_atom(Slash, Path),
   sys_find_prefix(Path, C, [package(library),file_extension(file)], J),
   sys_find_key(J, C, [package(library),file_extension(file)], H),
   sys_set_context_property(Pin, C, H).
/* verbatim */
sys_absolute_file_name(verbatim(Slash), Pin) :- !,
   sys_context_property(Slash, C),
   sys_path_to_atom(Slash, Path),
   sys_find_prefix(Path, C, [package(library),file_extension(file),failure(child)], J),
   sys_find_key(J, C, [package(library),file_extension(file)], H),
   sys_set_context_property(Pin, C, H).
/* foreign */
sys_absolute_file_name(foreign(Slash), Pin) :- !,
   sys_context_property(Slash, C),
   sys_path_to_atom(Slash, Path),
   sys_find_prefix(Path, C, [package(foreign),file_extension(file)], J),
   sys_find_key(J, C, [package(foreign),file_extension(file)], H),
   sys_set_context_property(Pin, C, H).
/* absolute and relative */
sys_absolute_file_name(Slash, Pin) :-
   sys_context_property(Slash, C),
   sys_path_to_atom(Slash, Path),
   sys_find_key(Path, C, [file_extension(file),failure(read)], H),
   sys_set_context_property(Pin, C, H).
:- set_predicate_property(sys_absolute_file_name/2, visible(private)).

% sys_absolute_file_name(+Spec, -Pin, +Opt)
sys_absolute_file_name(Spec, Pin, Opt) :-
   sys_access_opt(Opt, read, read), !,
   sys_absolute_file_name(Spec, Pin).
sys_absolute_file_name(Slash, Pin, _) :-
   sys_context_property(Slash, C),
   sys_path_to_atom(Slash, Path),
   sys_find_write(Path, H),
   sys_set_context_property(Pin, C, H).
:- set_predicate_property(sys_absolute_file_name/3, visible(private)).

% sys_access_opt(+Opt, +Value, -Value)
sys_access_opt([], V, V).
sys_access_opt([access(V)|L], _, W) :- !,
   sys_access_opt(L, V, W).
sys_access_opt([_|L], V, W) :-
   sys_access_opt(L, V, W).
:- set_predicate_property(sys_access_opt/3, visible(private)).

:- foreign(sys_find_write/2, 'ForeignPath',
      sysFindWrite('Interpreter','String')).
:- set_predicate_property(sys_find_write/2, visible(private)).

/****************************************************************/
/* File Unprobing                                               */
/****************************************************************/

% sys_absolute_file_name2(+Pin, -Spec)
sys_absolute_file_name2(Pin, Slash) :-
   sys_context_property(Pin, C),
   sys_unfind_key(Pin, C, [package(both),file_extension(file),failure(read)], H),
   sys_absolute_file_name3(H, C, Slash).
:- set_predicate_property(sys_absolute_file_name2/2, visible(private)).

% sys_absolute_file_name3(+Spec, +Context, -Spec)
sys_absolute_file_name3(library(Path), C, Slash) :- !,
   sys_unfind_prefix(Path, C, [package(library),file_extension(file),failure(child)], J),
   sys_absolute_file_name4(J, C, Slash).
sys_absolute_file_name3(foreign(Path), C, foreign(Slash)) :- !,
   sys_unfind_prefix(Path, C, [package(foreign),file_extension(file)], J),
   sys_path_to_atom(H, J),
   sys_set_context_property(Slash, C, H).
sys_absolute_file_name3(Path, C, Slash) :-
   sys_is_relative_uri(Path), !,
   sys_path_to_atom(H, Path),
   sys_set_context_property(Slash, C, H).
sys_absolute_file_name3(Path, C, Slash) :-
   sys_set_context_property(Slash, C, Path).
:- set_predicate_property(sys_absolute_file_name3/3, visible(private)).

% sys_absolute_file_name4(+Spec, +Context, -Spec)
sys_absolute_file_name4(verbatim(Path), C, verbatim(Slash)) :- !,
   sys_path_to_atom(H, Path),
   sys_set_context_property(Slash, C, H).
sys_absolute_file_name4(Path, C, library(Slash)) :- !,
   sys_path_to_atom(H, Path),
   sys_set_context_property(Slash, C, H).
:- set_predicate_property(sys_absolute_file_name4/3, visible(private)).

:- foreign(sys_is_relative_uri/1, 'ForeignUri',
      sysUriIsRelative('String')).
:- set_predicate_property(sys_is_relative_uri/1, visible(private)).

:- foreign(sys_unfind_key/4, 'ForeignPath',
      sysUnfindKey('Interpreter','String','TermAtomic','Object')).
:- set_predicate_property(sys_unfind_key/4, visible(private)).

:- foreign(sys_unfind_prefix/4, 'ForeignPath',
      sysUnfindPrefix('Interpreter','String','TermAtomic','Object')).
:- set_predicate_property(sys_unfind_prefix/4, visible(private)).

/****************************************************************/
/* Resource Probing                                             */
/****************************************************************/

% sys_absolute_resource_name(+Spec, -Pin)
/* library */
sys_absolute_resource_name(library(Slash), Pin) :- !,
   sys_context_property(Slash, C),
   sys_path_to_atom(Slash, Path),
   sys_find_prefix(Path, C, [package(library),file_extension(resource)], J),
   sys_find_key(J, C, [package(library),file_extension(resource)], H),
   sys_set_context_property(Pin, C, H).
/* relative */
sys_absolute_resource_name(Slash, Pin) :-
   sys_context_property(Slash, C),
   sys_path_to_atom(Slash, Path),
   sys_find_key(Path, C, [file_extension(resource),failure(read)], H),
   sys_set_context_property(Pin, C, H).
:- set_predicate_property(sys_absolute_resource_name/2, visible(private)).

:- foreign(sys_find_prefix/4, 'ForeignPath',
      sysFindPrefix('Interpreter','String','TermAtomic','Object')).
:- set_predicate_property(sys_find_prefix/4, visible(private)).

:- foreign(sys_find_key/4, 'ForeignPath',
      sysFindKey('Interpreter','String','TermAtomic','Object')).
:- set_predicate_property(sys_find_key/4, visible(private)).

/****************************************************************/
/* Term Representation                                          */
/****************************************************************/

/**
 * sys_path_to_atom(A, B):
 * Succeeds when B unifies with the atom representing the path A.
 */
% sys_path_to_atom(+-Slash, -+Atom)
sys_path_to_atom(Slash, Atom) :-
   var(Atom), !,
   sys_path_to_atom1(Slash, Atom).
sys_path_to_atom(Slash, Atom) :-
   sys_path_to_atom2(Atom, Slash).
:- set_predicate_property(sys_path_to_atom/2, visible(private)).

% sys_path_to_atom(+Slash, -Atom)
sys_path_to_atom1(Slash, _) :-
   var(Slash),
   throw(error(instantiation_error,_)).
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
sys_path_to_atom1(X, Path) :-
   sys_atom(X), !,
   X = Path.
sys_path_to_atom1(X, _) :-
   throw(error(type_error(path,X),_)).
:- set_predicate_property(sys_path_to_atom1/2, visible(private)).

% sys_path_to_atom2(+Atom, -Slash)
sys_path_to_atom2(Path, {Dir}) :-
   sub_atom(Path, Before, _, 0, []), !,
   sub_atom(Path, 0, Before, X),
   sys_path_to_atom2(X, Dir).
sys_path_to_atom2(Path, Dir/Name) :-
   last_sub_atom(Path, Before, _, After, /),
   sub_atom(Path, 0, Before, X),
   \+ X = ..,
   \+ last_sub_atom(X, _, 0, /..), !,
   last_sub_atom(Path, After, 0, Name),
   sys_path_to_atom2(X, Dir).
sys_path_to_atom2(Path, ../Dir) :-
   sub_atom(Path, 0, _, After, ../), !,
   last_sub_atom(Path, After, 0, X),
   sys_path_to_atom2(X, Dir).
sys_path_to_atom2(Atom, Atom).
:- set_predicate_property(sys_path_to_atom2/2, visible(private)).
