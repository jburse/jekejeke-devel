/**
 * A path that is not wrapped into a compound is resolved against the
 * current base when the call-site is system, user or orphan. The current
 * base is the Prolog flag “base_url”. Otherwise if the call-site is
 * not system, user or orphan it is resolved against the path of the
 * call-site itself. In both cases the suffixes of the use_file_extension/1
 * command are used, the prefixes of the use_package/1 command are not used:
 *
 * Example:
 * ?- absolute_file_name('my folder/my file', X).
 * Error: File 'my folder/my file' not found.
 * 	     absolute_file_name/2
 *
 * ?- set_prolog_flag(base_url, '/C:/Users/Jan Burse/Desktop/').
 * Yes
 *
 * ?- use_file_extension(text('.txt')).
 * Yes
 *
 * ?- absolute_file_name('my folder/my file', X).
 * X = 'file:/C:/Users/Jan Burse/Desktop/my folder/my file.txt'
 *
 * Paths should not use a system specific directory separator but always
 * use the forward slash (/). For convenience paths have an automatic
 * prefixing of a schema. Paths starting with a double slash (//) are
 * prefixed by the “http” schema. Paths starting with a single slash (/)
 * are prefixed by the “file” schema.
 *
 * If the path is wrapped into a compound and if the functor of the
 * compound is either library/1 or foreign/1 then the path is looked
 * up in the class loader. The class path can be updated and queried
 * by the predicates sys_add_path/1 and sys_current_path/1. The prefixes
 * of the system and of the call-site are first tried.
 *
 * Read access resolution:
 *   library(<path>)           lookup resource <path> in class path.
 *   foreign(<path>)           lookup class <path> in class path.
 *   verbatim(<path>)          take as is.
 *   auto(<path>)              lookup <path> as the auto loader would do.
 *   <path>                    resolve <path> in scope or base.
 *
 * Write or append access resolution:
 *   &lt;path&gt;              resolve &lt;path&gt; in base.
 *
 * If the path is wrapped into a compound and if the functor of the
 * compound is either library/1 or foreign/1 then the path is looked up
 * in the class path. The class path can be updated and queried by the
 * predicates sys_add_path/1 and sys_current_path/1. In both cases
 * the suffixes of the use_file_extension/1 command and the prefixes
 * of the use_package/1 command are used.
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
   set_source_property(C, use_package(foreign(jekpro/reference/bootload))).
:- sys_get_context(here, C),
   set_source_property(C, use_package(foreign(jekpro/tools/call))).
:- sys_get_context(here, C),
   reset_source_property(C, sys_source_visible(public)).

:- sys_op(400, fx, ../).
:- set_oper_property(prefix(../), visible(public)).

/********************************************************/
/* Class Path Modification & Access                     */
/********************************************************/

/**
 * sys_add_path(R):
 * The predicate adds the relative path R to the class loader.
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
 * The predicate succeeds with the currently added absolute paths A.
 */
% sys_current_path(-Path)
sys_current_path(Path) :-
   sys_get_class_paths(Paths),
   sys_member(Path, Paths).
:- set_predicate_property(sys_current_path/1, visible(public)).

:- foreign(sys_get_class_paths/1, 'ForeignPath',
      sysGetClassPaths('Interpreter')).
:- set_predicate_property(sys_get_class_paths/1, visible(private)).

/**************************************************/
/* File Resolution                                */
/**************************************************/

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
   sys_get_context(Pin, C),
   sys_key_spec(Pin, C, [], Slash).
absolute_file_name(Module:Slash, Pin) :- !,
   sys_module_site(Module, Site),
   absolute_file_name(Slash, Help),
   sys_replace_site(Pin, Site, Help).
absolute_file_name(Slash, Pin) :-
   sys_absolute_file_name(Slash, Pin), !.
absolute_file_name(library(Slash), _) :-
   throw(error(existence_error(library,Slash),_)).
absolute_file_name(foreign(Slash), _) :-
   throw(error(existence_error(class,Slash),_)).
absolute_file_name(verbatim(Slash), _) :-
   throw(error(existence_error(verbatim,Slash),_)).
absolute_file_name(auto(Slash), _) :-
   throw(error(existence_error(auto,Slash),_)).
absolute_file_name(Slash, _) :-
   throw(error(existence_error(source_sink,Slash),_)).
:- set_predicate_property(absolute_file_name/2, visible(public)).

% absolute_file_name(+Slash, -Pin, +Opt)
absolute_file_name(Slash, Pin, Opt) :-
   var(Slash), !,
   sys_get_context(Pin, C),
   sys_key_spec(Pin, C, Opt, Slash).
absolute_file_name(Module:Slash, Pin, Opt) :- !,
   sys_module_site(Module, Site),
   absolute_file_name(Slash, Help, Opt),
   sys_replace_site(Pin, Site, Help).
absolute_file_name(Slash, Pin, Opt) :-
   sys_absolute_file_name(Slash, Pin, Opt), !.
absolute_file_name(library(Slash), _, _) :-
   throw(error(existence_error(library,Slash),_)).
absolute_file_name(foreign(Slash), _, _) :-
   throw(error(existence_error(class,Slash),_)).
absolute_file_name(verbatim(Slash), _, _) :-
   throw(error(existence_error(verbatim,Slash),_)).
absolute_file_name(auto(Slash), _, _) :-
   throw(error(existence_error(auto,Slash),_)).
absolute_file_name(Slash, _, _) :-
   throw(error(existence_error(source_sink,Slash),_)).
:- set_predicate_property(absolute_file_name/3, visible(public)).

% sys_module_site(+Slash, -Term)
sys_module_site(N, J) :-
   atom_property(here, sys_context(C)),
   reset_atom_property(here, sys_context(C), H),
   absolute_file_name(auto(N), S),
   set_atom_property(H, sys_context(S), J).
:- set_predicate_property(sys_module_site/2, visible(public)).

:- foreign(sys_key_spec/4, 'ForeignPath',
      sysKeySpec('Interpreter','String','String','Object')).
:- set_predicate_property(sys_key_spec/4, visible(private)).

/********************************************************/
/* Resource Resolution                                  */
/********************************************************/

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

/********************************************************/
/* File Probing                                         */
/********************************************************/

% sys_absolute_file_name(+Spec, -Pin)
/* library */
sys_absolute_file_name(library(Slash), Pin) :- !,
   sys_get_context(Slash, C),
   sys_path_norm(Slash, Path),
   sys_find_prefix(Path, C, [package(library),file_extension(file)], J),
   sys_find_key(J, C, [package(library),file_extension(file)], H),
   sys_replace_site(Pin, Slash, H).
/* verbatim */
sys_absolute_file_name(verbatim(Slash), Pin) :- !,
   sys_get_context(Slash, C),
   sys_path_norm(Slash, Path),
   sys_find_prefix(Path, C, [package(library),file_extension(file),failure(child)], J),
   sys_find_key(J, C, [package(library),file_extension(file),failure(child)], H),
   sys_replace_site(Pin, Slash, H).
/* foreign */
sys_absolute_file_name(foreign(Slash), Pin) :- !,
   sys_get_context(Slash, C),
   sys_path_norm(Slash, Path),
   sys_find_prefix(Path, C, [package(foreign),file_extension(file)], J),
   sys_find_key(J, C, [package(foreign),file_extension(file)], H),
   sys_replace_site(Pin, Slash, H).
/* auto */
sys_absolute_file_name(auto(Slash), Pin) :- !,
   sys_get_context(Slash, C),
   sys_path_norm(Slash, Path),
   sys_find_prefix(Path, C, [package(both),file_extension(file),failure(child)], J),
   sys_find_key(J, C, [package(both),file_extension(file),failure(child)], H),
   sys_replace_site(Pin, Slash, H).
/* relative */
sys_absolute_file_name(Slash, Pin) :-
   sys_get_context(Slash, C),
   sys_path_norm(Slash, Path),
   sys_find_key(Path, C, [file_extension(file),failure(read)], H),
   sys_replace_site(Pin, Slash, H).
:- set_predicate_property(sys_absolute_file_name/2, visible(private)).

% sys_absolute_file_name(+Spec, -Pin, +Opt)
sys_absolute_file_name(Spec, Pin, Opt) :-
   sys_access_opt(Opt, read, read), !,
   sys_absolute_file_name(Spec, Pin).
sys_absolute_file_name(Slash, Pin, _) :-
   sys_path_norm(Slash, Path),
   sys_find_write(Path, H),
   sys_replace_site(Pin, Slash, H).
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

/********************************************************/
/* Resource Probing                                     */
/********************************************************/

% sys_absolute_resource_name(+Spec, -Pin)
/* library */
sys_absolute_resource_name(library(Slash), Pin) :- !,
   sys_get_context(Slash, C),
   sys_path_norm(Slash, Path),
   sys_find_prefix(Path, C, [package(library),file_extension(resource)], J),
   sys_find_key(J, C, [package(library),file_extension(resource)], H),
   sys_replace_site(Pin, Slash, H).
/* relative */
sys_absolute_resource_name(Slash, Pin) :-
   sys_get_context(Slash, C),
   sys_path_norm(Slash, Path),
   sys_find_key(Path, C, [file_extension(resource),failure(read)], H),
   sys_replace_site(Pin, Slash, H).
:- set_predicate_property(sys_absolute_resource_name/2, visible(private)).

:- foreign(sys_find_prefix/4, 'ForeignPath',
      sysFindPrefix('Interpreter','String','String','Object')).
:- set_predicate_property(sys_find_prefix/4, visible(private)).

:- foreign(sys_find_key/4, 'ForeignPath',
      sysFindKey('Interpreter','String','String','Object')).
:- set_predicate_property(sys_find_key/4, visible(private)).

/**
 * sys_path_norm(A, B):
 * Succeeds when B unifies with the atom representing the slash A.
 */
% sys_path_norm(+Slash, -Atom)
sys_path_norm(Slash, _) :-
   var(Slash),
   throw(error(instantiation_error,_)).
sys_path_norm(X, Path) :-
   sys_atom(X), !,
   sys_eq(X, Path).
sys_path_norm(../Dir, Path) :- !,
   sys_path_norm(Dir, Y),
   sys_atom_concat(../, Y, Path).
sys_path_norm(Dir/Name, Path) :- !,
   sys_path_norm(Dir, Y),
   sys_atom_concat(Y, /, H),
   sys_atom_concat(H, Name, Path).
sys_path_norm({Dir}, Path) :- !,
   sys_path_norm(Dir, Y),
   sys_atom_concat(Y, [], Path).
sys_path_norm(X, _) :-
   throw(error(type_error(path,X),_)).
:- set_predicate_property(sys_path_norm/2, visible(private)).

/**************************************************/
/* Some Testing                                   */
/**************************************************/

:- foreign(sys_push_kb/0, 'ForeignPath',
      sysPushKB('Interpreter')).
:- set_predicate_property(sys_push_kb/0, visible(public)).
