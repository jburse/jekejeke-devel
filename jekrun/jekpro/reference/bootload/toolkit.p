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

:- package(library(jekpro/reference/bootload)).
:- use_package(foreign(jekpro/reference/bootload)).
:- use_package(foreign(jekpro/tools/call)).

:- module(toolkit, []).

/**
 * sys_activate_capability(C, H):
 * The predicate activates the capability C by the license key H.
 */
% sys_activate_capability(+Capa, +Key)
:- public sys_activate_capability/2.
:- foreign(sys_activate_capability/2, 'ForeignToolkit',
      sysActivateCapability('Interpreter', 'String', 'String')).

/**
 * sys_calc_install_id(C, I):
 * The predicate calculates the install ID for the capability C
 * and unifies it with I.
 */
% sys_calc_install_id(+Capa, -ID)
:- public sys_calc_install_id/2.
:- foreign(sys_calc_install_id/2, 'ForeignToolkit',
      sysCalcInstallID('Interpreter', 'String')).

/**
 * sys_reg_license_text(C, T):
 * The predicate registers the license text T for the capability C in
 * the user preferences.
 */
% sys_reg_license_text(+Capa, +Text)
:- public sys_reg_license_text/2.
:- foreign(sys_reg_license_text/2, 'ForeignToolkit',
      sysRegLicenseText('Interpreter', 'String', 'String')).

/**
 * sys_reged_license_text(C, T):
 * The predicate returns the registeres license text in T for the
 * capability C in the user preferences.
 */
% sys_reged_license_text(+Capa, -Text)
:- public sys_reged_license_text/2.
:- foreign(sys_reged_license_text/2, 'ForeignToolkit',
      sysRegedLicenseText('Interpreter', 'String')).

/**
 * sys_init_capability(C):
 * sys_init_capability(C, O):
 * The unary predicate initializes and enlists the capability C. The binary
 * predicate additionally recognizes the following init options. For a list
 * of init options see the API documentation.
 */

% sys_init_capability(+Capa)
:- public sys_init_capability/1.
:- foreign(sys_init_capability/1, 'ForeignToolkit',
      sysInitCapability('Interpreter', 'String')).

% sys_init_capability(+Capa, +Opts)
:- public sys_init_capability/2.
:- foreign(sys_init_capability/2, 'ForeignToolkit',
      sysInitCapabilityOpt('Interpreter', 'String', 'Object')).

/**
 * sys_finit_capability(C):
 * The predicate removes the capability C.
 */
% sys_finit_capability(+Capa)
:- public sys_finit_capability/1.
:- foreign(sys_finit_capability/1, 'ForeignToolkit',
      sysFiniCapability('Interpreter', 'String')).

/**
 * sys_current_capability(C):
 * The predicate succeeds with the currently initialized capabilities C.
 */
% sys_current_capability(-Capa)
:- public sys_current_capability/1.
sys_current_capability(Cap) :-
   sys_get_capabilities(Caps),
   sys_member(Cap, Caps).

:- private sys_get_capabilities/1.
:- foreign(sys_get_capabilities/1, 'ForeignToolkit',
      sysGetCapabilities('Interpreter')).

/**
 * sys_capability_property(C, P):
 * The predicate succeeds with all the properties of the capability C that
 * unify with P. The following capability properties are supported. For
 * a list of capability properties see the API documentation.
 */
% sys_capability_property(+Capa, -Prop)
:- public sys_capability_property/2.
sys_capability_property(Cap, Prop) :- var(Prop), !,
   sys_get_capability_properties(Cap, Props),
   sys_member(Prop, Props).
sys_capability_property(Cap, Prop) :-
   functor(Prop, F, 1),
   sys_get_capability_property(Cap, F, Prop).

:- private sys_get_capability_properties/2.
:- foreign(sys_get_capability_properties/2, 'ForeignToolkit',
      sysGetCapabilityProperties('Interpreter', 'String')).

:- private sys_get_capability_property/3.
:- foreign(sys_get_capability_property/3, 'ForeignToolkit',
      sysGetCapabilityProperty('Interpreter', 'String', 'String')).

/**
 * sys_check_license(C):
 * The predicate updates the license status of the capability C. Throws an
 * exception if license status is not OK.
 */
% sys_check_license(+Capa)
:- public sys_check_license/1.
:- foreign(sys_check_license/1, 'ForeignToolkit',
      sysCheckLicense('Interpreter', 'String')).

/**
 * sys_check_licenses:
 * The predicate updates the knowledge base status. Throws an exception if
 * the knowledge base status is not OK.
 */
% sys_check_licenses
:- public sys_check_licenses/0.
:- foreign(sys_check_licenses/0, 'ForeignToolkit',
      sysCheckLicenses('Interpreter')).
