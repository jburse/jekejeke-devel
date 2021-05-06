package jekpro.tools.bundle;

import jekpro.tools.call.Capability;

/**
 * The class implements a capability that reads its model description
 * properties from a SWI-Prolog content package slip which is a Prolog
 * text. Additionally the capability can read platform description
 * properties from a Jekejeke Prolog icon package slip which is also
 * a Prolog text. The package has one parameter for initialization,
 * namely the main root of the package. The main root is then used to
 * determine the location of the package slips.
 * <pre>
 * Example pack.pl:
 * name(hello).
 * title('Hello World').
 * version('1.0.0').
 * home('http://www.jekejeke.ch/').
 * author('(c) 2019, XLOG Technologies GmbH, Switzerland', 'info@xlog.ch').
 * </pre>
 * The content package slip follows the format from SWI-Prolog. It
 * has to be a Prolog text named "pack.pl". An example content package
 * slip might read as follows, whereas the name attribute is not yet
 * used by Jekejeke Prolog. The name and the version attributes are
 * mandatory to be interoperable with the SWI-Prolog system. The
 * author attribute can have multiple entries, our example shows
 * only one entry:
 * <pre>
 * Example icon.pl:
 * icon('images/hello.png').
 * </pre>
 * The icon package slip follows a proprietary format of Jekejeke Prolog.
 * It has to be a Prolog text named "icon.pl". The SWI-Prolog system
 * will simply ignore this package slip. The Jekejeke Prolog system
 * on the other hand will use the icon package slip to associate an
 * icon with the package. For icons in a folder <directory>, high r
 * esolution icons can be placed in a sibling folder by the
 * name <directory>_hi.
 * <pre>
 * Example prolog/hello.pl:
 * :- module(hello, [hello/0]).
 * hello :-
 * write('Hello World!'), nl.
 * </pre>
 * The Prolog text itself of such a package can be placed in a directory
 * named "prolog". When a package is initialized, this directory is
 * automatically added to the file extension prefixes. It will impact
 * the lookup of Prolog texts without requiring a module or a package
 * declaration inside each Prolog text. For interoperability with
 * SWI-Prolog, it is recommended to not use the predicate indicator
 * main/0 inside a Prolog text.
 * <p/>
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 * <p/>
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 * <p/>
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 * <p/>
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class CapabilitySWI extends Capability {

    /**
     * <p>Create the capability headless.</p>
     *
     * @param root The main root.
     */
    public CapabilitySWI(String root) {
        super(new BranchSWI(root));
    }

}