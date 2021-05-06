package jekpro.model.molec;

import jekpro.model.pretty.Foyer;
import jekpro.tools.term.SkelAtom;

/**
 * <p>The polymorphic cache for structured package name.</p>
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
public final class CachePackage extends AbstractCache {
    public static final char OP_CHAR_SEG = '.';

    String fun;
    SkelAtom res;

    /**
     * <p>Create a package name.</p>
     *
     * @param sa   The atom.
     * @param fun2 The package, or null.
     * @return The sub package name.
     */
    private static SkelAtom lookupPackage(SkelAtom sa, String fun2) {
        if (fun2 != null) {
            fun2 = composeStruct(sa.fun, fun2);
        } else {
            fun2 = composeArray(sa.fun);
        }

        /* create with call-site */
        return new SkelAtom(fun2);
    }

    /*******************************************************************/
    /* Main Entries                                                    */
    /*******************************************************************/

    /**
     * <p>Retrieve a package name.</p>
     *
     * @param sa   The atom skeleton.
     * @param fun2 The package, or null.
     * @return The package name.
     */
    public static SkelAtom getPackage(SkelAtom sa, String fun2) {
        AbstractCache back = null;
        AbstractCache temp = sa.cache;
        for (; ; ) {
            if (temp == null) {
                SkelAtom sa2 = lookupPackage(sa, fun2);
                CachePackage ca = new CachePackage();
                ca.fun = fun2;
                ca.res = sa2;
                if (back == null) {
                    sa.cache = ca;
                } else {
                    back.next = ca;
                }
                return sa2;
            }
            if (temp instanceof CachePackage) {
                CachePackage ca = (CachePackage) temp;
                if (ca.fun != null ? ca.fun.equals(fun2) : null == fun2) {
                    return ca.res;
                }
            }
            back = temp;
            temp = back.next;
        }
    }

    /***************************************************************/
    /* Structured Paths                                            */
    /***************************************************************/

    /**
     * <p>Separate the package from the structured path.</p>
     *
     * @param path The structured path.
     * @return The package.
     */
    public static String sepPack(String path) {
        return path.substring(0, path.lastIndexOf(OP_CHAR_SEG));
    }

    /**
     * <p>Separate the base from the structured path.</p>
     *
     * @param path The structured path.
     * @return The base.
     */
    public static String sepBase(String path) {
        return path.substring(path.lastIndexOf(OP_CHAR_SEG) + 1);
    }

    /**
     * <p>Check whether a path is structured.</p>
     *
     * @param path The path.
     * @return True if the path is structured, otherwise false.
     */
    public static boolean isStruct(String path) {
        return (path.lastIndexOf(OP_CHAR_SEG) != -1);
    }

    /**
     * <p>Compose a structured path from a package and a base.</p>
     *
     * @param pack The package.
     * @param base The base.
     * @return The structured path.
     */
    public static String composeStruct(String pack, String base) {
        StringBuilder buf = new StringBuilder();
        buf.append(pack);
        buf.append(OP_CHAR_SEG);
        buf.append(base);
        return buf.toString();
    }

    /***************************************************************/
    /* Array Paths                                                 */
    /***************************************************************/

    /**
     * <p>Separate the component from the array path.</p>
     *
     * @param path The array path.
     * @return The component.
     */
    public static String sepComp(String path) {
        return path.substring(0, path.length() - Foyer.OP_NIL.length());
    }

    /**
     * <p>Check whether a path is an array.</p>
     *
     * @param path The path.
     * @return True if the path is an array, otherwise false.
     */
    public static boolean isArray(String path) {
        return path.endsWith(Foyer.OP_NIL);
    }

    /**
     * <p>Compose an array path from a component.</p>
     *
     * @param comp The component.
     * @return The array path.
     */
    public static String composeArray(String comp) {
        StringBuilder buf = new StringBuilder();
        buf.append(comp);
        buf.append(Foyer.OP_NIL);
        return buf.toString();
    }

}
