package jekpro.model.molec;

import jekpro.model.inter.Engine;
import jekpro.tools.term.SkelAtom;

import static jekpro.tools.term.SkelAtom.MASK_ATOM_QALI;

/**
 * <p>The polymorphic cache for qualified functor names.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class CacheFunctor extends AbstractCache {
    public static final char OP_CHAR_SEP = '\b';

    SkelAtom mod;
    SkelAtom res;

    /**
     * <p>Create a qualified functor name.</p>
     *
     * @param sa  The atom.
     * @param mod The module.
     * @param nsa The call-site, not null.
     * @param en  The engine.
     * @return The qualified functor name.
     */
    private static SkelAtom lookupFunctor(SkelAtom sa, SkelAtom mod,
                                          SkelAtom nsa, Engine en) {
        if (isQuali(sa.fun))
            return sa;
        String s1 = composeQuali(mod.fun, sa.fun);

        /* create with call-site */
        int m = (nsa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
        sa = en.store.foyer.createAtom(s1, nsa.scope, m | MASK_ATOM_QALI);
        sa.setPosition(nsa.getPosition());
        sa.setModule(mod);
        return sa;
    }

    /*******************************************************************/
    /* Main Entries                                                    */
    /*******************************************************************/

    /**
     * <p>Retrieve a qualified functor name.</p>
     *
     * @param sa  The atom.
     * @param mod The module.
     * @param nsa The call-site, not null.
     * @param en  The store.
     * @return The qualified functor.
     */
    public static SkelAtom getFunctor(SkelAtom sa, SkelAtom mod,
                                       SkelAtom nsa, Engine en) {
        AbstractCache back = null;
        AbstractCache temp = sa.cache;
        for (; ; ) {
            if (temp == null) {
                SkelAtom sa2 = lookupFunctor(sa, mod, nsa, en);
                CacheFunctor ca = new CacheFunctor();
                ca.mod = mod;
                ca.res = sa2;
                if (back == null) {
                    sa.cache = ca;
                } else {
                    back.next = ca;
                }
                return sa2;
            }
            if (temp instanceof CacheFunctor) {
                CacheFunctor ca = (CacheFunctor) temp;
                if (ca.mod.fun.equals(mod.fun) &&
                        ca.mod.scope == mod.scope &&
                        ca.res.scope == nsa.scope &&
                        (ca.res.getPosition() != null ?
                                ca.res.getPosition().equals(nsa.getPosition()) :
                                null == nsa.getPosition())) {
                    return ca.res;
                }
            }
            back = temp;
            temp = back.next;
        }
    }

    /***************************************************************/
    /* Qualified Names                                             */
    /***************************************************************/

    /**
     * <p>Separate the module name from the qualified name.</p>
     *
     * @param fun The qualified name.
     * @return The module name.
     */
    public static String sepModule(String fun) {
        return fun.substring(0, fun.lastIndexOf(OP_CHAR_SEP));
    }

    /**
     * <p>Separate the name from the qualified name.</p>
     *
     * @param fun The qualified name.
     * @return The name.
     */
    public static String sepName(String fun) {
        return fun.substring(fun.lastIndexOf(OP_CHAR_SEP) + 1);
    }

    /**
     * <p>Check whether a name is qualified.</p>
     *
     * @param fun The name.
     * @return True if the name is qualified, otherwise false.
     */
    public static boolean isQuali(String fun) {
        return (fun.lastIndexOf(OP_CHAR_SEP) != -1);
    }

    /**
     * <p>Compose a qualified name from a module name and a name.</p>
     *
     * @param mod The module name.
     * @param fun The name.
     * @return The qualified name.
     */
    public static String composeQuali(String mod, String fun) {
        StringBuilder buf = new StringBuilder();
        buf.append(mod);
        buf.append(OP_CHAR_SEP);
        buf.append(fun);
        return buf.toString();
    }

}
