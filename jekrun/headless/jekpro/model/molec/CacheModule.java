package jekpro.model.molec;

import jekpro.model.inter.Engine;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.SourceLocal;
import jekpro.reference.bootload.ForeignPath;
import jekpro.tools.term.SkelAtom;

/**
 * <p>The polymorphic cache for structured module name.</p>
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
public final class CacheModule extends AbstractCache {
    String fun;
    boolean sole;
    Object srcvers;
    SkelAtom res;

    /**
     * <p>Create a module name.</p>
     *
     * @param sa    The atom.
     * @param fun  The package, or null.
     * @param sole  The sole flag.
     * @param scope The call-site, not null.
     * @return The module name.
     * @throws EngineMessage Shit happens.
     */
    private static SkelAtom lookupModule(SkelAtom sa,
                                         String fun, boolean sole,
                                         AbstractSource scope)
            throws EngineMessage {
        if (!sole) {
            if (fun != null) {
                fun = CachePackage.composeStruct(sa.fun, fun);
            } else {
                fun = CachePackage.composeArray(sa.fun);
            }
        } else {
            fun = sa.fun;
        }

        /* lookup prefix from call-site */
        fun = fun.replace(CachePackage.OP_CHAR_SEG, SourceLocal.OP_CHAR_OS);
        fun = Engine.findPrefix(fun, scope, ForeignPath.MASK_MODL_AUTO);
        fun = fun.replace(SourceLocal.OP_CHAR_OS, CachePackage.OP_CHAR_SEG);

        /* create with call-site */
        return new SkelAtom(fun, scope);
    }

    /*******************************************************************/
    /* Main Entries                                                    */
    /*******************************************************************/

    /**
     * <p>Retrieve a module name.</p>
     *
     * @param sa    The atom skeleton.
     * @param fun  The package, or null.
     * @param sole  The sole flag.
     * @param scope The call-site, or null.
     * @param en    The engine.
     * @return The module name.
     * @throws EngineMessage Shit happens.
     */
    public static SkelAtom getModule(SkelAtom sa, String fun,
                                     boolean sole,
                                     AbstractSource scope,
                                     Engine en)
            throws EngineMessage {
        AbstractSource src = (scope != null ? scope : en.store.user);
        AbstractCache back = null;
        AbstractCache temp = sa.cache;
        for (; ; ) {
            if (temp == null) {
                /* cache miss, so lookup */
                Object fixvers = src.fixvers;
                SkelAtom sa2 = lookupModule(sa, fun, sole, src);
                CacheModule ca = new CacheModule();
                ca.fun = fun;
                ca.sole = sole;
                ca.res = sa2;
                ca.srcvers = fixvers;
                if (back == null) {
                    sa.cache = ca;
                } else {
                    back.next = ca;
                }
                return sa2;
            }
            if (temp instanceof CacheModule) {
                CacheModule ca = (CacheModule) temp;
                if ((ca.fun != null ? ca.fun.equals(fun) : null == fun) &&
                        ca.sole == sole && ca.res.scope == src) {
                    SkelAtom sa2;
                    if (ca.srcvers != src.fixvers) {
                        /* cache invalidated, so lookup */
                        Object fixvers = src.fixvers;
                        sa2 = lookupModule(sa, fun, sole, src);
                        ca.fun = fun;
                        ca.sole = sole;
                        ca.res = sa2;
                        ca.srcvers = fixvers;
                    } else {
                        sa2 = ca.res;
                    }
                    return sa2;
                }
            }
            back = temp;
            temp = back.next;
        }
    }

}
