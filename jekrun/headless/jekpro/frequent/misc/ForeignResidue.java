package jekpro.frequent.misc;

import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.tools.call.ArrayEnumeration;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.term.TermVar;
import matula.util.data.*;

/**
 * <p>Provides the methods for the module misc/residue.</p>
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
public final class ForeignResidue {

    /**
     * <p>Enumerate the residue attributed variables.</p>
     *
     * @param co The call out.
     * @return The attributed variables.
     */
    public static Object sysResidueAttr(CallOut co, Interpreter inter) {
        ArrayEnumeration<Object> dc;
        if (co.getFirst()) {
            Engine en = (Engine) inter.getEngine();
            ListArray<Object> list = ForeignResidue.listResidueAttrs(en);
            if (list == null)
                return null;
            Object[] res = new Object[list.size()];
            list.toArray(res);
            dc = new ArrayEnumeration<Object>(res);
            co.setData(dc);
        } else {
            dc = (ArrayEnumeration<Object>) co.getData();
        }
        if (!dc.hasMoreElements())
            return null;
        Object res = dc.nextElement();
        co.setRetry(dc.hasMoreElements());
        return res;
    }

    /**
     * <p>List the residue attributed variables.</p>
     *
     * @param en The engine.
     * @return The list or null.
     */
    private static ListArray<Object> listResidueAttrs(Engine en) {
        AbstractMap<BindUniv, Integer> map = en.visor.varmap;
        if (map == null)
            return null;
        ListArray<Object> list = null;
        for (MapEntry<BindUniv, Integer> entry = map.getFirstEntry();
             entry != null; entry = map.successor(entry)) {
            TermVar var = entry.key.getAttr();
            if (var == null)
                continue;
            if (list == null)
                list = new ListArray<Object>();
            list.add(var);
        }
        return list;
    }

}