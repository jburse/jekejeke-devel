package jekpro.model.molec;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Supervisor;
import matula.util.data.ListArray;

/**
 * <p>The class provides a continuation undo.</p>
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
public final class UndoCont extends AbstractBind {
    private final ListArray<BindVar> val;

    /**
     * <p>Create a last binder.</p>
     *
     * @param v The value.
     */
    private UndoCont(ListArray<BindVar> v) {
        val = v;
    }

    /**
     * <p>Set the continuation queue.</p>
     *
     * @param en The engine.
     */
    public void unbind(Engine en) {
        /* set back */
        Supervisor visor = en.visor;
        if (visor.cont != null)
            throw new IllegalStateException("cant override");
        visor.cont = val;

        removeBind(en);
    }

    /**
     * <p>Retrieve and clear the continuation queue.</p>
     *
     * @param en The engine.
     * @return The continuation queue.
     */
    public static ListArray<BindVar> bindCont(Engine en) {
        /* retrieve and clear */
        Supervisor visor = en.visor;
        ListArray<BindVar> val = visor.cont;
        if (val == null)
            throw new IllegalStateException("value missing");
        visor.cont = null;

        UndoCont bs = new UndoCont(val);
        bs.addBind(en);
        return val;
    }

}
