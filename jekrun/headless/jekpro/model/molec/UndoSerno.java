package jekpro.model.molec;

import jekpro.model.inter.Engine;

/**
 * <p>The class provides a serial number undo.</p>
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
public final class UndoSerno extends BindVar {

    /**
     * <p>Create a serno binder.</p>
     *
     * @param d The bind count.
     */
    private UndoSerno(BindLexical d) {
        skel = d;
    }

    /**
     * <p>Reset the serno.</p>
     *
     * @param en The engine.
     */
    public void unbind(Engine en) {
        /* reset serno */
        BindLexical bc = (BindLexical) skel;
        int k = bc.serno;
        if (k == -1)
            throw new IllegalStateException("value missing");
        bc.serno = -1;
        en.serno = k;

        removeBind(en);
    }

    /**
     * <p>Set a new serno.</p>
     *
     * @param d  The display.
     * @param en The engine.
     * @return The new serno.
     */
    public static int bindSerno(BindLexical d, Engine en) {
        /* set serno */
        if (d.serno != -1)
            throw new IllegalStateException("cant override");
        int k = en.serno;
        d.serno = k;
        en.serno = k + 1;

        UndoSerno bs = new UndoSerno(d);
        bs.addBind(en);

        return k;
    }

}
