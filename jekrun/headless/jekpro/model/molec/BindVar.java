package jekpro.model.molec;

import jekpro.model.inter.Engine;

/**
 * <p>This class provides a trailed variable binder.</p>
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
public class BindVar extends BindUniv {
    private BindVar next;
    private BindVar prev;

    /**
     * <p>Restore state as desired and remove bind from the engine.</p>
     * <p>The current exception is passed via the engine skel.</p>
     * <p>The new current exception is returned via the engine skel.</p>
     *
     * @param en The engine.
     */
    public void unbind(Engine en) {
        BindUniv.unbind(this, en);
    }

    /**
     * <p>Remove this bind from the engine.</p>
     *
     * @param en The engine.
     */
    void removeBind(Engine en) {
        BindVar f = prev;
        BindVar g = next;
        if (f != null) {
            f.next = g;
        } else {
            en.bind = g;
        }
        if (g != null)
            g.prev = f;
    }

    /**
     * <p>Add this bind to the engine.</p>
     *
     * @param en The engine.
     */
    void addBind(Engine en) {
        prev = null;
        BindVar f = en.bind;
        if (f != null)
            f.prev = this;
        next = f;
        en.bind = this;
    }

}
