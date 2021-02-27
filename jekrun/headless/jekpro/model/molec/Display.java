package jekpro.model.molec;

import jekpro.model.inter.Engine;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapHashLink;

/**
 * <p>The class provides a display.</p>
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
public class Display {
    public final static Display DISPLAY_CONST = new Display(0);

    public BindUniv[] bind;
    public MapHashLink<String, SkelVar> vars;

    /**
     * <p>Create a new display.</p>
     */
    public Display() {
    }

    /**
     * <p>Create a new display.</p>
     *
     * @param size The requested size.
     */
    public Display(int size) {
        if (size == 0) {
            bind = BindUniv.BIND_CONST;
        } else {
            BindUniv[] b = new BindUniv[size];
            for (int i = 0; i < size; i++)
                b[i] = new BindUniv();
            bind = b;
        }
    }

    /**
     * <p>Create a new display.</p>
     *
     * @param size The size.
     * @return The display.
     */
    public static Display valueOf(int size) {
        return (size != 0 ? new Display(size) :
                Display.DISPLAY_CONST);
    }

    /**
     * <p>Resize the display.</p>
     *
     * @param size The new size.
     */
    public final void setSize(int size) {
        BindUniv[] b = bind;
        if (size != b.length) {
            if (size == 0) {
                b = BindUniv.BIND_CONST;
            } else {
                b = new BindUniv[size];
                int n = Math.min(bind.length, size);
                if (n != 0)
                    System.arraycopy(bind, 0, b, 0, n);
            }
            bind = b;
        }
        for (int i = 0; i < size; i++) {
            if (b[i] == null)
                b[i] = new BindUniv();
        }
    }

    /**
     * <p>Set the marker flag.</p>
     *
     * @param m The marker flag.
     */
    public void setMarker(boolean m) {
        /* */
    }

    /**
     * <p>Retrieve and clear the marker flag.</p>
     *
     * @return The marker flag.
     */
    public boolean getAndReset() {
        return false;
    }

    /**
     * <p>Unbind all the variables of a display.</p>
     *
     * @param en The engine.
     */
    public void remTab(Engine en) {
        BindUniv[] b = bind;
        for (int k = 0; k < b.length; k++) {
            BindUniv bc = b[k];
            int j = bc.refs - BindUniv.MASK_VAR_WEAK;
            if (j == 0) {
                b[k] = null;
                if (bc.display != null)
                    BindUniv.unbind(bc, en);
            } else {
                bc.refs = j;
            }
        }
    }

}