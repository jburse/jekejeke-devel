package jekpro.model.molec;

import jekpro.model.inter.Engine;

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

    public final static int MASK_DISP_MLTI = 0x00000001;
    public final static int MASK_DISP_MORE = 0x00000002;
    public final static int MASK_DISP_SOFT = 0x00000004;
    public final static int MASK_DISP_LTGC = 0x00000008;

    public BindUniv[] bind;
    public int flags;

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
     * <p>Retrieve and clear the multi flag.</p>
     *
     * @return The multi flag.
     */
    public boolean getAndReset() {
        if ((flags & Display.MASK_DISP_MLTI) != 0) {
            flags &= ~Display.MASK_DISP_MLTI;
            return true;
        } else {
            return false;
        }
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
            int j = bc.refs;
            if (j == 0) {
                b[k] = null;
                if (bc.display != null)
                    BindUniv.unbind(bc, en);
            } else {
                bc.refs = j - 1;
            }
        }
    }

    /**
     * <p>Perform environment trimming optimization.</p>
     *
     * @param en The engine.
     */
    public void lastCollect(Engine en) {
        if ((flags & Display.MASK_DISP_LTGC) == 0) {
            if (bind.length > 0)
                remTab(en);
            flags |= Display.MASK_DISP_LTGC;
        }
    }

}