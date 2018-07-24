package jekpro.model.molec;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.inter.Engine;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>Provides built-in predicates for dict ops.</p>
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
public class Display {
    public final static Display DISPLAY_CONST = new Display(0);

    public BindCount[] bind;
    public int serno = -1;

    /**
     * <p>Create a display.</p>
     */
    public Display() {
    }

    /**
     * <p>Create a display.</p>
     * <p>Fill the binds with bindvars.</p>
     *
     * @param s The number of variables.
     */
    public Display(int s) {
        if (s == 0)
            return;

        BindCount[] b = new BindCount[s];
        for (int i = 0; i < s; i++)
            b[i] = new BindCount();
        bind = b;
    }

    /**
     * <p>Set the bind size.</p>
     * <p>Refill the binds with bindvars.</p>
     *
     * @param s The bind size.
     */
    public void setSize(int s) {
        BindCount[] b = bind;
        int n = (b != null ? b.length : 0);
        if (n != s) {
            if (s == 0) {
                b = null;
            } else {
                BindCount[] newbind = new BindCount[s];
                n = Math.min(n, s);
                if (n != 0)
                    System.arraycopy(b, 0, newbind, 0, n);
                b = newbind;
            }
            bind = b;
        }
        for (int i = 0; i < s; i++)
            if (b[i] == null)
                b[i] = new BindCount();
    }

    /*********************************************************************/
    /* Utilities                                                         */
    /*********************************************************************/

    /**
     * <p>Determine the display size.</p>
     * <p>Beware, works only for the root copy and not for sub terms.</p>
     *
     * @param m The skeleton.
     * @return The display size.
     */
    public static int displaySize(Object m) {
        Object var= EngineCopy.getVar(m);
        if (var == null)
            return 0;
        if (var instanceof SkelVar) {
            return 1;
        } else {
            return ((SkelVar[]) var).length;
        }
    }

    /**
     * <p>Same as engine remtab for univ builtins.</p>
     */
    public void remTab(Engine en) {
        int n = (bind != null ? bind.length : 0);
        int k = 0;
        if (k < n) {
            do {
                BindCount bc = bind[k];
                if ((--bc.refs) == 0) {
                    bind[k] = null;
                    if (bc.display != null)
                        BindVar.unbind(bc, en);
                }
                k++;
            } while (k < n);
        }
    }

}
