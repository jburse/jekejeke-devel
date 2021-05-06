package jekpro.model.molec;

/**
 * <p>The class provides a markable display.</p>
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
public class DisplayMarkable extends Display {
    private boolean marker;

    /**
     * <p>Create a new display.</p>
     *
     * @param size The requested size.
     */
    public DisplayMarkable(int size) {
        super(size);
        marker = true;
    }

    /**
     * <p>Create a new display.</p>
     *
     * @param size The requested size.
     * @param m    The markable flag.
     */
    public DisplayMarkable(int size, boolean m) {
        super(size);
        marker = m;
    }

    /**
     * <p>Set the marker flag.</p>
     *
     * @param m The marker flag.
     */
    public void setMarker(boolean m) {
        marker = m;
    }

    /**
     * <p>Retrieve and clear the marker flag.</p>
     *
     * @return The marker flag.
     */
    public boolean getAndReset() {
        if (marker) {
            marker = false;
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Create a new display markable.</p>
     *
     * @param size The size.
     * @return The display markable.
     */
    public static Display valueOf(int size) {
        return (size != 0 ? new DisplayMarkable(size) :
                Display.DISPLAY_CONST);
    }

    /**
     * <p>Create a new display markable.</p>
     *
     * @param size The size.
     * @param m    The markable flag.
     * @return The display markable.
     */
    public static Display valueOf(int size, boolean m) {
        return (size != 0 ? new DisplayMarkable(size, m) :
                Display.DISPLAY_CONST);
    }

}