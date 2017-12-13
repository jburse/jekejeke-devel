package jekpro.frequent.system;

import matula.util.data.MapHash;
import matula.util.format.DomNode;

/**
 * <p>Helper for dom options.</p>
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
final class DomOpts {
    private int mask = DomNode.MASK_LIST;
    private MapHash<String, Integer> control;

    /**
     * <p>Retrieve the mask.</p>
     *
     * @return The mask.
     */
    int getMask() {
        return mask;
    }

    /**
     * <p>Set the mask.</p>
     *
     * @param m The mask.
     */
    void setMask(int m) {
        mask = m;
    }

    /**
     * <p>Retrieve the control.</p>
     *
     * @return The control.
     */
    MapHash<String, Integer> getControl() {
        return control;
    }

    /**
     * <p>Set the control.</p>
     *
     * @param c The control.
     */
    void setControl(MapHash<String, Integer> c) {
        control = c;
    }

}