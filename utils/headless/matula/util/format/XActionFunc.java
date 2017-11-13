package matula.util.format;

import matula.util.regex.ScannerError;

/**
 * <p>This class is the base class of xquery functions.</p>
 * </p>
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
public abstract class XActionFunc {

    /**
     * <p>Perform this xquery function on a dom element.</p>
     *
     * @param r The target dom element.
     * @param e The source dom element.
     * @throws ScannerError Shit happens.
     */
    public abstract void updateElement(DomElement r, DomElement e)
            throws ScannerError;

    /**
     * <p>Convert this xquery function to a string.</p>
     *
     * @return The string.
     */
    public abstract String toString();

}
