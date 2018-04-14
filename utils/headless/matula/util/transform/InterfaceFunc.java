package matula.util.transform;

import matula.util.data.ListArray;
import matula.util.regex.ScannerError;

/**
 * <p>This class provides a function interface.</p>
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
public interface InterfaceFunc {

    /**
     * <p>Set the indicator.</p>
     *
     * @param key The indicator.
     */
    void setKey(String key);

    /**
     * <p>Set the arguments.</p>
     *
     * @param args The arguments.
     * @return True if the function node could be intialized, otherwise false.
     */
    boolean setArgs(Object[] args);

}