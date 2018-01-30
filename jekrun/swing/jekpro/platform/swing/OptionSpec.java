package jekpro.platform.swing;

/**
 * Command line option for the Swing version.
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
final class OptionSpec {
    private final int arity;
    private final String[] def;

    /**
     * <p>Create an option specification.</p>
     *
     * @param a The arity.
     * @param d The default, can be null.
     */
    OptionSpec(int a, String[] d) {
        arity = a;
        def = d;
    }

    /**
     * <p>Retrieve the arity.</p>
     *
     * @return The arity.
     */
    int getArity() {
        return arity;
    }

    /**
     * <p>Retrieve the default.</p>
     *
     * @return The default, can be null.
     */
    String[] getDefault() {
        return def;
    }

}
