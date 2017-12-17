package matula.util.transform;

/**
 * <p>The class provides a domain error type and an damain culprit.</p>
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
public final class ValidationError extends Exception {
    private String id;
    private String culprit;

    /**
     * <p>No stack filling.</p>
     *
     * @return This throwable.
     * @see com.sun.org.apache.xerces.internal.parsers.AbstractDOMParser.Abort
     */
    public Throwable fillInStackTrace() {
        return this;
    }

    /**
     * <p>Create a validation error.</p>
     *
     * @param i The error id.
     * @param c The culprit.
     */
    public ValidationError(String i, String c) {
        id = i;
        culprit = c;
    }

    /**
     * <p>Retrieve the error id.</p>
     *
     * @return The error id.
     */
    public String getError() {
        return id;
    }


    /**
     * <p>Set the culprit.</p>
     *
     * @param c The culprit.
     */
    public void setCulprit(String c) {
        culprit = c;
    }

    /**
     * <p>Retrieve the culprit.</p>
     *
     * @return The culprit.
     */
    public String getCulprit() {
        return culprit;
    }

    /*************************************************************/
    /* Error Parsing                                             */
    /*************************************************************/

    /**
     * <p>Parse a scanner error.</p>
     *
     * @param s The scanner error as a string.
     */
    public ValidationError(String s) {
        parse(s);
    }

    /**
     * <p>Parse a scanner error.</p>
     *
     * @param s The scanner error as a string.
     */
    public void parse(String s) {
        int k1 = s.indexOf('#');
        if (k1 != -1) {
            culprit = s.substring(k1 + 1);
            s = s.substring(0, k1);
        }
        id = s;
    }

    /**
     * Returns the detail message.
     *
     * @return The detail message.
     */
    public String getMessage() {
        return toString();
    }


    /**
     * <p>Unparse the scanner error.</p>
     *
     * @return The scanner error as a string.
     */
    public String toString() {
        String s=id;
        if (culprit != null)
            s+= "#" + culprit;
        return s;
    }

}