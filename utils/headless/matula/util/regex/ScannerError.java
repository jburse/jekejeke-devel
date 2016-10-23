package matula.util.regex;

/**
 * <p>The class provides a syntax error type and an error position.</p>
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
public final class ScannerError extends Exception {
    private String id;
    private int pos;

    /**
     * <p>Create a scanner error.</p>
     *
     * @param i The error id.
     * @param p The error position.
     */
    public ScannerError(String i, int p) {
        id = i;
        pos = p;
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
     * <p>Set the error position.</p>
     *
     * @param p The position.
     */
    public void setPos(int p) {
        pos = p;
    }

    /**
     * <p>Retrieve the error position.</p>
     *
     * @return The error position.
     */
    public int getPos() {
        return pos;
    }

    /**
     * <p>Parse a scanner error.</p>
     *
     * @param s The scanner error as a string.
     */
    public ScannerError(String s) {
        parse(s);
    }

    /**
     * <p>Parse a scanner error.</p>
     *
     * @param s The scanner error as a string.
     */
    public void parse(String s) {
        int k1 = s.indexOf('@');
        if (k1 != -1) {
            id = s.substring(0, k1);
            pos = Integer.parseInt(s.substring(k1 + 1));
        } else {
            id = s;
            pos = -1;
        }
    }

    /**
     * <p>Unparse the scanner error.</p>
     *
     * @return The scanner error as a string.
     */
    public String toString() {
        if (pos != -1) {
            return id + "@" + pos;
        } else {
            return id;
        }
    }

    /**
     * <p>Generate a line position.</p>
     * <p>We use two spaces for surrogate pairs which matches the font
     * width of ideographic characters.</p>
     *
     * @param s The line.
     * @param p The position.
     * @return The line position.
     */
    public static String linePosition(String s, int p) {
        StringBuilder buf = new StringBuilder();
        int n;
        if (s != null) {
            buf.append(s);
            if (s.length() != 0 &&
                    s.charAt(s.length() - 1) == CodeType.LINE_EOL) {
                n = s.length() - 1;
            } else {
                buf.append(CodeType.LINE_EOL);
                n = s.length();
            }
        } else {
            n = 0;
        }
        p = Math.min(p, n);
        while (p > 0) {
            buf.append(' ');
            p--;
        }
        buf.append('^');
        return buf.toString();
    }

}
