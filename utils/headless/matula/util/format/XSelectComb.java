package matula.util.format;

import matula.util.regex.ScannerError;

/**
 * <p>The class represent an xselect prim.</p>
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
public class XSelectComb extends XSelect {
    public static final int SELE_COMB_ADD = 0;

    private XSelect first;
    private XSelect second;
    private int combination;

    /**
     * <p>Retrieve the first argument.</p>
     *
     * @return The first argument.
     */
    public XSelect getFirst() {
        return first;
    }

    /**
     * <p>Retrieve the second argument.</p>
     *
     * @return The second argument.
     */
    public XSelect getSecond() {
        return second;
    }

    /**
     * <p>Retrieve the type of combination.</p>
     *
     * @return The type of combination.
     */
    public int getCombination() {
        return combination;
    }

    /**
     * <p>>Create a new xselect comb.</p>
     *
     * @param f The first xselect.
     * @param s The second xselect.
     * @param c The type of combination.
     */
    public XSelectComb(XSelect f, XSelect s, int c) {
        first = f;
        second = s;
        combination = c;
    }

    /**
     * <p>Eval an xselect.</p>
     *
     * @param d The dom element.
     * @return The value.
     * @throws ScannerError Shit happens.
     */
    public Object evalElement(DomElement d) throws ScannerError {
        switch (combination) {
            case SELE_COMB_ADD:
                Object val = first.evalElement(d);
                if (!(val instanceof Long))
                    throw new IllegalArgumentException("long expected");
                Object val2 = second.evalElement(d);
                if (!(val2 instanceof Long))
                    throw new IllegalArgumentException("long expected");
                return Long.valueOf(((Long) val).longValue() + ((Long) val2).longValue());
            default:
                throw new IllegalArgumentException("illegal combiination");
        }
    }

    /**
     * <p>Convert this xselect to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        switch (combination) {
            case SELE_COMB_ADD:
                StringBuilder buf = new StringBuilder();
                buf.append(first.toString());
                buf.append(" + ");
                buf.append(second.toString());
                return buf.toString();
            default:
                throw new IllegalArgumentException("illegal combiination");
        }
    }

}