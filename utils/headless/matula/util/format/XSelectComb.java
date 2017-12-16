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
    public static final int SELE_COMB_NEG = 0;
    public static final int SELE_COMB_ADD = 1;
    public static final int SELE_COMB_SUB = 2;
    public static final int SELE_COMB_MUL = 3;
    public static final int SELE_COMB_DIV = 4;

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
     * @param c The type of combination.
     */
    public XSelectComb(XSelect f, int c) {
        if (f == null)
            throw new NullPointerException("first missing");
        first = f;
        combination = c;
    }

    /**
     * <p>>Create a new xselect comb.</p>
     *
     * @param f The first xselect.
     * @param s The second xselect.
     * @param c The type of combination.
     */
    public XSelectComb(XSelect f, XSelect s, int c) {
        if (f == null)
            throw new NullPointerException("first missing");
        first = f;
        if (s == null)
            throw new NullPointerException("second missing");
        second = s;
        combination = c;
    }

    /**
     * <p>Eval an xselect.</p>
     *
     * @param d The dom element.
     * @return The value.
     * @throws IllegalArgumentException Shit happens.
     */
    public Object evalElement(DomElement d) throws IllegalArgumentException {
        if (combination <= SELE_COMB_NEG) {
            Long val = (Long) first.evalElement(d);
            switch (combination) {
                case SELE_COMB_NEG:
                    return Long.valueOf(-val.longValue());
                default:
                    throw new IllegalArgumentException("illegal combiination");
            }
        } else {
            Long val = (Long) first.evalElement(d);
            if (!(val instanceof Long))
                throw new IllegalArgumentException("long expected");
            Long val2 = (Long) second.evalElement(d);
            if (!(val2 instanceof Long))
                throw new IllegalArgumentException("long expected");
            switch (combination) {
                case SELE_COMB_ADD:
                    return Long.valueOf(val.longValue() + val2.longValue());
                case SELE_COMB_SUB:
                    return Long.valueOf(val.longValue() - val2.longValue());
                case SELE_COMB_MUL:
                    return Long.valueOf(val.longValue() * val2.longValue());
                case SELE_COMB_DIV:
                    return Long.valueOf(val.longValue() / val2.longValue());
                default:
                    throw new IllegalArgumentException("illegal combiination");
            }
        }
    }

    /**
     * <p>Convert this xselect to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        if (combination <= SELE_COMB_NEG) {
            switch (combination) {
                case SELE_COMB_NEG:
                    StringBuilder buf = new StringBuilder();
                    buf.append("-");
                    if (!isTerm(first))
                        buf.append("(");
                    buf.append(first.toString());
                    if (!isTerm(first))
                        buf.append(")");
                    return buf.toString();
                default:
                    throw new IllegalArgumentException("illegal combiination");
            }
        } else {
            switch (combination) {
                case SELE_COMB_ADD:
                    StringBuilder buf = new StringBuilder();
                    buf.append(first.toString());
                    buf.append("+");
                    if (!isTerm(second))
                        buf.append("(");
                    buf.append(second.toString());
                    if (!isTerm(second))
                        buf.append(")");
                    return buf.toString();
                case SELE_COMB_SUB:
                    buf = new StringBuilder();
                    buf.append(first.toString());
                    buf.append("-");
                    if (!isTerm(second))
                        buf.append("(");
                    buf.append(second.toString());
                    if (!isTerm(second))
                        buf.append(")");
                    return buf.toString();
                case SELE_COMB_MUL:
                    buf = new StringBuilder();
                    if (!isTerm(first))
                        buf.append("(");
                    buf.append(first.toString());
                    if (!isTerm(first))
                        buf.append(")");
                    buf.append("*");
                    if (!isSimple(second))
                        buf.append("(");
                    buf.append(second.toString());
                    if (!isSimple(second))
                        buf.append(")");
                    return buf.toString();
                case SELE_COMB_DIV:
                    buf = new StringBuilder();
                    if (!isTerm(first))
                        buf.append("(");
                    buf.append(first.toString());
                    if (!isTerm(first))
                        buf.append(")");
                    buf.append("/");
                    if (!isSimple(second))
                        buf.append("(");
                    buf.append(second.toString());
                    if (!isSimple(second))
                        buf.append(")");
                    return buf.toString();
                default:
                    throw new IllegalArgumentException("illegal combiination");
            }
        }
    }

    /**
     * <p>Check whether the given select is a term.</p>
     *
     * @param select The select.
     * @return True if the select is a term, otherwise false.
     */
    private static boolean isTerm(XSelect select) {
        if (isSimple(select))
            return true;
        if (select instanceof XSelectComb)
            return ((XSelectComb) select).getCombination() == XSelectComb.SELE_COMB_MUL ||
                    ((XSelectComb) select).getCombination() == XSelectComb.SELE_COMB_DIV;
        return false;
    }

    /**
     * <p>Check whether the given select is simple.</p>
     *
     * @param select The select.
     * @return True if the select is simple, otherwise false.
     */
    private static boolean isSimple(XSelect select) {
        if (select instanceof XSelectPrim)
            return true;
        return false;
    }

}