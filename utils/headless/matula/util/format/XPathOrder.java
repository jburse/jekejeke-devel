package matula.util.format;

import java.util.Comparator;

/**
 * <p>This class provides an order by.</p>
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
public final class XPathOrder implements Comparator<Object> {
    public static final int ORDER_ASC = 0;
    public static final int ORDER_DESC = 1;

    private XSelect select;
    private int direction;

    /**
     * <p>Retrieve the select.</p>
     *
     * @return The select.
     */
    public XSelect getSelect() {
        return select;
    }

    /**
     * <p>Retrieve the direction.</p>
     *
     * @return The direction.
     */
    public int getDirection() {
        return direction;
    }

    /*
     * <p>Create a order by clause.
     *
     * @param s The select.
     * @param d The direction.
     */
    public XPathOrder(XSelect s, int d) {
        if (s == null)
            throw new NullPointerException("select missing");
        select = s;
        direction = d;
    }

    /**
     * <p>Compare two values.</p>
     * @param o1 The first value.
     * @param o2 The second value.
     * @return less < 0, equals = 0, greater > 0
     */
    public int compare(Object o1, Object o2) {
        switch (direction) {
            case XPathOrder.ORDER_ASC:
                return XPathExprPrim.compareTo(o1, o2);
            case XPathOrder.ORDER_DESC:
                return XPathExprPrim.compareTo(o2, o1);
            default:
                throw new IllegalArgumentException("illegal order");
        }
    }

}