package matula.util.format;

import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

/**
 * <p>This class represents an xquery aggregate function.</p>
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
public final class XActionFuncAggregate extends XActionFunc {
    public static final int AGGEGATE_SEQ = 0;

    private MapHashLink<String, XActionFunc> funcs = new MapHashLink<String, XActionFunc>();
    private int aggregate;

    /**
     * <p>Retrieve the size.</p>
     *
     * @return The size.
     */
    int size() {
        return funcs.size();
    }

    /**
     * <p>Create a new xquery aggregate function.</p>
     *
     * @param a The type of aggegate.
     */
    public XActionFuncAggregate(int a) {
        aggregate = a;
    }

    /**
     * <p>Add an element name action.</p>
     *
     * @param n The name.
     */
    public void calcName(String n) {
        calcFunc(n, new XActionFuncUpdate(n, XActionFuncUpdate.UPDATE_NAME));
    }

    /**
     * <p>Add an element attribute action.</p>
     *
     * @param k The key.
     * @param v The value.
     */
    public void calcAttr(String k, String v) {
        calcFunc(k, new XActionFuncUpdate(k, v, XActionFuncUpdate.UPDATE_ATTR));
    }

    /**
     * <p>Add an xquery function.</p>
     *
     * @param s The slot name.
     * @param f The xquery update.
     */
    public void calcFunc(String s, XActionFunc f) {
        MapEntry<String, XActionFunc> entry = funcs.getEntry(s);
        if (entry != null) {
            entry.value = f;
        } else {
            funcs.put(s, f);
        }
    }

    /**
     * <p>Perform this xquery function on a dom element.</p>
     *
     * @param e The dom element.
     */
    void updateElement(DomElement e) {
        for (MapEntry<String, XActionFunc> entry = funcs.getFirstEntry();
             entry != null; entry = funcs.successor(entry)) {
            entry.value.updateElement(e);
        }
    }

    /**
     * <p>Convert this xquery function to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        switch (aggregate) {
            case AGGEGATE_SEQ:
                StringBuilder buf = new StringBuilder();
                boolean first = true;
                for (MapEntry<String, XActionFunc> entry = funcs.getFirstEntry();
                     entry != null; entry = funcs.successor(entry)) {
                    if (first) {
                        buf.append(entry.value.toString());
                        first = false;
                    } else {
                        buf.append("[");
                        buf.append(entry.value.toString());
                        buf.append("]");
                    }
                }
                return buf.toString();
            default:
                throw new IllegalArgumentException("illegal aggregate");
        }
    }

}
