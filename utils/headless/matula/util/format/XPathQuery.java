package matula.util.format;

import matula.util.data.ListArray;

/**
 * <p>This class provides an xpath query.</p>
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
public final class XPathQuery {
    private ListArray<XPathExprComb> locs = new ListArray<XPathExprComb>();
    private ListArray<ChoicePoint> hits = new ListArray<ChoicePoint>();

    /**
     * <p>Create a new xpath query.</p>
     */
    public XPathQuery() {
        locs.add(new XPathExprComb(XPathExprComb.CONBINATION_AND));
    }

    /*****************************************************/
    /* XPath Location                                    */
    /*****************************************************/

    /**
     * <p>Add an element name predicate.</p>
     *
     * @param n The name.
     */
    public void whereName(String n) {
        locs.get(locs.size() - 1).whereName(n);
    }

    /**
     * <p>Add an element attribute predicate.</p>
     *
     * @param k The key.
     * @param v The value.
     */
    public void whereAttr(String k, String v) {
        locs.get(locs.size() - 1).whereAttr(k, v);
    }

    /**
     * <p>Add a predicate.</p>
     *
     * @param slot The slot name.
     * @param pred The predicate.
     */
    public void whereExpr(String slot, XPathExpr pred) {
        locs.get(locs.size() - 1).whereExpr(slot, pred);
    }

    /**
     * <p>Add a new child condition.</p>
     */
    public void whereChild() {
        locs.add(new XPathExprComb(XPathExprComb.CONBINATION_AND));
        hits.add(new ChoicePoint());
    }

    /*****************************************************/
    /* Search Helper                                     */
    /*****************************************************/

    /**
     * <p>Find a first dom element.</p>
     *
     * @param pos The child number.
     * @param e   The child dom element.
     * @return The found dom element, or null.
     */
    public DomElement findFirst(int pos, DomElement e) {
        if (!locs.get(pos).checkElement(e))
            return null;
        if (pos == locs.size() - 1)
            return e;
        DomNode[] children = e.snapshotChildren();
        for (int i = 0; i < children.length; i++) {
            DomNode child = children[i];
            if (!(child instanceof DomElement))
                continue;
            e = findFirst(pos + 1, (DomElement) child);
            if (e != null) {
                ChoicePoint hit = hits.get(pos);
                hit.setChildren(children);
                hit.setPos(i);
                return e;
            }
        }
        return null;
    }

    /**
     * <p>Find a next dom element.</p>
     *
     * @return The found dom element, or null.
     */
    public DomElement findNext() {
        int pos = hits.size() - 1;
        while (pos >= 0) {
            ChoicePoint hit = hits.get(pos);
            DomNode[] children = hit.getChildren();
            for (int i = hit.getPos() + 1; i < children.length; i++) {
                DomNode child = children[i];
                if (!(child instanceof DomElement))
                    continue;
                DomElement e = findFirst(pos + 1, (DomElement) child);
                if (e != null) {
                    hit.setPos(i);
                    return e;
                }
            }
            hit.setChildren(null);
            pos--;
        }
        return null;
    }

    /**
     * <p>Close the cursor.</p>
     */
    public void findClose() {
        int pos = hits.size() - 1;
        while (pos >= 0) {
            ChoicePoint hit = hits.get(pos);
            hit.setChildren(null);
            pos--;
        }
    }

    /**
     * <p>Convert the location to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        StringBuilder buf = new StringBuilder();
        buf.append(locs.get(0).toString());
        for (int i = 1; i < locs.size(); i++) {
            buf.append("/");
            buf.append(locs.get(i).toString());
        }
        return buf.toString();
    }

}
