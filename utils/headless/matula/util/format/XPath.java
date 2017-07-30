package matula.util.format;

import matula.util.data.ListArray;

/**
 * <p>This class provides an xpath.</p>
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
public final class XPath {
    private ListArray<XPathExprComb> locs = new ListArray<XPathExprComb>();
    private ListArray<ChoicePoint> hits = new ListArray<ChoicePoint>();

    /*****************************************************/
    /* XPath Expressions                                 */
    /*****************************************************/

    /**
     * <p>Add a new child xpath expression.</p>
     */
    public void whereChild() {
        locs.add(new XPathExprComb(XPathExprComb.CONBINATION_AND));
        hits.add(new ChoicePoint(ChoicePoint.CHOICEPOINT_CHILDREN));
    }

    /**
     * <p>Add a new parent xpath expression.</p>
     */
    public void whereParent() {
        locs.add(new XPathExprComb(XPathExprComb.CONBINATION_AND));
        hits.add(new ChoicePoint(ChoicePoint.CHOICEPOINT_PARENT));
    }

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
     * <p>Add an xpath expression.</p>
     *
     * @param s The slot name.
     * @param p The xath expression.
     */
    public void whereExpr(String s, XPathExpr p) {
        locs.get(locs.size() - 1).whereExpr(s, p);
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
        if (pos == locs.size())
            return e;
        ChoicePoint hit = hits.get(pos);
        switch (hit.getChoice()) {
            case ChoicePoint.CHOICEPOINT_CHILDREN:
                DomNode[] children = e.snapshotChildren();
                for (int i = 0; i < children.length; i++) {
                    DomNode child = children[i];
                    if (!(child instanceof DomElement))
                        continue;
                    e = (DomElement) child;
                    if (!locs.get(pos).checkElement(e))
                        continue;
                    e = findFirst(pos + 1, e);
                    if (e != null) {
                        hit.setChildren(children);
                        hit.setPos(i);
                        return e;
                    }
                }
                return null;
            case ChoicePoint.CHOICEPOINT_PARENT:
                DomElement parent = e.getParent();
                return findFirst(pos + 1, parent);
            default:
                throw new IllegalArgumentException("illegal choice");
        }
    }

    /**
     * <p>Find a next dom element.</p>
     *
     * @return The found dom element, or null.
     */
    public DomElement findNext() {
        for (int pos = hits.size() - 1; pos >= 0; pos--) {
            ChoicePoint hit = hits.get(pos);
            switch (hit.getChoice()) {
                case ChoicePoint.CHOICEPOINT_CHILDREN:
                    DomNode[] children = hit.getChildren();
                    for (int i = hit.getPos() + 1; i < children.length; i++) {
                        DomNode child = children[i];
                        if (!(child instanceof DomElement))
                            continue;
                        DomElement e = (DomElement) child;
                        if (!locs.get(pos).checkElement(e))
                            continue;
                        e = findFirst(pos + 1, e);
                        if (e != null) {
                            hit.setPos(i);
                            return e;
                        }
                    }
                    hit.setChildren(null);
                    break;
                case ChoicePoint.CHOICEPOINT_PARENT:
                    break;
                default:
                    throw new IllegalArgumentException("illegal choice");
            }
        }
        return null;
    }

    /**
     * <p>Close the cursor.</p>
     */
    public void findClose() {
        for (int pos = hits.size() - 1; pos >= 0; pos--) {
            ChoicePoint hit = hits.get(pos);
            switch (hit.getChoice()) {
                case ChoicePoint.CHOICEPOINT_CHILDREN:
                    hit.setChildren(null);
                    break;
                case ChoicePoint.CHOICEPOINT_PARENT:
                    break;
                default:
                    throw new IllegalArgumentException("illegal choice");
            }
        }
    }

    /*****************************************************/
    /* Object Protocol                                   */
    /*****************************************************/

    /**
     * <p>Convert the xpath to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        StringBuilder buf = new StringBuilder();
        for (int i = 0; i < locs.size(); i++) {
            ChoicePoint hit = hits.get(i);
            switch (hit.getChoice()) {
                case ChoicePoint.CHOICEPOINT_CHILDREN:
                    break;
                case ChoicePoint.CHOICEPOINT_PARENT:
                    buf.append("..");
                    break;
                default:
                    throw new IllegalArgumentException("illegal choice");
            }
            if (i != 0)
                buf.append("/");
            buf.append(locs.get(i).toString());
        }
        return buf.toString();
    }

}
