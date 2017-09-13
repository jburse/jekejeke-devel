package matula.util.format;

import matula.util.data.ListArray;
import matula.util.regex.ScannerError;

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
    private ListArray<ChoicePoint> cps = new ListArray<ChoicePoint>();

    /**
     * <p>Retrieve the number of xpath choices.</p>
     *
     * @return The number of xpath choices.
     */
    public int size() {
        return cps.size();
    }

    /**
     * <p>Retrieve the choice points.</p>
     *
     * @return The choice points.
     */
    public ListArray<ChoicePoint> getChoicePoints() {
        return cps;
    }

    /*****************************************************/
    /* Choice Points                                     */
    /*****************************************************/

    /**
     * <p>Add a new child xpath expression.</p>
     */
    public void whereChild() {
        ChoicePoint cp = new ChoicePoint(ChoicePoint.CHOICEPOINT_CHILDREN);
        cp.setExpr(new XPathExprComb(XPathExprComb.CONBINATION_AND));
        cps.add(cp);
    }

    /**
     * <p>Add a new parent xpath expression.</p>
     */
    public void whereParent() {
        ChoicePoint cp = new ChoicePoint(ChoicePoint.CHOICEPOINT_PARENT);
        cps.add(cp);
    }

    /**
     * <p>Add a new index child xpath expression.</p>
     *
     * @param i The index.
     */
    public void whereChildIndex(int i) {
        ChoicePoint cp = new ChoicePoint(ChoicePoint.CHOICEPOINT_CHILD_INDEX);
        cp.setPos(i);
        cps.add(cp);
    }

    /*****************************************************/
    /* XPath Expressions                                 */
    /*****************************************************/

    /**
     * <p>Add an element name predicate.</p>
     *
     * @param n The name.
     */
    public void whereName(String n) {
        cps.get(cps.size() - 1).whereName(n);
    }

    /**
     * <p>Add an element attribute predicate.</p>
     *
     * @param k The key.
     * @param v The value.
     */
    public void whereAttr(String k, String v) {
        cps.get(cps.size() - 1).whereAttr(k, v);
    }

    /**
     * <p>Add an xpath expression.</p>
     *
     * @param s The slot name.
     * @param p The xath expression.
     */
    public void whereExpr(String s, XPathExpr p) {
        cps.get(cps.size() - 1).whereExpr(s, p);
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
     * @throws ScannerError Shit happens
     */
    public DomElement findFirst(int pos, DomElement e)
            throws ScannerError {
        if (pos == cps.size())
            return e;
        ChoicePoint hit = cps.get(pos);
        e = hit.findFirst(e);
        while (e != null) {
            e = findFirst(pos + 1, e);
            if (e != null)
                return e;
            e = hit.findNext();
        }
        return null;
    }

    /**
     * <p>Find a next dom element.</p>
     *
     * @return The found dom element, or null.
     * @throws ScannerError Shit happens
     */
    public DomElement findNext() throws ScannerError {
        for (int pos = cps.size() - 1; pos >= 0; pos--) {
            ChoicePoint hit = cps.get(pos);
            DomElement e = hit.findNext();
            while (e != null) {
                e = findFirst(pos + 1, e);
                if (e != null)
                    return e;
                e = hit.findNext();
            }
        }
        return null;
    }

    /**
     * <p>Close the cursor.</p>
     */
    public void findClose() {
        for (int pos = cps.size() - 1; pos >= 0; pos--) {
            ChoicePoint hit = cps.get(pos);
            hit.findClose();
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
        for (int i = 0; i < cps.size(); i++) {
            if (i != 0)
                buf.append("/");
            ChoicePoint hit = cps.get(i);
            buf.append(hit.toString());
        }
        return buf.toString();
    }

}
