package matula.util.format;

import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;
import matula.util.data.MapTree;
import matula.util.regex.ScannerError;

import java.text.ParseException;
import java.util.Comparator;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class XPath implements Comparator<Object[]> {
    private ListArray<ChoicePoint> cps;
    private MapHashLink<String, XPathOrder> obs;

    /**
     * <p>Retrieve the choice points.</p>
     *
     * @return The choice points.
     */
    public ListArray<ChoicePoint> getChoicePoints() {
        return cps;
    }

    /**
     * <p>Retrieve the order bys.</p>
     *
     * @return The order bys.
     */
    public MapHashLink<String, XPathOrder> getOrderBys() {
        return obs;
    }

    /*****************************************************/
    /* Choice Points                                     */
    /*****************************************************/

    /**
     * <p>Add a new child xpath expression.</p>
     */
    public void whereChild() {
        ChoicePoint cp = new ChoicePoint(ChoicePoint.CHOICEPOINT_CHILDREN);
        cp.setExpr(new XPathExprComb(XPathExprComb.EXPR_COMB_PRED));
        if (cps == null)
            cps = new ListArray<ChoicePoint>();
        cps.add(cp);
    }

    /**
     * <p>Add a new parent xpath expression.</p>
     */
    public void whereParent() {
        ChoicePoint cp = new ChoicePoint(ChoicePoint.CHOICEPOINT_PARENT);
        if (cps == null)
            cps = new ListArray<ChoicePoint>();
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
        if (cps == null)
            cps = new ListArray<ChoicePoint>();
        cps.add(cp);
    }

    /*****************************************************/
    /* XPath Where Conditions                            */
    /*****************************************************/

    /**
     * <p>Add an element name predicate.</p>
     *
     * @param n The element name.
     */
    public void whereName(String n) {
        cps.get(cps.size() - 1).getExpr().whereName(n);
    }

    /**
     * <p>Add a string attribute value predicate.</p>
     *
     * @param a The attribute name.
     * @param v The String value.
     */
    public void whereAttr(String a, String v) {
        cps.get(cps.size() - 1).getExpr().whereAttr(a, v);
    }

    /**
     * <p>Add a long attribute value predicate.</p>
     *
     * @param a The attribute name.
     * @param v The long value.
     */
    public void whereAttrLong(String a, long v) {
        cps.get(cps.size() - 1).getExpr().whereAttrLong(a, v);
    }

    /**
     * <p>Add an attribute value predicate.</p>
     *
     * @param a The attribute name.
     * @param v The value.
     */
    public void whereAttrObj(String a, Object v) {
        cps.get(cps.size() - 1).getExpr().whereAttrObj(a, v);
    }

    /**
     * <p>Add an xpath expression.</p>
     *
     * @param k The key.
     * @param v The xpath.
     */
    public void whereExpr(String k, XPathExpr v) {
        cps.get(cps.size() - 1).getExpr().whereExpr(k, v);
    }

    /*****************************************************/
    /* XPath Sort Conditions                             */
    /*****************************************************/

    /**
     * <p>Sort an attribute.</p>
     *
     * @param a The attribute name.
     */
    public void sortAttr(String a) {
        XSelect xs = new XSelectPrim(a, XSelectPrim.SELE_PRIM_ATTR);
        XPathOrder xo = new XPathOrder(xs, XPathOrder.ORDER_ASC);
        sortOrder(a, xo);
    }

    /**
     * <p>Sort by a clause.</p>
     *
     * @param k  The key.
     * @param xo The clause.
     */
    public void sortOrder(String k, XPathOrder xo) {
        if (obs == null)
            obs = new MapHashLink<String, XPathOrder>();
        MapEntry<String, XPathOrder> entry = obs.getEntry(k);
        if (entry != null) {
            entry.value = xo;
        } else {
            obs.add(k, xo);
        }
    }

    /*****************************************************/
    /* Search Helper                                     */
    /*****************************************************/

    /**
     * <p>Find a first dom element.</p>
     *
     * @param pos The start choice index.
     * @param e   The start dom element, can be null.
     * @return The found dom element, or null.
     * @throws ScannerError Syntax error.
     */
    public DomElement findFirst(int pos, DomElement e) throws ScannerError {
        if (e == null)
            return null;
        int n = (cps != null ? cps.size() : 0);
        if (!(pos < n))
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
     * @throws ScannerError Syntax error.
     */
    public DomElement findNext() throws ScannerError {
        if (cps != null) {
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
        }
        return null;
    }

    /**
     * <p>Close the cursor.</p>
     */
    public void findClose() {
        if (cps != null) {
            for (int pos = cps.size() - 1; pos >= 0; pos--) {
                ChoicePoint hit = cps.get(pos);
                hit.findClose();
            }
        }
    }

    /*****************************************************/
    /* Sort Helper                                       */
    /*****************************************************/

    /**
     * <p>Count the whered elements.</p>
     *
     * @param e The start dom element.
     * @return The count.
     * @throws ScannerError Syntax error.
     */
    public long findCount(DomElement e) throws ScannerError {
        long res = 0;
        DomElement found = findFirst(0, e);
        while (found != null) {
            res++;
            found = findNext();
        }
        findClose();
        return res;
    }

    /**
     * <p>Sort the whered elements.</p>
     *
     * @param e The start dom element.
     * @return The sort.
     * @throws ScannerError Syntax error.
     */
    public ListArray<DomElement> findSort(DomElement e) throws ScannerError {
        MapTree<Object[], ListArray<DomElement>> tm =
                new MapTree<Object[], ListArray<DomElement>>(this);

        DomElement found = findFirst(0, e);
        while (found != null) {
            Object[] key = computeKey(found);
            ListArray<DomElement> vec = tm.get(key);
            if (vec == null) {
                vec = new ListArray<DomElement>();
                tm.put(key, vec);
            }
            vec.add(found);
            found = findNext();
        }
        findClose();

        ListArray<DomElement> list = new ListArray<DomElement>();
        for (MapEntry<Object[], ListArray<DomElement>> etr = tm.getFirstEntry();
             etr != null; etr = tm.successor(etr)) {
            ListArray<DomElement> vec = etr.value;
            for (int i = 0; i < vec.size(); i++)
                list.add(vec.get(i));
        }
        return list;
    }

    /**
     * <p>Compare two keys.</p>
     *
     * @param o1 The first key.
     * @param o2 The second key.
     * @return less < 0, equals = 0, greater > 0
     */
    public int compare(Object[] o1, Object[] o2) {
        int i = 0;
        for (MapEntry<String, XPathOrder> entry = obs.getFirstEntry();
             entry != null; entry = obs.successor(entry)) {
            int res = entry.value.compare(o1[i], o2[i]);
            if (res != 0)
                return res;
            i++;
        }
        return 0;
    }

    /**
     * <p>Compute a key.</p>
     *
     * @param e The dom element.
     * @return The key.
     * @throws ScannerError Syntax error.
     */
    private Object[] computeKey(DomElement e) throws ScannerError {
        Object[] key = new Object[obs.size()];
        int i = 0;
        for (MapEntry<String, XPathOrder> entry = obs.getFirstEntry();
             entry != null; entry = obs.successor(entry)) {
            XSelect select = entry.value.getSelect();
            key[i] = select.evalElement(e);
            i++;
        }
        return key;
    }

    /*****************************************************/
    /* Object Protocol                                   */
    /*****************************************************/

    /**
     * <p>Convert the choice points to a string.</p>
     *
     * @return The string.
     */
    public String toStringChoicePoints() {
        StringBuilder buf = new StringBuilder();
        for (int i = 0; i < cps.size(); i++) {
            if (i != 0)
                buf.append("/");
            ChoicePoint hit = cps.get(i);
            buf.append(hit.toString());
        }
        return buf.toString();
    }

    /**
     * <p>Convert the order bys to a string.</p>
     *
     * @return The string.
     */
    public String toStringOrderBys() {
        StringBuilder buf = new StringBuilder();
        boolean first = true;
        for (MapEntry<String, XPathOrder> entry = obs.getFirstEntry();
             entry != null; entry = obs.successor(entry)) {
            if (first) {
                first = false;
            } else {
                buf.append(", ");
            }
            buf.append(entry.value.toString());
        }
        return buf.toString();
    }

}
