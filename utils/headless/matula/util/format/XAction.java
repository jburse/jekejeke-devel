package matula.util.format;

import matula.util.data.ListArray;
import matula.util.regex.ScannerError;

/**
 * <p>This class provides an xaction.</p>
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
public final class XAction {
    private ListArray<XActionFuncAggr> acts = new ListArray<XActionFuncAggr>();

    /**
     * <p>Retrieve the number of xaction functions.</p>
     *
     * @return The number of xaction functions.
     */
    public int size() {
        return acts.size();
    }

    /*****************************************************/
    /* XAction Functions                                 */
    /*****************************************************/

    /**
     * <p>Add a new delete xaction.</p>
     */
    public void calcDelete() {
        XActionFuncAggr xfa = new XActionFuncAggr(XActionFuncAggr.ACTION_DELETE);
        acts.add(xfa);
    }

    /**
     * <p>Add a new update xaction.</p>
     */
    public void calcUpdate() {
        XActionFuncAggr xfa = new XActionFuncAggr(XActionFuncAggr.ACTION_UPDATE);
        acts.add(xfa);
    }

    /**
     * <p>Add a new index insert xaction.</p>
     *
     * @param i The index.
     */
    public void calcInsertIndex(int i) {
        XActionFuncAggr xfa = new XActionFuncAggr(XActionFuncAggr.ACTION_INSERT_INDEX);
        xfa.setPos(i);
        acts.add(xfa);
    }

    /*****************************************************/
    /* XAction Updates                                   */
    /*****************************************************/

    /**
     * <p>Add an element name action.</p>
     *
     * @param n The name.
     */
    public void calcName(String n) {
        acts.get(acts.size() - 1).calcName(n);
    }

    /**
     * <p>Add an element attribute action.</p>
     *
     * @param a The attribute name.
     * @param v The String value.
     */
    public void calcAttr(String a, String v) {
        acts.get(acts.size() - 1).calcAttr(a, v);
    }

    /**
     * <p>Add an element attribute action.</p>
     *
     * @param a The attribute name.
     * @param v The long value.
     */
    public void calcAttrLong(String a, long v) {
        acts.get(acts.size() - 1).calcAttrLong(a, v);
    }

    /**
     * <p>Add an element attribute action.</p>
     *
     * @param a The attribute name.
     * @param v The value.
     */
    public void calcAttrObj(String a, Object v) {
        acts.get(acts.size() - 1).calcAttrObj(a, v);
    }

    /**
     * <p>Add an element children action.</p>
     *
     * @param n The child name.
     * @param v The list.
     */
    public void calcChild(String n, DomElement v) {
        acts.get(acts.size() - 1).calcChild(n, v);
    }

    /**
     * <p>Add an xaction function.</p>
     *
     * @param k The key.
     * @param v The xaction function.
     */
    public void calcFunc(String k, XActionFunc v) {
        acts.get(acts.size() - 1).calcFunc(k, v);
    }

    /*****************************************************/
    /* Modify Helper                                     */
    /*****************************************************/

    /**
     * <p>Perform the actions.</p>
     *
     * @param e The current dom element.
     * @throws ScannerError Shit happens.
     */
    public DomElement performActions(DomElement e)
            throws ScannerError {
        for (int i = 0; i < acts.size(); i++) {
            XActionFuncAggr act = acts.get(i);
            switch (act.getAction()) {
                case XActionFuncAggr.ACTION_DELETE:
                    DomElement e2 = e.getParent();
                    e2.removeNode(e);
                    e = e2;
                    break;
                case XActionFuncAggr.ACTION_UPDATE:
                    act.updateElement(e, e);
                    break;
                case XActionFuncAggr.ACTION_INSERT_INDEX:
                    e2 = new DomElement();
                    act.updateElement(e2, e);
                    e.addNode(act.getPos(), e2);
                    e = e2;
                    break;
                default:
                    throw new IllegalArgumentException("illegal action");
            }
        }
        return e;
    }

    /*****************************************************/
    /* Object Protocol                                   */
    /*****************************************************/

    /**
     * <p>Convert the xaction to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        StringBuilder buf = new StringBuilder();
        for (int i = 0; i < acts.size(); i++) {
            if (i != 0)
                buf.append("/");
            XActionFuncAggr act = acts.get(i);
            switch (act.getAction()) {
                case XActionFuncAggr.ACTION_DELETE:
                    buf.append("..");
                    break;
                case XActionFuncAggr.ACTION_UPDATE:
                    buf.append(".");
                    buf.append(act.toString());
                    break;
                case XActionFuncAggr.ACTION_INSERT_INDEX:
                    buf.append("[");
                    buf.append(act.getPos());
                    buf.append("]");
                    buf.append(act.toString());
                    break;
                default:
                    throw new IllegalArgumentException("illegal action");
            }
        }
        return buf.toString();
    }

}
