package matula.util.format;

import matula.util.data.ListArray;

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
    private ListArray<XActionFuncAggregate> acts = new ListArray<XActionFuncAggregate>();

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
     * <p>Add a new insert xaction.</p>
     */
    public void calcInsert() {
        acts.add(new XActionFuncAggregate(XActionFuncAggregate.ACTION_INSERT));
    }

    /**
     * <p>Add a new delete xaction.</p>
     */
    public void calcDelete() {
        acts.add(new XActionFuncAggregate(XActionFuncAggregate.ACTION_DELETE));
    }

    /**
     * <p>Add a new update xaction.</p>
     */
    public void calcUpdate() {
        acts.add(new XActionFuncAggregate(XActionFuncAggregate.ACTION_UPDATE));
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
     * @param k The key.
     * @param v The value.
     */
    public void calcAttr(String v, String k) {
        acts.get(acts.size() - 1).calcAttr(v, k);
    }

    /**
     * <p>Add an xquery function.</p>
     *
     * @param s The slot name.
     * @param f The xquery update.
     */
    public void calcFunc(String s, XActionFunc f) {
        acts.get(acts.size() - 1).calcFunc(s, f);
    }

    /*****************************************************/
    /* Modify Helper                                     */
    /*****************************************************/

    /**
     * <p>Perform the actions.</p>
     *
     * @param e The dom element.
     * @throws InterruptedException Transaction was interrupted.
     */
    public void performActions(DomElement e)
            throws InterruptedException {
        for (int i = 0; i < acts.size(); i++) {
            XActionFuncAggregate act = acts.get(i);
            switch (act.getAction()) {
                case XActionFuncAggregate.ACTION_INSERT:
                    DomElement e2 = new DomElement();
                    act.updateElement(e2);
                    e.getParent().addChild(e2);
                    e = e2;
                    break;
                case XActionFuncAggregate.ACTION_DELETE:
                    e2 = e.getParent();
                    e2.removeChild(e);
                    e = e2;
                    break;
                case XActionFuncAggregate.ACTION_UPDATE:
                    act.updateElement(e);
                    break;
                default:
                    throw new IllegalArgumentException("illegal action");
            }
        }
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
            XActionFuncAggregate act = acts.get(i);
            switch (act.getAction()) {
                case XActionFuncAggregate.ACTION_INSERT:
                    buf.append(act.toString());
                    break;
                case XActionFuncAggregate.ACTION_DELETE:
                    buf.append("..");
                    break;
                case XActionFuncAggregate.ACTION_UPDATE:
                    buf.append(".");
                    buf.append(act.toString());
                    break;
                default:
                    throw new IllegalArgumentException("illegal action");
            }
        }
        return buf.toString();
    }

}
