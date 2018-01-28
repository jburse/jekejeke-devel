package matula.util.format;

import matula.util.data.ListArray;
import matula.util.transform.InterfacePath;

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
    private ListArray<XActionFuncAggr> acts;

    /**
     * <p>Retrieve the acts.</p>
     *
     * @return The acts.
     */
    public ListArray<XActionFuncAggr> getActs() {
        return acts;
    }

    /*****************************************************/
    /* XAction Functions                                 */
    /*****************************************************/

    /**
     * <p>Add a new delete xaction.</p>
     */
    public void calcDelete() {
        XActionFuncAggr xfa = new XActionFuncAggr(XActionFuncAggr.ACTION_DELETE);
        calcAction(xfa);
    }

    /**
     * <p>Add a new update xaction.</p>
     */
    public void calcUpdate() {
        XActionFuncAggr xfa = new XActionFuncAggr(XActionFuncAggr.ACTION_UPDATE);
        calcAction(xfa);
    }

    /**
     * <p>Add a new index insert xaction.</p>
     *
     * @param i The index.
     */
    public void calcInsertIndex(int i) {
        XActionFuncAggr xfa = new XActionFuncAggr(XActionFuncAggr.ACTION_INSERT_INDEX);
        xfa.setPos(i);
        calcAction(xfa);
    }

    /**
     * <p>Add a new replace xaction.</p>
     */
    public void calcReplace() {
        XActionFuncAggr xfa = new XActionFuncAggr(XActionFuncAggr.ACTION_REPLACE);
        calcAction(xfa);
    }

    /**
     * <p>Add a new xaction aggregate.</p>
     *
     * @param xfa The xaction aggregate.
     */
    public void calcAction(XActionFuncAggr xfa) {
        if (acts == null)
            acts = new ListArray<XActionFuncAggr>();
        acts.add(xfa);
    }

    /**
     * <p>Retrieve the top action aggregate.</p>
     *
     * @return The top action aggregate.
     */
    public XActionFuncAggr getTop() {
        return acts.get(acts.size() - 1);
    }

    /*****************************************************/
    /* Modify Helper                                     */
    /*****************************************************/

    /**
     * <p>Perform the actions.</p>
     *
     * @param path The path.
     */
    public DomElement performActions(InterfacePath path) {
        DomElement e = path.getFound();
        if (acts != null) {
            for (int i = 0; i < acts.size(); i++) {
                XActionFuncAggr act = acts.get(i);
                switch (act.getAction()) {
                    case XActionFuncAggr.ACTION_DELETE:
                        DomElement e2 = e.getParent();
                        e2.removeNode(e);
                        e = e2;
                        break;
                    case XActionFuncAggr.ACTION_UPDATE:
                        e = act.updateElement(e, e);
                        break;
                    case XActionFuncAggr.ACTION_INSERT_INDEX:
                        e2 = new DomElement();
                        e2 = act.updateElement(e2, e);
                        e.addNode(act.getPos(), e2);
                        e = e2;
                        break;
                    case XActionFuncAggr.ACTION_REPLACE:
                        e2 = new DomElement();
                        e2 = act.updateElement(e2, e);
                        DomElement e3 = e.getParent();
                        if (e3 == null) {
                            path.setRoot(e2);
                        } else {
                            int k = e3.removeNode(e);
                            if (e2 != null)
                                e3.addNode(k, e2);
                        }
                        e = e2;
                        break;
                    default:
                        throw new IllegalArgumentException("illegal action");
                }
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
        if (acts != null) {
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
                    case XActionFuncAggr.ACTION_REPLACE:
                        buf.append("..[]");
                        buf.append(act.toString());
                        break;
                    default:
                        throw new IllegalArgumentException("illegal action");
                }
            }
        }
        return buf.toString();
    }

}
