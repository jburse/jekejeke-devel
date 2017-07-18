package jekpro.frequent.system;

import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import matula.util.format.DomElement;
import matula.util.format.DomNode;
import matula.util.format.DomText;

/**
 * <p>The foreign predicates for the module system/xml.</p>
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
public final class ForeignDom {

    /**
     * <p>Check whether the dom node is a dom element.</p>
     *
     * @param dn The node.
     * @return True if the node is a dom element.
     */
    public static boolean sysNodeIsElem(DomNode dn) {
        return (dn instanceof DomElement);
    }

    /**
     * <p>Check whether the dom node is a dom text.</p>
     *
     * @param dn The node.
     * @return True if the node is a dom text.
     */
    public static boolean sysNodeIsText(DomNode dn) {
        return (dn instanceof DomText);
    }

    /**
     * <p>Non-determinstic predicate for the attribute names.</p>
     *
     * @param co The call out.
     * @param dh The dom hashtable.
     * @return The attribute names.
     */
    public static String sysElemAttr(CallOut co, DomElement dh) {
        DomCursor<String> attributes;
        if (co.getFirst()) {
            attributes = new DomCursor<String>(dh.snapshotAttrs());
            co.setData(attributes);
        } else {
            attributes = (DomCursor<String>) co.getData();
        }
        if (attributes.hasMoreElements()) {
            co.setRetry(true);
            return attributes.nextElement();
        }
        return null;
    }

    /**
     * <p>Non-determinstic predicate for the children.</p>
     *
     * @param co The call out.
     * @param dh The dom hashtable.
     * @return The attribute names.
     */
    public static DomNode sysElemChild(CallOut co, DomElement dh) {
        DomCursor<DomNode> children;
        if (co.getFirst()) {
            children = new DomCursor<DomNode>(dh.snapshotChildren());
            co.setData(children);
        } else {
            children = (DomCursor<DomNode>) co.getData();
        }
        if (children.hasMoreElements()) {
            co.setRetry(true);
            return children.nextElement();
        }
        return null;
    }

}
