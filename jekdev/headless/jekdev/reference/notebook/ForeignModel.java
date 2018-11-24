package jekdev.reference.notebook;

import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.call.ArrayEnumeration;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.TermAtomic;
import matula.util.format.AbstractDom;
import matula.util.format.DomElement;
import matula.util.format.DomText;

import java.math.BigInteger;

/**
 * <p>The foreign predicates for the module notebook/model.</p>
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignModel {

    /*******************************************************************/
    /* Node Access & Modification                                      */
    /*******************************************************************/

    /**
     * <p>Check whether the dom node is a dom element.</p>
     *
     * @param dn The node.
     * @return True if the node is a dom element.
     */
    public static boolean sysNodeIsElem(AbstractDom dn) {
        return (dn instanceof DomElement);
    }

    /**
     * <p>Check whether the dom node is a dom text.</p>
     *
     * @param dn The node.
     * @return True if the node is a dom text.
     */
    public static boolean sysNodeIsText(AbstractDom dn) {
        return (dn instanceof DomText);
    }

    /**
     * <p>Copy a dom node.</p>
     *
     * @param dn The dom node.
     * @return The copied dom node.
     */
    public static AbstractDom sysNodeCopy(AbstractDom dn) {
        return (AbstractDom) dn.clone();
    }

    /*******************************************************************/
    /* Text Access & Modification                                      */
    /*******************************************************************/

    /**
     * <p>Retrieve the data of a text element.</p>
     *
     * @param text The text element.
     * @return The Prolog data.
     */
    public static Object sysGetData(DomText text) {
        Object val = text.getDataObj();
        if (!(val instanceof String)) {
            if (!(val instanceof Long)) {
                val = (TermAtomic.guardDouble(((Double) val).doubleValue()) ? val :
                        TermAtomic.ZERO_DOUBLE);
            } else {
                val = TermAtomic.normBigInteger(((Long) val).longValue());
            }
        }
        return val;
    }

    /**
     * <p>Set the data of a text element.</p>
     *
     * @param text The text element.
     * @param val  The Prolog data.
     * @throws ClassCastException Validation error.
     * @throws InterpreterMessage Validation error.
     */
    public static void sysSetData(DomText text, Object val)
            throws ClassCastException, InterpreterMessage {
        if (!(val instanceof String)) {
            Number num = InterpreterMessage.castNumber(val);
            if (!(val instanceof Integer) && !(val instanceof BigInteger)) {
                val = (num instanceof Double ? num :
                        TermAtomic.makeDouble(num.doubleValue()));
            } else {
                val = Long.valueOf(SpecialEval.castLongValue(num));
            }
        }
        text.setDataObj(val);
    }

    /*******************************************************************/
    /* Element Access & Modification                                   */
    /*******************************************************************/

    /**
     * <p>Non-determinstic predicate for the attribute names.</p>
     *
     * @param co The call out.
     * @param dh The dom hashtable.
     * @return The attribute names.
     */
    public static String sysElemAttr(CallOut co, DomElement dh) {
        ArrayEnumeration<AbstractDom> dc;
        if (co.getFirst()) {
            dc = new ArrayEnumeration<AbstractDom>(dh.snapshotAttrs());
            co.setData(dc);
        } else {
            dc = (ArrayEnumeration<AbstractDom>) co.getData();
        }
        if (!dc.hasMoreElements())
            return null;
        AbstractDom res = dc.nextElement();
        co.setRetry(dc.hasMoreElements());
        return res.getKey();
    }

    /**
     * <p>Non-determinstic predicate for the children.</p>
     *
     * @param co The call out.
     * @param dh The dom hashtable.
     * @return The DOM node.
     */
    public static AbstractDom sysElemNode(CallOut co, DomElement dh) {
        ArrayEnumeration<AbstractDom> dc;
        if (co.getFirst()) {
            dc = new ArrayEnumeration<AbstractDom>(dh.snapshotNodes());
            co.setData(dc);
        } else {
            dc = (ArrayEnumeration<AbstractDom>) co.getData();
        }
        if (!dc.hasMoreElements())
            return null;
        AbstractDom res = dc.nextElement();
        co.setRetry(dc.hasMoreElements());
        return res;
    }

}