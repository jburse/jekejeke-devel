package matula.util.format;

import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;
import matula.util.regex.ScannerError;
import matula.util.transform.ValidationError;
import matula.util.transform.XPathCheck;
import matula.util.transform.XSDDeclElem;
import matula.util.transform.XSDSchema;

import java.text.ParseException;

/**
 * <p>This class provides an choice point.</p>
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
public final class ChoicePoint {
    public static final String PATH_CANT_CHCPNT = "path_cant_chcpnt";
    public static final String PATH_OVERRUN_PARENT = "path_overrun_parent";

    public static final int CHOICEPOINT_CHILDREN = 0;
    public static final int CHOICEPOINT_PARENT = 1;
    public static final int CHOICEPOINT_CHILD_INDEX = 2;

    private AbstractDom[] children;
    private int pos;
    private XPathExprComb expr;
    private int choice;

    /**
     * <p>Create a new choice point.</p>
     *
     * @param c The type of choice.
     */
    ChoicePoint(int c) {
        choice = c;
    }

    /**
     * <p>Set the expression.</p>
     *
     * @param e The expression.
     */
    void setExpr(XPathExprComb e) {
        expr = e;
    }

    /**
     * <p>Set the index.</p>
     *
     * @param i The index.
     */
    void setPos(int i) {
        pos = i;
    }

    /**
     * <p>Retrieve the choice.</p>
     *
     * @return The choice.
     */
    public int getChoice() {
        return choice;
    }

    /**
     * <p>Retrieve the expression.</p>
     *
     * @return The expression.
     */
    public XPathExprComb getExpr() {
        return expr;
    }

    /*****************************************************/
    /* Search Helper                                     */
    /*****************************************************/

    /**
     * <p>Open the cursor.</p>
     *
     * @param e The dom element.
     * @return The dom element, or null.
     * @throws ScannerError Syntax error.
     */
    DomElement findFirst(DomElement e) throws ScannerError {
        switch (choice) {
            case ChoicePoint.CHOICEPOINT_CHILDREN:
                AbstractDom[] nodes = e.snapshotNodes();
                for (int i = 0; i < nodes.length; i++) {
                    AbstractDom node = nodes[i];
                    if (!(node instanceof DomElement))
                        continue;
                    e = (DomElement) node;
                    if (!expr.evalElement(e))
                        continue;
                    children = nodes;
                    pos = i;
                    return e;
                }
                return null;
            case ChoicePoint.CHOICEPOINT_PARENT:
                return e.getParent();
            case ChoicePoint.CHOICEPOINT_CHILD_INDEX:
                AbstractDom node = e.getNode(pos);
                if (!(node instanceof DomElement))
                    return null;
                return (DomElement) node;
            default:
                throw new IllegalArgumentException("illegal choice");
        }
    }

    /**
     * <p>Advance the cursor.</p>
     *
     * @return The dom element, or null.
     * @throws ScannerError Syntax error.
     */
    DomElement findNext() throws ScannerError {
        switch (choice) {
            case ChoicePoint.CHOICEPOINT_CHILDREN:
                AbstractDom[] nodes = children;
                for (int i = pos + 1; i < nodes.length; i++) {
                    AbstractDom node = nodes[i];
                    if (!(node instanceof DomElement))
                        continue;
                    DomElement e = (DomElement) node;
                    if (!expr.evalElement(e))
                        continue;
                    pos = i;
                    return e;
                }
                children = null;
                return null;
            case ChoicePoint.CHOICEPOINT_PARENT:
                return null;
            case ChoicePoint.CHOICEPOINT_CHILD_INDEX:
                return null;
            default:
                throw new IllegalArgumentException("illegal choice");
        }
    }

    /**
     * <p>Close the cursor.</p>
     */
    void findClose() {
        switch (choice) {
            case ChoicePoint.CHOICEPOINT_CHILDREN:
                children = null;
                break;
            case ChoicePoint.CHOICEPOINT_PARENT:
                break;
            case ChoicePoint.CHOICEPOINT_CHILD_INDEX:
                break;
            default:
                throw new IllegalArgumentException("illegal choice");
        }
    }

    /*****************************************************/
    /* Type Checking                                     */
    /*****************************************************/

    /**
     * <p>Check a choice point.</p>
     *
     * @param e The schema and simulation.
     * @throws ValidationError Check error.
     */
    public void checkElement(XPathCheck e) throws ValidationError {
        switch (getChoice()) {
            case ChoicePoint.CHOICEPOINT_CHILDREN:
                XPathExprComb ex = getExpr();
                if (ex.getCombination() != XPathExprComb.EXPR_COMB_PRED)
                    throw new ValidationError(PATH_CANT_CHCPNT, toString());
                MapHashLink<String, XPathExpr> exprs = ex.getExprs();
                if (exprs == null)
                    throw new ValidationError(PATH_CANT_CHCPNT, toString());
                MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                if (!(entry.value instanceof XPathExprPrim))
                    throw new ValidationError(PATH_CANT_CHCPNT, toString());
                XPathExprPrim prim = (XPathExprPrim) entry.value;
                if (prim.getPrimitive() != XPathExprPrim.EXPR_PRIM_NAME)
                    throw new ValidationError(PATH_CANT_CHCPNT, toString());
                String name = ((XSelectPrim) prim.getFirst()).getAttr();
                XSDSchema schema = e.getSchema();
                XSDDeclElem decl = schema.getDeclElem(name);
                String context = e.getContext();
                if (!XPathCheck.checkParent(context, decl))
                    throw new ValidationError(XPathCheck.PATH_ILLEGAL_PARENT,
                            name + " (" + e.getSchema().getName() + ")");
                ListArray<String> simulation = e.getSimulation();
                simulation.add(name);
                entry = exprs.successor(entry);
                while (entry != null) {
                    entry.value.checkElement(e);
                    entry = exprs.successor(entry);
                }
                break;
            case ChoicePoint.CHOICEPOINT_PARENT:
                simulation = e.getSimulation();
                if (!(simulation.size() > 0))
                    throw new ValidationError(PATH_OVERRUN_PARENT, toString());
                simulation.remove(simulation.size() - 1);
                break;
            case ChoicePoint.CHOICEPOINT_CHILD_INDEX:
                throw new ValidationError(PATH_CANT_CHCPNT, toString());
            default:
                throw new ValidationError(PATH_CANT_CHCPNT, toString());
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
        switch (choice) {
            case ChoicePoint.CHOICEPOINT_CHILDREN:
                return expr.toString();
            case ChoicePoint.CHOICEPOINT_PARENT:
                return "..";
            case ChoicePoint.CHOICEPOINT_CHILD_INDEX:
                return "[" + pos + "]";
            default:
                throw new IllegalArgumentException("illegal choice");
        }
    }

}
