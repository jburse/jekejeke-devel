package matula.util.transform;

import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.data.MapHashLink;
import matula.util.format.*;
import matula.util.regex.ScannerError;

import java.io.IOException;

/**
 * <p>This class provides an xpath checker.</p>
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
final class XPathCheck {
    private static final String ERROR_FOREACH_MISSING = "for-each missing";
    public static final String ERROR_CANT_CHECK = "cant check";
    public static final String ERROR_PARENT_OVERRUN = "parent overrun";

    private XSDSchema schema;
    private ListArray<String> simulation;

    /**
     * <p>Set the XSD schema declarations.</p>
     *
     * @param s The XSD schema declarations.
     */
    void setSchema(XSDSchema s) {
        schema = s;
    }

    /**
     * <p>Set the path names simulation.</p>
     *
     * @param sim The path names simulation.
     */
    void setSimulation(ListArray<String> sim) {
        simulation = sim;
    }

    /**
     * <p>Retrieve the path name simulation.</p>
     *
     * @return The path name simulation.
     */
    ListArray<String> getSimulation() {
        return simulation;
    }

    /**
     * <p>Check an xpath.</p>
     *
     * @param xp The xpath.
     */
    void xpath(XPath xp) throws ScannerError {
        ListArray<ChoicePoint> cps = xp.getChoicePoints();
        for (int i = 0; i < cps.size(); i++) {
            ChoicePoint cp = cps.get(i);
            choicepoint(cp);
        }
    }

    /**
     * <p>Check a choice point.</p>
     *
     * @param cp The choice point.
     */
    private void choicepoint(ChoicePoint cp) throws ScannerError {
        switch (cp.getChoice()) {
            case ChoicePoint.CHOICEPOINT_CHILDREN:
                XPathExprComb ex = cp.getExpr();
                if (ex.getCombination() != XPathExprComb.EXPR_COMB_PRED)
                    throw new ScannerError(ERROR_CANT_CHECK);
                MapHashLink<String, XPathExpr> exs = ex.getExprs();
                MapEntry<String, XPathExpr> entry = exs.getFirstEntry();
                if (entry == null)
                    throw new ScannerError(ERROR_CANT_CHECK);
                if (!(entry.value instanceof XPathExprPrim))
                    throw new ScannerError(ERROR_CANT_CHECK);
                XPathExprPrim prim = (XPathExprPrim) entry.value;
                if (prim.getPrimitive() != XPathExprPrim.EXPR_PRIM_NAME)
                    throw new ScannerError(ERROR_CANT_CHECK);
                String name = ((XSelectPrim) prim.getFirst()).getAttr();
                XSDDecl decl = schema.getDecl(name);
                if (decl == null || !(decl instanceof XSDDeclElem))
                    throw new ScannerError(XMLCheck.ERROR_UNDECLARED_NAME);
                XSDDeclElem xe = (XSDDeclElem) decl;
                checkParent(xe);
                simulation.add(name);
                entry = exs.successor(entry);
                while (entry != null) {
                    predicate(entry.value);
                    entry = exs.successor(entry);
                }
                break;
            case ChoicePoint.CHOICEPOINT_PARENT:
                if (!(simulation.size() > 0))
                    throw new ScannerError(ERROR_PARENT_OVERRUN);
                simulation.remove(simulation.size() - 1);
                break;
            case ChoicePoint.CHOICEPOINT_CHILD_INDEX:
                throw new ScannerError(ERROR_CANT_CHECK);
            default:
                throw new IllegalArgumentException("illegal choice");
        }
    }

    /**
     * <p>Check parent element constraint.</p>
     *
     * @param xe The XSD schema element declaration.
     * @throws ScannerError Shit happens.
     */
    private void checkParent(XSDDeclElem xe) throws ScannerError {
        String parent = xe.getParent();
        if (parent == null)
            return;
        if (!"".equals(parent)) {
            if (!(simulation.size() > 0) || !simulation.get(simulation.size() - 1).equalsIgnoreCase(parent))
                throw new ScannerError(XMLCheck.ERROR_PARENT_MISMATCH);
        } else {
            if (simulation.size() > 0)
                throw new ScannerError(XMLCheck.ERROR_PARENT_MISMATCH);
        }
    }

    /**
     * <p>Check a predicate.</p>
     *
     * @param ex The predicate.
     * @throws ScannerError Shit happens.
     */
    void predicate(XPathExpr ex) throws ScannerError {
        if (ex instanceof XPathExprPrim) {
            XPathExprPrim prim = (XPathExprPrim) ex;
            switch (prim.getPrimitive()) {
                case XPathExprPrim.EXPR_PRIM_EQ:
                case XPathExprPrim.EXPR_PRIM_NQ:
                    select(prim.getFirst());
                    select(prim.getSecond());
                    break;
                case XPathExprPrim.EXPR_PRIM_LS:
                case XPathExprPrim.EXPR_PRIM_GR:
                case XPathExprPrim.EXPR_PRIM_LQ:
                case XPathExprPrim.EXPR_PRIM_GQ:
                    int typeid = select(prim.getFirst());
                    if (typeid != XSDDeclAttr.TYPE_INTEGER)
                        throw new ScannerError(XMLCheck.ERROR_TYPE_MISMATCH);
                    typeid = select(prim.getSecond());
                    if (typeid != XSDDeclAttr.TYPE_INTEGER)
                        throw new ScannerError(XMLCheck.ERROR_TYPE_MISMATCH);
                    break;
                default:
                    throw new ScannerError(ERROR_CANT_CHECK);
            }
        } else if (ex instanceof XPathExprComb) {
            XPathExprComb comb = (XPathExprComb) ex;
            switch (comb.getCombination()) {
                case XPathExprComb.EXPR_COMB_OR:
                case XPathExprComb.EXPR_COMB_AND:
                    MapHashLink<String, XPathExpr> exprs = comb.getExprs();
                    for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                         entry != null; entry = exprs.successor(entry)) {
                        predicate(entry.value);
                    }
                    break;
                default:
                    throw new ScannerError(ERROR_CANT_CHECK);
            }
        } else {
            throw new ScannerError(ERROR_CANT_CHECK);
        }
    }

    /**
     * <p>Check an xselect.</p>
     *
     * @param xs The xselect.
     * @return The type id.
     */
    int select(XSelect xs) throws ScannerError {
        if (xs instanceof XSelectPrim) {
            XSelectPrim xp = (XSelectPrim) xs;
            switch (xp.getPrimitive()) {
                case XSelectPrim.SELE_PRIM_ATTR:
                    if (!(simulation.size() > 0))
                        throw new ScannerError(ERROR_FOREACH_MISSING);
                    String name = simulation.get(simulation.size() - 1);
                    String attr = xp.getAttr();
                    XSDDecl decl = schema.getDecl(name + "." + attr);
                    if (decl == null || !(decl instanceof XSDDeclAttr))
                        throw new ScannerError(XMLCheck.ERROR_UNDECLARED_ATTRIBUTE);
                    XSDDeclAttr declattr = (XSDDeclAttr) decl;
                    return declattr.getType();
                case XSelectPrim.SELE_PRIM_CONST:
                    Object val = xp.getCnst();
                    if (val instanceof String) {
                        return XSDDeclAttr.TYPE_STRING;
                    } else if (val instanceof Long) {
                        return XSDDeclAttr.TYPE_INTEGER;
                    } else {
                        return XSDDeclAttr.TYPE_OBJECT;
                    }
                default:
                    throw new IllegalArgumentException("illegal primitive");
            }
        } else if (xs instanceof XSelectComb) {
            XSelectComb xc = (XSelectComb) xs;
            switch (xc.getCombination()) {
                case XSelectComb.SELE_COMB_NEG:
                    int typeid = select(xc.getFirst());
                    if (typeid != XSDDeclAttr.TYPE_INTEGER)
                        throw new ScannerError(XMLCheck.ERROR_TYPE_MISMATCH);
                    break;
                case XSelectComb.SELE_COMB_ADD:
                case XSelectComb.SELE_COMB_SUB:
                case XSelectComb.SELE_COMB_MUL:
                case XSelectComb.SELE_COMB_DIV:
                    typeid = select(xc.getFirst());
                    if (typeid != XSDDeclAttr.TYPE_INTEGER)
                        throw new ScannerError(XMLCheck.ERROR_TYPE_MISMATCH);
                    typeid = select(xc.getSecond());
                    if (typeid != XSDDeclAttr.TYPE_INTEGER)
                        throw new ScannerError(XMLCheck.ERROR_TYPE_MISMATCH);
                    break;
                default:
                    throw new IllegalArgumentException("illegal combination");
            }
            return XSDDeclAttr.TYPE_INTEGER;
        } else {
            throw new ScannerError(ERROR_CANT_CHECK);
        }
    }

    /**
     * <p>Some test cases.</p
     *
     * @param args Not used.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    /*
    public static void main(String[] args)
            throws IOException, ScannerError {
        XSDSchema schema = new XSDSchema();
        schema.putDecl("jack", new XSDDeclElem());
        XSDDeclAttr declattr = new XSDDeclAttr();
        declattr.setType(XSDDeclAttr.TYPE_INTEGER);
        schema.putDecl("jack.foo", declattr);
        schema.putDecl("jill", new XSDDeclElem());

        ListArray<String> simulation = new ListArray<String>();
        XPathCheck xc = new XPathCheck();
        xc.setSimulation(simulation);
        xc.setSchema(schema);

        XPathReadCheck xr = new XPathReadCheck();
        MapHash<String, Integer> parameters = new MapHash<String, Integer>();
        parameters.add("x", Integer.valueOf(XSDDeclAttr.TYPE_STRING));
        parameters.add("y", Integer.valueOf(XSDDeclAttr.TYPE_INTEGER));
        xr.setParameters(parameters);

        XPath xpath = xr.createXPath("jack[@foo=$x or ~ (@foo=<$y)]/jill");
        System.out.println("xpath=" + xpath);
        try {
            xc.xpath(xpath);
            System.out.println("check(xpath)=passed");
        } catch (ScannerError x) {
            System.out.println("check(xpath)=failed");
        }

        XSelect xs = xr.createXSelect("$y*(3+5) + 1000/($y-1)");
        System.out.println("xselect=" + xs);
        try {
            xc.select(xs);
            System.out.println("check(xselect)=passed");
        } catch (ScannerError x) {
            System.out.println("check(xselect)=failed");
        }

        XPathExpr xe = xr.createXPathExpr("$x='bar' and $y<456");
        System.out.println("xpathexpr=" + xe);
        try {
            xc.predicate(xe);
            System.out.println("check(xpathexpr)=passed");
        } catch (ScannerError x) {
            System.out.println("check(xpathexpr)=failed");
        }
    }
    */

}