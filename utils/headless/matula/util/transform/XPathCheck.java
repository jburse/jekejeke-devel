package matula.util.transform;

import matula.util.data.*;
import matula.util.format.*;
import matula.util.regex.ScannerError;
import matula.util.wire.XSelectFormat;

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
public final class XPathCheck {
    public static final String PATH_STRING_SELE = "path_string_sele";
    public static final String PATH_ILLEGAL_PARENT = "path_illegal_parent";
    public static final String PATH_DUPLICATE_VAR = "path_duplicate_var";

    private XSDSchema schema;
    private ListArray<String> simulation;

    /**
     * <p>Set the XSD schema declarations.</p>
     *
     * @param s The XSD schema declarations.
     */
    public void setSchema(XSDSchema s) {
        schema = s;
    }

    /**
     * <p>Retrieve the XSD schema declarations.</p>
     *
     * @return The XSD schema declarations.
     */
    public XSDSchema getSchema() {
        return schema;
    }

    /**
     * <p>Set the path names simulation.</p>
     *
     * @param sim The path names simulation.
     */
    public void setSimulation(ListArray<String> sim) {
        simulation = sim;
    }

    /**
     * <p>Retrieve the path name simulation.</p>
     *
     * @return The path name simulation.
     */
    public ListArray<String> getSimulation() {
        return simulation;
    }

    /**
     * <p>Check an xpath.</p>
     *
     * @param xp The xpath.
     * @throws ValidationError Check error.
     */
    void xpath(XPath xp) throws ValidationError {
        ListArray<ChoicePoint> cps = xp.getChoicePoints();
        for (int i = 0; i < cps.size(); i++) {
            ChoicePoint cp = cps.get(i);
            cp.checkElement(this);
        }
    }

    /**************************************************************/
    /* Parent Constraint                                          */
    /**************************************************************/

    /**
     * <p>Check parent element.</p>
     *
     * @param context The context.
     * @param xe      The XSD schema element declaration.
     * @return True if parent constraint is ok, otherwise false.
     */
    public static boolean checkParent(String context, XSDDeclElem xe) {
        AssocArray<String, Integer> parent = xe.getParent();
        if (parent == null)
            return true;
        for (int i = 0; i < parent.size(); i++) {
            String par = parent.getKey(i);
            if (context.equalsIgnoreCase(par))
                return true;
        }
        return false;
    }

    /**
     * <p>Retrieve the current context.</p>
     *
     * @return The current context.
     */
    public String getContext() {
        return (simulation.size() > 0 ? simulation.get(simulation.size() - 1) : "");
    }

    /**
     * <p>Some test cases.</p
     *
     * @param args Not used.
     * @throws ScannerError  Syntax error.
     * @throws ValidationError Check error.
     */
    /*
    public static void main(String[] args)
            throws ValidationError, ScannerError {
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
        MapHash<String, Class<? extends InterfaceFunc>> functions=new MapHash<String, Class<? extends InterfaceFunc>>();
        functions.add(XSelectFormat.KEY_FORM_DATE, XSelectFormat.class);
        xr.setFunctions(functions);
        MapHash<String, Integer> parameters = new MapHash<String, Integer>();
        parameters.add("x", Integer.valueOf(XSDDeclAttr.TYPE_STRING));
        parameters.add("y", Integer.valueOf(XSDDeclAttr.TYPE_INTEGER));
        xr.setParameters(parameters);

        XPath xpath = xr.createXPath("jack[@foo=$x or not (@foo=<$y)]/jill");
        System.out.println("xpath=" + xpath.toStringChoicePoints());
        try {
            xc.xpath(xpath);
            System.out.println("check(xpath)=passed");
        } catch (ValidationError x) {
            System.out.println("check(xpath)=failed");
        }

        System.out.println();

        XSelect xs = xr.createXSelect("$y*(3+5) + 1000/($y-1)");
        System.out.println("xselect=" + xs);
        try {
            xs.checkElement(xc);
            System.out.println("check(xselect)=passed");
        } catch (ValidationError x) {
            System.out.println("check(xselect)=failed");
        }

        System.out.println();

        XPathExpr xe = xr.createXPathExpr("$x='bar' and $y<456");
        System.out.println("xpathexpr=" + xe);
        try {
            xe.checkElement(xc);
            System.out.println("check(xpathexpr)=passed");
        } catch (ValidationError x) {
            System.out.println("check(xpathexpr)=failed");
        }

        System.out.println();

        xs = xr.createXSelect("format_date('2018-04-12', 'dd. MMM yyyy')");
        System.out.println("xselect=" + xs);
        try {
            xs.checkElement(xc);
            System.out.println("check(xselect)=passed");
        } catch (ValidationError x) {
            System.out.println("check(xselect)=failed");
        }
    }
    */

}