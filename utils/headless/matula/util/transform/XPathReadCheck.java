package matula.util.transform;

import matula.util.data.MapHash;
import matula.util.format.DomElement;
import matula.util.regex.ScannerError;
import matula.util.system.OpenOpts;

import java.sql.Timestamp;

/**
 * <p>This class provides an xpath reader during check.</p>
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
final class XPathReadCheck extends XPathRead {
    private static final String PATH_UNDECLARED_VAR = "path_undeclared_var";

    private static Object VOID_OBJECT = new Object();
    private static Object VOID_ELEMENT = new DomElement();

    private MapHash<String, Integer> parameters;

    /**
     * <p>Set the parameters.</p>
     *
     * @param p The parameters.
     */
    void setParameters(MapHash<String, Integer> p) {
        parameters = p;
    }

    /**************************************************************/
    /* Variation Points                                           */
    /**************************************************************/

    /**
     * <p>Retrieve a variable value.</p>
     *
     * @param n The variable name.
     * @return The variable value.
     * @throws ScannerError Syntax error.
     */
    Object getVariable(String n) throws ScannerError {
        Integer type = parameters.get(n);
        if (type == null)
            throw new ScannerError(PATH_UNDECLARED_VAR, OpenOpts.getOffset(reader));
        switch (type.intValue()) {
            case XSDDeclAttr.TYPE_OBJECT:
                return VOID_OBJECT;
            case XSDDeclAttr.TYPE_STRING:
                return "";
            case XSDDeclAttr.TYPE_INTEGER:
                return Long.valueOf(0);
            case XSLSheet.TYPE_ELEMENT:
                return VOID_ELEMENT;
            default:
                throw new IllegalArgumentException("illegal type");
        }
    }

}