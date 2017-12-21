package matula.util.format;

import matula.util.regex.ScannerError;

/**
 * <p>An xml state machine that does validation.</p>
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
public final class DomMachine extends XmlMachine {
    static final String STRING_BANG_DASH_DASH = "!--";
    static final String STRING_QUESTION = "?";

    static final String XML_ILLEGAL_ATTR = "xml_illegal_attr";
    static final String XML_DUPLICATE_ATTR = "xml_duplicate_attr";
    static final String XML_ILLEGAL_VALUE = "xml_illegal_value";

    private boolean ignore;

    /**
     * <p>Type call-back.</p>
     *
     * @param t The type.
     */
    protected void saxType(String t) {
        if (t.startsWith(DomMachine.STRING_BANG_DASH_DASH)) {
            ignore = true;
        } else if (t.startsWith(DomMachine.STRING_QUESTION)) {
            ignore = true;
        } else {
            ignore = false;
        }
        super.saxType(t);
    }

    /**
     * <p>Key call-back.</p>
     *
     * @param k The key.
     * @throws ScannerError Syntax error,
     */
    protected void saxKey(String k) throws ScannerError {
        if (!ignore) {
            if (XmlMachine.isQuoted(k))
                throw new ScannerError(DomMachine.XML_ILLEGAL_ATTR, -1);
            if (XmlMachine.indexAttr(kvs, k) != -1)
                throw new ScannerError(DomMachine.XML_DUPLICATE_ATTR, -1);
        }
        super.saxKey(k);
    }

    /**
     * <p>Value call-back.</p>
     *
     * @param v The value.
     * @throws ScannerError Syntax error,
     */
    protected void saxValue(String v) throws ScannerError {
        if (!ignore) {
            if (isNumber(v)) {
                try {
                    Long.parseLong(v);
                } catch (NumberFormatException x) {
                    throw new ScannerError(DomMachine.XML_ILLEGAL_VALUE, -1);
                }
            }
        }
        super.saxValue(v);
    }

    /**
     * <p>Check whether the value starts as a number.</p>
     *
     * @param v The value.
     * @return True if the value starts as a number, otherwise false.
     */
    static boolean isNumber(String v) {
        return (v.length() > 0 && (Character.isDigit(v.codePointAt(0)) ||
                (v.charAt(0) == '-' && v.length() > 1 &&
                        Character.isDigit(v.codePointAt(1)))));
    }

}