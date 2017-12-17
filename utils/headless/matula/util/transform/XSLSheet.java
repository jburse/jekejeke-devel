package matula.util.transform;

import matula.util.regex.ScannerError;
import matula.util.system.AbstractRuntime;

/**
 * <p>This class provides an XSL style sheet base.</p>
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
class XSLSheet {
    private static final String SHEET_MISSING_CLASS = "sheet_missing_class";
    private static final String SHEET_MISMATCHED_BEAN = "sheet_mismatched_bean";
    private static final String SHEET_ILLEGAL_ACCESS = "sheet_illegal_access";
    private static final String SHEET_INST_EXCEPTION = "sheet_inst_exception";

    /**
     * <p>Resolve the bean.</p>
     *
     * @param bean The bean.
     * @return The interface path.
     * @throws ScannerError Validation error.
     */
    public InterfacePath resolveBean(String bean) throws ScannerError {
        try {
            ClassLoader loader = getClass().getClassLoader();
            Class<?> _class = AbstractRuntime.stringToClass(bean, loader);
            if (_class == null)
                throw new ScannerError(SHEET_MISSING_CLASS, -1);
            Object obj = _class.newInstance();
            if (!(obj instanceof InterfacePath))
                throw new ScannerError(SHEET_MISMATCHED_BEAN, -1);
            return (InterfacePath) obj;
        } catch (IllegalAccessException x) {
            throw new ScannerError(SHEET_ILLEGAL_ACCESS, -1);
        } catch (InstantiationException x) {
            throw new ScannerError(SHEET_INST_EXCEPTION, -1);
        }
    }

}