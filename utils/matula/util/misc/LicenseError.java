package matula.util.misc;

import matula.util.wire.LangProperties;

import java.util.Locale;
import java.util.Properties;

/**
 * <p>An exception during the consumption of a license.</p>
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
public final class LicenseError extends Exception {
    public static final String ERROR_LICENSE_ERROR = "license_error";
    public static final String ERROR_LICENSE_OK = "";
    public static final String ERROR_LICENSE_RELEASE_MISMATCH = "release_mismatch";
    public static final String ERROR_LICENSE_LANG_MISMATCH = "lang_mismatch";
    public static final String ERROR_LICENSE_INSTALL_MISMATCH = "install_mismatch";

    public static final String ERROR_LICENSE_HASH_MISSING = "hash_missing";
    public static final String ERROR_LICENSE_NAME_MISSING = "name_missing";
    public static final String ERROR_LICENSE_DATE_MISSING = "date_missing";
    public static final String ERROR_LICENSE_HASH_LOST = "hash_lost";
    public static final String ERROR_LICENSE_NOT_GRANTED = "not_granted";
    public static final String ERROR_LICENSE_ALREADY_CONSUMED = "already_consumed";

    public static final String ERROR_LICENSE_CONNECT_FAILED = "connect_failed";
    public static final String ERROR_LICENSE_UNKNOWN_HOST = "unknown_host";
    public static final String ERROR_LICENSE_IO_EXCEPTION = "io_exception";
    public static final String ERROR_LICENSE_ENCODING = "encoding";
    public static final String ERROR_LICENSE_NOT_FOUND = "not_found";
    public static final String ERROR_LICENSE_NAME_MISMATCH = "name_mismatch";
    public static final String ERROR_LICENSE_DATE_MISMATCH = "date_mismatch";
    public static final String ERROR_LICENSE_ILLEGAL_STORE = "illegal_store";
    public static final String ERROR_LICENSE_EXPIRED = "expired";
    public static final String ERROR_LICENSE_NOT_NEEDSACT = "not_needsact";
    public static final String ERROR_LICENSE_INCOMPLETE = "incomplete";

    public static final String ERROR_LICENSE_FILE_EXPECTED = "file_expected";
    public static final String ERROR_LICENSE_MALFORMED_URL = "malformed_url";
    public static final String ERROR_LICENSE_TRACKING_MISSING = "tracking_missing";
    public static final String ERROR_LICENSE_TRACKING_ERRORNEOUS = "tracking_errorneous";

    /**
     * <p>No stack filling.</p>
     *
     * @return This throwable.
     * @see com.sun.org.apache.xerces.internal.parsers.AbstractDOMParser.Abort
     */
    public Throwable fillInStackTrace() {
        return this;
    }

    /**
     * <p>Create a license error.</p>
     *
     * @param e The error type.
     */
    public LicenseError(String e) {
        super(e);
    }

    /**
     * <p>Retrieve the text bundle.</p>
     *
     * @param locale The locale.
     * @return The text bundle.
     */
    public static Properties getLang(Locale locale) {
        return LangProperties.getLang(LicenseError.class, "license", locale);
    }

}
