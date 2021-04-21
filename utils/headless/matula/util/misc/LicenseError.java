package matula.util.misc;

import matula.util.wire.LangProperties;

import java.util.Locale;
import java.util.Properties;

/**
 * <p>An exception during the consumption of a license.</p>
 *
 * @author Copyright 2011-2018, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.8.29 (smart and slim prolog interpreter)
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
