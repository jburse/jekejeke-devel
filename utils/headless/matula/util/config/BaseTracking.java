package matula.util.config;

import matula.util.misc.LicenseError;
import matula.util.wire.LangProperties;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * <p>The base class for license grant tracking.</p>
 * <p>The syntax of a license grant is as follows:</p>
 * <pre>
 *   tracking = license [ "," expiration ].
 *   expiration = { char }                  format "yyyy-MM-dd"
 * </pre>
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
public class BaseTracking {
    private long lastmodified = -1;
    private String error = LicenseError.ERROR_LICENSE_OK;
    private String license = "";
    private Date expiration;

    /**
     * <p>Create a undefined license grant.</p>
     */
    public BaseTracking() {
    }

    /**
     * <p>Create a license grant from string.</p>
     *
     * @param s The string.
     */
    public BaseTracking(String s) {
        try {
            parse(s);
        } catch (Exception x) {
            throw new RuntimeException(x);
        }
    }

    /**
     * <p>Retrieve the last modified.</p>
     *
     * @return The last modified.
     */
    public long getLastModified() {
        return lastmodified;
    }

    /**
     * <p>Set the last modified.</p>
     *
     * @param l The last modified.
     */
    public void setLastModified(long l) {
        lastmodified = l;
    }

    /**
     * <p>Retrieve the error.</p>
     *
     * @return The error.
     */
    public String getError() {
        return error;
    }

    /**
     * <p>Set the error.</p>
     *
     * @param e The error.
     */
    public void setError(String e) {
        error = e;
    }

    /**
     * <p>Retrieve the license type.</p>
     *
     * @return The license type.
     */
    public String getLicense() {
        return license;
    }

    /**
     * <p>Set the license type.</p>
     *
     * @param t The license type.
     */
    public void setLicense(String t) {
        license = t;
    }

    /**
     * <p>Retrieve the expiration.</p>
     *
     * @return The expiration.
     */
    public Date getExpiration() {
        return expiration;
    }

    /**
     * <p>Set the expiration.</p>
     *
     * @param e The expiration.
     */
    public void setExpiration(Date e) {
        expiration = e;
    }

    /**
     * <p>Unparse to string.</p>
     *
     * @return The string.
     */
    public String toString() {
        if (expiration != null) {
            SimpleDateFormat df = new SimpleDateFormat(LangProperties.PATTERN_DATE);
            return license + BaseBundle.SEPER_ATTR + df.format(expiration);
        } else {
            return license;
        }
    }

    /**
     * <p>Parse from string.</p>
     *
     * @param info The string.
     * @throws ParseException Shit happens.
     */
    public void parse(String info) throws ParseException {
        int k = info.indexOf(BaseBundle.SEPER_ATTR);
        if (k != -1) {
            SimpleDateFormat df = new SimpleDateFormat(LangProperties.PATTERN_DATE);
            expiration = df.parse(info.substring(k + 1));
            license = info.substring(0, k);
        } else {
            expiration = null;
            license = info;
        }
    }

    /**
     * <p>Check whether the license is missing.</p>
     *
     * @return True if the license is missing, otherwise false.
     */
    public boolean isMissing() {
        return ("".equals(license));
    }

    /**
     * <p>Check whether the license is expired.</p>
     *
     * @return True if the license is expired, otherwise false.
     */
    public boolean isExpired() {
        if (expiration != null) {
            long time1 = expiration.getTime();
            long time2 = System.currentTimeMillis();
            return time1 <= time2;
        } else {
            return false;
        }
    }

}