package matula.util.config;

import matula.util.system.ForeignArchive;
import matula.util.system.ForeignFile;
import matula.util.system.ForeignUri;
import matula.util.wire.LangProperties;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

/**
 * <p>The base class for a bundle.</p>
 * <p>The syntax of a bundle is as follows:</p>
 * <pre>
 *   bundle = release "," build [ "," lang [ "," name [ "," install [ "," date ]]]].
 *   name = { char \ "," \ "|" }                max length 32
 *   date = { char }                            format "yyyy-MM-dd HH:mm:ss"
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
public class BaseBundle {
    private final static int MAX_USERNAME = 32;

    public final static int MASK_BNDL_NACT = 0x00000001;

    public static char SEPER_ATTR = ',';
    public static char SEPER_OBJ = '|';

    /* platform specific */
    public final static String PROP_CAPA_ICON = "capa.icon";
    public final static String PROP_CAPA_BIGICON = "capa.bigicon";

    public final static String PROP_PRODUCT_COMPANY = "product.company";
    public final static String PROP_PRODUCT_LANG = "product.lang";
    public final static String PROP_PRODUCT_PACK = "product.pack";
    public final static String PROP_PRODUCT_INST = "product.inst";

    public final static String PROP_LICENSE_INFO = "license.info";
    public final static String PROP_LICENSE_SERVICE = "license.action";
    public final static String PROP_LICENSE_EMAIL = "license.email";

    public final static String PROP_SLIP_CAPA = "slip.capa";
    public final static String PROP_SLIP_DONTASK = "slip.dontask";

    public static final String[] VOID_LIST = new String[0];

    private int flags;
    private AbstractDescription description;
    private String release;
    private String lang;
    private String name;
    private Date date;

    /**
     * <p>Create a new check.</p>
     */
    public BaseBundle() {
    }

    /**
     * Retrieve the release.
     *
     * @return The release.
     */
    public String getRelease() {
        return release;
    }

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public int getFlags() {
        return flags;
    }

    /**
     * <p>Set the flags.</p>
     *
     * @param f The flags.
     */
    public void setFlags(int f) {
        flags = f;
    }

    /**
     * <p>Retrieve the description.</p>
     *
     * @return The description.
     */
    public AbstractDescription getDescription() {
        return description;
    }

    /**
     * <p>Set the description.</p>
     *
     * @param d The description.
     */
    public void setDescription(AbstractDescription d) {
        description = d;
    }

    /**
     * Set the release.
     *
     * @param f The release, cannot be null.
     */
    public void setRelease(String f) {
        if (f == null)
            throw new NullPointerException();
        release = f;
    }

    /**
     * Retrieve the language.
     *
     * @return The language.
     */
    public String getLang() {
        return lang;
    }

    /**
     * Set the language.
     *
     * @param l The language, can be null.
     */
    public void setLang(String l) {
        lang = l;
    }

    /**
     * <p>Set the user name.</p>
     * <p>It will be stripped down to 32 characters and
     * all "," and "|" will be removed.</p>
     *
     * @param u The user name, can be null.
     */
    public void setName(String u) {
        if (u != null) {
            StringBuilder buf = new StringBuilder();
            for (int i = 0; i < u.length() && buf.length() < MAX_USERNAME; i++) {
                char ch = u.charAt(i);
                if (ch == SEPER_ATTR) {
                    /* skip */
                } else if (ch == SEPER_OBJ) {
                    /* skip */
                } else {
                    buf.append(ch);
                }
            }
            name = buf.toString();
        } else {
            name = null;
        }
    }

    /**
     * Retrieve the user name.
     *
     * @return The user name.
     */
    public String getName() {
        return name;
    }

    /**
     * <p>Set the install date.</p>
     *
     * @param d The install date, can be null.
     */
    public void setDate(Date d) {
        date = d;
    }

    /**
     * <p>Retrieve the install date.</p>
     *
     * @return The install date.
     */
    public Date getDate() {
        return date;
    }

    /***************************************************************/
    /* Variation Points                                            */
    /***************************************************************/

    /**
     * <p>Create the base tracking.
     *
     * @return The base tracking.
     */
    public BaseTracking createTracking() {
        return null;
    }

    /**
     * <p>Retrieve the parameters of this branch.</p>
     *
     * @return The parameters of this brach.
     */
    public String[] getParams() {
        return VOID_LIST;
    }

    /**
     * <p>Retrieve the hash code of the bundle.</p>
     *
     * @return The hash code.
     */
    public int hashCode() {
        int res = getClass().hashCode();
        String[] params = getParams();
        for (int i = 0; i < params.length; i++)
            res = res * 31 + params[i].hashCode();
        return res;
    }

    /**
     * <p>Check the identity.</p>
     *
     * @param o The other object.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof BaseBundle))
            return false;
        if (!getClass().equals(o.getClass()))
            return false;
        String[] params = getParams();
        String[] oparams = ((BaseBundle) o).getParams();
        if (params.length != oparams.length)
            return false;
        for (int i = 0; i < params.length; i++) {
            if (!params[i].equals(oparams[i]))
                return false;
        }
        return true;
    }

    /**
     * <p>Unparse a check.</p>
     *
     * @param install The install architecture.
     * @return The unparsed check.
     */
    public String unparseCheck(String install) {
        if (date != null) {
            SimpleDateFormat df = new SimpleDateFormat(LangProperties.PATTERN_DATETIME, Locale.UK);
            return release + SEPER_ATTR +
                    lang + SEPER_ATTR +
                    install + SEPER_ATTR +
                    name + SEPER_ATTR +
                    df.format(date);
        } else if (name != null) {
            return release + SEPER_ATTR +
                    lang + SEPER_ATTR +
                    install + SEPER_ATTR +
                    name;
        } else if (install != null) {
            return release + SEPER_ATTR +
                    lang + SEPER_ATTR +
                    install;
        } else if (lang != null) {
            return release + SEPER_ATTR +
                    lang;
        } else {
            return release;
        }
    }

    /**
     * Parse a check.
     *
     * @param s The parse check.
     * @return The install architecture.
     * @throws ParseException Shit happens.
     */
    public String parseCheck(String s) throws ParseException {
        String install;
        int k1 = s.indexOf(SEPER_ATTR);
        if (k1 != -1) {
            release = s.substring(0, k1);
            int k0 = s.indexOf(SEPER_ATTR, k1 + 1);
            if (k0 != -1) {
                lang = s.substring(k1 + 1, k0);
                k1 = s.indexOf(SEPER_ATTR, k0 + 1);
                if (k1 != -1) {
                    install = s.substring(k0 + 1, k1);
                    k0 = s.indexOf(SEPER_ATTR, k1 + 1);
                    if (k0 != -1) {
                        name = s.substring(k1 + 1, k0);
                        SimpleDateFormat df = new SimpleDateFormat(LangProperties.PATTERN_DATETIME, Locale.UK);
                        date = df.parse(s.substring(k0 + 1));
                    } else {
                        name = s.substring(k1 + 1);
                        date = null;
                    }
                } else {
                    install = s.substring(k0 + 1);
                    name = null;
                    date = null;
                }
            } else {
                lang = s.substring(k1 + 1);
                install = null;
                name = null;
                date = null;
            }
        } else {
            release = s;
            lang = null;
            install = null;
            name = null;
            date = null;
        }
        return install;
    }

    /**
     * <p>Retrieve the install date of a class.</p>
     *
     * @param clazz The class.
     * @param name  The resource name.
     * @return The install date, or null.
     */
    public static Date getInstallDate(Class<?> clazz, String name) {
        URL url = clazz.getResource(name);
        if (url == null)
            throw new RuntimeException("get install date: " + name + " not found");
        String adr = url.toString();
        try {
            adr = BaseBundle.removeJar(adr);
        } catch (MalformedURLException x) {
            throw new RuntimeException("get install date: " + name + " not supported", x);
        }
        String path = ForeignArchive.extractPath(adr);
        if (path == null)
            throw new RuntimeException("get install date: " + name + " not supported");
        File file = new File(path);
        Date date = new Date(file.lastModified());
        try {
            date = BaseBundle.truncateDate(date);
        } catch (ParseException x) {
            throw new RuntimeException("get install date: " + name + " not supported", x);
        }
        return date;
    }

    /**
     * <p>Remove the jar prefix and suffix from an URI.</p>
     *
     * @param adr The URI.
     * @return The URI without jar prefix and suffix.
     * @throws MalformedURLException Shit happens.
     */
    private static String removeJar(String adr) throws MalformedURLException {
        String spec = ForeignUri.sysUriSpec(adr);
        String scheme = ForeignUri.sysSpecScheme(spec);
        if (!scheme.equals(ForeignUri.SCHEME_JAR))
            return adr;
        String authority = ForeignUri.sysSpecAuthority(spec);
        String path = ForeignUri.sysSpecPath(spec);
        int k = path.lastIndexOf(ForeignUri.JAR_SEP);
        if (k != -1) {
            spec = ForeignUri.sysSpecMake(ForeignFile.STRING_EMPTY, authority, path.substring(0, k));
        } else {
            spec = ForeignUri.sysSpecMake(ForeignFile.STRING_EMPTY, authority, path);
        }
        return ForeignUri.sysUriMake(spec, ForeignFile.STRING_EMPTY, ForeignFile.STRING_EMPTY);
    }

    /**
     * <p>Truncate a date.</p></P></o>
     *
     * @param d The date.
     * @return The truncated date.
     * @throws ParseException Shit happens.
     */
    private static Date truncateDate(Date d) throws ParseException {
        SimpleDateFormat df = new SimpleDateFormat(LangProperties.PATTERN_DATETIME, Locale.UK);
        String str = df.format(d);
        return df.parse(str);
    }

}
