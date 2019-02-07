package matula.util.wire;

import matula.util.data.MapHash;
import matula.util.format.DomElement;
import matula.util.format.XSelect;
import matula.util.format.XSelectPrim;
import matula.util.regex.ScannerError;
import matula.util.system.OpenOpts;
import matula.util.transform.*;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Properties;

/**
 * <p>This class provides some xpath formatting functions.</p>
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
public final class XSelectFormat extends XSelect implements InterfaceFunc {
    public static final String PATTERN_DATE = "yyyy-MM-dd";
    public static final String PATTERN_DATETIME = "yyyy-MM-dd HH:mm:ss";

    public static final int SELE_FORM_DATE = 0;
    public static final int SELE_FORM_DATE_PAT = 1;
    public static final int SELE_FORM_DATE_PAT_LOC = 2;
    public static final int SELE_FORM_DATETIME = 3;
    public static final int SELE_FORM_DATETIME_PAT = 4;
    public static final int SELE_FORM_DATETIME_PAT_LOC = 5;
    public static final int SELE_GET_PROPERTY = 6;
    public static final int SELE_GET_PROPERTY_LOC = 7;

    public static final String OP_FORMAT_DATE = "format_date";
    public static final String OP_FORMAT_DATETIME = "format_datetime";
    public static final String OP_GET_PROPERTY = "get_property";
    public static final String OP_LOCALE = "locale";

    private XSelect date;
    private XSelect pattern;
    private XSelect locale;
    private int format;

    /**
     * <p>Set the indicator.</p>
     *
     * @param key The indicator.
     */
    public void setKey(String key) {
        int k = key.lastIndexOf('/');
        String name = key.substring(0, k);
        int arity = Integer.parseInt(key.substring(k + 1));
        if (OP_FORMAT_DATE.equals(name) && arity == 1) {
            format = SELE_FORM_DATE;
        } else if (OP_FORMAT_DATE.equals(name) && arity == 2) {
            format = SELE_FORM_DATE_PAT;
        } else if (OP_FORMAT_DATE.equals(name) && arity == 3) {
            format = SELE_FORM_DATE_PAT_LOC;
        } else if (OP_FORMAT_DATETIME.equals(name) && arity == 1) {
            format = SELE_FORM_DATETIME;
        } else if (OP_FORMAT_DATETIME.equals(name) && arity == 2) {
            format = SELE_FORM_DATETIME_PAT;
        } else if (OP_FORMAT_DATETIME.equals(name) && arity == 3) {
            format = SELE_FORM_DATETIME_PAT_LOC;
        } else if (OP_GET_PROPERTY.equals(name) && arity == 1) {
            format = SELE_GET_PROPERTY;
        } else if (OP_GET_PROPERTY.equals(name) && arity == 2) {
            format = SELE_GET_PROPERTY_LOC;
        } else {
            throw new IllegalArgumentException("illegal indicator");
        }
    }

    /**
     * <p>Set the arguments.</p>
     *
     * @param a The arguments.
     * @param c The parsing context.
     */
    public void setArgs(Object[] a, XPathRead c)
            throws ScannerError {
        switch (format) {
            case SELE_FORM_DATE:
                if (!(a[0] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                date = (XSelect) a[0];

                XSelect xs1 = new XSelectPrim("date.medium", XSelectPrim.SELE_PRIM_CONST);
                Object[] args = new Object[]{xs1};
                XSelectFormat xs2 = new XSelectFormat();
                xs2.setKey("get_property/1");
                xs2.setArgs(args, c);
                pattern = xs2;

                Object cnst = c.getVariable(OP_LOCALE);
                if (cnst != null) {
                    locale = new XSelectPrim(cnst, XSelectPrim.SELE_PRIM_CONST);
                } else {
                    locale = new XSelectPrim(XSelectPrim.SELE_PRIM_NULL);
                }
                break;
            case SELE_FORM_DATE_PAT:
                if (!(a[0] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                date = (XSelect) a[0];
                if (!(a[1] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                pattern = (XSelect) a[1];
                cnst = c.getVariable(OP_LOCALE);
                if (cnst != null) {
                    locale = new XSelectPrim(cnst, XSelectPrim.SELE_PRIM_CONST);
                } else {
                    locale = new XSelectPrim(XSelectPrim.SELE_PRIM_NULL);
                }
                break;
            case SELE_FORM_DATE_PAT_LOC:
                if (!(a[0] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                date = (XSelect) a[0];
                if (!(a[1] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                pattern = (XSelect) a[1];
                if (!(a[2] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                locale = (XSelect) a[2];
                break;
            case SELE_FORM_DATETIME:
                if (!(a[0] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                date = (XSelect) a[0];

                xs1 = new XSelectPrim("datetime.medium", XSelectPrim.SELE_PRIM_CONST);
                args = new Object[]{xs1};
                xs2 = new XSelectFormat();
                xs2.setKey("get_property/1");
                xs2.setArgs(args, c);
                pattern = xs2;

                cnst = c.getVariable(OP_LOCALE);
                if (cnst != null) {
                    locale = new XSelectPrim(cnst, XSelectPrim.SELE_PRIM_CONST);
                } else {
                    locale = new XSelectPrim(XSelectPrim.SELE_PRIM_NULL);
                }
                break;
            case SELE_FORM_DATETIME_PAT:
                if (!(a[0] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                date = (XSelect) a[0];
                if (!(a[1] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                pattern = (XSelect) a[1];
                cnst = c.getVariable(OP_LOCALE);
                if (cnst != null) {
                    locale = new XSelectPrim(cnst, XSelectPrim.SELE_PRIM_CONST);
                } else {
                    locale = new XSelectPrim(XSelectPrim.SELE_PRIM_NULL);
                }
                break;
            case SELE_FORM_DATETIME_PAT_LOC:
                if (!(a[0] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                date = (XSelect) a[0];
                if (!(a[1] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                pattern = (XSelect) a[1];
                if (!(a[2] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                locale = (XSelect) a[2];
                break;
            case SELE_GET_PROPERTY:
                if (!(a[0] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                pattern = (XSelect) a[0];
                cnst = c.getVariable(OP_LOCALE);
                if (cnst != null) {
                    locale = new XSelectPrim(cnst, XSelectPrim.SELE_PRIM_CONST);
                } else {
                    locale = new XSelectPrim(XSelectPrim.SELE_PRIM_NULL);
                }
                break;
            case SELE_GET_PROPERTY_LOC:
                if (!(a[0] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                pattern = (XSelect) a[0];
                if (!(a[1] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISMATCH_ARGS, OpenOpts.getOffset(c.getReader()));
                locale = (XSelect) a[1];
                break;
            default:
                throw new IllegalArgumentException("illegal format");
        }
    }

    /**
     * <p>Eval an xpath select.</p>
     *
     * @param d The dom element.
     * @return The value.
     * @throws ScannerError Syntax error.
     */
    public Object evalElement(DomElement d) throws ScannerError {
        switch (format) {
            case SELE_FORM_DATE:
            case SELE_FORM_DATE_PAT:
            case SELE_FORM_DATE_PAT_LOC:
                String help = (String) date.evalElement(d);
                SimpleDateFormat sdf2 = new SimpleDateFormat(PATTERN_DATE);
                Date dateres;
                try {
                    dateres = sdf2.parse(help);
                } catch (ParseException x) {
                    throw new ScannerError("illegal date", -1);
                }
                String patternres = (String) pattern.evalElement(d);
                help = (String) locale.evalElement(d);
                Locale loc = stringToLocale(help);
                SimpleDateFormat sdf = new SimpleDateFormat(patternres, loc);
                return sdf.format(dateres);
            case SELE_FORM_DATETIME:
            case SELE_FORM_DATETIME_PAT:
            case SELE_FORM_DATETIME_PAT_LOC:
                help = (String) date.evalElement(d);
                sdf2 = new SimpleDateFormat(PATTERN_DATETIME);
                try {
                    dateres = sdf2.parse(help);
                } catch (ParseException x) {
                    throw new ScannerError("illegal datetime", -1);
                }
                patternres = (String) pattern.evalElement(d);
                help = (String) locale.evalElement(d);
                loc = stringToLocale(help);
                sdf = new SimpleDateFormat(patternres, loc);
                return sdf.format(dateres);
            case SELE_GET_PROPERTY:
            case SELE_GET_PROPERTY_LOC:
                patternres = (String) pattern.evalElement(d);

                help = (String) locale.evalElement(d);
                loc = stringToLocale(help);
                Properties prop = LangProperties.getLang(XSelectFormat.class, "preset", loc);
                return prop.getProperty(patternres);
            default:
                throw new IllegalArgumentException("illegal format");
        }
    }

    /**
     * <p>Check an xpath select.</p>
     *
     * @param d The schema and simulation.
     * @return The type id.
     * @throws ValidationError Check error.
     */
    public int checkElement(XPathCheck d) throws ValidationError {
        switch (format) {
            case SELE_FORM_DATE:
            case SELE_FORM_DATE_PAT:
            case SELE_FORM_DATE_PAT_LOC:
                int typeid = date.checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, date.toString());
                typeid = pattern.checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, pattern.toString());
                typeid = locale.checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, locale.toString());
                return XSDDeclAttr.TYPE_STRING;
            case SELE_FORM_DATETIME:
            case SELE_FORM_DATETIME_PAT:
            case SELE_FORM_DATETIME_PAT_LOC:
                typeid = date.checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, date.toString());
                typeid = pattern.checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, pattern.toString());
                typeid = locale.checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, locale.toString());
                return XSDDeclAttr.TYPE_STRING;
            case SELE_GET_PROPERTY:
            case SELE_GET_PROPERTY_LOC:
                typeid = pattern.checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, pattern.toString());
                typeid = locale.checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, locale.toString());
                return XSDDeclAttr.TYPE_STRING;
            default:
                throw new IllegalArgumentException("illegal format");
        }
    }

    /**
     * <p>Convert this xpath select to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        switch (format) {
            case SELE_FORM_DATE:
                StringBuilder buf = new StringBuilder();
                buf.append(OP_FORMAT_DATE);
                buf.append("(");
                buf.append(date.toString());
                buf.append(")");
                return buf.toString();
            case SELE_FORM_DATE_PAT:
                buf = new StringBuilder();
                buf.append(OP_FORMAT_DATE);
                buf.append("(");
                buf.append(date.toString());
                buf.append(", ");
                buf.append(pattern.toString());
                buf.append(")");
                return buf.toString();
            case SELE_FORM_DATE_PAT_LOC:
                buf = new StringBuilder();
                buf.append(OP_FORMAT_DATE);
                buf.append("(");
                buf.append(date.toString());
                buf.append(", ");
                buf.append(pattern.toString());
                buf.append(", ");
                buf.append(locale.toString());
                buf.append(")");
                return buf.toString();
            case SELE_FORM_DATETIME:
                buf = new StringBuilder();
                buf.append(OP_FORMAT_DATETIME);
                buf.append("(");
                buf.append(date.toString());
                buf.append(")");
                return buf.toString();
            case SELE_FORM_DATETIME_PAT:
                buf = new StringBuilder();
                buf.append(OP_FORMAT_DATETIME);
                buf.append("(");
                buf.append(date.toString());
                buf.append(", ");
                buf.append(pattern.toString());
                buf.append(")");
                return buf.toString();
            case SELE_FORM_DATETIME_PAT_LOC:
                buf = new StringBuilder();
                buf.append(OP_FORMAT_DATETIME);
                buf.append("(");
                buf.append(date.toString());
                buf.append(", ");
                buf.append(pattern.toString());
                buf.append(", ");
                buf.append(locale.toString());
                buf.append(")");
                return buf.toString();
            case SELE_GET_PROPERTY:
                buf = new StringBuilder();
                buf.append(OP_GET_PROPERTY);
                buf.append("(");
                buf.append(pattern.toString());
                buf.append(")");
                return buf.toString();
            case SELE_GET_PROPERTY_LOC:
                buf = new StringBuilder();
                buf.append(OP_GET_PROPERTY);
                buf.append("(");
                buf.append(pattern.toString());
                buf.append(", ");
                buf.append(locale.toString());
                buf.append(")");
                return buf.toString();
            default:
                throw new IllegalArgumentException("illegal format");
        }
    }

    /****************************************************************/
    /* Locale Conversion                                            */
    /****************************************************************/

    /**
     * <p>Convert a string to a locale.</p>
     *
     * @param locstr The string.
     * @return The locale.
     */
    public static Locale stringToLocale(String locstr) {
        if (!"en_GB".equals(locstr)) {
            int k1 = locstr.indexOf('_');
            if (k1 == -1) {
                return new Locale(locstr);
            } else {
                int k2 = locstr.indexOf('_', k1 + 1);
                if (k2 == -1) {
                    return new Locale(locstr.substring(0, k1),
                            locstr.substring(k1 + 1));
                } else {
                    return new Locale(locstr.substring(0, k1),
                            locstr.substring(k1 + 1, k2), locstr.substring(k2 + 1));
                }
            }
        } else {
            return Locale.UK;
        }
    }

    /**
     * <p>Some test cases.</p
     *
     * @param args Not used.
     * @throws ScannerError Syntax error.
     */
    /*
    public static void main(String[] args)
            throws ScannerError, ParseException {
        MapHash<String, Object> variables = new MapHash<String, Object>();
        variables.put("locale", "de_CH");
        XPathReadTransform xr = new XPathReadTransform();
        xr.setVariables(variables);
        xr.setMeta(XSLSheet.meta);

        XSelect xs = xr.createXSelect("format_date('2017-12-31')");
        System.out.println("xselect=" + xs);
        System.out.println("eval(xselect)=" + xs.evalElement(null));

        System.out.println();

        xs = xr.createXSelect("format_datetime('2017-12-31 09:20:13')");
        System.out.println("xselect=" + xs);
        System.out.println("eval(xselect)=" + xs.evalElement(null));

        System.out.println();

        variables = new MapHash<String, Object>();
        variables.put("locale", "en_UK");
        xr.setVariables(variables);

        xs = xr.createXSelect("format_date('2017-12-31')");
        System.out.println("xselect=" + xs);
        System.out.println("eval(xselect)=" + xs.evalElement(null));

        System.out.println();

        xs = xr.createXSelect("format_datetime('2017-12-31 09:20:13')");
        System.out.println("xselect=" + xs);
        System.out.println("eval(xselect)=" + xs.evalElement(null));
    }
    */

}