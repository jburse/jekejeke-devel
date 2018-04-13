package matula.util.wire;

import matula.util.data.ListArray;
import matula.util.data.MapHash;
import matula.util.format.DomElement;
import matula.util.format.XSelect;
import matula.util.regex.ScannerError;
import matula.util.transform.*;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class XSelectFormat extends XSelect implements InterfaceFunc {
    public static final String PATTERN_DATE = "yyyy-MM-dd";
    public static final String PATTERN_DATETIME = "yyyy-MM-dd HH:mm:ss";

    public static final int SELE_FORM_DATE = 0;
    public static final int SELE_FORM_DATE_LOC = 1;
    public static final int SELE_FORM_DATETIME = 2;
    public static final int SELE_FORM_DATETIME_LOC = 3;

    public static final String KEY_FORM_DATE = "format_date/2";
    public static final String KEY_FORM_DATE_LOC = "format_date/3";
    public static final String KEY_FORM_DATETIME = "format_datetime/2";
    public static final String KEY_FORM_DATETIME_LOC = "format_datetime/3";

    private String key;
    private Object[] args;
    private int format;

    /**
     * <p>Set the indicator.</p>
     *
     * @param k The indicator.
     */
    public void setKey(String k) {
        if (KEY_FORM_DATE.equals(k)) {
            format = SELE_FORM_DATE;
        } else if (KEY_FORM_DATE_LOC.equals(k)) {
            format = SELE_FORM_DATE_LOC;
        } else if (KEY_FORM_DATETIME.equals(k)) {
            format = SELE_FORM_DATETIME;
        } else if (KEY_FORM_DATETIME_LOC.equals(k)) {
            format = SELE_FORM_DATETIME_LOC;
        } else {
            throw new IllegalArgumentException("illegal indicator");
        }
        key = k;
    }

    /**
     * <p>Set the arguments.</p>
     *
     * @param a The arguments.
     * @param p The positions.
     */
    public void setArgs(Object[] a, ListArray<Integer> p) throws ScannerError {
        switch (format) {
            case SELE_FORM_DATE:
                if (!(a[0] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISSING_SELE, p.get(0).intValue());
                if (!(a[1] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISSING_SELE, p.get(1).intValue());
                args = a;
                break;
            case SELE_FORM_DATE_LOC:
                if (!(a[0] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISSING_SELE, p.get(0).intValue());
                if (!(a[1] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISSING_SELE, p.get(1).intValue());
                if (!(a[2] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISSING_SELE, p.get(2).intValue());
                args = a;
                break;
            case SELE_FORM_DATETIME:
                if (!(a[0] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISSING_SELE, p.get(0).intValue());
                if (!(a[1] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISSING_SELE, p.get(1).intValue());
                args = a;
                break;
            case SELE_FORM_DATETIME_LOC:
                if (!(a[0] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISSING_SELE, p.get(0).intValue());
                if (!(a[1] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISSING_SELE, p.get(1).intValue());
                if (!(a[2] instanceof XSelect))
                    throw new ScannerError(XPathRead.PATH_MISSING_SELE, p.get(2).intValue());
                args = a;
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
     */
    public Object evalElement(DomElement d) throws ParseException {
        switch (format) {
            case SELE_FORM_DATE:
                String help = (String) ((XSelect) args[0]).evalElement(d);
                SimpleDateFormat sdf2 = new SimpleDateFormat(PATTERN_DATE);
                Date date = sdf2.parse(help);

                String pattern = (String) ((XSelect) args[1]).evalElement(d);
                SimpleDateFormat sdf = new SimpleDateFormat(pattern);
                return sdf.format(date);
            case SELE_FORM_DATE_LOC:
                help = (String) ((XSelect) args[0]).evalElement(d);
                sdf2 = new SimpleDateFormat(PATTERN_DATE);
                date = sdf2.parse(help);

                pattern = (String) ((XSelect) args[1]).evalElement(d);
                help = (String) ((XSelect) args[2]).evalElement(d);
                Locale loc = stringToLocale(help);
                sdf = new SimpleDateFormat(pattern, loc);
                return sdf.format(date);
            case SELE_FORM_DATETIME:
                help = (String) ((XSelect) args[0]).evalElement(d);
                sdf2 = new SimpleDateFormat(PATTERN_DATETIME);
                date = sdf2.parse(help);

                pattern = (String) ((XSelect) args[1]).evalElement(d);
                sdf = new SimpleDateFormat(pattern);
                return sdf.format(date);
            case SELE_FORM_DATETIME_LOC:
                help = (String) ((XSelect) args[0]).evalElement(d);
                sdf2 = new SimpleDateFormat(PATTERN_DATETIME);
                date = sdf2.parse(help);

                pattern = (String) ((XSelect) args[1]).evalElement(d);
                help = (String) ((XSelect) args[2]).evalElement(d);
                loc = stringToLocale(help);
                sdf = new SimpleDateFormat(pattern, loc);
                return sdf.format(date);
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
                int typeid = ((XSelect) args[0]).checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, args[0].toString());
                typeid = ((XSelect) args[1]).checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, args[1].toString());
                return XSDDeclAttr.TYPE_STRING;
            case SELE_FORM_DATE_LOC:
                typeid = ((XSelect) args[0]).checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, args[0].toString());
                typeid = ((XSelect) args[1]).checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, args[1].toString());
                typeid = ((XSelect) args[2]).checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, args[2].toString());
                return XSDDeclAttr.TYPE_STRING;
            case SELE_FORM_DATETIME:
                typeid = ((XSelect) args[0]).checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, args[0].toString());
                typeid = ((XSelect) args[1]).checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, args[1].toString());
                return XSDDeclAttr.TYPE_STRING;
            case SELE_FORM_DATETIME_LOC:
                typeid = ((XSelect) args[0]).checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, args[0].toString());
                typeid = ((XSelect) args[1]).checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, args[1].toString());
                typeid = ((XSelect) args[2]).checkElement(d);
                if (typeid != XSDDeclAttr.TYPE_STRING)
                    throw new ValidationError(XPathCheck.PATH_STRING_SELE, args[2].toString());
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
        StringBuilder buf = new StringBuilder();
        int k = key.lastIndexOf('/');
        buf.append(key.substring(0, k));
        buf.append("(");
        buf.append(args[0].toString());
        for (int i = 1; i < args.length; i++) {
            buf.append(", ");
            buf.append(args[i].toString());
        }
        buf.append(")");
        return buf.toString();
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
    }

    /**
     * <p>Some test cases.</p
     *
     * @param args Not used.
     * @throws ScannerError Syntax error.
     */
    public static void main(String[] args)
            throws ScannerError, ParseException {
        XPathReadTransform xr = new XPathReadTransform();
        xr.setMeta(XSLSheet.meta);

        XSelect xs = xr.createXSelect("format_date('2017-12-31', 'dd. MMM yyyy', 'de_CH')");
        System.out.println("xselect=" + xs);
        System.out.println("eval(xselect)=" + xs.evalElement(null));

        System.out.println();

        xs = xr.createXSelect("format_date('2017-12-31', 'MMM dd. yyyy', 'en_UK')");
        System.out.println("xselect=" + xs);
        System.out.println("eval(xselect)=" + xs.evalElement(null));

        System.out.println();

        xs = xr.createXSelect("format_datetime('2017-12-31 09:20:13', 'dd. MMM yyyy, HH:mm', 'de_CH')");
        System.out.println("xselect=" + xs);
        System.out.println("eval(xselect)=" + xs.evalElement(null));

        System.out.println();

        xs = xr.createXSelect("format_datetime('2017-12-31 09:20:13', 'MMM dd. yyyy, HH:mm', 'en_UK')");
        System.out.println("xselect=" + xs);
        System.out.println("eval(xselect)=" + xs.evalElement(null));
    }

}