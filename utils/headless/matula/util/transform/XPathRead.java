package matula.util.transform;

import matula.util.data.MapHash;
import matula.util.data.SetHash;
import matula.util.format.*;
import matula.util.regex.ScannerError;
import matula.util.system.ConnectionReader;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;

/**
 * <p>This class provides an xpath reader.</p>
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
abstract class XPathRead {
    private static final String PATH_MISSING_PRED = "path_missing_pred";
    private static final String PATH_MISSING_SQRBKT = "path_missing_sqrbkt";
    private static final String PATH_MISSING_PERIOD = "path_missing_period";
    private static final String PATH_MISSING_CHCPNT = "path_missing_chcpnt";
    private static final String PATH_MISSING_SELE = "path_missing_sele";
    private static final String PATH_MISSING_PRNTHS = "path_missing_prnths";
    private static final String PATH_MISSING_ATTR = "path_missing_attr";
    private static final String PATH_MISSING_VAR = "path_missing_var";
    private static final String PATH_SUPERFLOUS_TOKEN = "path_superflous_token";
    private static final String PATH_ILLEGAL_VALUE = "path_illegal_value";

    private static SetHash<String> reserved = new SetHash<String>();

    protected Reader reader;
    protected StreamTokenizer st;

    static {
        reserved.add(XPathExprComb.OP_TRUE);
        reserved.add(XPathExprComb.OP_FALSE);
        reserved.add(XPathExprComb.OP_OR);
        reserved.add(XPathExprComb.OP_AND);
        reserved.add(XSelectPrim.OP_TS);
    }

    /**
     * <p>Set the reader.</p>
     *
     * @param r The reader.
     * @throws IOException Shit happens.
     */
    void setReader(Reader r) throws IOException {
        reader = r;
        st = createTokenizer(r);
        st.nextToken();
    }

    /**
     * <p>Create a tokenizer suitable for the systax.</p>
     *
     * @param r The reader.
     * @return The tokenizer.
     */
    private static StreamTokenizer createTokenizer(Reader r) {
        StreamTokenizer st = new StreamTokenizer(r);
        st.resetSyntax();
        st.wordChars('a', 'z');
        st.wordChars('A', 'Z');
        st.wordChars('0', '9');
        st.wordChars('.', '.');
        st.whitespaceChars(0, ' ');
        st.quoteChar('"');
        st.quoteChar('\'');
        st.slashSlashComments(false);
        st.commentChar('%');
        st.slashStarComments(true);
        return st;
    }

    /**************************************************************/
    /* Choicepoints                                               */
    /**************************************************************/

    /**
     * <p>Parse an xpath.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     xpath --> choicepoint { "/" choicepoint }.
     * </pre>
     *
     * @return The xpath.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private XPath xpath()
            throws IOException, ScannerError {
        XPath xpath = new XPath();
        choicepoint(xpath);
        while (st.ttype == '/') {
            st.nextToken();
            choicepoint(xpath);
        }
        return xpath;
    }

    /**
     * <p>Parse a choice point.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     choicepoint --> name { "[" predicate "]" }
     *                   | ".."
     *                   | "[" integer "]".
     * </pre>
     *
     * @param xp The xpath.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private void choicepoint(XPath xp)
            throws IOException, ScannerError {
        if (isName()) {
            xp.whereChild();
            String name = st.sval;
            st.nextToken();
            xp.whereName(name);
            while (st.ttype == '[') {
                st.nextToken();
                Object res2 = predicate();
                if (!(res2 instanceof XPathExpr))
                    throw new ScannerError(PATH_MISSING_PRED, OpenOpts.getOffset(reader));
                res2 = ((XPathExpr) res2).lift(XPathExprComb.EXPR_COMB_PRED);
                XPathExprComb res = xp.getChoicePoints().get(xp.size() - 1).getExpr();
                res.join((XPathExprComb) res2);
                if (st.ttype != ']')
                    throw new ScannerError(PATH_MISSING_SQRBKT, OpenOpts.getOffset(reader));
                st.nextToken();
            }
        } else if (st.ttype == '.') {
            st.nextToken();
            if (st.ttype == '.') {
                st.nextToken();
                xp.whereParent();
            } else {
                throw new ScannerError(PATH_MISSING_PERIOD, OpenOpts.getOffset(reader));
            }
        } else if (st.ttype == '[') {
            st.nextToken();
            if (st.ttype == StreamTokenizer.TT_WORD && Character.isDigit(st.sval.charAt(0))) {
                int index = Integer.parseInt(st.sval);
                st.nextToken();
                xp.whereChildIndex(index);
            }
            if (st.ttype != ']')
                throw new ScannerError(PATH_MISSING_SQRBKT, OpenOpts.getOffset(reader));
            st.nextToken();
        } else {
            throw new ScannerError(PATH_MISSING_CHCPNT, OpenOpts.getOffset(reader));
        }
    }


    /**
     * <p>Convenience method to parse an xpath.</p>
     *
     * @param s The string.
     * @return The xpath.
     * @throws ScannerError Syntax error.
     */
    XPath createXPath(String s)
            throws ScannerError {
        ConnectionReader cr = new ConnectionReader(new StringReader(s));
        cr.setLineNumber(1);
        try {
            setReader(cr);
            XPath c = xpath();
            checkEof();
            return c;
        } catch (IOException x) {
            throw new RuntimeException("internal error", x);
        } catch (ScannerError sc) {
            sc.setLine(OpenOpts.getLine(cr));
            throw sc;
        }
    }

    /**************************************************************/
    /* Predicate                                                  */
    /**************************************************************/

    /**
     * <p>Parse a predicate.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     predicate      --> predicate "or" term
     *                      | term.
     * <pre>
     * @throws IOException Shit happens.
     * @throws ScannerError Syntax error.
     */
    private Object predicate()
            throws IOException, ScannerError {
        Object res = predicateTerm();
        while (st.ttype == StreamTokenizer.TT_WORD && st.sval.equals(XPathExprComb.OP_OR)) {
            st.nextToken();
            if (!(res instanceof XPathExpr))
                throw new ScannerError(PATH_MISSING_PRED, OpenOpts.getOffset(reader));
            res = ((XPathExpr) res).lift(XPathExprComb.EXPR_COMB_OR);
            Object res2 = predicateTerm();
            if (!(res2 instanceof XPathExpr))
                throw new ScannerError(PATH_MISSING_PRED, OpenOpts.getOffset(reader));
            res2 = ((XPathExpr) res2).lift(XPathExprComb.EXPR_COMB_OR);
            ((XPathExprComb) res).join((XPathExprComb) res2);
        }
        return res;
    }

    /**
     * <p>Parse a term predicate.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     term      --> term "and" simple
     *                 | simple.
     * <pre>
     * @throws IOException Shit happens.
     * @throws ScannerError Syntax error.
     */
    private Object predicateTerm()
            throws IOException, ScannerError {
        Object res = predicatesSimple();
        while (st.ttype == StreamTokenizer.TT_WORD && st.sval.equals(XPathExprComb.OP_AND)) {
            st.nextToken();
            if (!(res instanceof XPathExpr))
                throw new ScannerError(PATH_MISSING_PRED, OpenOpts.getOffset(reader));
            res = ((XPathExpr) res).lift(XPathExprComb.EXPR_COMB_AND);
            Object res2 = predicatesSimple();
            if (!(res2 instanceof XPathExpr))
                throw new ScannerError(PATH_MISSING_PRED, OpenOpts.getOffset(reader));
            res2 = ((XPathExpr) res2).lift(XPathExprComb.EXPR_COMB_AND);
            ((XPathExprComb) res).join((XPathExprComb) res2);
        }
        return res;
    }

    /**
     * <p>Parse a simple predicate.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     simple    -->  "~" simple
     *                 | "false"
     *                 | "true"
     *                 | select "=" select.
     *
     *     const --> "'" { char } "'"
     *             | """" { char } """"
     *             | "$" name.
     * </pre>
     *
     * @return The simple predicate.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private Object predicatesSimple()
            throws IOException, ScannerError {
        Object res;
        if (st.ttype == '~') {
            st.nextToken();
            res = select();
            if (!(res instanceof XPathExpr))
                throw new ScannerError(PATH_MISSING_PRED, OpenOpts.getOffset(reader));
            ((XPathExpr) res).complement();
        } else if (st.ttype == StreamTokenizer.TT_WORD && st.sval.equals(XPathExprComb.OP_FALSE)) {
            st.nextToken();
            res = new XPathExprComb(XPathExprComb.EXPR_COMB_OR);
        } else if (st.ttype == StreamTokenizer.TT_WORD && st.sval.equals(XPathExprComb.OP_TRUE)) {
            st.nextToken();
            res = new XPathExprComb(XPathExprComb.EXPR_COMB_AND);
        } else {
            res = select();
            int compid;
            if (st.ttype == '=') {
                st.nextToken();
                if (st.ttype == '<') {
                    st.nextToken();
                    compid = XPathExprPrim.EXPR_PRIM_LQ;
                } else {
                    compid = XPathExprPrim.EXPR_PRIM_EQ;
                }
            } else if (st.ttype == '<') {
                st.nextToken();
                if (st.ttype == '>') {
                    st.nextToken();
                    compid = XPathExprPrim.EXPR_PRIM_NQ;
                } else {
                    compid = XPathExprPrim.EXPR_PRIM_LS;
                }
            } else if (st.ttype == '>') {
                st.nextToken();
                if (st.ttype == '=') {
                    st.nextToken();
                    compid = XPathExprPrim.EXPR_PRIM_GQ;
                } else {
                    compid = XPathExprPrim.EXPR_PRIM_GR;
                }
            } else {
                compid = -1;
            }
            if (compid != -1) {
                if (!(res instanceof XSelect))
                    throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
                Object res2 = select();
                if (!(res2 instanceof XSelect))
                    throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
                res = new XPathExprPrim((XSelect) res, (XSelect) res2, compid);
            }
        }
        return res;
    }

    /**
     * <p>Convenience method to parse a xpath expr.</p>
     *
     * @param s The string.
     * @return The xpath expr.
     * @throws ScannerError Syntax error.
     */
    XPathExpr createXPathExpr(String s)
            throws ScannerError {
        ConnectionReader cr = new ConnectionReader(new StringReader(s));
        cr.setLineNumber(1);
        try {
            setReader(cr);
            Object res = predicate();
            if (!(res instanceof XPathExpr))
                throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
            checkEof();
            return (XPathExpr) res;
        } catch (IOException x) {
            throw new RuntimeException("internal error", x);
        } catch (ScannerError sc) {
            sc.setLine(OpenOpts.getLine(cr));
            throw sc;
        }
    }

    /**************************************************************/
    /* Select                                                     */
    /**************************************************************/

    /**
     * <p>Parse an xselect.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     select      --> "-" term
     *                   | select "+" term
     *                   | select "-" term
     *                   | term.
     * </pre>
     *
     * @return The xselect.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private Object select()
            throws IOException, ScannerError {
        Object res;
        if (st.ttype == '-') {
            st.nextToken();
            res = selectTerm();
            if (!(res instanceof XSelect))
                throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
            res = new XSelectComb((XSelect) res, XSelectComb.SELE_COMB_NEG);
        } else {
            res = selectTerm();
        }
        for (; ; ) {
            if (st.ttype == '+') {
                st.nextToken();
                if (!(res instanceof XSelect))
                    throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
                Object res2 = selectTerm();
                if (!(res2 instanceof XSelect))
                    throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
                res = new XSelectComb((XSelect) res, (XSelect) res2, XSelectComb.SELE_COMB_ADD);
            } else if (st.ttype == '-') {
                st.nextToken();
                if (!(res instanceof XSelect))
                    throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
                Object res2 = selectTerm();
                if (!(res2 instanceof XSelect))
                    throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
                res = new XSelectComb((XSelect) res, (XSelect) res2, XSelectComb.SELE_COMB_SUB);
            } else {
                break;
            }
        }
        return res;
    }

    /**
     * <p>Parse an xselect term.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     term      --> term "*" simple
     *                 | term "/" simple
     *                 | simple.
     * </pre>
     *
     * @return The xselect.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private Object selectTerm()
            throws IOException, ScannerError {
        Object res = selectSimple();
        for (; ; ) {
            if (st.ttype == '*') {
                st.nextToken();
                if (!(res instanceof XSelect))
                    throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
                Object res2 = selectSimple();
                if (!(res2 instanceof XSelect))
                    throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
                res = new XSelectComb((XSelect) res, (XSelect) res2, XSelectComb.SELE_COMB_MUL);
            } else if (st.ttype == '/') {
                st.nextToken();
                if (!(res instanceof XSelect))
                    throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
                Object res2 = selectSimple();
                if (!(res2 instanceof XSelect))
                    throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
                res = new XSelectComb((XSelect) res, (XSelect) res2, XSelectComb.SELE_COMB_DIV);
            } else {
                break;
            }
        }
        return res;
    }

    /**
     * <p>Parse an xselect simple.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     simple      --> "(" predicate ")"
     *                   | "@" name
     *                   | "'" { char } "'"
     *                   | digit { digit }
     *                   | "$" name.
     *                   | """" { char } """"
     *                   | name
     * </pre>
     *
     * @return The xselect.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private Object selectSimple()
            throws IOException, ScannerError {
        Object res;
        if (st.ttype == '(') {
            st.nextToken();
            res = predicate();
            if (st.ttype != ')')
                throw new ScannerError(PATH_MISSING_PRNTHS, OpenOpts.getOffset(reader));
            st.nextToken();
        } else if (st.ttype == '@') {
            st.nextToken();
            if (!isName())
                throw new ScannerError(PATH_MISSING_ATTR, OpenOpts.getOffset(reader));
            String name = st.sval;
            st.nextToken();
            res = new XSelectPrim(name, XSelectPrim.SELE_PRIM_ATTR);
        } else if (st.ttype == '\'') {
            String cnst = st.sval;
            st.nextToken();
            res = new XSelectPrim(cnst, XSelectPrim.SELE_PRIM_CONST);
        } else if (st.ttype == StreamTokenizer.TT_WORD && Character.isDigit(st.sval.charAt(0))) {
            Object cnst;
            try {
                if (st.sval.indexOf('.') != -1) {
                    cnst = Double.valueOf(st.sval);
                } else {
                    cnst = Long.valueOf(st.sval);
                }
            } catch (NumberFormatException x) {
                throw new ScannerError(PATH_ILLEGAL_VALUE, OpenOpts.getOffset(reader));
            }
            st.nextToken();
            res = new XSelectPrim(cnst, XSelectPrim.SELE_PRIM_CONST);
        } else if (st.ttype == StreamTokenizer.TT_WORD && st.sval.equals(XSelectPrim.OP_TS)) {
            st.nextToken();
            if (st.ttype != '\'')
                throw new ScannerError(PATH_ILLEGAL_VALUE, OpenOpts.getOffset(reader));
            Object cnst;
            try {
                SimpleDateFormat sd = new SimpleDateFormat(XSelectPrim.TIMESTAMP_XPATH);
                cnst = new Timestamp(sd.parse(st.sval).getTime());
            } catch (ParseException x) {
                throw new ScannerError(PATH_ILLEGAL_VALUE, OpenOpts.getOffset(reader));
            }
            st.nextToken();
            res = new XSelectPrim(cnst, XSelectPrim.SELE_PRIM_CONST);
        } else if (st.ttype == '$') {
            st.nextToken();
            if (!isName())
                throw new ScannerError(PATH_MISSING_VAR, OpenOpts.getOffset(reader));
            String var = st.sval;
            st.nextToken();
            Object cnst = getVariable(var);
            res = new XSelectPrim(cnst, XSelectPrim.SELE_PRIM_CONST);
        } else if (st.ttype == '"') {
            String cnst = st.sval;
            st.nextToken();
            StringReader sr = new StringReader(cnst);
            DomElement de = new DomElement();
            int mask = AbstractDom.MASK_LIST + AbstractDom.MASK_TEXT;
            de.load(sr, mask);
            res = new XSelectPrim(de, XSelectPrim.SELE_PRIM_CONST);
        } else if (isName()) {
            String name = st.sval;
            st.nextToken();
            res = new XSelectPrim(name, XSelectPrim.SELE_PRIM_CHILD);
        } else {
            throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
        }
        return res;
    }

    /**
     * <p>Convenience method to parse a xselect.</p>
     *
     * @param s The string.
     * @return The xselect.
     * @throws ScannerError Syntax error.
     */
    XSelect createXSelect(String s)
            throws ScannerError {
        ConnectionReader cr = new ConnectionReader(new StringReader(s));
        cr.setLineNumber(1);
        try {
            setReader(cr);
            Object res = select();
            if (!(res instanceof XSelect))
                throw new ScannerError(PATH_MISSING_SELE, OpenOpts.getOffset(reader));
            checkEof();
            return (XSelect) res;
        } catch (IOException x) {
            throw new RuntimeException("internal error", x);
        } catch (ScannerError sc) {
            sc.setLine(OpenOpts.getLine(cr));
            throw sc;
        }
    }

    /**************************************************************/
    /* Some Helpers                                               */
    /**************************************************************/


    /**
     * <p>Check whether the given token is a name.</p>
     *
     * @return True if the token is a name, otherwise false.
     */
    boolean isName() {
        if (st.ttype != StreamTokenizer.TT_WORD)
            return false;
        if (Character.isDigit(st.sval.charAt(0)))
            return false;
        if (reserved.getKey(st.sval) != null)
            return false;
        return true;
    }

    /**
     * <p>Check whether the eof was reached.</p>
     */
    void checkEof() throws ScannerError {
        if (st.ttype != StreamTokenizer.TT_EOF)
            throw new ScannerError(PATH_SUPERFLOUS_TOKEN, OpenOpts.getOffset(reader));
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
    abstract Object getVariable(String n)
            throws ScannerError;

    /**
     * <p>Some test cases.</p
     *
     * @param args Not used.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    /*
    public static void main(String[] args)
            throws IOException, ScannerError {
        XPathReadTransform xr = new XPathReadTransform();
        MapHash<String, Object> variables = new MapHash<String, Object>();
        variables.add("x", "bar");
        variables.add("y", Long.valueOf(123));
        xr.setVariables(variables);

        XPath xpath = xr.createXPath("jack[@foo=$x or ~ (@foo=<$y)]/jill");
        System.out.println("xpath=" + xpath);

        XSelect xs = xr.createXSelect("$y*(3+5) + 1000/($y-1)");
        System.out.println("xselect=" + xs);
        System.out.println("eval(xselect)=" + xs.evalElement(null));

        XPathExpr xe = xr.createXPathExpr("$x='bar' and $y<456");
        System.out.println("xpathexpr=" + xe);
        System.out.println("check(xpathexpr)=" + xe.checkElement(null));

        xe = xr.createXPathExpr("child = \"Hello <b>Jack</b>!\"");
        System.out.println("xpathexpr=" + xe);

        xe = xr.createXPathExpr("1.234 < 12.34");
        System.out.println("xpathexpr=" + xe);
        System.out.println("check(xpathexpr)=" + xe.checkElement(null));

        xe = xr.createXPathExpr("ts '2018-01-17 00:44:19.062' > ts '2018-01-17 00:44:19.061'");
        System.out.println("xpathexpr=" + xe);
        System.out.println("check(xpathexpr)=" + xe.checkElement(null));
    }
    */

}