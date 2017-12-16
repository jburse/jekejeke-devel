package matula.util.transform;

import matula.util.data.SetHash;
import matula.util.format.*;
import matula.util.regex.ScannerError;

import java.io.IOException;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.io.StringReader;

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
    private static final String ERROR_CBRAKET_EXPECTED = "] expected";
    private static final String ERROR_PERIOD_EXPECTED = ". expected";
    private static final String ERROR_CHOICEPOINT_MISSING = "choice point missing";
    private static final String ERROR_NAME_EXPECTED = "name expected";
    private static final String ERROR_SUPERFLOUS_TOKEN = "superflous token";
    private static final String ERROR_CPARENTHESIS_EXPECTED = ") expected";
    private static final String ERROR_SELECT_MISSING = "select expected";
    private static final String ERROR_PREDICATE_MISSING = "predicate missing";

    private static SetHash<String> reserved = new SetHash<String>();

    StreamTokenizer st;

    static {
        reserved.add(XPathExprComb.OP_TRUE);
        reserved.add(XPathExprComb.OP_FALSE);
        reserved.add(XPathExprComb.OP_OR);
        reserved.add(XPathExprComb.OP_AND);
    }

    /**
     * <p>Set the reader.</p>
     *
     * @param r The reader.
     * @throws IOException Shit happens.
     */
    void setReader(Reader r) throws IOException {
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
     * <>p>The following syntax is used:</p>
     * <pre>
     *     xpath --> choicepoint { "/" choicepoint }.
     * </pre>
     *
     * @return The xpath.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
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
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
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
                    throw new ScannerError(ERROR_PREDICATE_MISSING);
                res2 = ((XPathExpr) res2).lift(XPathExprComb.EXPR_COMB_PRED);
                XPathExprComb res = xp.getChoicePoints().get(xp.size() - 1).getExpr();
                res.join((XPathExprComb) res2);
                if (st.ttype != ']')
                    throw new ScannerError(ERROR_CBRAKET_EXPECTED);
                st.nextToken();
            }
        } else if (st.ttype == '.') {
            st.nextToken();
            if (st.ttype == '.') {
                st.nextToken();
                xp.whereParent();
            } else {
                throw new ScannerError(ERROR_PERIOD_EXPECTED);
            }
        } else if (st.ttype == '[') {
            st.nextToken();
            if (st.ttype == StreamTokenizer.TT_WORD && Character.isDigit(st.sval.charAt(0))) {
                int index = Integer.parseInt(st.sval);
                st.nextToken();
                xp.whereChildIndex(index);
            }
            if (st.ttype != ']')
                throw new ScannerError(ERROR_CBRAKET_EXPECTED);
            st.nextToken();
        } else {
            throw new ScannerError(ERROR_CHOICEPOINT_MISSING);
        }
    }


    /**
     * <p>Convenience method to parse an xpath.</p>
     *
     * @param s The string.
     * @return The xpath.
     * @throws IOException  IO error.
     * @throws ScannerError Shit happens.
     */
    XPath createXPath(String s)
            throws IOException, ScannerError {
        StringReader sr = new StringReader(s);
        setReader(sr);
        XPath c = xpath();
        checkEof();
        return c;
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
     * @throws ScannerError Shit happens.
     */
    private Object predicate() throws IOException, ScannerError {
        Object res = predicateTerm();
        while (st.ttype == StreamTokenizer.TT_WORD && st.sval.equals(XPathExprComb.OP_OR)) {
            st.nextToken();
            if (!(res instanceof XPathExpr))
                throw new ScannerError(ERROR_PREDICATE_MISSING);
            res = ((XPathExpr) res).lift(XPathExprComb.EXPR_COMB_OR);
            Object res2 = predicateTerm();
            if (!(res2 instanceof XPathExpr))
                throw new ScannerError(ERROR_PREDICATE_MISSING);
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
     * @throws ScannerError Shit happens.
     */
    private Object predicateTerm() throws IOException, ScannerError {
        Object res = predicatesSimple();
        while (st.ttype == StreamTokenizer.TT_WORD && st.sval.equals(XPathExprComb.OP_AND)) {
            st.nextToken();
            if (!(res instanceof XPathExpr))
                throw new ScannerError(ERROR_PREDICATE_MISSING);
            res = ((XPathExpr) res).lift(XPathExprComb.EXPR_COMB_AND);
            Object res2 = predicatesSimple();
            if (!(res2 instanceof XPathExpr))
                throw new ScannerError(ERROR_PREDICATE_MISSING);
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
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private Object predicatesSimple() throws IOException, ScannerError {
        Object res;
        if (st.ttype == '~') {
            st.nextToken();
            res = select();
            if (!(res instanceof XPathExpr))
                throw new ScannerError(ERROR_PREDICATE_MISSING);
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
                    throw new ScannerError(ERROR_SELECT_MISSING);
                Object res2 = select();
                if (!(res2 instanceof XSelect))
                    throw new ScannerError(ERROR_SELECT_MISSING);
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
     * @throws IOException  IO error.
     * @throws ScannerError Shit happens.
     */
    XPathExpr createXPathExpr(String s) throws IOException, ScannerError {
        StringReader sr = new StringReader(s);
        setReader(sr);
        Object res = predicate();
        if (!(res instanceof XPathExpr))
            throw new ScannerError(ERROR_SELECT_MISSING);
        checkEof();
        return (XPathExpr) res;
    }

    /**************************************************************/
    /* Select                                                     */
    /**************************************************************/

    /**
     * <p>Parse an xselect.</p>
     * <>p>The following syntax is used:</p>
     * <pre>
     *     select      --> "-" term
     *                   | select "+" term
     *                   | select "-" term
     *                   | term.
     * </pre>
     *
     * @return The xselect.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private Object select()
            throws IOException, ScannerError {
        Object res;
        if (st.ttype == '-') {
            st.nextToken();
            res = selectTerm();
            if (!(res instanceof XSelect))
                throw new ScannerError(ERROR_SELECT_MISSING);
            res = new XSelectComb((XSelect) res, XSelectComb.SELE_COMB_NEG);
        } else {
            res = selectTerm();
        }
        for (; ; ) {
            if (st.ttype == '+') {
                st.nextToken();
                if (!(res instanceof XSelect))
                    throw new ScannerError(ERROR_SELECT_MISSING);
                Object res2 = selectTerm();
                if (!(res2 instanceof XSelect))
                    throw new ScannerError(ERROR_SELECT_MISSING);
                res = new XSelectComb((XSelect) res, (XSelect) res2, XSelectComb.SELE_COMB_ADD);
            } else if (st.ttype == '-') {
                st.nextToken();
                if (!(res instanceof XSelect))
                    throw new ScannerError(ERROR_SELECT_MISSING);
                Object res2 = selectTerm();
                if (!(res2 instanceof XSelect))
                    throw new ScannerError(ERROR_SELECT_MISSING);
                res = new XSelectComb((XSelect) res, (XSelect) res2, XSelectComb.SELE_COMB_SUB);
            } else {
                break;
            }
        }
        return res;
    }

    /**
     * <p>Parse an xselect term.</p>
     * <>p>The following syntax is used:</p>
     * <pre>
     *     term      --> term "*" simple
     *                 | term "/" simple
     *                 | simple.
     * </pre>
     *
     * @return The xselect.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private Object selectTerm()
            throws IOException, ScannerError {
        Object res = selectSimple();
        for (; ; ) {
            if (st.ttype == '*') {
                st.nextToken();
                if (!(res instanceof XSelect))
                    throw new ScannerError(ERROR_SELECT_MISSING);
                Object res2 = selectSimple();
                if (!(res2 instanceof XSelect))
                    throw new ScannerError(ERROR_SELECT_MISSING);
                res = new XSelectComb((XSelect) res, (XSelect) res2, XSelectComb.SELE_COMB_MUL);
            } else if (st.ttype == '/') {
                st.nextToken();
                if (!(res instanceof XSelect))
                    throw new ScannerError(ERROR_SELECT_MISSING);
                Object res2 = selectSimple();
                if (!(res2 instanceof XSelect))
                    throw new ScannerError(ERROR_SELECT_MISSING);
                res = new XSelectComb((XSelect) res, (XSelect) res2, XSelectComb.SELE_COMB_DIV);
            } else {
                break;
            }
        }
        return res;
    }

    /**
     * <p>Parse an xselect simple.</p>
     * <>p>The following syntax is used:</p>
     * <pre>
     *     simple      --> "(" predicate ")"
     *                   | "@" name
     *                   | "'" { char } "'"
     *                   | """" { char } """"
     *                   | digit { digit }
     *                   | "$" name.
     * </pre>
     *
     * @return The xselect.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private Object selectSimple()
            throws IOException, ScannerError {
        Object res;
        if (st.ttype == '(') {
            st.nextToken();
            res = predicate();
            if (st.ttype != ')')
                throw new ScannerError(ERROR_CPARENTHESIS_EXPECTED);
            st.nextToken();
        } else if (st.ttype == '@') {
            st.nextToken();
            if (!isName())
                throw new ScannerError(ERROR_NAME_EXPECTED);
            String name = st.sval;
            st.nextToken();
            res = new XSelectPrim(name, XSelectPrim.SELE_PRIM_ATTR);
        } else if (st.ttype == '"' || st.ttype == '\'') {
            String cnst = st.sval;
            st.nextToken();
            res = new XSelectPrim(cnst, XSelectPrim.SELE_PRIM_CONST);
        } else if (st.ttype == StreamTokenizer.TT_WORD && Character.isDigit(st.sval.charAt(0))) {
            Long cnst = Long.parseLong(st.sval);
            st.nextToken();
            res = new XSelectPrim(cnst, XSelectPrim.SELE_PRIM_CONST);
        } else if (st.ttype == '$') {
            st.nextToken();
            if (!isName())
                throw new ScannerError(ERROR_NAME_EXPECTED);
            String var = st.sval;
            st.nextToken();
            Object cnst = getVariable(var);
            res = new XSelectPrim(cnst, XSelectPrim.SELE_PRIM_CONST);
        } else {
            throw new ScannerError(ERROR_SELECT_MISSING);
        }
        return res;
    }

    /**
     * <p>Convenience method to parse a xselect.</p>
     *
     * @param s The string.
     * @return The xselect.
     * @throws IOException  IO error.
     * @throws ScannerError Shit happens.
     */
    XSelect createXSelect(String s)
            throws IOException, ScannerError {
        StringReader sr = new StringReader(s);
        setReader(sr);
        Object res = select();
        if (!(res instanceof XSelect))
            throw new ScannerError(ERROR_SELECT_MISSING);
        checkEof();
        return (XSelect) res;
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
            throw new ScannerError(ERROR_SUPERFLOUS_TOKEN);
    }

    /**************************************************************/
    /* Variation Points                                           */
    /**************************************************************/

    /**
     * <p>Retrieve a variable value.</p>
     *
     * @param n The variable name.
     * @return The variable value.
     * @throws ScannerError Shit happens.
     */
    abstract Object getVariable(String n) throws ScannerError;

    /**
     * <p>Some test cases.</p
     *
     * @param args Not used.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
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
    }
    */

}