package matula.util.wire;

import matula.util.data.ListArray;
import matula.util.format.AbstractDom;
import matula.util.format.AbstractReader;
import matula.util.format.DomElement;
import matula.util.format.DomText;
import matula.util.regex.CodeType;
import matula.util.regex.CompLang;
import matula.util.regex.ScannerError;
import matula.util.regex.ScannerToken;

import java.io.IOException;
import java.io.Reader;

/**
 * <p>This class provides a JSON reader.</p>
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class JsonReader extends AbstractReader {
    public final static CodeType JSON_CODETYPE = new CodeType();
    public final static CompLang JSON_COMPLANG = new CompLang();

    public final static String JSON_DUPLICATE_KEY = "json_duplicate_key";

    public final static String JSON_KEY_MISSING = "json_key_missing";
    public final static String JSON_COLON_MISSING = "json_colon_missing";
    public final static String JSON_ELEMENT_MISSING = "json_element_missing";
    public final static String JSON_UNBLANCED_ARRAY = "json_unblanced_array";
    public final static String JSON_UNBLANCED_OBJECT = "json_unblanced_object";
    public final static String JSON_SUPERFLOUS_TOKEN = "json_superflous_token";

    public final static String OP_COMMA = ",";
    public final static String OP_COLON = ":";
    public final static String OP_LBRACKET = "[";
    public final static String OP_RBRACKET = "]";
    public final static String OP_LBRACE = "{";
    public final static String OP_RBRACE = "}";

    public ScannerToken st;

    static {
        JSON_CODETYPE.setHints("\u200C\u200D");
        JSON_CODETYPE.setDelemiters(",:");
        JSON_CODETYPE.setQuotes("\"");
        JSON_CODETYPE.setInvalids("\uFFFD\b\f\r");
        JSON_CODETYPE.setJoiners("");

        JSON_COMPLANG.setLineComment("//");
        JSON_COMPLANG.setBlockCommentStart("/*");
        JSON_COMPLANG.setBlockCommentEnd("*/");
    }

    /**
     * <p>Set the reader.</p>
     *
     * @param r The reader.
     * @throws IOException I/O Error.
     */
    public void setReader(Reader r)
            throws IOException {
        st = new ScannerToken();
        st.setDelemiter(JSON_CODETYPE);
        st.setRemark(JSON_COMPLANG);
        st.setReader(r);
        st.setFlags(0);
        st.firstChar();
    }

    /**********************************************************/
    /* Load Methods                                           */
    /**********************************************************/

    /**
     * <p>Load the children.</p>
     *
     * @return The children.
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public ListArray<AbstractDom> loadNodes()
            throws IOException, ScannerError {
        return loadNodes(false);
    }

    /**
     * <p>Load the children.</p>
     *
     * @param keys The key flag.
     * @return The children.
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public ListArray<AbstractDom> loadNodes(boolean keys)
            throws IOException, ScannerError {
        nextTagOrText();
        ListArray<AbstractDom> res = null;
        if (st.getHint() != 0 ||
                (!"".equals(st.getData()) &&
                        !OP_RBRACKET.equals(st.getData())) &&
                        !OP_RBRACE.equals(st.getData())) {
            for (; ; ) {
                String key;
                if (keys) {
                    if (st.getHint() == 0)
                        throw new ScannerError(JSON_KEY_MISSING,
                                st.getTokenOffset());
                    key = JSON_CODETYPE.resolveDouble(st.getData(),
                            st.getHint(), st.getTokenOffset());
                    key = CompLang.resolveEscape(key, st.getHint(), true,
                            st.getTokenOffset(), JSON_CODETYPE);
                    if (res!=null && DomElement.indexAttr(res, key)!=-1)
                        throw new ScannerError(JSON_DUPLICATE_KEY,
                                st.getTokenOffset());
                    nextTagOrText();
                    if (st.getHint() != 0 ||
                            !OP_COLON.equals(st.getData()))
                        throw new ScannerError(JSON_COLON_MISSING,
                                st.getTokenOffset());
                    nextTagOrText();
                } else {
                    key = null;
                }
                if (st.getHint() != 0 ||
                        (st.getData().length() > 0 &&
                                Character.isDigit(st.getData().codePointAt(0))) ||
                        ("-".equals(st.getData()) &&
                                Character.isDigit(st.lookAhead()))) {
                    DomText dt = new DomText();
                    if (keys)
                        dt.setKey(key);
                    loadNode(dt);
                    if (res == null)
                        res = new ListArray<AbstractDom>();
                    res.add(dt);
                } else if (OP_LBRACKET.equals(st.getData()) || OP_LBRACE.equals(st.getData())) {
                    DomElement dh = new DomElement();
                    if (keys)
                        dh.setKey(key);
                    loadNode(dh);
                    if (res == null)
                        res = new ListArray<AbstractDom>();
                    res.add(dh);
                } else {
                    throw new ScannerError(JSON_ELEMENT_MISSING,
                            st.getTokenOffset());
                }
                if (st.getHint() == 0 && OP_COMMA.equals(st.getData())) {
                    nextTagOrText();
                } else {
                    break;
                }
            }
        }
        return res;
    }

    /**
     * <p>Load a dom node.</p>
     * <p>Not synchronized, uses cut-over.</p>
     *
     * @param node The dom node.
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public void loadNode(AbstractDom node)
            throws IOException, ScannerError {
        if (node instanceof DomText) {
            DomText dt = (DomText) node;
            if (st.getHint() != 0) {
                String res = JSON_CODETYPE.resolveDouble(st.getData(),
                        st.getHint(), st.getTokenOffset());
                res = CompLang.resolveEscape(res, st.getHint(), true,
                        st.getTokenOffset(), JSON_CODETYPE);
                dt.setData(res);
            } else {
                long res;
                if ("-".equals(st.getData())) {
                    nextTagOrText();
                    res = Long.parseLong("-" + st.getData());
                } else {
                    res = Long.parseLong(st.getData());
                }
                dt.setDataLong(res);
            }
        } else {
            DomElement de = (DomElement) node;
            if (OP_LBRACKET.equals(st.getData())) {
                ListArray<AbstractDom> newchildren = loadNodes(false);
                if (st.getHint() != 0 ||
                        !OP_RBRACKET.equals(st.getData()))
                    throw new ScannerError(JSON_UNBLANCED_ARRAY,
                            st.getTokenOffset());
                de.setName(JsonWriter.JSON_ARRAY);
                de.setChildrenFast(newchildren);
            } else {
                ListArray<AbstractDom> newattrs = loadNodes(true);
                if (st.getHint() != 0 ||
                        !OP_RBRACE.equals(st.getData()))
                    throw new ScannerError(JSON_UNBLANCED_OBJECT,
                            st.getTokenOffset());
                de.setName(JsonWriter.JSON_OBJECT);
                de.setAttrsFast(newattrs);
            }
        }
        nextTagOrText();
    }

    /**********************************************************/
    /* Text & Element Reading                                 */
    /**********************************************************/

    /**
     * <p>Get the next tag.</p>
     *
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public void nextTagOrText()
            throws IOException, ScannerError {
        st.nextToken();
    }

    /**
     * <p>Check whether the dom reader is at eof.</p>
     * .
     *
     * @throws ScannerError Syntax error.
     */
    public void checkEof()
            throws ScannerError {
        if (st.getHint() != 0 || !"".equals(st.getData()))
            throw new ScannerError(JSON_SUPERFLOUS_TOKEN,
                    st.getTokenOffset());
    }

}