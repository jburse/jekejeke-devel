package matula.util.wire;

import matula.util.format.AbstractDom;
import matula.util.format.AbstractWriter;
import matula.util.format.DomElement;
import matula.util.format.DomText;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;

/**
 * <p>This class provides a JSON writer.</p>
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
public final class JsonWriter extends AbstractWriter {
    public static String JSON_ARRAY = "array";
    public static String JSON_OBJECT = "object";

    /****************************************************************/
    /* Store Methods                                                */
    /****************************************************************/

    /**
     * <p>Store the childeren.</p>
     *
     * @param nodes The nodes.
     * @throws IOException Shit happens.
     */
    public void storeNodes(AbstractDom[] nodes)
            throws IOException {
        for (int i = 0; i < nodes.length; i++) {
            if (i != 0) {
                Writer wr = getWriter();
                wr.write(",\n");
                writeIndent();
            }
            AbstractDom node = nodes[i];
            String key = node.getKey();
            if (key != null) {
                copyScalar(key);
                Writer wr = getWriter();
                wr.write(": ");
            }
            storeNode(node);
        }
    }

    /**
     * <p>Store this dom node.</p>
     * <p>Not synchronized, uses cursors.</p>
     *
     * @param node The dom node.
     * @throws IOException Shit happens.
     */
    public void storeNode(AbstractDom node)
            throws IOException {
        if (node instanceof DomText) {
            DomText dt = (DomText) node;
            copyScalar(dt.getDataObj());
        } else {
            DomElement de = (DomElement) node;
            AbstractDom[] nodes;
            if (de.isName(JSON_ARRAY)) {
                nodes = de.snapshotNodes();
            } else if (de.isName(JSON_OBJECT)) {
                nodes = de.snapshotAttrs();
            } else {
                throw new IllegalArgumentException("illegal JSON");
            }
            if (nodes.length == 0) {
                copyEmpty(de);
            } else {
                copyStart(de);
                storeNodes(nodes);
                copyEnd(de);
            }
        }
    }

    /****************************************************************/
    /* DomText Writing                                              */
    /****************************************************************/

    /**
     * <p>Copy the given scalar.</p>
     *
     * @param data The scalar.
     * @throws IOException IO error.
     */
    public void copyScalar(Object data)
            throws IOException {
        Writer wr = getWriter();
        if (!(data instanceof String)) {
            wr.write(data.toString());
        } else {
            wr.write("\"");
            wr.write((String) data);
            wr.write("\"");
        }
    }

    /***************************************************************/
    /* DomElement Writing                                          */
    /***************************************************************/

    /**
     * <p>Copy an empty dom element.</p>
     *
     * @param de The template dom element.
     * @throws IOException IO error.
     */
    public void copyEmpty(DomElement de)
            throws IOException {
        Writer wr = getWriter();
        if (de.isName(JSON_ARRAY)) {
            wr.write("[]");
        } else {
            wr.write("{}");
        }
    }

    /**
     * <p>Copy a start dom element.</p>
     *
     * @param de The template dom element.
     * @throws IOException IO error.
     */
    public void copyStart(DomElement de) throws IOException {
        Writer wr = getWriter();
        if (de.isName(JSON_ARRAY)) {
            wr.write("[\n");
        } else {
            wr.write("{\n");
        }
        incIndent();
        writeIndent();
    }

    /**
     * <p>Copy an end dom element.</p>
     *
     * @param de The template dom element.
     * @throws IOException IO error.
     */
    public void copyEnd(DomElement de) throws IOException {
        decIndent();
        Writer wr = getWriter();
        wr.write("\n");
        writeIndent();
        if (de.isName(JSON_ARRAY)) {
            wr.write("]");
        } else {
            wr.write("}");
        }
    }

    /****************************************************************/
    /* Text Wrapping                                                */
    /****************************************************************/

    /**
     * <p>Flush the dom writer.</p>
     *
     * @throws IOException Shit happens.
     */
    public void flush()
            throws IOException {
        Writer wr = getWriter();
        wr.flush();
    }

    /***************************************************************/
    /* Comment & Indent                                            */
    /***************************************************************/

    /**
     * <p>Write a first line comment.</p>
     *
     * @param comment The line comment.
     * @throws IOException IO error.
     */
    public void writeComment(String comment)
            throws IOException {
        Writer wr = getWriter();
        int i = 0;
        int k = comment.indexOf('\n');
        while (k != -1) {
            wr.write("// ");
            wr.write(comment, i, k + 1 - i);
            i = k + 1;
            k = comment.indexOf('\n', i);
        }
        wr.write("// ");
        wr.write(comment, i, comment.length() - i);
        wr.write("\n");
    }

    /***************************************************************/
    /* Indent                                                      */
    /***************************************************************/

    /**
     * <p>Write the indent.</p>
     *
     * @throws IOException IO error.
     */
    public void writeIndent() throws IOException {
        Writer wr = getWriter();
        int n = getIndent();
        for (int i = 0; i < n; i++)
            wr.write(" ");
    }

    /**
     * <p>Some tests.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String args[]) throws IOException {
       JsonWriter jw=new JsonWriter();
       jw.setWriter(new PrintWriter(System.out));
       jw.writeComment("Line 1\nLine 2");
       jw.flush();
    }
    */

}