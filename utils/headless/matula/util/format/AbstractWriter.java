package matula.util.format;

import matula.util.data.MapHash;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignXml;
import matula.util.transform.XmlWriter;
import matula.util.wire.JsonWriter;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

/**
 * <p>This class provides a abstract dom writer.</p>
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
public abstract class AbstractWriter {
    private static final int INDENT_INCREMENT = 4;

    private Writer writer;
    private int mask;
    private MapHash<String, Integer> control;
    private int indent;

    /**
     * <p>Set the writer.</p>
     *
     * @param w The writer.
     */
    public void setWriter(Writer w) {
        writer = w;
    }

    /**
     * <p>Retrieve the writer.</p>
     *
     * @return The writer.
     */
    public Writer getWriter() {
        return writer;
    }

    /**
     * <p>Set the return mask.</p>
     *
     * @param r The return mask.
     */
    public void setMask(int r) {
        mask = r;
    }

    /**
     * <p>Retrieve the return mask.</p>
     *
     * @return The return mask.
     */
    public int getMask() {
        return mask;
    }

    /**
     * <p>Set the tag control.</p>
     *
     * @param c The tag control.
     */
    public void setControl(MapHash<String, Integer> c) {
        control = c;
    }

    /**
     * <p>Retrieve the tag control.</p>
     *
     * @return The tag control.
     */
    public MapHash<String, Integer> getControl() {
        return control;
    }

    /**
     * <p>Set the indent.</p>
     *
     * @param indent The indent.
     */
    public void setIndent(int indent) {
        this.indent = indent;
    }

    /**
     * <p>Retrieve the indent.</p>
     *
     * @return The indent.
     */
    public int getIndent() {
        return indent;
    }

    /****************************************************************/
    /* Store API                                                    */
    /****************************************************************/

    /**
     * <p>Store this dom node.</p>
     * <p>Not synchronized, uses cursors.</p>
     *
     * @param writer  The writer.
     * @param node    The dom node.
     * @param comment The comment
     * @param mask    The return mask.
     * @throws IOException Shit happens.
     */
    public static void store(Writer writer, AbstractDom node,
                             String comment, int mask)
            throws IOException {
        AbstractWriter dw;
        if ((mask & AbstractDom.MASK_JSON) != 0) {
            dw = new JsonWriter();
        } else {
            dw = new XmlWriter();
        }
        dw.setWriter(writer);
        dw.setMask(mask);
        if (comment != null && !"".equals(comment))
            dw.writeComment(comment);
        if ((mask & AbstractDom.MASK_LIST) != 0) {
            DomElement elem = (DomElement) node;
            AbstractDom[] nodes = elem.snapshotNodes();
            dw.storeNodes(nodes);
        } else {
            dw.storeNode(node);
        }
        dw.flush();
    }

    /**
     * <p>Store this dom node.</p>
     * <p>Not synchronized, uses cursors.</p>
     *
     * @param writer  The writer.
     * @param node    The dom node.
     * @param comment The comment
     * @param mask    The return mask.
     * @param control The control.
     * @throws IOException Shit happens.
     */
    public static void store(Writer writer, AbstractDom node,
                             String comment, int mask,
                             MapHash<String, Integer> control)
            throws IOException {
        AbstractWriter dw;
        if ((mask & AbstractDom.MASK_JSON) != 0) {
            dw = new JsonWriter();
        } else {
            dw = new XmlWriter();
        }
        dw.setWriter(writer);
        dw.setMask(mask);
        dw.setControl(control);
        if (comment != null && !"".equals(comment))
            dw.writeComment(comment);
        if ((mask & AbstractDom.MASK_LIST) != 0) {
            DomElement elem = (DomElement) node;
            AbstractDom[] nodes = elem.snapshotNodes();
            dw.storeNodes(nodes);
        } else {
            dw.storeNode(node);
        }
        dw.flush();
    }

    /****************************************************************/
    /* Store Methods                                                */
    /****************************************************************/


    /**
     * <p>Store the childeren.</p>
     *
     * @param nodes The nodes.
     * @throws IOException Shit happens.
     */
    public abstract void storeNodes(AbstractDom[] nodes)
            throws IOException;

    /**
     * <p>Store this dom node.</p>
     * <p>Not synchronized, uses cursors.</p>
     *
     * @param node The dom node.
     * @throws IOException Shit happens.
     */
    public abstract void storeNode(AbstractDom node)
            throws IOException;

    /****************************************************************/
    /* Text Wrapping                                                */
    /****************************************************************/

    /**
     * <p>Flush the dom writer.</p>
     *
     * @throws IOException Shit happens.
     */
    public abstract void flush()
            throws IOException;

    /***************************************************************/
    /* Comment                                                     */
    /***************************************************************/

    /**
     * <p>Write a first line comment.</p>
     *
     * @param comment The line comment.
     * @throws IOException IO error.
     */
    public abstract void writeComment(String comment)
            throws IOException;

    /***************************************************************/
    /* Indent                                                      */
    /***************************************************************/

    /**
     * <p>Increment the indent.</p>
     */
    public void incIndent() {
        indent += INDENT_INCREMENT;
    }

    /**
     * <p>Decrement the indent.</p>
     */
    public void decIndent() {
        indent -= INDENT_INCREMENT;
    }

    /**
     * <p>Some test cases.</p>
     *
     * @param args The arguments.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    /*
    public static void main(String[] args)
            throws IOException, ScannerError {
        String text = "<foo bar='123'/>  <foo bar='456'/>";
        StringReader sr = new StringReader(text);
        DomElement de = new DomElement();
        de.load(sr, AbstractDom.MASK_LIST);
        PrintWriter pw = new PrintWriter(System.out);
        de.store(pw, null, AbstractDom.MASK_LIST);
        pw.println();

        text = "<foo>123</foo>  <foo>456</foo>";
        MapHash<String, Integer> control = new MapHash<String, Integer>();
        control.add("foo", Integer.valueOf(AbstractDom.TYPE_ANY));
        sr = new StringReader(text);
        de = new DomElement();
        de.load(sr, AbstractDom.MASK_LIST, control);
        de.store(pw, null, AbstractDom.MASK_LIST, control);
        pw.println();

        MapHash<String, Integer> control = new MapHash<String, Integer>();
        control.add("p", Integer.valueOf(AbstractDom.TYPE_TEXT));
        control.add("img", Integer.valueOf(AbstractDom.TYPE_EMPTY));

        File file = new File("D:\\Tablespace\\Config2\\usrtab\\logtab\\richdoc.html");
        BufferedReader reader = new BufferedReader(
                new InputStreamReader(
                        new FileInputStream(file), "UTF-8"));
        DomElement de = new DomElement();
        de.load(reader, AbstractDom.MASK_LIST, control);

        PrintWriter pw = new PrintWriter(System.out);
        de.store(pw, null, AbstractDom.MASK_LIST, control);
        pw.println();
    }
    */

}
