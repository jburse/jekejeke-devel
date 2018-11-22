package matula.util.format;

import matula.util.data.ListArray;
import matula.util.data.MapHash;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignXml;
import matula.util.system.OpenOpts;
import matula.util.transform.XmlReader;
import matula.util.wire.JsonReader;

import java.io.IOException;
import java.io.Reader;

/**
 * <p>This class provides an abstract dom reader.</p>
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
public abstract class AbstractReader {
    private int mask;
    private MapHash<String, Integer> control;

    /**
     * <p>Set the return mask.</p>
     *
     * @param m The return mask.
     */
    public void setMask(int m) {
        mask = m;
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
     * <p>Retrieve the return mask.</p>
     *
     * @return The return mask.
     */
    public int getMask() {
        return mask;
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
     * <p>Set the reader.</p>
     *
     * @param r The reader.
     * @throws IOException I/O Error.
     */
    public abstract void setReader(Reader r)
            throws IOException;

    /**********************************************************/
    /* Load API                                               */
    /**********************************************************/

    /**
     * <p>Load this dom node.</p>
     * <p>Not synchronized, uses cut-over.</p>
     *
     * @param reader The input stream.
     * @param node   The dom node.
     * @param mask   The return mask.
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public static void load(Reader reader, AbstractDom node, int mask)
            throws IOException, ScannerError {
        AbstractReader dr;
        if ((mask & AbstractDom.MASK_JSON) != 0) {
            dr = new JsonReader();
        } else {
            dr = new XmlReader();
        }
        dr.setReader(reader);
        dr.setMask(mask);
        if ((mask & AbstractDom.MASK_LIST) != 0) {
            DomElement de = (DomElement) node;
            ListArray<AbstractDom> cs = dr.loadNodes();
            de.setChildrenFast(cs);
        } else {
            dr.nextTagOrText();
            dr.loadNode(node);
        }
        dr.checkEof();
    }

    /**
     * <p>Load this dom node.</p>
     * <p>Not synchronized, uses cut-over.</p>
     *
     * @param reader  The input stream.
     * @param node    The dom node.
     * @param mask    The return mask.
     * @param control The tag control.
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public static void load(Reader reader, AbstractDom node, int mask,
                            MapHash<String, Integer> control)
            throws IOException, ScannerError {
        AbstractReader dr;
        if ((mask & AbstractDom.MASK_JSON) != 0) {
            dr = new JsonReader();
        } else {
            dr = new XmlReader();
        }
        dr.setReader(reader);
        dr.setMask(mask);
        dr.setControl(control);
        if ((mask & AbstractDom.MASK_LIST) != 0) {
            DomElement de = (DomElement) node;
            ListArray<AbstractDom> cs = dr.loadNodes();
            de.setChildrenFast(cs);
        } else {
            dr.nextTagOrText();
            dr.loadNode(node);
        }
        dr.checkEof();
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
    public abstract ListArray<AbstractDom> loadNodes()
            throws IOException, ScannerError;

    /**
     * <p>Load a dom node.</p>
     * <p>Not synchronized, uses cut-over.</p>
     *
     * @param node The dom node.
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public abstract void loadNode(AbstractDom node)
            throws IOException, ScannerError;

    /**********************************************************/
    /* Text & Element Reading                                 */
    /**********************************************************/

    /**
     * <p>Get the next tag.</p>
     *
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public abstract void nextTagOrText()
            throws IOException, ScannerError;

    /**
     * <p>Check whether the dom reader is at eof.</p>
     *
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public abstract void checkEof()
            throws IOException, ScannerError;

    /**
     * <p>Some test cases.</p>
     *
     * @param args The arguments.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    /*
    public static void main(String[] args) throws IOException, ScannerError {
        String text = "<p>The quick brown fox <img/> jumps over the lazy dog.</p>";
        StringReader sr = new StringReader(text);
        DomElement de = new DomElement();
        de.load(sr, AbstractDom.MASK_TEXT);
        PrintWriter pw = new PrintWriter(System.out);
        de.store(pw, null, AbstractDom.MASK_TEXT);
        pw.println();

        text = "<p>The quick brown fox <img> jumps over the lazy dog.</p>";
        MapHash<String, Integer> control = new MapHash<String, Integer>();
        control.add("img", Integer.valueOf(AbstractDom.TYPE_EMPTY));
        sr = new StringReader(text);
        de = new DomElement();
        de.load(sr, AbstractDom.MASK_TEXT, control);
        de.store(pw, null, AbstractDom.MASK_TEXT, control);
        pw.println();
    }
    */

}
