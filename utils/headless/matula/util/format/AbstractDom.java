package matula.util.format;

import matula.util.data.ListArray;
import matula.util.data.MapHash;
import matula.util.regex.ScannerError;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * <p>This class provides a dom node.</p>
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
public abstract class AbstractDom
        implements Cloneable {
    public static final String TIMESTAMP_DOM = "yyyy-MM-dd'T'HH:mm:ss.SSS";

    public static final int MASK_TEXT = 0x00000001;
    public static final int MASK_LIST = 0x00000002;

    public static final int TYPE_NONE = 0;
    public static final int TYPE_EMPTY = 1; /* disable text and list */
    public static final int TYPE_ANY = 2; /* enable text and list */

    DomElement parent;

    /**
     * <p>Retrieve the parent.</p>
     *
     * @return The parent.
     */
    public DomElement getParent() {
        return parent;
    }

    /**
     * <p>Set the parent.</p>
     *
     * @param p The parent.
     */
    public void setParent(DomElement p) {
        parent = p;
    }

    /*****************************************************/
    /* Properties Like Lifecycle                         */
    /*****************************************************/

    /**
     * <p>Load this dom node.</p>
     * <p>Not synchronized, uses cut-over.</p>
     *
     * @param reader The input stream.
     * @param mask   The return mask.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    public void load(Reader reader, int mask)
            throws IOException, ScannerError {
        DomReader dr = new DomReader();
        dr.setReader(reader);
        dr.setMask(mask);
        if ((mask & MASK_LIST) != 0) {
            DomElement de = (DomElement) this;
            ListArray<AbstractDom> cs = DomElement.loadNodes(dr);
            de.setChildrenFast(cs);
        } else {
            dr.nextTagOrText();
            loadNode(dr);
        }
        dr.checkEof();
    }

    /**
     * <p>Load this dom node.</p>
     * <p>Not synchronized, uses cut-over.</p>
     *
     * @param reader  The input stream.
     * @param mask    The return mask.
     * @param control The tag control.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    public void load(Reader reader, int mask,
                     MapHash<String, Integer> control)
            throws IOException, ScannerError {
        DomReader dr = new DomReader();
        dr.setReader(reader);
        dr.setMask(mask);
        dr.setControl(control);
        if ((mask & MASK_LIST) != 0) {
            DomElement de = (DomElement) this;
            ListArray<AbstractDom> cs = DomElement.loadNodes(dr);
            de.setChildrenFast(cs);
        } else {
            dr.nextTagOrText();
            loadNode(dr);
        }
        dr.checkEof();
    }


    /**
     * <p>Store this dom node.</p>
     * <p>Not synchronized, uses cursors.</p>
     *
     * @param writer  The writer.
     * @param comment The comment
     * @param mask    The return mask.
     * @throws IOException Shit happens.
     */
    public void store(Writer writer, String comment, int mask)
            throws IOException {
        DomWriter dw = new DomWriter();
        dw.setWriter(writer);
        dw.setMask(mask);
        if (comment != null && !"".equals(comment))
            dw.writeComment(comment);
        if ((mask & MASK_LIST) != 0) {
            DomElement elem = (DomElement) this;
            AbstractDom[] nodes = elem.snapshotNodes();
            DomElement.storeNodes(dw, nodes);
        } else {
            storeNode(dw);
        }
        writer.flush();
    }

    /**
     * <p>Store this dom node.</p>
     * <p>Not synchronized, uses cursors.</p>
     *
     * @param writer  The writer.
     * @param comment The comment
     * @param mask    The return mask.
     * @throws IOException Shit happens.
     */
    public void store(Writer writer, String comment, int mask,
                      MapHash<String, Integer> control)
            throws IOException {
        DomWriter dw = new DomWriter();
        dw.setWriter(writer);
        dw.setMask(mask);
        dw.setControl(control);
        if (comment != null && !"".equals(comment))
            dw.writeComment(comment);
        if ((mask & MASK_LIST) != 0) {
            DomElement elem = (DomElement) this;
            AbstractDom[] nodes = elem.snapshotNodes();
            DomElement.storeNodes(dw, nodes);
        } else {
            storeNode(dw);
        }
        writer.flush();
    }

    /**
     * <p>Load a dom node.</p>
     * <p>Not synchronized, uses cut-over.</p>
     *
     * @param dr The dom reader.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    abstract void loadNode(DomReader dr)
            throws IOException, ScannerError;

    /**
     * <p>Store this dom node.</p>
     * <p>Not synchronized, uses cursors.</p>
     *
     * @param dw The dom writer.
     * @throws IOException Shit happens.
     */
    abstract void storeNode(DomWriter dw)
            throws IOException;

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * <p>Create a deep copy.</p>
     *
     * @return The deep copy.
     */
    public Object clone() {
        AbstractDom res;
        try {
            res = (AbstractDom) super.clone();
        } catch (CloneNotSupportedException x) {
            throw new RuntimeException("internal error", x);
        }
        res.reinitialize();
        return res;
    }

    /**
     * Reset to initial default state.
     */
    void reinitialize() {
        parent = null;
    }

    /***************************************************************/
    /* Name Token Conversion                                       */
    /***************************************************************/

    /**
     * <p>Some test cases.</p>
     *
     * @param args Not used.
     */
    public static void main(String[] args) throws ParseException {
        String TIMESTAMP_DOM = "yyyy-MM-dd'T'HH:mm:ss.SSS";
        SimpleDateFormat sd = new SimpleDateFormat(TIMESTAMP_DOM);

        Timestamp ts = new Timestamp(new Date().getTime());
        System.out.println("ts=" + ts);
        String str = sd.format(new Date(ts.getTime()));
        System.out.println("format(ts)=" + str);

        System.out.println();

        System.out.println("str=" + str);
        System.out.println("parse(str)=" + new Timestamp(sd.parse(str).getTime()));
    }

}
