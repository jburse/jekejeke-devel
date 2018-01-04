package matula.util.format;

import matula.util.data.MapHash;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignXml;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>This class provides a dom writer.</p>
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
public final class DomWriter {
    private static final int INDENT_INCREMENT = 4;
    private static final int LINE_WIDTH = 80;

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
     * <p>Retrieve the indent.</p>
     *
     * @return The indent.
     */
    int getIndent() {
        return indent;
    }

    /**
     * <p>Write the indent.</p>
     *
     * @throws IOException Shit happens.
     */
    public void writeIndent() throws IOException {
        for (int i = 0; i < indent; i++)
            writer.write(" ");
    }

    /**
     * <p>Write a time stamp.</p>
     *
     * @throws IOException Shit happens.
     */
    public void writeComment(String comment) throws IOException {
        write("<!-- ");
        write(ForeignXml.sysTextEscape(comment));
        write(" -->\n");
    }

    /**
     * <p>Write a string.</p>
     *
     * @param str The string.
     * @throws IOException Shit happens.
     */
    public void write(String str) throws IOException {
        writer.write(str);
    }

    /****************************************************************/
    /* Tag Writing                                                  */
    /****************************************************************/

    /**
     * <p>Copy an empty dom element.</p>
     *
     * @param de The template dom element.
     * @throws IOException Shit happens.
     */
    public void copyEmpty(DomElement de) throws IOException {
        write("<");
        write(de.getName());
        copyAttributes(de);
        write("/>");
    }

    /**
     * <p>Copy a start dom element.</p>
     *
     * @param de The template dom element.
     * @throws IOException Shit happens.
     */
    public void copyStart(DomElement de) throws IOException {
        write("<");
        write(de.getName());
        copyAttributes(de);
        write(">");
    }

    /**
     * <p>Copy the attributes of a dom element.</p>
     *
     * @param de The template dom element.
     * @throws IOException Shit happens.
     */
    private void copyAttributes(DomElement de) throws IOException {
        String[] attrs = de.snapshotAttrs();
        if ((getMask() & AbstractDom.MASK_TEXT) != 0) {
            for (int i = 0; i < attrs.length; i++) {
                String attr = attrs[i];
                Object val = de.getAttrObj(attr);
                if (val == null)
                    continue;
                write(" ");
                write(attr);
                if (!"".equals(val)) {
                    write("=");
                    if (val instanceof String) {
                        write("\"");
                        write(ForeignXml.sysTextEscape((String) val));
                        write("\"");
                    } else {
                        write(Long.toString(((Long) val).longValue()));
                    }
                }
            }
        } else {
            String name = de.getName();
            int off = getIndent() + 1 + name.length();
            incIndent();
            for (int i = 0; i < attrs.length; i++) {
                name = attrs[i];
                Object val = de.getAttrObj(name);
                if (val == null)
                    continue;
                int off2 = off + 1 + name.length();
                StringBuilder buf = new StringBuilder();
                if (!"".equals(val)) {
                    buf.append("=");
                    if (val instanceof String) {
                        buf.append("\"");
                        buf.append(ForeignXml.sysTextEscape((String) val));
                        buf.append("\"");
                    } else {
                        buf.append(Long.toString(((Long) val).longValue()));
                    }
                }
                off2 += 3 + buf.length();
                if (off2 >= LINE_WIDTH) {
                    write("\n");
                    writeIndent();
                    off = getIndent();
                } else {
                    write(" ");
                    off++;
                }
                write(name);
                off += name.length();
                write(buf.toString());
                off += 3 + buf.length();
            }
            decIndent();
        }
    }

    /**
     * <p>Copy an end dom element.</p>
     *
     * @param de The template dom element.
     * @throws IOException Shit happens.
     */
    public void copyEnd(DomElement de) throws IOException {
        write("</");
        write(de.getName());
        write(">");
    }

    /*****************************************************/
    /* Tag Control                                       */
    /*****************************************************/

    /**
     * <p>Check whether the tag type is empty.</p>
     *
     * @param control The tag control.
     * @param type    The tag type.
     * @return True if the tag type is empty, otherwise false.
     */
    public static boolean checkEmpty(MapHash<String, Integer> control,
                                     String type) {
        if (control == null)
            return false;
        Integer val = control.get(type);
        if (val == null) {
            return false;
        } else if (val.intValue() == AbstractDom.TYPE_EMPTY) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Check whether the tag type is any.</p>
     *
     * @param control The tag control.
     * @param type    The tag type.
     * @return True if the tag type is any, otherwise false.
     */
    public static boolean checkAny(MapHash<String, Integer> control,
                                   String type) {
        if (control == null)
            return false;
        Integer val = control.get(type);
        if (val == null) {
            return false;
        } else if (val.intValue() == AbstractDom.TYPE_ANY) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Some test cases.</p>
     *
     * @param args The arguments.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    /*
    public static void main(String[] args) throws IOException, ScannerError {
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
    }
    */

}
