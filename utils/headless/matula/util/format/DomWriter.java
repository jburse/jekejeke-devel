package matula.util.format;

import matula.util.data.MapHash;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignXml;

import java.io.*;

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
    private int pos;
    private String pending;

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
     * <p>Write a string.</p>
     *
     * @param str The string.
     * @throws IOException IO error.
     */
    public void write(String str)
            throws IOException {
        write(str, false);
    }

    /**
     * <p>Write a string.</p>
     *
     * @param str  The string.
     * @param wrap The space wrap flag.
     * @throws IOException IO error.
     */
    private void write(String str, boolean wrap)
            throws IOException {
        int len = str.length();
        int k;
        boolean hasspace;
        if (pending != null) {
            k = -pending.length();
            hasspace = true;
        } else {
            k = 0;
            hasspace = false;
        }
        for (int i = 0; i < len; i++) {
            char ch = str.charAt(i);
            if (wrap && ch == XmlMachine.CHAR_SPACE) {
                if (k < 0) {
                    writer.write(pending, pending.length() + k, -k);
                    writer.write(str, 0, i);
                } else {
                    writer.write(str, k, i - k);
                }
                k = i;
                hasspace = true;
                pos++;
            } else if (ch == '\n') {
                if (k < 0) {
                    writer.write(pending, pending.length() + k, -k);
                    writer.write(str, 0, i + 1);
                } else {
                    writer.write(str, k, i + 1 - k);
                }
                k = i + 1;
                pos = 0;
                hasspace = false;
            } else if (hasspace && pos >= LINE_WIDTH) {
                incIndent();
                writer.write("\n");
                k++;
                writeIndent();
                pos = indent + i + 1 - k;
                decIndent();
                hasspace = false;
            } else {
                pos++;
            }
        }
        if (hasspace) {
            StringWriter sw = new StringWriter();
            if (k < 0) {
                sw.write(pending, pending.length() + k, -k);
                sw.write(str, 0, len);
            } else {
                sw.write(str, k, len - k);
            }
            pending = sw.toString();
        } else {
            if (k < 0) {
                writer.write(pending, pending.length() + k, -k);
                writer.write(str, 0, len);
            } else {
                writer.write(str, k, len - k);
            }
            pending = null;
        }
    }

    /**
     * <p>Flush the dom writer.</p>
     *
     * @throws IOException Shit happens.
     */
    public void flush() throws IOException {
        if (pending != null)
            writer.write(pending);
        writer.flush();
    }

    /****************************************************************/
    /* Tag Writing                                                  */
    /****************************************************************/

    /**
     * <p>Copy the given text.</p>
     *
     * @param data The text.
     * @throws IOException IO error.
     */
    public void copyText(String data) throws IOException {
        boolean wrap;
        if ((mask & AbstractDom.MASK_STRP) != 0) {
            wrap = true;
        } else {
            wrap = false;
        }
        if ((mask & AbstractDom.MASK_PLIN) == 0)
            data = sysTextEscapeUnstrip(data, 0, data.length(), wrap);
        write(data, wrap);
    }

    /**
     * <p>Text escape a string.</p>
     *
     * @param s     The string.
     * @param begin the beginning index.
     * @param end   the ending index.
     * @param wrap  The wrap flag.
     * @return The text escaped string, or null.
     */
    private String sysTextEscapeUnstrip(String s, int begin,
                                        int end, boolean wrap) {
        /* we keep buf = null as long as no character was escaped */
        int back = begin;
        boolean lastspace = ((mask & AbstractDom.MASK_LTSP) != 0);
        StringBuilder buf = null;
        while (begin < end) {
            int ch = s.codePointAt(begin);
            String help;
            if (wrap && ch == XmlMachine.CHAR_SPACE) {
                if (!lastspace) {
                    help = null;
                    lastspace = true;
                } else {
                    help = "#32";
                }
            } else {
                help = getEntityRev(ch, wrap);
                lastspace = false;
            }
            if (help != null) {
                if (buf == null)
                    buf = new StringBuilder(s.substring(back, begin));
                buf.appendCodePoint(XmlMachine.CHAR_AMPER);
                buf.append(help);
                buf.appendCodePoint(XmlMachine.CHAR_SEMI);
            } else {
                if (buf != null)
                    buf.appendCodePoint(ch);
            }
            begin += Character.charCount(ch);
        }
        if (lastspace) {
            mask |= AbstractDom.MASK_LTSP;
        } else {
            mask &= ~AbstractDom.MASK_LTSP;
        }
        if (buf == null)
            return s.substring(back, end);
        return buf.toString();
    }


    /**
     * <p>Retrieve an entity rev.</p>
     *
     * @param ch   The character.
     * @param wrap The wrap flag.
     * @return The name or null.
     */
    private static String getEntityRev(int ch, boolean wrap) {
        if (wrap && (ch < XmlMachine.CHAR_SPACE || ch == XmlMachine.CHAR_BOM)) {
            if (ch < XmlMachine.CHAR_SPACE) {
                return "#" + Integer.toString(ch);
            } else {
                return "#x" + Integer.toString(ch, 16);
            }
        } else {
            return ForeignXml.getEntityRev(ch);
        }
    }

    /***************************************************************/
    /* Tag Writing                                                 */
    /***************************************************************/

    /**
     * <p>Copy an empty dom element.</p>
     *
     * @param de The template dom element.
     * @throws IOException IO error.
     */
    public void copyEmpty(DomElement de) throws IOException {
        if ((mask & AbstractDom.MASK_PLIN) != 0)
            return;
        write("<");
        write(de.getName());
        copyAttributes(de);
        write("/>");
    }

    /**
     * <p>Copy a start dom element.</p>
     *
     * @param de The template dom element.
     * @throws IOException IO error.
     */
    public void copyStart(DomElement de) throws IOException {
        if ((mask & AbstractDom.MASK_PLIN) != 0)
            return;
        write("<");
        write(de.getName());
        copyAttributes(de);
        write(">");
    }

    /**
     * <p>Copy the attributes of a dom element.</p>
     *
     * @param de The template dom element.
     * @throws IOException IO error.
     */
    private void copyAttributes(DomElement de) throws IOException {
        String[] attrs = de.snapshotAttrs();
        for (int i = 0; i < attrs.length; i++) {
            String attr = attrs[i];
            Object val = de.getAttrObj(attr);
            if (val == null)
                continue;
            write(" ", true);
            write(attr);
            if (!"".equals(val)) {
                write("=");
                if (val instanceof String) {
                    write("\"");
                    copyText((String) val);
                    write("\"");
                } else {
                    write(Long.toString(((Long) val).longValue()));
                }
            }
        }
    }

    /**
     * <p>Copy an end dom element.</p>
     *
     * @param de The template dom element.
     * @throws IOException IO error.
     */
    public void copyEnd(DomElement de)
            throws IOException {
        if ((mask & AbstractDom.MASK_PLIN) != 0)
            return;
        write("</");
        write(de.getName());
        write(">");
    }

    /***************************************************************/
    /* Comment & Indent                                            */
    /***************************************************************/

    /**
     * <p>Write a first line comment.</p>
     *
     * @throws IOException IO error.
     */
    public void writeComment(String comment)
            throws IOException {
        writer.write("<!-- ");
        writer.write(ForeignXml.sysTextEscape(comment));
        writer.write(" -->\n");
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
     * <p>Write the indent.</p>
     *
     * @throws IOException IO error.
     */
    public void writeIndent() throws IOException {
        for (int i = 0; i < indent; i++) {
            pos++;
            writer.write(" ");
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
