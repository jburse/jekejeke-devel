package matula.util.transform;

import matula.util.format.*;
import matula.util.system.ForeignXml;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

/**
 * <p>This class provides an XML writer.</p>
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
public final class XmlWriter extends AbstractWriter {
    private static final int INDENT_INCREMENT = 4;
    private static final int LINE_WIDTH = 75;

    private int pos;
    private String pending;
    private int indent;

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
            AbstractDom node = nodes[i];
            if ((getMask() & AbstractDom.MASK_TEXT) != 0) {
                storeNode(node);
            } else {
                writeIndent();
                storeNode(node);
                write("\n");
            }
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
            copyText(dt.getData());
        } else {
            DomElement de = (DomElement) node;
            int backmask = getMask();
            if (((backmask & AbstractDom.MASK_TEXT) == 0 ||
                    (backmask & AbstractDom.MASK_STRP) != 0) &&
                    AbstractDom.getControl(getControl(), de.getName()) == AbstractDom.TYPE_ANY) {
                int mask = backmask;
                mask |= AbstractDom.MASK_TEXT;
                mask &= ~AbstractDom.MASK_STRP;
                setMask(mask);
                try {
                    storeNode2(de);
                    setMask(backmask);
                } catch (IOException x) {
                    setMask(backmask);
                    throw x;
                }
            } else if (((backmask & AbstractDom.MASK_TEXT) == 0 ||
                    (backmask & AbstractDom.MASK_STRP) == 0) &&
                    AbstractDom.getControl(getControl(), de.getName()) == AbstractDom.TYPE_TEXT) {
                int mask = backmask;
                mask |= AbstractDom.MASK_TEXT;
                mask |= AbstractDom.MASK_STRP;
                setMask(mask);
                try {
                    storeNode2(de);
                    setMask(backmask);
                } catch (IOException x) {
                    setMask(backmask);
                    throw x;
                }
            } else {
                storeNode2(de);
            }
        }
    }

    /**
     * <p>Store the childeren.</p>
     *
     * @param de The dom element.
     * @throws IOException Shit happens.
     */
    private void storeNode2(DomElement de)
            throws IOException {
        boolean lastspace = ((getMask() & AbstractDom.MASK_LTSP) != 0);
        AbstractDom[] nodes = de.snapshotNodes();
        if (nodes.length == 0 &&
                (AbstractDom.getControl(getControl(), de.getName()) == AbstractDom.TYPE_EMPTY)) {
            copyStart(de);
        } else if (nodes.length != 0 ||
                (getMask() & AbstractDom.MASK_TEXT) != 0) {
            copyStart(de);
            if ((getMask() & AbstractDom.MASK_TEXT) != 0) {
                storeNodes(nodes);
            } else {
                write("\n");
                incIndent();
                storeNodes(nodes);
                decIndent();
                writeIndent();
            }
            copyEnd(de);
        } else {
            copyEmpty(de);
        }
        if (lastspace) {
            setMask(getMask() | AbstractDom.MASK_LTSP);
        } else {
            setMask(getMask() & ~AbstractDom.MASK_LTSP);
        }
    }

    /****************************************************************/
    /* Text Wrapping                                                */
    /****************************************************************/

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
                Writer wr = getWriter();
                if (k < 0) {
                    wr.write(pending, pending.length() + k, -k);
                    wr.write(str, 0, i);
                } else {
                    wr.write(str, k, i - k);
                }
                k = i;
                hasspace = true;
                pos++;
            } else if (ch == '\n') {
                Writer wr = getWriter();
                if (k < 0) {
                    wr.write(pending, pending.length() + k, -k);
                    wr.write(str, 0, i + 1);
                } else {
                    wr.write(str, k, i + 1 - k);
                }
                k = i + 1;
                pos = 0;
                hasspace = false;
            } else if (hasspace && pos >= LINE_WIDTH) {
                if ((getMask() & AbstractDom.MASK_PLIN) == 0)
                    incIndent();
                Writer wr = getWriter();
                wr.write("\n");
                k++;
                writeIndent();
                pos = indent + i + 1 - k;
                if ((getMask() & AbstractDom.MASK_PLIN) == 0)
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
            Writer wr = getWriter();
            if (k < 0) {
                wr.write(pending, pending.length() + k, -k);
                wr.write(str, 0, len);
            } else {
                wr.write(str, k, len - k);
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
        Writer wr = getWriter();
        if (pending != null)
            wr.write(pending);
        wr.flush();
    }

    /****************************************************************/
    /* DomText Writing                                              */
    /****************************************************************/

    /**
     * <p>Copy the given text.</p>
     *
     * @param data The text.
     * @throws IOException IO error.
     */
    public void copyText(String data) throws IOException {
        boolean wrap;
        if ((getMask() & AbstractDom.MASK_STRP) != 0) {
            wrap = true;
        } else {
            wrap = false;
        }
        if ((getMask() & AbstractDom.MASK_PLIN) == 0)
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
        boolean lastspace = ((getMask() & AbstractDom.MASK_LTSP) != 0);
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
            setMask(getMask() | AbstractDom.MASK_LTSP);
        } else {
            setMask(getMask() & ~AbstractDom.MASK_LTSP);
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
    /* DomElement Writing                                          */
    /***************************************************************/

    /**
     * <p>Copy an empty dom element.</p>
     *
     * @param de The template dom element.
     * @throws IOException IO error.
     */
    public void copyEmpty(DomElement de) throws IOException {
        if ((getMask() & AbstractDom.MASK_PLIN) != 0)
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
        if ((getMask() & AbstractDom.MASK_PLIN) != 0)
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
        AbstractDom[] attrs = de.snapshotAttrs();
        for (int i = 0; i < attrs.length; i++) {
            AbstractDom node = attrs[i];
            String attr = node.getKey();
            Object val = ((DomText) node).getDataObj();
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
        if ((getMask() & AbstractDom.MASK_PLIN) != 0)
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
        Writer wr = getWriter();
        wr.write("<!-- ");
        wr.write(ForeignXml.sysTextEscape(comment));
        wr.write(" -->\n");
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
        Writer wr = getWriter();
        for (int i = 0; i < indent; i++) {
            pos++;
            wr.write(" ");
        }
    }

}