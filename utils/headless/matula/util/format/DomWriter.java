package matula.util.format;

import matula.util.data.MapHash;
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
final class DomWriter {
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
    void setWriter(Writer w) {
        writer = w;
    }

    /**
     * <p>Set the return mask.</p>
     *
     * @param r The return mask.
     */
    void setMask(int r) {
        mask = r;
    }

    /**
     * <p>Retrieve the return mask.</p>
     *
     * @return The return mask.
     */
    int getMask() {
        return mask;
    }

    /**
     * <p>Set the tag control.</p>
     *
     * @param c The tag control.
     */
    void setControl(MapHash<String, Integer> c) {
        control = c;
    }

    /**
     * <p>Retrieve the tag control.</p>
     *
     * @return The tag control.
     */
    MapHash<String, Integer> getControl() {
        return control;
    }

    /**
     * <p>Increment the indent.</p>
     */
    void incIndent() {
        indent += INDENT_INCREMENT;
    }

    /**
     * <p>Decrement the indent.</p>
     */
    void decIndent() {
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
    void writeIndent() throws IOException {
        for (int i = 0; i < indent; i++)
            writer.write(" ");
    }

    /**
     * <p>Write a time stamp.</p>
     *
     * @throws IOException Shit happens.
     */
    void writeComment(String comment) throws IOException {
        write("<!-- ");
        write(ForeignXml.sysTextEscape(comment));
        write(" -->\n");
    }

    /**
     * <p>Write a string.</p>
     * @param str The string.
     * @throws IOException Shit happens.
     */
    void write(String str) throws IOException {
        writer.write(str);
    }

}
