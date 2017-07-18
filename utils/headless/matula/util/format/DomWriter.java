package matula.util.format;

import matula.util.system.ForeignXml;

import java.io.IOException;
import java.io.Writer;
import java.util.Date;

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
    private static final int OFFSET_INCREMENT = 4;

    Writer writer;
    int ret;
    int offset;

    /**
     * <p>Increment the indent.</p>
     */
    void incIndent() {
        offset += OFFSET_INCREMENT;
    }

    /**
     * <p>Decrement the indent.</p>
     */
    void decIndent() {
        offset -= OFFSET_INCREMENT;
    }

    /**
     * <p>Write the indent.</p>
     *
     * @throws IOException Shit happens.
     */
    void writeIndent() throws IOException {
        for (int i = 0; i < offset; i++)
            writer.write(" ");
    }

    /**
     * <p>Write a time stamp.</p>
     *
     * @throws IOException Shit happens.
     */
    void writeComment(String comment) throws IOException {
        writer.write("<!-- ");
        writer.write(ForeignXml.sysTextEscape(comment));
        writer.write(" -->\n");
    }

}
