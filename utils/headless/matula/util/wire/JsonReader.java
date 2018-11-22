package matula.util.wire;

import matula.util.data.ListArray;
import matula.util.format.AbstractDom;
import matula.util.format.AbstractReader;
import matula.util.regex.ScannerError;

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

    /**
     * <p>Set the reader.</p>
     *
     * @param r The reader.
     * @throws IOException I/O Error.
     */
    public void setReader(Reader r)
            throws IOException {
        /* t.b.d. */
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
        /* t.b.d. */
        return null;
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
        /* t.b.d. */
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
        /* t.b.d. */
    }

    /**
     * <p>Check whether the dom reader is at eof.</p>
     *
     * @throws IOException  I/O Error..
     * @throws ScannerError Syntax error.
     */
    public void checkEof()
            throws IOException, ScannerError {
        /* t.b.d. */
    }

}