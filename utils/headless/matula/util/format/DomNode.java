package matula.util.format;

import matula.util.regex.ScannerError;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

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
public abstract class DomNode {
    DomElement parent;
    private boolean lock;

    /**
     * <p>Retrieve the parent.</p>
     *
     * @return The parent.
     */
    public DomElement getParent() {
        return parent;
    }

    /**
     * <p>Begin of reparaenting of this node.</p>
     *
     * @throws InterruptedException Shit happens.
     */
    void beginReparent() throws InterruptedException {
        synchronized (this) {
            while (lock)
                this.wait();
            lock = true;
        }
    }

    /**
     * <p>End of reparaenting of this node.</p>
     */
    void endReparent() {
        synchronized (this) {
            lock = false;
            this.notifyAll();
        }
    }

    /*****************************************************/
    /* Properties Like Lifecycle                         */
    /*****************************************************/

    /**
     * <p>Load this dom node.</p>
     * <p>Not synchronized, uses cut-over.</p>
     *
     * @param reader The input stream.
     * @param ret    The return flags.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    public void load(Reader reader, int ret)
            throws IOException, ScannerError {
        DomReader dr = new DomReader();
        dr.setReader(reader);
        dr.ret=ret;
        dr.nextTagOrText();
        load(dr);
        dr.checkEof();
    }

    /**
     * <p>Store this dom node.</p>
     * <p>Not synchronized, uses cursors.</p>
     *
     * @param writer  The writer.
     * @param comment The comment
     * @param ret    The return flags.
     */
    public void store(Writer writer, String comment, int ret)
            throws IOException {
        DomWriter dw = new DomWriter();
        dw.writer=writer;
        dw.ret=ret;
        if (comment != null && !"".equals(comment))
            dw.writeComment(comment);
        store(dw);
    }

    /**
     * <p>Load a dom node.</p>
     * <p>Not synchronized, uses cut-over.</p>
     *
     * @param dr The dom reader.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    abstract void load(DomReader dr)
            throws IOException, ScannerError;

    /**
     * <p>Store this dom node.</p>
     * <p>Not synchronized, uses cursors.</p>
     *
     * @param dw The dom writer.
     * @throws IOException Shit happens.
     */
    abstract void store(DomWriter dw)
            throws IOException;

}
