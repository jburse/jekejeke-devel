package matula.util.system;

import derek.util.protect.LicenseError;
import matula.util.config.AbstractRecognizer;

import java.io.*;
import java.net.Socket;

/**
 * <p>This class represent the socket open options.</p>
 * <p>The following flags are supported:</p>
 * <ul>
 * <li><b>MASK_OPEN_BINR:</b> For binary stream.</li>
 * </ul>
 * <p>For text stream, if no encoding is specified, UTF-8 is used.</p>
 * <p/>
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
public class OpenDuplex extends OpenCheck {
    public static final int MASK_OPEN_BINR = 0x00000010;
    public static final int MASK_OPEN_STRG = 0x00000020;

    public static final String UNIX_NEWLINE = "\n";
    public static final String MAC_NEWLINE = "\r";
    public static final String WINDOWS_NEWLINE = "\r\n";

    private String encoding;
    private int buffer = 8192;
    private String newline = OpenOpts.UNIX_NEWLINE;

    /**
     * <p>Set the character set encoding.</p>
     *
     * @param c The character set encoding.
     */
    public void setEncoding(String c) {
        encoding = c;
    }

    /**
     * <p>Retrieve the character set encoding.</p>
     *
     * @return The character set encoding, or null.
     */
    public String getEncoding() {
        return encoding;
    }

    /**
     * <p>Set the buffer size.</p>
     *
     * @param b The buffer size.
     */
    public void setBuffer(int b) {
        buffer = b;
    }

    /**
     * <p>Retrieve the buffer size.</p>
     *
     * @return The buffer size.
     */
    public int getBuffer() {
        return buffer;
    }

    /**
     * <p>Set the new line string.</p>
     *
     * @param n The new line string.
     */
    public void setNewLine(String n) {
        newline = n;
    }

    /**
     * <p>Retrieve the new line string.</p>
     *
     * @return The new line string.
     */
    public String getNewLine() {
        return newline;
    }

    /**
     * <p>Open a read stream.</p>
     *
     * @param know The knowledgebase.
     * @param sock The socket.
     * @return The read stream, or null if not modified.
     * @throws IOException              IO error.
     * @throws LicenseError             Decryption error.
     * @throws IllegalArgumentException Illegal paremeter combination.
     */
    public Object openRead(AbstractRecognizer know, Socket sock)
            throws IOException, LicenseError {
        InputStream in = sock.getInputStream();
        ConnectionInput cin;
        if (getBuffer() != 0) {
            cin = new ConnectionInput(new BufferedInputStream(in, getBuffer()));
            cin.setBuffer(getBuffer());
        } else {
            cin = new ConnectionInput(in);
        }
        if ((getFlags() & MASK_OPEN_BINR) != 0) {
            return cin;
        } else {
            String theencoding = getEncoding();
            if (theencoding == null)
                theencoding = ForeignUri.ENCODING_UTF8;
            InputStreamReader isr;
            try {
                isr = new InputStreamReader(cin, theencoding);
            } catch (UnsupportedEncodingException x) {
                cin.close();
                throw x;
            }
            ConnectionReader crd = new ConnectionReader(isr);
            crd.setUncoded(cin);
            return crd;
        }
    }

    /**
     * <p>Open a write stream.</p>
     *
     * @param sock The socket.
     * @return The write stream.
     * @throws IOException              IO error.
     * @throws IllegalArgumentException Illegal paremeter combination.
     */
    public Object openWrite(Socket sock)
            throws IOException {
        OutputStream out = sock.getOutputStream();
        ConnectionOutput cout;
        if (getBuffer() != 0) {
            cout = new ConnectionOutput(new BufferedOutputStream(out, getBuffer()));
            cout.setBuffer(getBuffer());
        } else {
            cout = new ConnectionOutput(out);
        }
        if ((getFlags() & MASK_OPEN_BINR) != 0) {
            return cout;
        } else {
            String theencoding = getEncoding();
            if (theencoding == null)
                theencoding = ForeignUri.ENCODING_UTF8;
            OutputStreamWriter osw;
            try {
                osw = new OutputStreamWriter(cout, theencoding);
            } catch (UnsupportedEncodingException x) {
                cout.close();
                throw x;
            }
            ConnectionWriter cwr = new ConnectionWriter(osw);
            cwr.setUncoded(cout);
            cwr.setNewLine(newline);
            return cwr;
        }
    }

}