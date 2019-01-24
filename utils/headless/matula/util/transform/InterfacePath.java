package matula.util.transform;

import matula.util.format.DomElement;
import matula.util.regex.ScannerError;

import java.io.IOException;

/**
 * <p>This class provides a path interface.</p>
 * <p>The following flags are used to control a path:</p>
 * <ul>
 * <li>FLAG_DIRE: Read-only, asks if setDocument() needed.</li>
 * <li>FLAG_STYL: Read-only, asks if sheet and not data.</li>
 * <li>FLAG_SCHM: Write-only, tells if schema is requested.</li>
 * </ul>
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
public interface InterfacePath {
    int FLAG_DIRE = 0x00000001;
    int FLAG_STYL = 0x00000002;
    int FLAG_SCHM = 0x00000004;

    /**
     * <p>Retrieve the actual flags.</p>
     *
     * @return The flags.
     */
    int getFlags();

    /**
     * <p>Set the actual flags.</p>
     *
     * @param flags The flags.
     */
    void setFlags(int flags);

    /**
     * <p>Set the document inside a collection.</p>
     *
     * @param d The document.
     */
    void setDocument(String d);

    /**
     * <p>Open a cursor and log it.</p>
     *
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    void list() throws IOException, ScannerError;

    /**
     * <p>Advance the cursor.</p>
     *
     * @return True if a dom element was found.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    boolean next() throws IOException, ScannerError;

    /**
     * <p>Close the cursor.</p>
     */
    void close();

    /**
     * <p>Retrieve the found dom elememnt.</p>
     *
     * @return The found dom element.
     */
    DomElement getFound();

    /**
     * <p>Set the root.</p>
     *
     * @param r The root.
     */
    void setRoot(DomElement r);

}