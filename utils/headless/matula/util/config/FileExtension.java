package matula.util.config;

/**
 * <p>An object that encapsulates a file extension.</p>
 * <p>The following flags are supported:</p
 * <ul>
 * <li><b>MASK_USES_TEXT:</b> Library file.</li>
 * <li><b>MASK_USES_BNRY:</b> Foreign file.</li>
 * <li><b>MASK_USES_RSCS:</b> Resource file.</li>
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
public final class FileExtension {
    /* suffix relationship flags */
    public static final int MASK_USES_TEXT = 0x00000010;
    public static final int MASK_USES_BNRY = 0x00000020;
    public static final int MASK_USES_RSCS = 0x00000040;

    public static final int MASK_DATA_ECRY = 0x00000100;

    /* combined suffix flags */
    public static final int MASK_USES_SUFX
            = MASK_USES_TEXT | MASK_USES_BNRY | MASK_USES_RSCS;

    private int type;
    private String mimetype;

    /**
     * <p>Create a new file extension object.</p>
     *
     * @param t The type.
     * @param m The mimetype.
     */
    public FileExtension(int t, String m) {
        type = t;
        mimetype = m;
    }

    /**
     * <p>Retrieve the type.</p>
     *
     * @return The type.
     */
    public int getType() {
        return type;
    }

    /**
     * <p>Retrieve the mime type.</p>
     *
     * @return The mime type.
     */
    public String getMimeType() {
        return mimetype;
    }

}