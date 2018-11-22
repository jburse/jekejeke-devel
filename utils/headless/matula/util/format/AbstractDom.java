package matula.util.format;

import matula.util.data.MapHash;

/**
 * <p>This class provides an abstract dom node.</p>
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
public abstract class AbstractDom
        implements Cloneable {
//    public static final String TIMESTAMP_DOM = "yyyy-MM-dd'T'HH:mm:ss.SSS";

    public static final int MASK_TEXT = 0x00000001; /* should not ignore space */
    public static final int MASK_LIST = 0x00000002; /* should ignore root */
    public static final int MASK_STRP = 0x00000004; /* should strip space */

    public static final int MASK_LTSP = 0x00000010; /* last read or write was space */
    public static final int MASK_PLIN = 0x00000020; /* write suppress tags and entities */

    public static final int MASK_JSON = 0x00000100; /* format is JSON */

    public static final int TYPE_NONE = 0;
    public static final int TYPE_EMPTY = 1; /* disable list */
    public static final int TYPE_ANY = 2; /* enable text and disable strip */
    public static final int TYPE_TEXT = 3; /* enable text and enable strip */

    private DomElement parent;
    private String key;

    /**
     * <p>Retrieve the parent.</p>
     *
     * @return The parent.
     */
    public DomElement getParent() {
        return parent;
    }

    /**
     * <p>Set the parent.</p>
     *
     * @param p The parent.
     */
    public void setParent(DomElement p) {
        if (p == null)
            throw new NullPointerException("parent missing");
        parent = p;
    }

    /**
     * <p>Retrieve the key name.</p>
     *
     * @return The key name.
     */
    public String getKey() {
        return key;
    }

    /**
     * <p>Set the key name.</p>
     *
     * @param k The key name.
     */
    public void setKey(String k) {
        if (k == null)
            throw new NullPointerException("key missing");
        key = k;
    }

    /**
     * <p>Check the key.</p>
     *
     * @param n The key.
     * @return True if the key matches, otherwise false.
     */
    public boolean isKey(String n) {
        return (key != null ? key.equalsIgnoreCase(n) : null == n);
    }

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * <p>Create a deep copy.</p>
     *
     * @return The deep copy.
     */
    public Object clone() {
        AbstractDom res;
        try {
            res = (AbstractDom) super.clone();
        } catch (CloneNotSupportedException x) {
            throw new RuntimeException("internal error", x);
        }
        res.reinitialize();
        return res;
    }

    /**
     * Reset to initial default state.
     */
    void reinitialize() {
        parent = null;
    }

    /****************************************************************/
    /* Complex Types                                                */
    /****************************************************************/

    /**
     * <p>Retrieve the complex type value for an element type.</p>
     *
     * @param control The control.
     * @param type    The element type.
     * @return The complex type.
     */
    public static int getControl(MapHash<String, Integer> control,
                                 String type) {
        if (control == null)
            return TYPE_NONE;
        Integer val = control.get(type);
        if (val == null)
            return TYPE_NONE;
        return val.intValue();
    }

    /***************************************************************/
    /* Name Token Conversion                                       */
    /***************************************************************/

    /**
     * <p>Some test cases.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) throws ParseException {
        String TIMESTAMP_DOM = "yyyy-MM-dd'T'HH:mm:ss.SSS";
        SimpleDateFormat sd = new SimpleDateFormat(TIMESTAMP_DOM);

        Timestamp ts = new Timestamp(new Date().getTime());
        System.out.println("ts=" + ts);
        String str = sd.format(new Date(ts.getTime()));
        System.out.println("format(ts)=" + str);

        System.out.println();

        System.out.println("str=" + str);
        System.out.println("parse(str)=" + new Timestamp(sd.parse(str).getTime()));
    }
    */

}
