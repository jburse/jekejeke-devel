package matula.util.format;

/**
 * <p>This class provides a dom text.</p>
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
public final class DomText extends AbstractDom {
    private Object data;

    /**
     * <p>Retrieve the string data.</p>
     *
     * @return The string data.
     */
    public String getData() {
        return (String) getDataObj();
    }

    /**
     * <p>Retrieve the long data.</p>
     *
     * @return The long data.
     */
    public long getDataLong() {
        return ((Long) getDataObj()).longValue();
    }

    /**
     * <p>Retrieve the data.</p>
     *
     * @return The data.
     */
    public Object getDataObj() {
        return data;
    }

    /**
     * <p>Set the string data.</p>
     *
     * @param val The string data.
     */
    public void setData(String val) {
        setDataObj(val);
    }

    /**
     * <p>Set the long data.</p>
     *
     * @param val The long data.
     */
    public void setDataLong(long val) {
        setDataObj(Long.valueOf(val));
    }

    /**
     * <p>Set the data.</p>
     *
     * @param val The data.
     */
    public void setDataObj(Object val) {
        if (val == null)
            throw new NullPointerException("data missing");
        data = val;
    }

}
