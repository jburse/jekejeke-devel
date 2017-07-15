package matula.util.format;

import matula.util.regex.ScannerError;

import java.io.IOException;
import java.io.Reader;

/**
 * <p>The base class for a xml scanner.</p>
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
public class XmlScanner<T extends XmlMachine> {
    protected Reader reader;
    protected T mach;
    protected int ch;

    /**
     * <p>Create an xml scanner.</p>
     *
     * @param m The xml machine.
     */
    public XmlScanner(T m) {
        mach = m;
    }

    /**
     * <p>Reset the scanner.</p>
     *
     * @param r The reader.
     * @throws IOException problem reading first character.
     */
    public void setReader(Reader r) throws IOException {
        reader = r;
        ch = reader.read();
    }

    /**
     * <p>Retrieve the reader.</p>
     *
     * @return The reader.
     */
    public Reader getReader() {
        return reader;
    }

    /**
     * <p>Retrieve the next tag or text.</p>
     *
     * @throws IOException  problem reading character.
     * @throws ScannerError Rewriting problem.
     */
    public void nextTagOrText() throws IOException, ScannerError {
        mach.startTagOrText();
        for (; ; ) {
            if (mach.consume(ch))
                ch = reader.read();
            if (mach.getRes() != XmlMachine.RES_NONE)
                return;
        }
    }

    /**
     * <p>Retrieve result type.</p>
     *
     * @return The result type.
     */
    public int getRes() {
        return mach.getRes();
    }

    /**
     * <p>Retrieve the actual text.</p>
     * <p>Can be retrieved when the result type is RES_TEXT.</p>
     * <p>It can also be used for result type RES_TAG, and it will
     * then return the full tag.</p>
     *
     * @return The actual text.
     */
    public String getText() {
        return mach.getText();
    }

    /**
     * For efficiency, we allow to return the text buf.
     *
     * @return The text buf.
     */
    public char[] getTextBuf() {
        return mach.getTextBuf();
    }

    /**
     * For efficiency, we allow to return the text len.
     *
     * @return The text len.
     */
    public int getTextLen() {
        return mach.getTextLen();
    }

    /**
     * <p>Retrieve the actual tag type. can be retrieved when
     * the result type is RES_TAG.</p>
     *
     * @return The actual tag type.
     */
    public String getType() {
        return mach.getType();
    }

    /**
     * <p>Convenience method to check the actual tag type.</p>
     * <p>Will check the actual tag type ignoring case.</p>
     *
     * @param t The tag type.
     * @return true if actual tag type matches parameter.
     */
    public boolean isType(String t) {
        return mach.isType(t);
    }

    /**
     * <p>Retrieve an actual tag argument.</p>
     * <p>Can be retrieved when the result type is RES_TAG.</p>
     * <p>The attribute name will be matched ignore case.</p>
     *
     * @param a The attribute name
     * @return The value or null if attribute is not present.
     */
    public String getValue(String a) {
        return mach.getValue(a);
    }

    /**
     * <p>Retrieve the an attribute value.</p>
     *
     * @param a The attribute name.
     * @param d The default attribute value.
     * @return The attribute value.
     */
    public String getValue(String a, String d) {
        return mach.getValue(a, d);
    }

    /**
     * Retrieve the number of attributes. Can be retrieved when the
     * result type is RES_TAG.
     *
     * @return The number of attributes.
     */
    public int getAttrCount() {
        return mach.getAttrCount();
    }

    /**
     * Retrieve the nth attribute name. Can be retrieved when the
     * result type is RES_TAG.
     *
     * @param i The index.
     * @return The attribute name.
     */
    public String getAttr(int i) {
        return mach.getAttr(i);
    }

    /**
     * Retrieve the nth attribute value. Can be retrieved when the
     * result type is RES_TAG.
     *
     * @param i The index.
     * @return The attribute value.
     */
    public String getValueAt(int i) {
        return mach.getValueAt(i);
    }

    /**
     * <p>Find the index of an attribute.</p>
     * <p>The case of the attribute is ignored.</p>
     *
     * @param a The attribute.
     * @return The index, or -1.
     */
    public int indexAttr(String a) {
        return mach.indexAttr(a);
    }

}
