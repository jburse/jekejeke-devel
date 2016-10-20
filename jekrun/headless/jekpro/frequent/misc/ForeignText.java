package jekpro.frequent.misc;

import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;
import matula.util.data.ListArray;
import matula.util.regex.CodeType;

/**
 * Provides the methods for the module text.
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignText {

    /**
     * <p>Retrieve the ISO prolog core/Jekejeke Unicode extension classifier.</p>
     *
     * @return The classifier.
     */
    public static CodeType sysGetISOClassifier() {
        return CodeType.ISO_CODETYPE;
    }

    /**
     * <p>Retrieve the type of a character.</p>
     *
     * @param ct The classifier.
     * @param ch The character.
     * @return The type.
     * @throws InterpreterMessage Not a character.
     */
    public static int sysCharType(CodeType ct, String ch)
            throws InterpreterMessage {
        return ct.classOf(InterpreterMessage.castCharacter(ch));
    }

    /**
     * <p>Retrieve the type of a code point.</p>
     *
     * @param ct The classifier.
     * @param cp The code point.
     * @return The type.
     */
    public static int sysCodeType(CodeType ct, int cp) {
        return ct.classOf(cp);
    }

    /**
     * <p>Tokenize an atom.</p>
     *
     * @param ct  The classifier.
     * @param str The atom.
     * @return The list of atoms.
     */
    public static Object sysTokenizeAtom(CodeType ct, String str) {
        ListArray<String> list = null;
        int n = str.length();
        int pos = 0;
        int last = -1;
        while (pos < n) {
            int ch = str.codePointAt(pos);
            if (ct.wordBreak2(last, ch)) {
                StringBuilder buf = new StringBuilder();
                buf.appendCodePoint(ch);
                pos += Character.charCount(ch);
                last = ch;
                while (pos < n && !ct.wordBreak1(last, ch = str.codePointAt(pos))) {
                    buf.appendCodePoint(ch);
                    pos += Character.charCount(ch);
                    last = ch;
                }
                if (list == null)
                    list = new ListArray<String>();
                list.add(buf.toString());
            } else {
                pos += Character.charCount(ch);
                last = ch;
            }
        }
        Object res = Knowledgebase.OP_NIL;
        if (list != null) {
            for (int i = list.size - 1; i >= 0; i--)
                res = new TermCompound(Knowledgebase.OP_CONS, list.get(i), res);
        }
        return res;
    }

}
