package jekpro.frequent.misc;

import jekpro.tools.call.InterpreterMessage;
import matula.util.misc.CodeType;

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

}
