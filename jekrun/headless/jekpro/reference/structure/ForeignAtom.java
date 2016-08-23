package jekpro.reference.structure;

import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.Term;
import jekpro.tools.term.TermCompound;

/**
 * <p>The foreign predicates for the module atom.</p>
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
public final class ForeignAtom {
    private static final int REP_CHARS = 0;
    private static final int REP_CODES = 1;

    /****************************************************************/
    /* Codes & Chars Conversion                                     */
    /****************************************************************/

    /**
     * <p>Convert a string either to a char or code list.</p>
     *
     * @param str The string.
     * @param rep The representation.
     * @return The list.
     */
    public static Object sysAtomToList(String str, int rep) {
        Object res = Knowledgebase.OP_NIL;
        int i = str.length();
        while (i > 0) {
            int ch = str.codePointBefore(i);
            Object val;
            switch (rep) {
                case REP_CHARS:
                    val = new String(Character.toChars(ch));
                    break;
                case REP_CODES:
                    val = Integer.valueOf(ch);
                    break;
                default:
                    throw new IllegalArgumentException("illegal rep");
            }
            i -= Character.charCount(ch);
            res = new TermCompound(Knowledgebase.OP_CONS, val, res);
        }
        return res;
    }

    /**
     * <p>Convert either a char or code list to a string.</p>
     *
     * @param list The list.
     * @param rep  The representation.
     * @return The string.
     * @throws InterpreterMessage Validation error.
     */
    public static String sysListToAtom(Object list, int rep)
            throws InterpreterMessage {
        StringBuilder buf = new StringBuilder();
        while (list instanceof TermCompound &&
                ((TermCompound) list).getArity() == 2 &&
                ((TermCompound) list).getFunctor().equals(
                        Knowledgebase.OP_CONS)) {
            TermCompound tc = (TermCompound) list;
            Object elem = tc.getArg(0);
            InterpreterMessage.checkInstantiated(elem);
            switch (rep) {
                case REP_CHARS:
                    String fun = InterpreterMessage.castString(elem);
                    int n = InterpreterMessage.castCharacter(fun);
                    buf.appendCodePoint(n);
                    break;
                case REP_CODES:
                    Number num = InterpreterMessage.castInteger(elem);
                    InterpreterMessage.checkCharacterCode(num.intValue());
                    buf.appendCodePoint(num.intValue());
                    break;
                default:
                    throw new IllegalArgumentException("illegal rep");
            }
            list = tc.getArg(1);
        }
        if (list.equals(Knowledgebase.OP_NIL)) {
            /* do nothing */
        } else {
            InterpreterMessage.checkInstantiated(list);
            throw new InterpreterMessage(
                    InterpreterMessage.typeError("list", list));
        }
        return buf.toString();
    }

    /**
     * <p>Convert a char to a code.</p>
     *
     * @param str The char.
     * @return The code.
     * @throws InterpreterMessage Validation error.
     */
    public static int sysCharToCode(String str)
            throws InterpreterMessage {
        return InterpreterMessage.castCharacter(str);
    }

    /**
     * <p>Convert a code to a char.</p>
     *
     * @param val The code.
     * @return The char.
     * @throws InterpreterMessage Validation error.
     */
    public static String sysCodeToChar(int val)
            throws InterpreterMessage {
        InterpreterMessage.checkCharacterCode(val);
        return new String(Character.toChars(val));
    }

    /****************************************************************/
    /* 16-bit Word Helpers                                          */
    /****************************************************************/

    /**
     * <p>Determine the length of a string in words.</p>
     *
     * @param str The string.
     * @return The length in words.
     */
    public static int sysAtomWordLen(String str) {
        return str.length();
    }

    /**
     * <p>Determine a sub string.</p>
     *
     * @param str  The string.
     * @param from The from word index.
     * @param to   The to word index.
     * @return The sub string.
     */
    public static String sysAtomWordSubstring(String str, int from, int to) {
        return str.substring(from, to);
    }

    /**
     * <p>Determine the length of a sub string in code points.</p>
     *
     * @param str  The string.
     * @param from The from word index.
     * @param to   The to word index.
     * @return The length in code points.
     */
    public static int sysAtomWordCount(String str, int from, int to) {
        return str.codePointCount(from, to);
    }

    /**
     * <p>Enumerate the positions in a string.</p>
     * <p>If to &lt; from enumerate backwards.</p>
     *
     * @param co   The call out.
     * @param str  The string.
     * @param from The from word index.
     * @param to   The to word index.
     * @return The word position.
     */
    public static int sysAtomWordPos(CallOut co, String str,
                                     int from, int to) {
        int pos;
        if (co.getFirst()) {
            pos = from;
        } else {
            pos = ((Integer) co.getData()).intValue();
            if (to < from) {
                int ch = str.codePointBefore(pos);
                pos = pos - Character.charCount(ch);
            } else {
                int ch = str.codePointAt(pos);
                pos = pos + Character.charCount(ch);
            }
        }
        if (to < from) {
            if (pos > to) {
                co.setData(Integer.valueOf(pos));
                co.setRetry(true);
            }
        } else {
            if (pos < to) {
                co.setData(Integer.valueOf(pos));
                co.setRetry(true);
            }
        }
        return pos;
    }

    /**
     * <p>Convert a code point position to a word position.</p>
     * <p>If off &lt; 0 convert backwards.</p>
     *
     * @param from The from word index.
     * @param off  The code point offset.
     * @return The word position.
     */
    public static int sysAtomWordOffset(String str, int from, int off)
            throws InterpreterMessage {
        try {
            return str.offsetByCodePoints(from, off);
        } catch (IndexOutOfBoundsException x) {
            throw new InterpreterMessage(
                    InterpreterMessage.representationError("index bounds"));
        }
    }

    /**
     * <p>Check whether a string region matches another string.</p>
     *
     * @param str   The string.
     * @param toff  The offset in words.
     * @param other The other string.
     * @param ooff  The other offset in words.
     * @param len   The length in words.
     * @return True if the region matches, otherwise false.
     */
    public static boolean sysAtomWordMatch(String str, int toff,
                                           String other, int ooff, int len) {
        return str.regionMatches(toff, other, ooff, len);
    }

    /****************************************************************/
    /* Number Helpers                                               */
    /****************************************************************/

    /**
     * <p>Convert a string to a number.</p>
     *
     * @param inter The call-in.
     * @param co    The call out.
     * @param num   The string.
     * @return The number.
     * @throws InterpreterException Error and position.
     */
    public static Object sysAtomToNumber(Interpreter inter, CallOut co, String num)
            throws InterpreterException, InterpreterMessage {
        return Term.parseNumber(num, inter);
    }

    /**
     * <p>Convert a number to a string.</p>
     *
     * @param num The number.
     * @return The string.
     * @throws InterpreterMessage Validation error.
     */
    public static String sysNumberToAtom(Number num)
            throws InterpreterMessage {
        return Term.toString(Term.FLAG_QUOTED, num);
    }

}
