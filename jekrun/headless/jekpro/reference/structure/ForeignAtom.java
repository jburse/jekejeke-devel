package jekpro.reference.structure;

import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologWriter;
import jekpro.reference.arithmetic.EvaluableElem;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.*;
import matula.util.regex.CodeType;
import matula.util.regex.CompLang;
import matula.util.regex.ScannerError;
import matula.util.regex.ScannerToken;

import java.math.BigDecimal;
import java.math.BigInteger;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignAtom {
    public final static String ERROR_SYNTAX_ILLEGAL_NUMBER = "illegal_number"; /* SWI, e163 */
    public final static String ERROR_SYNTAX_NUMBER_OVERFLOW = "number_overflow";
    public final static String ERROR_SYNTAX_CHARACTER_MISSING = "character_missing";
    public final static String ERROR_SYNTAX_REF_NOT_READABLE = "ref_unreadable";

    public static final int MASK_NUMB_SIGN = 0x00000001;
    public static final int MASK_NUMB_USCR = 0x00000002;

    private static final int REP_CHARS = 0;
    private static final int REP_CODES = 1;

    /****************************************************************/
    /* Codes & Chars Conversion                                     */
    /****************************************************************/

    /**
     * <p>Convert a string either to a char or code list.</p>
     *
     * @param inter The interpreter.
     * @param str   The string.
     * @param rep   The representation.
     * @return The list.
     */
    public static Object sysAtomToList(Interpreter inter,
                                       String str, int rep) {
        Lobby lobby = inter.getKnowledgebase().getLobby();
        Object res = lobby.ATOM_NIL;
        int i = str.length();
        while (i > 0) {
            int ch = str.codePointBefore(i);
            Object val;
            switch (rep) {
                case REP_CHARS:
                    val = SkelAtom.valueOf(ch);
                    break;
                case REP_CODES:
                    val = Integer.valueOf(ch);
                    break;
                default:
                    throw new IllegalArgumentException("illegal rep");
            }
            i -= Character.charCount(ch);
            res = new TermCompound(lobby.ATOM_CONS, val, res);
        }
        return res;
    }

    /**
     * <p>Convert either a char or code list to a string.</p>
     *
     * @param list The list.
     * @param rep  The representation.
     * @return The string.
     * @throws ClassCastException Validation error.
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
            int n;
            switch (rep) {
                case REP_CHARS:
                    String fun = InterpreterMessage.castString(elem);
                    n = SpecialUniv.castCharacter(fun);
                    break;
                case REP_CODES:
                    Number num = InterpreterMessage.castInteger(elem);
                    n = SpecialEval.castCodePoint(num);
                    break;
                default:
                    throw new IllegalArgumentException("illegal rep");
            }
            buf.appendCodePoint(n);
            list = tc.getArg(1);
        }
        if (list.equals(Foyer.OP_NIL)) {
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
     * @throws ClassCastException Validation error.
     */
    public static int sysCharToCode(String str)
            throws ClassCastException {
        return SpecialUniv.castCharacter(str);
    }

    /**
     * <p>Convert a code to a char.</p>
     *
     * @param val The code.
     * @return The char.
     * @throws ClassCastException Validation error.
     */
    public static String sysCodeToChar(Integer val)
            throws ClassCastException {
        int n = SpecialEval.castCodePoint(val);
        return SkelAtom.valueOf(n);
    }

    /****************************************************************/
    /* 16-bit Word Helpers                                          */
    /****************************************************************/

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
    public static Integer sysAtomWordPos(CallOut co, String str,
                                         int from, int to) {
        AtomCursor ac;
        if (co.getFirst()) {
            ac = new AtomCursor(str, 0, from, to);
            if (!ac.hasMoreElements())
                return null;
            co.setData(ac);
        } else {
            ac = (AtomCursor) co.getData();
        }
        Integer res = ac.nextElement();
        co.setRetry(ac.hasMoreElements());
        return res;
    }

    /**
     * <p>Enumerate the positions in a string.</p>
     * <p>If to &lt; from enumerate backwards.</p>
     *
     * @param inter The interpreter.
     * @param co    The call out.
     * @param str   The string.
     * @param cfrom The from codepoint index.
     * @param from  The from word index.
     * @param to    The to word index.
     * @param cout  The codepoint position.
     * @return The word position.
     * @throws InterpreterException Shit hapens.
     */
    public static Integer sysAtomWordPos(Interpreter inter, CallOut co,
                                         String str, int cfrom, int from,
                                         int to, AbstractTerm cout)
            throws InterpreterException {
        AtomCursor ac;
        if (co.getFirst()) {
            ac = new AtomCursor(str, cfrom, from, to);
            co.setData(ac);
        } else {
            ac = (AtomCursor) co.getData();
        }
        while (ac.hasMoreElements()) {
            Integer val1 = ac.getCFrom();
            Integer val2 = ac.nextElement();
            Object mark = AbstractTerm.markBind(inter);
            if (AbstractTerm.unifyTerm(inter, cout, val1)) {
                co.setRetry(true);
                return val2;
            } else {
                AbstractTerm.releaseBind(inter, mark);
            }
        }
        return null;
    }

    /**
     * <p>Advance a word index by code point offset.</p>
     *
     * @param val    The string.
     * @param index  The word index.
     * @param offset The code point offset.
     * @return The new word index.
     */
    public static Integer sysOffsetByCodePoints(String val,
                                                int index, int offset) {
        int x = index;
        if (offset >= 0) {
            int i;
            for (i = 0; x < val.length() && i < offset; i++) {
                if (Character.isHighSurrogate(val.charAt(x++)) &&
                        x < val.length() &&
                        Character.isLowSurrogate(val.charAt(x))) {
                    x++;
                }
            }
            if (i < offset)
                return null;
        } else {
            int i;
            for (i = offset; x > 0 && i < 0; i++) {
                if (Character.isLowSurrogate(val.charAt(--x)) &&
                        x > 0 &&
                        Character.isHighSurrogate(val.charAt(x - 1))) {
                    x--;
                }
            }
            if (i < 0)
                return null;
        }
        return Integer.valueOf(x);
    }

    /****************************************************************/
    /* Number Predicates                                            */
    /****************************************************************/

    /**
     * <p>Parse a number.</p>
     *
     * @param inter The interpreter.
     * @param s     The string.
     * @return The number.
     * @throws InterpreterException Error and position.
     */
    public static Number sysAtomToNumber(Interpreter inter, String s)
            throws InterpreterException, InterpreterMessage {
        Number num;
        try {
            int ch;
            if (0 < s.length() && (ch = s.codePointAt(0)) == ScannerToken.SCAN_NEG) {
                int pos = Character.charCount(ch);
                num = toNumber(s.substring(pos), pos, MASK_NUMB_SIGN);
                num = EvaluableElem.neg(num);
            } else {
                num = toNumber(s, 0, MASK_NUMB_SIGN);
            }
        } catch (ScannerError y) {
            String line = ScannerError.linePosition(s, y.getErrorOffset());
            InterpreterMessage x = new InterpreterMessage(
                    InterpreterMessage.syntaxError(y.getMessage()));
            throw new InterpreterException(x,
                    InterpreterException.fetchPos(
                            InterpreterException.fetchStack(inter), line, inter));
        } catch (EngineMessage y) {
            throw new InterpreterMessage(y);
        }
        return num;
    }

    /**
     * <p>Convert a string to a number.</p>
     *
     * @param str    The string.
     * @param offset The error offset.
     * @param mask   The mask.
     * @return The number.
     * @throws ScannerError Error and position.
     */
    public static Number toNumber(String str, int offset, int mask)
            throws ScannerError {
        try {
            int ch = (0 < str.length() ? str.codePointAt(0) : -1);
            if (ch == CodeType.LINE_ZERO) {
                int k = Character.charCount(ch);
                ch = (k < str.length() ? str.codePointAt(k) : -1);
                switch (ch) {
                    case ScannerToken.PREFIX_BINARY:
                        k += Character.charCount(ch);
                        String val = prepareUnderscore(k, str, offset, mask);
                        if (val.length() < 63)
                            return TermAtomic.normBigInteger(Long.parseLong(val, 2));
                        return TermAtomic.normBigInteger(new BigInteger(val, 2));
                    case ScannerToken.PREFIX_OCTAL:
                        k += Character.charCount(ch);
                        val = prepareUnderscore(k, str, offset, mask);
                        if (val.length() < 21)
                            return TermAtomic.normBigInteger(Long.parseLong(val, 8));
                        return TermAtomic.normBigInteger(new BigInteger(val, 8));
                    case ScannerToken.PREFIX_HEX:
                        k += Character.charCount(ch);
                        val = prepareUnderscore(k, str, offset, mask);
                        if (val.length() < 16)
                            return TermAtomic.normBigInteger(Long.parseLong(val, 16));
                        return TermAtomic.normBigInteger(new BigInteger(val, 16));
                    case CodeType.LINE_SINGLE:
                        k += Character.charCount(ch);
                        val = CompLang.ISO_COMPLANG.resolveEscape(
                                str.substring(k), CodeType.LINE_SINGLE,
                                false, k + offset, CodeType.ISO_CODETYPE);
                        int res = SpecialUniv.castCharacter(val);
                        return Integer.valueOf(res);
                    case ScannerToken.PREFIX_REFERENCE:
                        throw new ScannerError(ERROR_SYNTAX_REF_NOT_READABLE, k + offset);
                    case ScannerToken.PREFIX_DECIMAL:
                        k += Character.charCount(ch);
                        val = prepareParts(k, str, offset, mask);
                        return TermAtomic.normBigDecimal(new BigDecimal(val));
                    case ScannerToken.PREFIX_FLOAT32:
                        k += Character.charCount(ch);
                        val = prepareParts(k, str, offset, mask);
                        return TermAtomic.makeFloat(Float.parseFloat(val));
                    default:
                        break;
                }
            }
            int j = str.indexOf(ScannerToken.SCAN_PERIOD);
            if (j != -1) {
                j += Character.charCount(ScannerToken.SCAN_PERIOD);
                if (j < str.length() && Character.isDigit(str.codePointAt(j))) {
                    String val = prepareParts(0, str, offset, mask);
                    return TermAtomic.makeDouble(Double.parseDouble(val));
                }
            }
            String val = prepareUnderscore(0, str, offset, mask);
            if (val.length() < 19)
                return TermAtomic.normBigInteger(Long.parseLong(val));
            return TermAtomic.normBigInteger(new BigInteger(val));
        } catch (ClassCastException x) {
            throw new ScannerError(ERROR_SYNTAX_CHARACTER_MISSING, str.length() + offset);
        } catch (ArithmeticException x) {
            throw new ScannerError(ERROR_SYNTAX_NUMBER_OVERFLOW, str.length() + offset);
        } catch (NumberFormatException x) {
            throw new ScannerError(ERROR_SYNTAX_ILLEGAL_NUMBER, str.length() + offset);
        }
    }

    /**
     * <p>Convert a number to a string.</p>
     *
     * @param m     The number.
     * @param flags The flags.
     * @return The string.
     * @throws InterpreterMessage Not a Prolog number.
     */
    public static String sysNumberToAtom(Number m, int flags)
            throws InterpreterMessage {
        try {
            return numToString(m, flags);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Convert a number to a string.</p>
     *
     * @param m     The number.
     * @param flags The flags.
     * @return The string.
     * @throws EngineMessage Not a Prolog number.
     */
    public static String numToString(Number m, int flags)
            throws EngineMessage {
        if (m instanceof Integer ||
                m instanceof BigInteger ||
                m instanceof Double) {
            return m.toString();
        } else if (m instanceof Float) {
            if ((flags & PrologWriter.FLAG_QUOT) != 0) {
                StringBuilder buf = new StringBuilder();
                float y = m.floatValue();
                if (y < 0) {
                    buf.append(Knowledgebase.OP_SUB);
                    buf.appendCodePoint(CodeType.LINE_ZERO);
                    buf.appendCodePoint(ScannerToken.PREFIX_FLOAT32);
                    buf.append(-y);
                } else {
                    buf.appendCodePoint(CodeType.LINE_ZERO);
                    buf.appendCodePoint(ScannerToken.PREFIX_FLOAT32);
                    buf.append(y);
                }
                return buf.toString();
            } else {
                return m.toString();
            }
        } else if (m instanceof Long ||
                m instanceof BigDecimal) {
            if ((flags & PrologWriter.FLAG_QUOT) != 0) {
                StringBuilder buf = new StringBuilder();
                if (EvaluableElem.sign(m).longValue() < 0) {
                    buf.append(Knowledgebase.OP_SUB);
                    buf.appendCodePoint(CodeType.LINE_ZERO);
                    buf.appendCodePoint(ScannerToken.PREFIX_DECIMAL);
                    buf.append(EvaluableElem.neg(m).toString());
                } else {
                    buf.appendCodePoint(CodeType.LINE_ZERO);
                    buf.appendCodePoint(ScannerToken.PREFIX_DECIMAL);
                    buf.append(m.toString());
                }
                return buf.toString();
            } else {
                return m.toString();
            }
        } else {
            return refToString(m);
        }
    }

    /**
     * <p>Convert a reference to a string.</p>
     *
     * @param obj The reference.
     * @return The string.
     */
    public static String refToString(Object obj) {
        StringBuilder buf = new StringBuilder();
        buf.appendCodePoint(CodeType.LINE_ZERO);
        buf.appendCodePoint(ScannerToken.PREFIX_REFERENCE);
        buf.append(Integer.toHexString(obj.hashCode()));
        return buf.toString();
    }

    /**
     * <p>Parse a number in some base.</p>
     *
     * @param inter The interpreter.
     * @param s     The string.
     * @param radix The radix.
     * @return The number.
     * @throws InterpreterException Error and position.
     */
    public static Number sysAtomToInteger(Interpreter inter, String s, int radix)
            throws InterpreterException, InterpreterMessage {
        Number num;
        try {
            int ch;
            if (0 < s.length() && (ch = s.codePointAt(0)) == ScannerToken.SCAN_NEG) {
                int pos = Character.charCount(ch);
                num = toInteger(s.substring(pos), pos, MASK_NUMB_SIGN, radix);
                num = EvaluableElem.neg(num);
            } else {
                num = toInteger(s, 0, MASK_NUMB_SIGN, radix);
            }
        } catch (ScannerError y) {
            String line = ScannerError.linePosition(s, y.getErrorOffset());
            InterpreterMessage x = new InterpreterMessage(
                    InterpreterMessage.syntaxError(y.getMessage()));
            throw new InterpreterException(x,
                    InterpreterException.fetchPos(
                            InterpreterException.fetchStack(inter), line, inter));
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        return num;
    }

    /**
     * <p>Convert a string to a number in some base.</p>
     *
     * @param str   The string.
     * @param radix The radix.
     * @return The number.
     * @throws ScannerError Error and position.
     */
    public static Number toInteger(String str, int offset, int mask, int radix)
            throws ScannerError {
        try {
            String val = prepareUnderscore(0, str, offset, mask);
            try {
                return TermAtomic.normBigInteger(Long.parseLong(val, radix));
            } catch (NumberFormatException x) {
                return TermAtomic.normBigInteger(new BigInteger(str, radix));
            }
        } catch (ClassCastException x) {
            throw new ScannerError(ERROR_SYNTAX_CHARACTER_MISSING, str.length() + offset);
        } catch (ArithmeticException x) {
            throw new ScannerError(ERROR_SYNTAX_NUMBER_OVERFLOW, str.length() + offset);
        } catch (NumberFormatException x) {
            throw new ScannerError(ERROR_SYNTAX_ILLEGAL_NUMBER, str.length() + offset);
        }
    }

    /**
     * <p>Convert a number to a string in some base.</p>
     *
     * @param num   The number.
     * @param radix The radix.
     * @return The string.
     * @throws InterpreterMessage Not a Prolog integer.
     */
    public static String sysIntegerToAtom(Number num, int radix)
            throws InterpreterMessage {
        if (num instanceof Integer) {
            return Integer.toString(num.intValue(), radix);
        } else if (num instanceof BigInteger) {
            return ((BigInteger) num).toString(radix);
        } else {
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    EngineMessage.OP_TYPE_INTEGER, num));
        }
    }

    /****************************************************************/
    /* Number Helpers                                               */
    /****************************************************************/

    /**
     * <p>Possible check sign or check underscore and strip underscore.</p>
     *
     * @param k      The position.
     * @param str    The string.
     * @param offset The offset.
     * @param mask   The mask.
     * @return The result.
     * @throws ScannerError Error and position.
     */
    private static String prepareUnderscore(int k, String str,
                                            int offset, int mask)
            throws ScannerError {
        if ((mask & MASK_NUMB_SIGN) != 0)
            checkSign(k, str, offset);
        if ((mask & MASK_NUMB_USCR) != 0) {
            CodeType.ISO_CODETYPE.checkUnderscore(k, str.length(), str, offset);
            return CodeType.ISO_CODETYPE.stripUnderscore(str.substring(k));
        } else {
            return str.substring(k);
        }
    }

    /**
     * <p>Check whether the string does not start with a sign.</p>
     * <p>We have to check again, since the number conversion accepts it
     * and toNumber() is also called from number_codes/2.</p>
     *
     * @param pos    The index.
     * @param str    The string.
     * @param offset The offset.
     * @throws ScannerError If the string starts with a sign.
     */
    private static void checkSign(int pos, String str, int offset)
            throws ScannerError {
        if (!(pos < str.length()))
            return;
        int ch = str.codePointAt(pos);
        if (ch == ScannerToken.SCAN_NEG || ch == ScannerToken.SCAN_POS)
            throw new ScannerError(ERROR_SYNTAX_ILLEGAL_NUMBER, pos + offset);
    }

    /**
     * <p>Possible check sign or check underscore in mantissa, fraction
     * or exponent and strip underscore.</p>
     *
     * @param k      The position.
     * @param str    The string.
     * @param offset The offset.
     * @param mask   The mask.
     * @return The result.
     * @throws ScannerError Error and position.
     */
    private static String prepareParts(int k, String str,
                                       int offset, int mask)
            throws ScannerError {
        if ((mask & MASK_NUMB_SIGN) != 0)
            checkSign(k, str, offset);
        if ((mask & MASK_NUMB_USCR) != 0) {
            checkParts(k, str, offset);
            return CodeType.ISO_CODETYPE.stripUnderscore(str.substring(k));
        } else {
            return str.substring(k);
        }
    }

    /**
     * <p>Check whether the mantissa, fraction and exponent satisfy the
     * underscore condition.</p>
     * <p>We have to check again, since we strip underscores and toNumber()
     * is also called from number_codes/2.</p>
     *
     * @param pos    The index.
     * @param str    The string.
     * @param offset The offset.
     * @throws ScannerError If the string starts with a sign.
     */
    private static void checkParts(int pos, String str, int offset)
            throws ScannerError {
        int k = str.indexOf(ScannerToken.SCAN_PERIOD, pos);
        if (k != -1) {
            CodeType.ISO_CODETYPE.checkUnderscore(pos, k, str, offset);
            pos = k + Character.charCount(ScannerToken.SCAN_PERIOD);
        }
        k = indexExponent(pos, str);
        if (k != -1) {
            CodeType.ISO_CODETYPE.checkUnderscore(pos, k, str, offset);
            pos = k + Character.charCount(ScannerToken.SCAN_EXPLOW);
            int ch;
            if (pos < str.length() &&
                    ((ch = str.codePointAt(pos)) == ScannerToken.SCAN_NEG ||
                            ch == ScannerToken.SCAN_POS)) {
                pos += Character.charCount(ScannerToken.SCAN_NEG);
            }
        }
        CodeType.ISO_CODETYPE.checkUnderscore(pos, str.length(), str, offset);
    }

    /**
     * <p>Find an exponent in a string.</p>
     *
     * @param pos The start index.
     * @param str The string.
     * @return The index, or -1.
     */
    private static int indexExponent(int pos, String str) {
        for (; pos < str.length(); pos++) {
            int ch = str.codePointAt(pos);
            if (ch == ScannerToken.SCAN_EXPLOW ||
                    ch == ScannerToken.SCAN_EXPCAP)
                return pos;
            pos += Character.charCount(ch) - 1;
        }
        return -1;
    }

    /**
     * <p>Some testing.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) throws EngineMessage {
        String str="?\u00FF?"; // "?\uFFFD?"
        byte[] buf=sysAtomToBlock(str);
        str=sysBlockToAtom(buf);
        System.out.println("str="+str);
    }
    */

}
