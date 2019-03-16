package jekpro.model.pretty;

import jekpro.frequent.basic.SpecialProxy;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.*;
import jekpro.model.rope.Operator;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.reference.structure.ForeignAtom;
import jekpro.tools.term.*;
import matula.util.data.MapHashLink;
import matula.util.regex.CodeType;
import matula.util.regex.CompLang;
import matula.util.regex.ScannerToken;
import matula.util.wire.JsonReader;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>This class provides the writing of prolog terms.</p>
 * <p>The following parameters are recognized:</p>
 * <ul>
 * <li><b>flags:</b> FLAG_QUOT, FLAG_NUMV and FLAG_IGNO control.</li>
 * <li><b>store:</b> Operator definitions, or null.</li>
 * <li><b>en:</b> Serno generator, or null.</li>
 * <li><b>printmap:</b> Variable names, or null.</li>
 * <li><b>level:</b> Initial operator level.</li>
 * <li><b>spez:</b> SPEZ_OPLE, SPEZ_LEFT, SPEZ_TERM and SPEZ_GOAL control.</li>
 * </ul>
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
public class PrologWriter {
    public final static String OP_DOLLAR_VAR = "$VAR";
    public final static String OP_DOLLAR_STR = "$STR";

    public final static int SPACES = 3;

    public final static int STANDALONE = 0;
    public final static int ENDLINE = 50;

    public final static int FLAG_QUOT = 0x00000001;
    public final static int FLAG_NUMV = 0x00000002;
    public final static int FLAG_IGNO = 0x00000004;
    public final static int FLAG_IGNM = 0x00000008;

    public final static int FLAG_MKDT = 0x00000010;
    public final static int FLAG_FILL = 0x00000020;
    public final static int FLAG_HINT = 0x00000040;

    /* only write opts */
    public final static int FLAG_NEWL = 0x00000100;
    public final static int FLAG_NAVI = 0x00000200;
    public final static int FLAG_CMMT = 0x00000400;
    public final static int FLAG_STMT = 0x00000800;

    public final static int FLAG_DFLT = FLAG_CMMT | FLAG_STMT;

    public final static int SPEZ_OPLE = 0x00000001;
    public final static int SPEZ_LEFT = 0x00000002;
    public final static int SPEZ_META = 0x00000004;
    public final static int SPEZ_EVAL = 0x00000008;

    final static int SPEZ_FUNC = 0x00000100;
    final static int SPEZ_MINS = 0x00000200;

    final static int SPEZ_ICUT = 0x00001000;
    final static int SPEZ_ICAT = 0x00002000;

    private final static String noTermChs = "([{}])";
    private final static String noOperChs = ".,|";

    final static int MASK_ATOM_OPER = 0x00000001;

    public Engine engine;
    protected Writer wr;
    private int toff;
    private int lch = -1;
    public int flags = FLAG_DFLT;
    int lev = Operator.LEVEL_HIGH;
    private MapHashLink<Object, NamedDistance> printmap;
    int spez;
    int offset;
    int shift;
    private byte utildouble = ReadOpts.UTIL_CODES;
    private byte utilback = ReadOpts.UTIL_ERROR;
    private byte utilsingle = ReadOpts.UTIL_ATOM;
    private AbstractSource source;

    public PrologWriter() {
    }

    /**
     * <p>Set the engine.</p>
     *
     * @param en The engine.
     */
    public void setEngineRaw(Engine en) {
        engine = en;
    }

    /**
     * <p>Retrieve the engine.</p>
     *
     * @return The engine.
     */
    public Engine getEngine() {
        return engine;
    }

    /**
     * <p>Set the writer.</p>
     *
     * @param w The writer.
     */
    public void setWriter(Writer w) {
        wr = w;
    }

    /**
     * <p>Retrieve the buffer.</p>
     *
     * @return The buffer.
     */
    public Writer getWriter() {
        return wr;
    }

    /**
     * <p>Set the flags.</p>
     *
     * @param f The flags.
     */
    public void setFlags(int f) {
        flags = f;
    }

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public int getFlags() {
        return flags;
    }

    /**
     * <p>Set the operator level.</p>
     *
     * @param l The operator level.
     */
    void setLevel(int l) {
        lev = l;
    }

    /**
     * <p>Retrieve the operand spez.</p>
     *
     * @return The operand spez.
     */
    public int getSpez() {
        return spez;
    }

    /**
     * <p>Set the operand spez.</p>
     *
     * @param s The operand spez.
     */
    public void setSpez(int s) {
        spez = s;
    }

    /**
     * <p>Retrieve the meta offset.</p>
     *
     * @return The meta offset.
     */
    public int getOffset() {
        return offset;
    }

    /**
     * <p>Set the meta offset.</p>
     *
     * @param o The meta offset.
     */
    public void setOffset(int o) {
        offset = o;
    }

    /**
     * <p>Retrieve the meta shift.</p>
     *
     * @return The meta shift.
     */
    public int getShift() {
        return shift;
    }

    /**
     * <p>Set the meta shift.</p>
     *
     * @param s The meta shift.
     */
    public void setShift(int s) {
        shift = s;
    }

    /**
     * <p>Set the utilization of double quotes.</p>
     *
     * @param u The double quotes utilization.
     */
    public void setUtilDouble(int u) {
        utildouble = (byte) u;
    }

    /**
     * <p>Set the utilization of back quotes.</p>
     *
     * @param u The back quotes utilization.
     */
    public void setUtilBack(int u) {
        utilback = (byte) u;
    }

    /**
     * <p>Set the utilization of single quotes.</p>
     *
     * @param u The single quotes utilization.
     */
    public void setUtilSingle(int u) {
        utilsingle = (byte) u;
    }

    /**
     * <p>Set the var map.</p>
     *
     * @param v The var map.
     */
    public void setPrintMap(MapHashLink<Object, NamedDistance> v) {
        printmap = v;
    }

    /**
     * <p>Retrieve the source.</p>
     *
     * @return The source.
     */
    protected AbstractSource getSource() {
        return source;
    }

    /**
     * <p>Set the source.</p>
     *
     * @param s The source.
     */
    public void setSource(AbstractSource s) {
        source = s;
        utildouble = s.utildouble;
        utilback = s.utilback;
        utilsingle = s.utilsingle;
    }

    /***************************************************************/
    /* Meta-Predicate Declaration Access                           */
    /***************************************************************/

    /**
     * <p>Retrieve a predicate based on the current meta spezifyer.</p>
     *
     * @param t   The term skeleton.
     * @param mod The module context, or null.
     * @param nsa The call-site, not null.
     * @return The predicate or null.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    public CachePredicate offsetToPredicate(Object t,
                                            Object mod, SkelAtom nsa)
            throws EngineMessage, EngineException {
        if (engine == null)
            return null;
        int k = (offset < 0 ? -offset - 1 : offset);
        SkelAtom sa;
        if (t instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) t;
            sa = sc.sym;
            k += sc.args.length;
        } else if (t instanceof SkelAtom) {
            sa = (SkelAtom) t;
        } else {
            return null;
        }
        if ((flags & FLAG_IGNM) == 0 && mod != null) {
            if (nsa.fun.equals(SpecialQuali.OP_COLON)) {
                if (!(mod instanceof AbstractSkel) &&
                        !(mod instanceof Number)) {
                    mod = SpecialProxy.classOrProxyName(mod, engine);
                    if (mod == null)
                        return null;
                    sa = CacheFunctor.getFunctor(sa, (SkelAtom) mod, nsa, engine);
                } else if (mod instanceof SkelAtom) {
                    sa = CacheFunctor.getFunctor(sa, (SkelAtom) mod, nsa, engine);
                } else {
                    return null;
                }
            } else {
                if (!(mod instanceof AbstractSkel) &&
                        !(mod instanceof Number)) {
                    mod = SpecialProxy.refClassOrProxy(mod);
                    if (mod == null)
                        return null;
                    mod = SpecialProxy.classOrProxyName(mod, engine);
                    if (mod == null)
                        return null;
                    sa = CacheFunctor.getFunctor(sa, (SkelAtom) mod, nsa, engine);
                    k++;
                } else if (mod instanceof SkelAtom) {
                    sa = CacheFunctor.getFunctor(sa, (SkelAtom) mod, nsa, engine);
                    k++;
                } else {
                    return null;
                }
            }
        }
        return CachePredicate.getPredicate(sa, k, engine);
    }

    /**
     * <p>Retrieve a meta declaration based on current meta spezifyer.</p>
     *
     * @param cp The cache predicate.
     * @return The meta declaration or null.
     */
    public Object[] predicateToMeta(CachePredicate cp) {
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0)
            return null;
        Object t;
        if ((spez & SPEZ_META) != 0) {
            t = cp.pick.meta_predicate;
        } else {
            t = cp.pick.meta_function;
        }
        return (t != null ? ((SkelCompound) t).args : null);
    }

    /**
     * <p>Retrieve the argument meta spezification.</p>
     *
     * @param args     The argument meta spezifications.
     * @param k        The index.
     * @param backspez The old spez.
     * @param cp       The cache predicate.
     * @return The argument meta spezification.
     */
    public static Object getArg(Object[] args, int k, int backspez,
                                CachePredicate cp) {
        Object obj = (args != null ? args[k] : null);
        if (WriteOpts.spezToMeta(obj))
            return obj;
        if ((backspez & SPEZ_EVAL) != 0) {
            if (k != 0)
                return AbstractSkel.VOID_OBJ;
            if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0)
                return AbstractSkel.VOID_OBJ;
            if ((cp.pick.getBits() & Predicate.MASK_PRED_VIRT) == 0)
                return AbstractSkel.VOID_OBJ;
        }
        return obj;
    }

    /**
     * <p>Retrieve the spez flag.</p>
     *
     * @param obj The argument meta spezification.
     * @return The new spez flag.
     */
    public static int getSpez(Object obj) {
        int flags = 0;
        if (WriteOpts.spezToMeta(obj))
            flags |= SPEZ_META;
        if (WriteOpts.spezToEval(obj))
            flags |= SPEZ_EVAL;
        return flags;
    }

    /**
     * <p>Retrieve the meta offset.</p>
     *
     * @param obj        The argument meta spezification.
     * @param backoffset The old offset.
     * @return The meta offset.
     */
    public static int getOffset(Object obj, int backoffset) {
        int offset = WriteOpts.spezToOffset(obj);
        return (backoffset >= 0 ? offset : -offset - 1);
    }

    /**
     * <p>Retrieve the meta shift.</p>
     *
     * @param obj The argument meta spezification.
     * @return The meta shift.
     */
    public static int getShift(Object obj) {
        return WriteOpts.spezToShift(obj);
    }

    /**
     * <p>Check whether the callable has a receiver.</p>
     *
     * @param mod The module.
     * @param nsa The call-site.
     * @return True if the callable has a receiver, otherwise false.
     */
    public int modShift(Object mod, SkelAtom nsa) {
        if ((flags & FLAG_IGNM) == 0 && mod != null) {
            if (nsa.fun.equals(SpecialQuali.OP_COLONCOLON)) {
                if (!(mod instanceof AbstractSkel) &&
                        !(mod instanceof Number)) {
                    return 1;
                } else {
                    if (mod instanceof SkelAtom) {
                        return 1;
                    }
                }
            }
        }
        return 0;
    }

    /***********************************************************************/
    /* Text Offset Handling                                                */
    /***********************************************************************/

    /**
     * <p>Append a string and do text offset bookeeping.</p>
     *
     * @param s The string.
     * @throws IOException IO error.
     */
    public void append(String s, int start, int end)
            throws IOException {
        wr.write(s, start, end - start);
        int i = start;
        while (i < end) {
            int ch = s.codePointAt(i);
            if (ch == CodeType.LINE_EOL) {
                toff = 0;
            } else {
                toff++;
            }
            lch = ch;
            i += Character.charCount(ch);
        }
    }

    /**
     * <p>Append a string and do text offset bookeeping.</p>
     *
     * @param s The string.
     * @throws IOException IO error.
     */
    public void append(String s) throws IOException {
        append(s, 0, s.length());
    }

    /**
     * <p>Append a code point and do text offset bookeeping.</p>
     *
     * @param ch The code point.
     * @throws IOException IO error.
     */
    public void append(char ch) throws IOException {
        wr.write(ch);
        if (ch == CodeType.LINE_EOL) {
            toff = 0;
        } else {
            toff++;
        }
        lch = ch;
    }

    /**
     * <p>Place space if necessary.</p>
     *
     * @param t The token.
     * @throws IOException IO error.
     */
    protected final void safeSpace(String t) throws IOException {
        int ch = (t.length() == 0 ? -1 : t.codePointAt(0));
        if (!CodeType.ISO_CODETYPE.wordBreak1(lch, ch) &&
                !CodeType.ISO_CODETYPE.wordBreak2(lch, ch)) {
            append(' ');
        } else if (lch == ch &&
                CodeType.ISO_CODETYPE.getQuotes().indexOf(lch) != -1) {
            append(' ');
        }
    }

    /**
     * <p>Retrieve the text offset.</p>
     *
     * @return The text offset.
     */
    protected final int getTextOffset() {
        return toff;
    }

    /**
     * <p>Set the text offset.</p>
     *
     * @param k The text offset.
     */
    protected final void setTextOffset(int k) {
        toff = k;
    }

    /*********************************************************/
    /* Variable Quoting                                      */
    /*********************************************************/

    /**
     * <p>Compute a quoted variable.</p>
     *
     * @param var The variable.
     * @return The quoted variable.
     */
    public String variableQuoted(String var) {
        int quote;
        if (utilsingle == ReadOpts.UTIL_VARIABLE) {
            quote = CodeType.LINE_SINGLE;
        } else if (utildouble == ReadOpts.UTIL_VARIABLE) {
            quote = CodeType.LINE_DOUBLE;
        } else if (utilback == ReadOpts.UTIL_VARIABLE) {
            quote = CodeType.LINE_BACK;
        } else {
            return var;
        }
        if ((flags & FLAG_QUOT) != 0) {
            if (variableNeedsQuotes(var)) {
                StringBuilder buf = new StringBuilder();
                buf.appendCodePoint(quote);
                var = CompLang.ISO_COMPLANG.escapeControl(var,
                        CodeType.ISO_CODETYPE, quote);
                buf.append(var);
                buf.appendCodePoint(quote);
                return buf.toString();
            } else {
                return var;
            }
        } else {
            return var;
        }
    }

    /**
     * <p>Check whether a variable needs quotes.</p>
     *
     * @param var The variable name.
     * @return True if the variable needs quotes, false otherwise.
     */
    public boolean variableNeedsQuotes(String var) {
        if (var.length() == 0)
            return true;
        int ch = var.codePointAt(0);
        if (var.length() == 1 &&
                (noTermChs.indexOf(ch) != -1 || noOperChs.indexOf(ch) != -1))
            return true;
        if (!CodeType.ISO_CODETYPE.isUpper(ch) && !CodeType.ISO_CODETYPE.isUnderscore(ch))
            return true;
        if (Character.isDigit(ch))
            return true;
        if (!CompLang.ISO_COMPLANG.relevantToken(var))
            return true;
        if (!CodeType.ISO_CODETYPE.singleToken(var))
            return true;
        return false;
    }

    /*********************************************************/
    /* Atom Quoting                                          */
    /*********************************************************/

    /**
     * <p>Write an atom.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sa  The atom.
     * @param mod The module context, or null.
     * @param nsa The module context, or null.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected void writeAtom(SkelAtom sa,
                             Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        CachePredicate cp;
        if ((spez & SPEZ_META) != 0 && (flags & FLAG_NAVI) !=0) {
            cp = offsetToPredicate(sa, mod, nsa);
        } else {
            cp = null;
        }
        if (engine != null && (flags & FLAG_IGNO) == 0 &&
                (spez & SPEZ_OPLE) != 0) {
            Operator op = OperatorSearch.getOper(sa.scope, sa.fun,
                    Operator.TYPE_PREFIX, engine);
            if (op != null) {
                if ((spez & SPEZ_FUNC) != 0)
                    append(' ');
                append(PrologReader.OP_LPAREN);
                if ((spez & SPEZ_META) != 0 && (flags & FLAG_NAVI) !=0)
                    writeLinkBegin(cp);
                append(atomQuoted(sa.fun, 0));
                if ((spez & SPEZ_META) != 0 && (flags & FLAG_NAVI) !=0)
                    writeLinkEnd();
                append(PrologReader.OP_RPAREN);
                return;
            }
        }
        if (Foyer.OP_UNIT.equals(sa.fun)) {
            if ((spez & SPEZ_FUNC) != 0)
                append(' ');
            if ((spez & SPEZ_META) != 0 && (flags & FLAG_NAVI) !=0)
                writeLinkBegin(cp);
            append(sa.fun);
            if ((spez & SPEZ_META) != 0 && (flags & FLAG_NAVI) !=0)
                writeLinkEnd();
            return;
        }
        String t = atomQuoted(sa.fun, 0);
        safeSpace(t);
        if ((spez & SPEZ_META) != 0 && (flags & FLAG_NAVI) !=0)
            writeLinkBegin(cp);
        append(t);
        if ((spez & SPEZ_META) != 0 && (flags & FLAG_NAVI) !=0)
            writeLinkEnd();
    }

    /**
     * <p>Compute a quoted atom.</p>
     *
     * @param fun  The atom.
     * @param oper The atom flags.
     * @return The quoted atom.
     */
    final String atomQuoted(String fun, int oper) {
        int quote;
        if (utildouble == ReadOpts.UTIL_ATOM) {
            quote = CodeType.LINE_DOUBLE;
        } else if (utilsingle == ReadOpts.UTIL_ATOM) {
            quote = CodeType.LINE_SINGLE;
        } else if (utilback == ReadOpts.UTIL_ATOM) {
            quote = CodeType.LINE_BACK;
        } else {
            return fun;
        }
        if ((flags & FLAG_QUOT) != 0) {
            if (atomNeedsQuotes(fun, oper)) {
                StringBuilder buf = new StringBuilder();
                buf.appendCodePoint(quote);
                fun = CompLang.ISO_COMPLANG.escapeControl(fun,
                        CodeType.ISO_CODETYPE, quote);
                buf.append(fun);
                buf.appendCodePoint(quote);
                return buf.toString();
            } else {
                return fun;
            }
        } else {
            return fun;
        }
    }

    /**
     * <p>Check whether an atom needs quotes.</p>
     *
     * @param fun  The atom name.
     * @param oper The atom flags.
     * @return True if the atom needs quotes, false otherwise.
     */
    private boolean atomNeedsQuotes(String fun, int oper) {
        if (fun.length() == 0)
            return true;
        int ch = fun.codePointAt(0);
        if (fun.length() == 1 &&
                (noTermChs.indexOf(ch) != -1 ||
                        ((oper & MASK_ATOM_OPER) == 0 && noOperChs.indexOf(ch) != -1)))
            return true;
        if (CodeType.ISO_CODETYPE.isUpper(ch) || CodeType.ISO_CODETYPE.isUnderscore(ch))
            return true;
        if (Character.isDigit(ch))
            return true;
        if (!CompLang.ISO_COMPLANG.relevantToken(fun))
            return true;
        if (!CodeType.ISO_CODETYPE.singleToken(fun) &&
                !fun.equals(Foyer.OP_UNIT) &&
                !fun.equals(Foyer.OP_SET) &&
                !fun.equals(Foyer.OP_NIL))
            return true;
        return false;
    }

    /**
     * <p>Compute a quoted atom.</p>
     *
     * @param fun The atom.
     * @return The quoted atom or null.
     */
    final String stringQuoted(String fun) {
        int quote;
        if (utildouble == ReadOpts.UTIL_STRING) {
            quote = CodeType.LINE_DOUBLE;
        } else if (utilsingle == ReadOpts.UTIL_STRING) {
            quote = CodeType.LINE_SINGLE;
        } else if (utilback == ReadOpts.UTIL_STRING) {
            quote = CodeType.LINE_BACK;
        } else {
            return null;
        }
        StringBuilder buf = new StringBuilder();
        buf.appendCodePoint(quote);
        fun = JsonReader.JSON_COMPLANG.escapeControl(fun,
                CodeType.ISO_CODETYPE, quote);
        buf.append(fun);
        buf.appendCodePoint(quote);
        return buf.toString();
    }

    /********************************************************/
    /* Write AbstractTerm                                   */
    /********************************************************/

    /**
     * <p>Write atomic or var.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param term The atomic or var.
     * @param ref  The display.
     * @param mod  The module context, or null.
     * @param nsa  The module context, or null.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected void writeAtomicOrVar(Object term, Display ref,
                                    Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if (term instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) term;
            writeAtom(sa, mod, nsa);
        } else if (term instanceof SkelVar) {
            SkelVar sv = (SkelVar) term;
            if (printmap != null) {
                Object obj = TermAtomic.createMolec(sv, ref);
                NamedDistance nd = printmap.get(obj);
                if (nd != null) {
                    String t = variableQuoted(nd.getName());
                    safeSpace(t);
                    append(t);
                    return;
                }
            }
            int k = sv.getValue(ref, engine);
            String t = SkelVar.sernoToString(k, true);
            safeSpace(t);
            append(t);
        } else if (term instanceof Number) {
            if ((spez & SPEZ_MINS) != 0)
                append(' ');
            String t = ForeignAtom.sysNumberToAtom((Number) term, flags);
            safeSpace(t);
            append(t);
        } else {
            if ((spez & SPEZ_MINS) != 0)
                append(' ');
            String t = refToString(term);
            safeSpace(t);
            append(t);
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

    /********************************************************/
    /* Operator Handling                                    */
    /********************************************************/

    /**
     * <p>Determine whether the operator needs parenthesis.</p>
     *
     * @param op       The operator.
     * @param backspez The back spez.
     * @param level    The level.
     * @return True if the operator needs parenthesis.
     */
    private boolean needsParen(Operator op, int backspez, int level) {
        switch (op.getType()) {
            case Operator.TYPE_PREFIX:
                if ((level < op.getLevel()) ||
                        (level == op.getLevel() &&
                                (backspez & SPEZ_LEFT) != 0 &&
                                op.getRight() == 0))
                    return true;
                return false;
            case Operator.TYPE_INFIX:
                if ((level < op.getLevel()) ||
                        (level == op.getLevel() &&
                                (backspez & SPEZ_LEFT) != 0 &&
                                op.getRight() == 0))
                    return true;
                return false;
            case Operator.TYPE_POSTFIX:
                if (level < op.getLevel())
                    return true;
                return false;
            default:
                throw new IllegalArgumentException("illegal type");
        }
    }

    /**
     * <p>Write the operator.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param op         The operator.
     * @param sa         The atom.
     * @param cp         The predicate or null.
     * @param backspez   The spezification.
     * @throws IOException IO Error.
     */
    protected final void writePrefix(Operator op, SkelAtom sa,
                               CachePredicate cp,
                               int backspez)
            throws IOException {
        /**
         * - spacing.
         * - anti specification
         */
        String t = atomQuoted(op.getPortrayOrName(), 0);
        safeSpace(t);
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkBegin(cp);
        append(t);
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkEnd();
        if ((op.getBits() & Operator.MASK_OPER_NSPR) == 0 &&
                (backspez & SPEZ_META) != 0 &&
                (backspez & SPEZ_EVAL) == 0) {
            append(' ');
        } else {
            spez |= SPEZ_FUNC;
            if (sa.fun.equals(Foyer.OP_SUB))
                spez |= SPEZ_MINS;
        }
    }

    /**
     * <p>Write the operator.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param op         The operator.
     * @param sa         The atom.
     * @param cp         The predicate or null.
     * @param decl       The declaration or null.
     * @param backspez   The spezification.
     * @throws IOException IO Error.
     */
    protected void writeInfix(Operator op, SkelAtom sa,
                              CachePredicate cp,
                              Object[] decl, int indent,
                              int backspez)
            throws IOException {
        /**
         * - implication newln and indent.
         * - conjunction newln and indent.
         * - newln disjunction and indent
         * - spacing.
         */
        if (isFunr(decl) && (isLowr(decl) || isNewr(decl)) &&
                (backspez & SPEZ_META) != 0 &&
                (backspez & SPEZ_EVAL) == 0 &&
                (flags & FLAG_NEWL) != 0) {
            if ((backspez & SPEZ_ICUT) != 0) {
                if ((op.getBits() & Operator.MASK_OPER_NSPL) == 0)
                    append(' ');
                String t = atomQuoted(op.getPortrayOrName(), MASK_ATOM_OPER);
                safeSpace(t);
                if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                    writeLinkBegin(cp);
                append(t);
                if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                    writeLinkEnd();
                if ((op.getBits() & Operator.MASK_OPER_NSPR) == 0)
                    append(' ');
            } else if (op.getLevel() >= 1025 && !isLowr(decl)) {
                append(CodeType.LINE_EOL);
                for (int i = 0; i < indent - SPACES; i++)
                    append(' ');
                String t = atomQuoted(op.getPortrayOrName(), MASK_ATOM_OPER);
                safeSpace(t);
                if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                    writeLinkBegin(cp);
                append(t);
                if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                    writeLinkEnd();
                for (int i = t.length(); i < SPACES; i++)
                    append(' ');
            } else {
                if ((op.getBits() & Operator.MASK_OPER_NSPL) == 0)
                    append(' ');
                String t = atomQuoted(op.getPortrayOrName(), MASK_ATOM_OPER);
                safeSpace(t);
                if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                    writeLinkBegin(cp);
                append(t);
                if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                    writeLinkEnd();
                append(CodeType.LINE_EOL);
                for (int i = 0; i < indent; i++)
                    append(' ');
            }
            if ((backspez & SPEZ_ICAT) != 0)
                setTextOffset(indent);
        } else {
            if ((op.getBits() & Operator.MASK_OPER_NSPL) == 0 &&
                    (backspez & SPEZ_META) != 0 &&
                    (backspez & SPEZ_EVAL) == 0)
                append(' ');
            String t = atomQuoted(op.getPortrayOrName(), MASK_ATOM_OPER);
            safeSpace(t);
            if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                writeLinkBegin(cp);
            append(t);
            if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                writeLinkEnd();
            if ((op.getBits() & Operator.MASK_OPER_NSPR) == 0 &&
                    (backspez & SPEZ_META) != 0 &&
                    (backspez & SPEZ_EVAL) == 0)
                append(' ');
        }
    }

    /**
     * <p>Write the operator.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param op         The operator.
     * @param sc         The compound skeleton.
     * @param ref        The compoun display.
     * @param cp         The predicate or null.
     * @param decl       The declaration or null.
     * @param backshift  The shift.
     * @param backspez   The spezification.
     * @param backoffset The offset.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected final void writePostfix(Operator op, SkelCompound sc, Display ref,
                                CachePredicate cp, Object[] decl,
                                int backshift, int backspez, int backoffset,
                                Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        /**
         * - spacing
         */
        if ((op.getBits() & Operator.MASK_OPER_NSPL) == 0 &&
                (backspez & SPEZ_META) != 0 &&
                (backspez & SPEZ_EVAL) == 0)
            append(' ');
        if (isIndex(sc)) {
            writeIndex(sc, ref, cp, decl,
                    backshift, backspez, backoffset, mod, nsa);
        } else if (isStruct(sc)) {
            writeStruct(sc, ref, cp, decl,
                    backshift, backspez, backoffset, mod, nsa);
        } else {
            String t = atomQuoted(op.getPortrayOrName(), MASK_ATOM_OPER);
            safeSpace(t);
            if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                writeLinkBegin(cp);
            append(t);
            if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                writeLinkEnd();
        }
    }

    /**
     * <p>Check whether the compound is an index.</p>
     *
     * @param sc The compound.
     * @return True if the compound is an index, otherwise false.
     */
    static boolean isIndex(SkelCompound sc) {
        return sc.args.length > 1 &&
                sc.sym.fun.equals(Foyer.OP_INDEX);
    }

    /**
     * <p>Check whether the compound is a struct.</p>
     *
     * @param sc The compound.
     * @return True if the compound is a struct, otherwise false.
     */
    static boolean isStruct(SkelCompound sc) {
        return sc.args.length >= 1 &&
                sc.args.length <= 2 &&
                sc.sym.fun.equals(Foyer.OP_STRUCT);
    }

    /*********************************************************************/
    /* Special Compounds                                                 */
    /*********************************************************************/

    /**
     * <p>Write a set.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sc  The set skeleton.
     * @param ref The set display.
     * @param mod The module.
     * @param nsa The call-site.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected void writeSet(SkelCompound sc, Display ref,
                            Object mod, SkelAtom nsa)
            throws IOException, EngineException, EngineMessage {
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object[] decl = predicateToMeta(cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkBegin(cp);
        append(PrologReader.OP_LBRACE);
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkEnd();
        Object z = getArg(decl, backshift + modShift(mod, nsa), backspez, cp);
        spez = getSpez(z);
        offset = getOffset(z, backoffset);
        shift = getShift(z);
        write(sc.args[0], ref, Operator.LEVEL_HIGH, null, null);
        spez = backspez;
        offset = backoffset;
        shift = backshift;
        append(PrologReader.OP_RBRACE);
    }

    /**
     * <p>Write a list.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sc  The list skeleton.
     * @param ref The list display.
     * @param mod The module.
     * @param nsa The call-site.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected void writeList(SkelCompound sc, Display ref,
                             Object mod, SkelAtom nsa,
                             int indent, int backspez,
                             int backoffset, int backshift)
            throws IOException, EngineMessage, EngineException {
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object[] decl = predicateToMeta(cp);
        Object z = getArg(decl, backshift + modShift(mod, nsa), backspez, cp);
        spez = getSpez(z);
        offset = getOffset(z, backoffset);
        shift = getShift(z);
        write(sc.args[0], ref, Operator.LEVEL_MIDDLE, null, null);
        z = getArg(decl, backshift + 1 + modShift(mod, nsa), backspez, cp);
        spez = getSpez(z);
        offset = getOffset(z, backoffset);
        shift = getShift(z);
        for (; ; ) {
            Object term = sc.args[1];
            if (engine != null) {
                engine.skel = term;
                engine.display = ref;
                engine.deref();
                term = engine.skel;
                ref = engine.display;
            }
            if (term instanceof SkelCompound &&
                    ((SkelCompound) term).args.length == 2 &&
                    ((SkelCompound) term).sym.fun.equals(Foyer.OP_CONS)) {
                cp = offsetToPredicate(term, null, null);
                decl = predicateToMeta(cp);
                backspez = spez;
                backoffset = offset;
                backshift = shift;
                z = getArg(decl, backshift, backspez, cp);
                spez = getSpez(z);
                offset = getOffset(z, backoffset);
                shift = getShift(z);
                if ((backspez & SPEZ_META) != 0 &&
                        (backspez & SPEZ_EVAL) == 0 &&
                        (flags & FLAG_NEWL) != 0) {
                    if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                        writeLinkBegin(cp);
                    append(',');
                    if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                        writeLinkEnd();
                    append(CodeType.LINE_EOL);
                    for (int i = 0; i < indent; i++)
                        append(' ');
                } else {
                    if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                        writeLinkBegin(cp);
                    append(',');
                    if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                        writeLinkEnd();
                    if ((backspez & SPEZ_META) != 0 &&
                            (backspez & SPEZ_EVAL) == 0)
                        append(' ');
                }
                sc = (SkelCompound) term;
                write(sc.args[0], ref, Operator.LEVEL_MIDDLE, null, null);
                z = getArg(decl, backshift + 1, backspez, cp);
                spez = getSpez(z);
                offset = getOffset(z, backoffset);
                shift = getShift(z);
            } else if (!(term instanceof SkelAtom) ||
                    !((SkelAtom) term).fun.equals(Foyer.OP_NIL)) {
                if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                    writeLinkBegin(cp);
                append('|');
                if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                    writeLinkEnd();
                write(term, ref, Operator.LEVEL_MIDDLE, null, null);
                break;
            } else {
                break;
            }
        }
    }

    /**
     * <p>Write a compound.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sc  The compound skeleton.
     * @param ref The compound display.
     * @param mod The module.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected void writeCompound(SkelCompound sc, Display ref,
                                 Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object[] decl = predicateToMeta(cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        String t = atomQuoted(sc.sym.fun, 0);
        safeSpace(t);
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkBegin(cp);
        append(t);
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkEnd();
        append(PrologReader.OP_LPAREN);
        int j = 0;
        Object z = getArg(decl, backshift + j + modShift(mod, nsa), backspez, cp);
        spez = getSpez(z);
        offset = getOffset(z, backoffset);
        shift = getShift(z);
        write(sc.args[j], ref, Operator.LEVEL_MIDDLE, null, null);
        for (j = 1; j < sc.args.length; j++) {
            z = getArg(decl, backshift + j + modShift(mod, nsa), backspez, cp);
            spez = getSpez(z);
            offset = getOffset(z, backoffset);
            shift = getShift(z);
            append(',');
            if ((backspez & SPEZ_META) != 0 &&
                    (backspez & SPEZ_EVAL) == 0)
                append(' ');
            Object mod2;
            SkelAtom nsa2;
            if (j == 1 &&
                    sc.args.length == 2 &&
                    sc.sym.fun.equals(SpecialQuali.OP_COLON)) {
                mod2 = (engine != null ? SpecialQuali.slashToClass(sc.args[0],
                        ref, false, false, engine) : null);
                nsa2 = sc.sym;
            } else if (j == 1 &&
                    sc.args.length == 2 &&
                    sc.sym.fun.equals(SpecialQuali.OP_COLONCOLON)) {
                mod2 = (engine != null ? SpecialQuali.slashToClass(sc.args[0],
                        ref, true, false, engine) : null);
                nsa2 = sc.sym;
            } else {
                mod2 = null;
                nsa2 = null;
            }
            write(sc.args[j], ref, Operator.LEVEL_MIDDLE, mod2, nsa2);
        }
        spez = backspez;
        offset = backoffset;
        shift = backshift;
        append(PrologReader.OP_RPAREN);
    }

    /**
     * <p>Write an array index.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sc  The term.
     * @param ref The display.
     * @param mod The module.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected void writeIndex(SkelCompound sc, Display ref,
                              CachePredicate cp, Object[] decl,
                              int backshift, int backspez, int backoffset,
                              Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkBegin(cp);
        append(PrologReader.OP_LBRACKET);
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkEnd();
        int j = 1;
        Object z = getArg(decl, backshift + j + modShift(mod, nsa), backspez, cp);
        spez = getSpez(z);
        offset = getOffset(z, backoffset);
        shift = getShift(z);
        write(sc.args[j], ref, Operator.LEVEL_MIDDLE, null, null);
        for (j = 2; j < sc.args.length; j++) {
            z = getArg(decl, backshift + j + modShift(mod, nsa), backspez, cp);
            spez = getSpez(z);
            offset = getOffset(z, backoffset);
            shift = getShift(z);
            append(',');
            if ((backspez & SPEZ_META) != 0 &&
                    (backspez & SPEZ_EVAL) == 0)
                append(' ');
            Object mod2;
            SkelAtom nsa2;
            if (j == 1 &&
                    sc.args.length == 2 &&
                    sc.sym.fun.equals(SpecialQuali.OP_COLON)) {
                mod2 = (engine != null ? SpecialQuali.slashToClass(sc.args[0],
                        ref, false, false, engine) : null);
                nsa2 = sc.sym;
            } else if (j == 1 &&
                    sc.args.length == 2 &&
                    sc.sym.fun.equals(SpecialQuali.OP_COLONCOLON)) {
                mod2 = (engine != null ? SpecialQuali.slashToClass(sc.args[0],
                        ref, true, false, engine) : null);
                nsa2 = sc.sym;
            } else {
                mod2 = null;
                nsa2 = null;
            }
            write(sc.args[j], ref, Operator.LEVEL_MIDDLE, mod2, nsa2);
        }
        append(PrologReader.OP_RBRACKET);
    }

    /**
     * <p>Write a struct.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sc  The term.
     * @param ref The display.
     * @param mod The module.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected void writeStruct(SkelCompound sc, Display ref,
                               CachePredicate cp, Object[] decl,
                               int backshift, int backspez, int backoffset,
                               Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkBegin(cp);
        append(PrologReader.OP_LBRACE);
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkEnd();
        if (sc.args.length == 2) {
            Object z = getArg(decl, backshift + 1 + modShift(mod, nsa), backspez, cp);
            spez = getSpez(z);
            offset = getOffset(z, backoffset);
            shift = getShift(z);
            write(sc.args[1], ref, Operator.LEVEL_HIGH, null, null);
        }
        append(PrologReader.OP_RBRACE);
    }

    /**************************************************************/
    /* Main Write                                                 */
    /**************************************************************/

    /**
     * <p>Write a term.</p>
     *
     * @param term  The term skeleton.
     * @param ref   The term display.
     * @param level The level.
     * @param mod   The module context, or null.
     * @param nsa   The module context, or null.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    public final void write(Object term, Display ref, int level,
                            Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if (engine != null) {
            engine.skel = term;
            engine.display = ref;
            engine.deref();
            term = engine.skel;
            ref = engine.display;
        }
        if (!(term instanceof SkelCompound)) {
            writeAtomicOrVar(term, ref, mod, nsa);
            return;
        }
        SkelCompound sc = ((SkelCompound) term);
        if ((flags & FLAG_NUMV) != 0 && sc.args.length == 1 &&
                sc.sym.fun.equals(OP_DOLLAR_VAR)) {
            Object help = sc.args[0];
            if (engine != null) {
                engine.skel = help;
                engine.display = ref;
                engine.deref();
                help = engine.skel;
            }
            if (help instanceof Integer &&
                    ((Integer) help).intValue() >= 0) {
                String t = SkelVar.sernoToString(((Integer) help).intValue(), false);
                safeSpace(t);
                append(t);
                return;
            }
        }
        if (sc.args.length == 1 && sc.sym.fun.equals(OP_DOLLAR_STR)) {
            Object help = sc.args[0];
            if (engine != null) {
                engine.skel = help;
                engine.display = ref;
                engine.deref();
                help = engine.skel;
            }
            String t;
            if (help instanceof SkelAtom &&
                    (t = stringQuoted(((SkelAtom) help).fun)) != null) {
                safeSpace(t);
                append(t);
                return;
            }
        }
        if (engine != null && (flags & FLAG_IGNO) == 0) {
            if (sc.args.length == 1 && sc.sym.fun.equals(Foyer.OP_SET)) {
                writeSet(sc, ref, mod, nsa);
                return;
            }
            if (sc.args.length == 1 || isIndex(sc) || isStruct(sc)) {
                Operator op = OperatorSearch.getOper(sc.sym.scope, sc.sym.fun,
                        Operator.TYPE_PREFIX, engine);
                if (op != null) {
                    CachePredicate cp = offsetToPredicate(term, mod, nsa);
                    Object[] decl = predicateToMeta(cp);
                    int backspez = spez;
                    int backoffset = offset;
                    int backshift = shift;
                    if (needsParen(op, backspez, level)) {
                        if ((backspez & SPEZ_FUNC) != 0)
                            append(' ');
                        append(PrologReader.OP_LPAREN);
                        spez &= ~SPEZ_OPLE;
                    }
                    /* right operand */
                    Object z = getArg(decl, backshift + modShift(mod, nsa), backspez, cp);
                    spez = (spez & SPEZ_OPLE) + getSpez(z);
                    offset = getOffset(z, backoffset);
                    shift = getShift(z);
                    writePrefix(op, sc.sym, cp, backspez);
                    write(sc.args[0], ref, op.getLevel() - op.getRight(), null, null);
                    spez = backspez;
                    offset = backoffset;
                    shift = backshift;
                    if (needsParen(op, backspez, level))
                        append(PrologReader.OP_RPAREN);
                    return;
                }
                op = OperatorSearch.getOper(sc.sym.scope, sc.sym.fun,
                        Operator.TYPE_POSTFIX, engine);
                if (op != null) {
                    CachePredicate cp = offsetToPredicate(term, mod, nsa);
                    Object[] decl = predicateToMeta(cp);
                    int backspez = spez;
                    int backoffset = offset;
                    int backshift = shift;
                    if (needsParen(op, backspez, level)) {
                        if ((backspez & SPEZ_FUNC) != 0) {
                            append(' ');
                            spez &= ~(SPEZ_FUNC | SPEZ_MINS);
                        }
                        append(PrologReader.OP_LPAREN);
                    }
                    /* left operand */
                    Object z = getArg(decl, backshift + modShift(mod, nsa), backspez, cp);
                    spez = (spez & (SPEZ_FUNC | SPEZ_MINS)) +
                            (isOperEscape(op.getPortrayOrName()) ? 0 : SPEZ_OPLE) +
                            getSpez(z);
                    offset = getOffset(z, backoffset);
                    shift = getShift(z);
                    if (op.getLeft() == 0)
                        spez |= SPEZ_LEFT;
                    write(sc.args[0], ref, op.getLevel() - op.getLeft(), null, null);
                    writePostfix(op, sc, ref, cp, decl,
                            backshift, backspez, backoffset, mod, nsa);
                    spez = backspez;
                    offset = backoffset;
                    shift = backshift;
                    if (needsParen(op, backspez, level))
                        append(PrologReader.OP_RPAREN);
                    return;
                }
            }
            if (sc.args.length == 2 && sc.sym.fun.equals(Foyer.OP_CONS)) {
                int backspez = spez;
                int backoffset = offset;
                int backshift = shift;
                int indent = -1;
                append(PrologReader.OP_LBRACKET);
                if ((backspez & SPEZ_META) != 0 &&
                        (backspez & SPEZ_EVAL) == 0 &&
                        (flags & FLAG_NEWL) != 0) {
                    indent = getTextOffset();
                }
                writeList(sc, ref, mod, nsa, indent, backspez, backoffset, backshift);
                spez = backspez;
                offset = backoffset;
                shift = backshift;
                append(PrologReader.OP_RBRACKET);
                return;
            }
            if (sc.args.length == 2) {
                Operator op = OperatorSearch.getOper(sc.sym.scope,
                        sc.sym.fun, Operator.TYPE_INFIX, engine);
                if (op != null) {
                    CachePredicate cp = offsetToPredicate(term, mod, nsa);
                    Object[] decl = predicateToMeta(cp);
                    int backspez = spez;
                    int backoffset = offset;
                    int backshift = shift;
                    int indent = -1;
                    if (isFunr(decl) && (isLowr(decl) || isNewr(decl)) &&
                            (backspez & SPEZ_META) != 0 &&
                            (backspez & SPEZ_EVAL) == 0 &&
                            (flags & FLAG_NEWL) != 0) {
                        indent = getTextOffset();
                        if (isLowr(decl))
                            indent += SPACES;
                    }
                    if (needsParen(op, backspez, level)) {
                        if ((backspez & SPEZ_FUNC) != 0) {
                            append(' ');
                            spez &= ~(SPEZ_FUNC | SPEZ_MINS);
                        }
                        append(PrologReader.OP_LPAREN);
                        if (isFunr(decl) && (!isLowr(decl) && isNewr(decl)) &&
                                (backspez & SPEZ_META) != 0 &&
                                (backspez & SPEZ_EVAL) == 0 &&
                                (flags & FLAG_NEWL) != 0) {
                            indent += SPACES;
                            for (int i = getTextOffset(); i < indent; i++)
                                append(' ');
                        }
                    }
                    /* left operand */
                    Object z = getArg(decl, backshift + modShift(mod, nsa), backspez, cp);
                    spez = (spez & (SPEZ_FUNC | SPEZ_MINS)) +
                            (isOperEscape(op.getPortrayOrName()) ? 0 : SPEZ_OPLE) +
                            getSpez(z);
                    offset = getOffset(z, backoffset);
                    shift = getShift(z);
                    if (op.getLeft() == 0)
                        spez |= SPEZ_LEFT;
                    write(sc.args[0], ref, op.getLevel() - op.getLeft(), null, null);
                    spez = backspez;
                    if (needsParen(op, backspez, level))
                        spez &= ~SPEZ_OPLE;
                    /* right operand */
                    z = getArg(decl, backshift + 1 + modShift(mod, nsa), backspez, cp);
                    spez = (spez & SPEZ_OPLE) + getSpez(z);
                    offset = getOffset(z, backoffset);
                    shift = getShift(z);
                    Object mod2;
                    SkelAtom nsa2;
                    if ((sc.sym.fun.equals(SpecialQuali.OP_COLON))) {
                        mod2 = (engine != null ? SpecialQuali.slashToClass(sc.args[0],
                                ref, false, false, engine) : null);
                        nsa2 = sc.sym;
                    } else if ((sc.sym.fun.equals(SpecialQuali.OP_COLONCOLON))) {
                        mod2 = (engine != null ? SpecialQuali.slashToClass(sc.args[0],
                                ref, true, false, engine) : null);
                        nsa2 = sc.sym;
                    } else {
                        mod2 = null;
                        nsa2 = null;
                    }
                    Operator op2;
                    if (isSimple(sc.args[1], ref)) {
                        writeInfix(op, sc.sym, cp, decl, indent, backspez
                                | SPEZ_ICUT);
                    } else if ((op2 = isOperSimple(sc.args[1], ref, mod2, nsa2)) != null &&
                            !needsParen(op2, spez, op.getLevel() - op.getRight())) {
                        writeInfix(op, sc.sym, cp, decl, indent, backspez
                                | (SPEZ_ICUT | SPEZ_ICAT));
                    } else {
                        writeInfix(op, sc.sym, cp, decl, indent, backspez);
                    }
                    write(sc.args[1], ref, op.getLevel() - op.getRight(), mod2, nsa2);
                    spez = backspez;
                    offset = backoffset;
                    shift = backshift;
                    if (needsParen(op, backspez, level))
                        append(PrologReader.OP_RPAREN);
                    return;
                }
            }
        }
        writeCompound(sc, ref, mod, nsa);
    }

    /**
     * <p>Check whether the operator name is an operator escape.</p>
     *
     * @param s The operator name.
     * @return If the operator name is an operator escape.
     */
    private boolean isOperEscape(String s) {
        if (s.length() == 1) {
            if (PrologReader.noOperChs.indexOf(s.charAt(0)) != -1)
                return true;
        }
        return Foyer.OP_UNIT.equals(s);
    }

    /***********************************************************************/
    /* Classification for Opers and Compounds                              */
    /***********************************************************************/

    /**
     * <p>Check whether the argument is a simple.</p>
     *
     * @param term The argument skeleton.
     * @param ref  The argument display.
     * @return True if the argument is a simple, otherwise false.
     */
    private boolean isSimple(Object term, Display ref) {
        if (engine != null) {
            engine.skel = term;
            engine.display = ref;
            engine.deref();
            term = engine.skel;
        }
        if (!(term instanceof SkelCompound))
            return true;
        return false;
    }

    /**
     * <p>Check whether the argument is an oper and a simple.</p>
     * <p>Spez and offset must have been set appropriatly.</p>
     *
     * @param term The argument skeleton.
     * @param ref  The argument display.
     * @param mod  The module context, or null.
     * @return True if the argument is a an oper and a simple, otherwise false.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    private Operator isOperSimple(Object term, Display ref,
                                  Object mod, SkelAtom nsa)
            throws EngineMessage, EngineException {
        if (engine == null)
            return null;
        engine.skel = term;
        engine.display = ref;
        engine.deref();
        term = engine.skel;
        ref = engine.display;
        if (!(term instanceof SkelCompound))
            return null;
        SkelCompound sc = (SkelCompound) term;
        if (sc.args.length != 2)
            return null;
        if (!isSimple(sc.args[0], ref))
            return null;
        Operator op = OperatorSearch.getOper(sc.sym.scope, sc.sym.fun,
                Operator.TYPE_INFIX, engine);
        if (op == null)
            return null;
        Object[] decl = predicateToMeta(offsetToPredicate(term, mod, nsa));
        if (!isFunr(decl))
            return null;
        if (!isLowr(decl) && !isNewr(decl))
            return null;
        return op;
    }

    /**
     * <p>Check whether the meta predicate is newr.</p>
     *
     * @param decl The declaration.
     * @return True if the meta predicate is newr, otherwise false.
     */
    public static boolean isNewr(Object[] decl) {
        Object obj1 = decl[0];
        if (!(obj1 instanceof Integer))
            return false;
        return true;
    }

    /**
     * <p>Check whether the meta predicate is lowr.</p>
     *
     * @param decl The declaration.
     * @return True if the meta predicate is lowr, otherwise false.
     */
    public static boolean isLowr(Object[] decl) {
        Object obj1 = decl[0];
        if (!(obj1 instanceof Integer))
            return false;
        Object obj2 = decl[1];
        if (!(obj2 instanceof Integer))
            return false;
        if (obj1.equals(obj2))
            return false;
        return true;
    }

    /**
     * <p>Check whether the compound is a funr.</p>
     *
     * @param decl The declaration.
     * @return True if the compound is a funr.
     */
    public boolean isFunr(Object[] decl) {
        if (decl == null)
            return false;
        return true;
    }

    /************************************************************/
    /* Unparse Fillers                                          */
    /************************************************************/

    /**
     * <p>Unparse end of file.</p>
     * <p>Can be ovrerridden by sub classes.</p>
     *
     * @param sa The atom.
     * @throws IOException IO Error.
     */
    protected void unparseEndOfFile(SkelAtom sa) throws IOException {
        /* */
    }

    /**
     * <p>Unparse period.</p>
     *
     * @param sa The atom.
     * @param t  The argument.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected void unparsePeriod(SkelAtom sa, Object t, Display ref)
            throws IOException, EngineMessage, EngineException {
        write(t, ref, lev, null, null);
        safeSpace(".");
        append(".");
        append(CodeType.LINE_EOL);
    }

    /**
     * <p>Method to unparse a term into a writer.</p>
     *
     * @param t   The term.
     * @param ref The display of the term.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    public void unparseStatement(Object t, Display ref)
            throws EngineMessage, EngineException {
        try {
            toff = 0;
            lch = -1;
            if ((flags & PrologWriter.FLAG_MKDT) != 0) {
                if (engine != null) {
                    engine.skel = t;
                    engine.display = ref;
                    engine.deref();
                    t = engine.skel;
                    ref = engine.display;
                }
                if (t instanceof SkelAtom &&
                        ((SkelAtom) t).fun.equals(AbstractSource.OP_END_OF_FILE)) {
                    unparseEndOfFile((SkelAtom) t);
                    return;
                }
                if (t instanceof SkelCompound &&
                        ((SkelCompound) t).args.length == 1 &&
                        ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONS)) {
                    SkelCompound sc = (SkelCompound) t;
                    unparsePeriod(sc.sym, sc.args[0], ref);
                    return;
                }
                throw new IllegalArgumentException("illegal form");
            } else {
                write(t, ref, lev, null, null);
            }
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Check whether the filler has an eol.</p>
     *
     * @param filler The filler.
     * @return True if the filler has eol, otherwise false.
     */
    public static boolean hasEol(String[] filler) {
        if (filler == null)
            return false;
        int i = filler.length;
        while (i > 0 && filler[i - 1].indexOf(CodeType.LINE_EOL) == -1)
            i--;
        return (i != 0);
    }

    /***********************************************************************/
    /* Unparsing Convenience                                               */
    /***********************************************************************/

    /**
     * <p>Writer a term to a stream.</p>
     *
     * @param t     The term skeleton.
     * @param ref   The term display.
     * @param wr    The writer.
     * @param flags The flags.
     * @param en    The engine, can be null.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    public static void toString(Object t, Display ref, Writer wr, int flags,
                                Engine en)
            throws EngineMessage, EngineException {
        PrologWriter pw;
        if (en != null) {
            pw = en.store.foyer.createWriter(Foyer.IO_TERM);
            pw.setSource(en.visor.peekStack());
            pw.setEngineRaw(en);
        } else {
            pw = new PrologWriter();
        }
        pw.setFlags(flags);
        pw.setWriter(wr);
        pw.unparseStatement(t, ref);
    }

    /**
     * <p>Right or left align a string.</p>
     *
     * @param s The string.
     * @param l The width
     * @param r true=right align, false=left align
     * @return The aligned string.
     */
    public static String align(String s, int l, boolean r) {
        StringBuilder buf = new StringBuilder();
        if (r) {
            buf.append(s);
            for (int i = s.codePointCount(0, s.length()); i < l; i++)
                buf.appendCodePoint(' ');
        } else {
            for (int i = s.codePointCount(0, s.length()); i < l; i++)
                buf.appendCodePoint(' ');
            buf.append(s);
        }
        return buf.toString();
    }

    /************************************************************/
    /* Navigation Code                                          */
    /************************************************************/

    /**
     * <p>Write the navigation link begin comment.</p>
     *
     * @param cp The predicate.
     * @throws IOException IO Error.
     */
    protected void writeLinkBegin(CachePredicate cp)
            throws IOException {
        /* do nothing */
    }

    /**
     * <p>Write the navigation link end comment.</p>
     *
     * @throws IOException IO Error.
     */
    protected void writeLinkEnd()
            throws IOException {
        /* do nothing */
    }

}
