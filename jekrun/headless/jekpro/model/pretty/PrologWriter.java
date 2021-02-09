package jekpro.model.pretty;

import jekpro.frequent.basic.SpecialProxy;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Operator;
import jekpro.reference.runtime.EvaluableLogic;
import jekpro.reference.structure.ForeignAtom;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapHash;
import matula.util.regex.CodeType;
import matula.util.regex.CompLang;

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
 * <li><b>spez:</b> SPEZ_OPER, SPEZ_LEFT, SPEZ_TERM and SPEZ_GOAL control.</li>
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
    public final static int MARGIN = 56;

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

    public final static int SPEZ_LEFT = 0x00000001;
    public final static int SPEZ_META = 0x00000002;

    public final static int SPEZ_OPER = 0x00000010;
    public final static int SPEZ_FUNC = 0x00000020;
    public final static int SPEZ_MINS = 0x00000040;
    public final static int SPEZ_UNIT = 0x00000080;

    private final static String noTermChs = "([{}])";
    private final static String noOperChs = ".,|";

    final static int MASK_ATOM_OPER = 0x00000001;

    public Engine engine;
    protected Writer wr;
    private int toff;
    private int lch = -1;
    public int flags = FLAG_DFLT;
    int lev = Operator.LEVEL_HIGH;
    private MapHash<BindUniv, String> printmap;
    protected int spez;
    int offset;
    int shift;
    private byte utildouble = ReadOpts.UTIL_CODES;
    private byte utilback = ReadOpts.UTIL_ERROR;
    private byte utilsingle = ReadOpts.UTIL_ATOM;
    private AbstractSource source;
    protected int indent;

    /**
     * <p>Set the engine.</p>
     *
     * @param en The engine.
     */
    public void setEngine(Engine en) {
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
    public void setPrintMap(MapHash<BindUniv, String> v) {
        printmap = v;
    }

    /**
     * <p>Retrieve the indent.</p>
     *
     * @return The indent.
     */
    public int getIndent() {
        return indent;
    }

    /**
     * <p>Set the indent.</p>
     *
     * @param i The indent.
     */
    public void setIndent(int i) {
        indent = i;
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
    public void setSourceRaw(AbstractSource s) {
        source = s;
    }

    /***************************************************************/
    /* Meta-Predicate Declaration Access                           */
    /***************************************************************/

    /**
     * <p>Set the source.</p>
     *
     * @param s The source.
     */
    public void setDefaults(AbstractSource s) {
        source = s;
        utildouble = s.utildouble;
        utilback = s.utilback;
        utilsingle = s.utilsingle;
    }

    /**
     * <p>Retrieve a predicate based on the current meta spezifyer.</p>
     *
     * @param t   The term skeleton.
     * @param mod The module context, or null.
     * @param nsa The call-site, non null.
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
            if (nsa.fun.equals(EvaluableLogic.OP_COLON)) {
                if (!(mod instanceof AbstractSkel) &&
                        !(mod instanceof Number)) {
                    mod = SpecialProxy.classOrProxyName(mod, engine);
                    if (mod == null)
                        return null;
                    sa = CacheFunctor.getModFunc(sa, (SkelAtom) mod, nsa, engine);
                } else if (mod instanceof SkelAtom) {
                    sa = CacheFunctor.getModFunc(sa, (SkelAtom) mod, nsa, engine);
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
                    sa = CacheFunctor.getModFunc(sa, (SkelAtom) mod, nsa, engine);
                    k++;
                } else if (mod instanceof SkelAtom) {
                    sa = CacheFunctor.getModFunc(sa, (SkelAtom) mod, nsa, engine);
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
    public Object predicateToMeta(CachePredicate cp) {
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0)
            return null;
        return ((spez & SPEZ_META) != 0 ? cp.pick.meta_predicate : null);
    }

    /**
     * <p>Retrieve the argument meta spezification.</p>
     *
     * @param args The argument meta spezifications.
     * @param k    The index.
     * @return The argument meta spezification.
     */
    private static Object getArg(Object args, int k) {
        if (args == null)
            return null;
        while (k > 0) {
            if (args instanceof SkelCompound &&
                    ((SkelCompound) args).args.length == 2 &&
                    ((SkelCompound) args).sym.fun.equals(Foyer.OP_CONS)) {
                args = ((SkelCompound) args).args[1];
            } else {
                return null;
            }
            k--;
        }
        if (args instanceof SkelCompound &&
                ((SkelCompound) args).args.length == 2 &&
                ((SkelCompound) args).sym.fun.equals(Foyer.OP_CONS)) {
            return ((SkelCompound) args).args[0];
        } else {
            return null;
        }
    }

    /**
     * <p>Retrieve the meta offset.</p>
     *
     * @param obj        The argument meta spezification.
     * @param backoffset The old offset.
     * @return The meta offset.
     */
    private static int getOffset(Object obj, int backoffset) {
        int offset = WriteOpts.spezToOffset(obj);
        return (backoffset >= 0 ? offset : -offset - 1);
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
            if (nsa.fun.equals(EvaluableLogic.OP_COLONCOLON)) {
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
    public void append(String s)
            throws IOException {
        append(s, 0, s.length());
    }

    /**
     * <p>Append a code point and do text offset bookeeping.</p>
     *
     * @param ch The code point.
     * @throws IOException IO error.
     */
    public void append(char ch)
            throws IOException {
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
    final void safeSpace(String t)
            throws IOException {
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
    final int getTextOffset() {
        return toff;
    }

    /**
     * <p>Set the text offset.</p>
     *
     * @param k The text offset.
     */
    final void setTextOffset(int k) {
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
    public static boolean variableNeedsQuotes(String var) {
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
    final void writeAtom(SkelAtom sa,
                         Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        CachePredicate cp;
        if ((spez & SPEZ_META) != 0 &&
                (flags & FLAG_NAVI) != 0) {
            cp = offsetToPredicate(sa, mod, nsa);
        } else {
            cp = null;
        }
        if (engine != null && (flags & FLAG_IGNO) == 0 &&
                (spez & SPEZ_OPER) != 0) {
            Operator oper = OperatorSearch.getOperQuick(sa.scope, sa.fun,
                    Operator.TYPE_PREFIX, engine);
            if (oper != null && oper.getLevel() != 0) {
                if ((spez & SPEZ_FUNC) != 0)
                    append(' ');
                append(PrologReader.OP_LPAREN);
                appendLink(atomQuoted(sa.fun, 0), cp);
                append(PrologReader.OP_RPAREN);
                return;
            }
        }
        String t = atomQuoted(sa.fun, 0);
        if (Foyer.OP_UNIT.equals(sa.fun)) {
            if ((spez & SPEZ_FUNC) != 0)
                append(' ');
        } else {
            safeSpace(t);
        }
        appendLink(t, cp);
        if ((spez & SPEZ_UNIT) != 0)
            append(' ');
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
    private static boolean atomNeedsQuotes(String fun, int oper) {
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
        fun = CompLang.JSON_COMPLANG.escapeControl(fun,
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
                String t = printmap.get(ref.bind[sv.id]);
                if (t != null) {
                    t = variableQuoted(t);
                    safeSpace(t);
                    append(t);
                    return;
                }
            }
            int k;
            if (engine != null) {
                k = ref.bind[sv.id].getValue(engine);
            } else {
                k = sv.id;
            }
            String t = SkelVar.sernoToString(k, true);
            safeSpace(t);
            append(t);
        } else if (term instanceof Number) {
            String t = ForeignAtom.numToString((Number) term, flags);
            if ((spez & SPEZ_MINS) != 0) {
                append(' ');
            } else {
                safeSpace(t);
            }
            append(t);
        } else {
            String t = ForeignAtom.refToString(term);
            if ((spez & SPEZ_MINS) != 0) {
                append(' ');
            } else {
                safeSpace(t);
            }
            append(t);
        }
    }

    /*********************************************************************/
    /* Operator Handling I                                               */
    /*********************************************************************/

    /**
     * <p>Write an unary term in prefix.</p>
     *
     * @param sc    The unary skeleton.
     * @param ref   The unary display.
     * @param level The level.
     * @param mod   The module context, or null.
     * @param nsa   The module context, or null.
     * @param oper  The operator.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    private void writeIndex(SkelCompound sc, Display ref, int level,
                            Object mod, SkelAtom nsa, Operator oper)
            throws IOException, EngineException, EngineMessage {
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object decl = predicateToMeta(cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        if (needsParen(oper, level)) {
            spez &= ~SPEZ_UNIT;
            if ((backspez & SPEZ_FUNC) != 0) {
                append(' ');
                spez &= ~(SPEZ_FUNC | SPEZ_MINS);
            }
            append(PrologReader.OP_LPAREN);
            writeBreak(sc.sym, 0, false);
        }
        /* left operand */
        Object z = getArg(decl, backshift + modShift(mod, nsa));
        spez = (spez & (SPEZ_FUNC | SPEZ_MINS)) +
                (isOperEscape(oper.getPortrayOrName()) ? 0 : SPEZ_OPER) +
                (isOperUnit(oper.getPortrayOrName()) ? SPEZ_UNIT : 0) +
                WriteOpts.spezToMeta(z);
        offset = getOffset(z, backoffset);
        shift = WriteOpts.spezToShift(z);
        if (oper.getLeft() == 0)
            spez |= SPEZ_LEFT;
        write(sc.args[0], ref, oper.getLevel() - oper.getLeft(), null, null);
        spez = backspez;
        offset = backoffset;
        shift = backshift;
        if (Foyer.OP_UNIT.equals(oper.getPortrayOrName())) {
            appendLink(PrologReader.OP_LPAREN, cp);
            writeArgs(sc, ref, 1, decl, mod, nsa);
            append(PrologReader.OP_RPAREN);
        } else if (Foyer.OP_SET.equals(oper.getPortrayOrName())) {
            appendLink(PrologReader.OP_LBRACE, cp);
            writeSet(sc, ref, 1, decl, mod, nsa);
            append(PrologReader.OP_RBRACE);
        } else {
            appendLink(PrologReader.OP_LBRACKET, cp);
            writeArgs(sc, ref, 1, decl, mod, nsa);
            append(PrologReader.OP_RBRACKET);
        }
        if (needsParen(oper, level))
            append(PrologReader.OP_RPAREN);
    }

    /**
     * <p>Write an unary term in prefix.</p>
     *
     * @param sc    The unary skeleton.
     * @param ref   The unary display.
     * @param level The level.
     * @param mod   The module context, or null.
     * @param nsa   The module context, or null.
     * @param oper  The operator.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    private void writePrefix(SkelCompound sc, Display ref, int level,
                             Object mod, SkelAtom nsa, Operator oper)
            throws IOException, EngineException, EngineMessage {
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object decl = predicateToMeta(cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        if (needsParen(oper, level)) {
            spez &= ~(SPEZ_OPER | SPEZ_UNIT);
            if ((backspez & SPEZ_FUNC) != 0)
                append(' ');
            append(PrologReader.OP_LPAREN);
        }
        if ((oper.getBits() & Operator.MASK_OPER_TABR) != 0 &&
                (oper.getBits() & Operator.MASK_OPER_NEWR) != 0)
            indent += SPACES;
        String t = atomQuoted(oper.getPortrayOrName(), 0);
        if (Foyer.OP_UNIT.equals(oper.getPortrayOrName())) {
            if ((spez & SPEZ_FUNC) != 0)
                append(' ');
        } else {
            safeSpace(t);
        }
        appendLink(t, cp);
        writeBreak(sc.sym, 0, (oper.getBits() & Operator.MASK_OPER_NSPR) == 0);
        if (lch == ' ' || lch == CodeType.LINE_EOL) {
            spez &= ~SPEZ_FUNC;
            spez &= ~SPEZ_MINS;
        } else {
            spez |= SPEZ_FUNC;
            if (sc.sym.fun.equals(Foyer.OP_SUB)) {
                spez |= SPEZ_MINS;
            } else {
                spez &= ~SPEZ_MINS;
            }
        }
        /* right operand */
        Object z = getArg(decl, backshift + modShift(mod, nsa));
        spez = (spez & (SPEZ_OPER | SPEZ_FUNC | SPEZ_MINS | SPEZ_UNIT)) + WriteOpts.spezToMeta(z);
        offset = getOffset(z, backoffset);
        shift = WriteOpts.spezToShift(z);
        write(sc.args[0], ref, oper.getLevel() - oper.getRight(), null, null);
        if ((oper.getBits() & Operator.MASK_OPER_TABR) != 0 &&
                (oper.getBits() & Operator.MASK_OPER_NEWR) != 0)
            indent -= SPACES;
        spez = backspez;
        offset = backoffset;
        shift = backshift;
        if (needsParen(oper, level)) {
            writeBreak(sc.sym, 1, false);
            append(PrologReader.OP_RPAREN);
        }
    }

    /**
     * <p>Write an unary term in prefix.</p>
     *
     * @param sc    The unary skeleton.
     * @param ref   The unary display.
     * @param level The level.
     * @param mod   The module context, or null.
     * @param nsa   The module context, or null.
     * @param oper  The operator.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    private void writePostfix(SkelCompound sc, Display ref, int level,
                              Object mod, SkelAtom nsa, Operator oper)
            throws IOException, EngineException, EngineMessage {
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object decl = predicateToMeta(cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        if (needsParen(oper, level)) {
            spez &= ~SPEZ_UNIT;
            if ((backspez & SPEZ_FUNC) != 0) {
                append(' ');
                spez &= ~(SPEZ_FUNC | SPEZ_MINS);
            }
            append(PrologReader.OP_LPAREN);
            writeBreak(sc.sym, 0, false);
        }
        /* left operand */
        Object z = getArg(decl, backshift + modShift(mod, nsa));
        spez = (spez & (SPEZ_FUNC | SPEZ_MINS)) +
                (isOperEscape(oper.getPortrayOrName()) ? 0 : SPEZ_OPER) +
                (isOperUnit(oper.getPortrayOrName()) ? SPEZ_UNIT : 0) +
                WriteOpts.spezToMeta(z);
        offset = getOffset(z, backoffset);
        shift = WriteOpts.spezToShift(z);
        if (oper.getLeft() == 0)
            spez |= SPEZ_LEFT;
        write(sc.args[0], ref, oper.getLevel() - oper.getLeft(), null, null);
        spez = backspez;
        offset = backoffset;
        shift = backshift;
        writeBreak(sc.sym, 1, (oper.getBits() & Operator.MASK_OPER_NSPL) == 0);
        String t = atomQuoted(oper.getPortrayOrName(), MASK_ATOM_OPER);
        safeSpace(t);
        appendLink(t, cp);
        if (needsParen(oper, level))
            append(PrologReader.OP_RPAREN);
    }

    /**
     * <p>Write a binary term.</p>
     *
     * @param sc    The binary skeleton.
     * @param ref   The binary display.
     * @param level The level.
     * @param mod   The module context, or null.
     * @param nsa   The module context, or null.
     * @param oper  The operator.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    private void writeInfix(SkelCompound sc, Display ref, int level,
                            Object mod, SkelAtom nsa, Operator oper)
            throws EngineException, EngineMessage, IOException {
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object decl = predicateToMeta(cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        if (needsParen(oper, level)) {
            spez &= ~SPEZ_UNIT;
            if ((backspez & SPEZ_FUNC) != 0) {
                append(' ');
                spez &= ~(SPEZ_FUNC | SPEZ_MINS);
            }
            append(PrologReader.OP_LPAREN);
            if (needsSpaces(oper, sc.sym, 1)) {
                for (int i = 1; i < SPACES; i++)
                    append(' ');
                indent += SPACES;
            } else {
                writeBreak(sc.sym, 0, false);
            }
        }
        /* left operand */
        Object z = getArg(decl, backshift + modShift(mod, nsa));
        spez = (spez & (SPEZ_FUNC | SPEZ_MINS)) +
                (isOperEscape(oper.getPortrayOrName()) ? 0 : SPEZ_OPER) +
                (isOperUnit(oper.getPortrayOrName()) ? SPEZ_UNIT : 0) +
                WriteOpts.spezToMeta(z);
        offset = getOffset(z, backoffset);
        shift = WriteOpts.spezToShift(z);
        if (oper.getLeft() == 0)
            spez |= SPEZ_LEFT;
        write(sc.args[0], ref, oper.getLevel() - oper.getLeft(), null, null);
        spez = backspez;
        if (needsParen(oper, level))
            spez &= ~(SPEZ_OPER | SPEZ_UNIT);
        if ((oper.getBits() & Operator.MASK_OPER_TABR) != 0 &&
                (oper.getBits() & Operator.MASK_OPER_NEWR) != 0) {
            indent += SPACES;
        } else if ((oper.getBits() & Operator.MASK_OPER_TABR) != 0 &&
                (oper.getBits() & Operator.MASK_OPER_NEWR) == 0) {
            indent -= SPACES;
        }
        /* comma etc.. */
        if ((oper.getBits() & Operator.MASK_OPER_NEWR) != 0) {
            if ((oper.getBits() & Operator.MASK_OPER_NSPL) == 0)
                append(' ');
            String t = atomQuoted(oper.getPortrayOrName(), MASK_ATOM_OPER);
            safeSpace(t);
            appendLink(t, cp);
            writeBreakForce(sc.sym, 1, (oper.getBits() & Operator.MASK_OPER_NSPR) == 0);
            /* semicolon etc.. */
        } else if ((oper.getBits() & Operator.MASK_OPER_TABR) != 0) {
            writeBreakForce(sc.sym, 1, (oper.getBits() & Operator.MASK_OPER_NSPL) == 0);
            String t = atomQuoted(oper.getPortrayOrName(), MASK_ATOM_OPER);
            safeSpace(t);
            appendLink(t, cp);
            writeAdjust(sc.sym, 1, t, (oper.getBits() & Operator.MASK_OPER_NSPR) == 0);
        } else {
            if ((oper.getBits() & Operator.MASK_OPER_NSPL) == 0)
                append(' ');
            String t = atomQuoted(oper.getPortrayOrName(), MASK_ATOM_OPER);
            safeSpace(t);
            appendLink(t, cp);
            writeBreak(sc.sym, 1, (oper.getBits() & Operator.MASK_OPER_NSPR) == 0);
        }
        if ((oper.getBits() & Operator.MASK_OPER_TABR) != 0 &&
                (oper.getBits() & Operator.MASK_OPER_NEWR) == 0)
            indent += SPACES;
        z = getArg(decl, backshift + 1 + modShift(mod, nsa));
        spez = (spez & (SPEZ_OPER | SPEZ_UNIT)) + WriteOpts.spezToMeta(z);
        offset = getOffset(z, backoffset);
        shift = WriteOpts.spezToShift(z);
        Object mod2 = decodeQualification(sc, ref);
        SkelAtom nsa2 = (mod2 != null ? sc.sym : null);
        write(sc.args[1], ref, oper.getLevel() - oper.getRight(), mod2, nsa2);
        if ((oper.getBits() & Operator.MASK_OPER_TABR) != 0 &&
                (oper.getBits() & Operator.MASK_OPER_NEWR) != 0)
            indent -= SPACES;
        spez = backspez;
        offset = backoffset;
        shift = backshift;
        if (needsParen(oper, level)) {
            if (needsSpaces(oper, sc.sym, 1)) {
                indent -= SPACES;
            } else {
                writeBreak(sc.sym, 2, false);
            }
            append(PrologReader.OP_RPAREN);
        }
    }

    /*********************************************************************/
    /* Operator Handling II                                              */
    /*********************************************************************/

    /**
     * <p>Check whether the compound is unary.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sc The compound.
     * @return True if unary, otherwise false.
     */
    protected boolean isUnary(SkelCompound sc) {
        return sc.args.length == 1;
    }

    /**
     * <p>Check whether the compound is binary.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sc The compound.
     * @return True if binary, otherwise false.
     */
    protected boolean isBinary(SkelCompound sc) {
        return sc.args.length == 2;
    }

    /**
     * <p>Check whether the compound is an index.</p>
     *
     * @param sc The compound.
     * @return True if the compound is an index, otherwise false.
     */
    protected boolean isIndex(SkelCompound sc) {
        return sc.args.length >= 2;
    }

    /*********************************************************************/
    /* Parenthesis Handling                                              */
    /*********************************************************************/

    /**
     * <p>Write a break if necessary.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sa    The call site.
     * @param j     The argument index.
     * @param space The space flag.
     * @throws IOException IO Error.
     */
    protected void writeBreak(SkelAtom sa, int j, boolean space)
            throws IOException {
        if (MARGIN < getTextOffset() &&
                (flags & FLAG_NEWL) != 0) {
            append(CodeType.LINE_EOL);
            for (int i = 0; i < indent; i++)
                append(' ');
        } else {
            if (space)
                append(' ');
        }
    }

    /**
     * <p>Write always a break.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sa    The call site.
     * @param j     The argument index.
     * @param space The space flag.
     * @throws IOException IO Error.
     */
    protected void writeBreakForce(SkelAtom sa, int j, boolean space)
            throws IOException {
        if ((flags & FLAG_NEWL) != 0) {
            append(CodeType.LINE_EOL);
            for (int i = 0; i < indent; i++)
                append(' ');
        } else {
            if (space)
                append(' ');
        }
    }

    /**
     * <p>Write operator adjust.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sa    The call site.
     * @param j     The argument index.
     * @param t     The operator.
     * @param space The space flag.
     * @throws IOException IO Error.
     */
    protected void writeAdjust(SkelAtom sa, int j,
                               String t, boolean space)
            throws IOException {
        if ((flags & FLAG_NEWL) != 0) {
            for (int i = t.length(); i < SPACES; i++)
                append(' ');
        } else {
            if (space)
                append(' ');
        }

    }

    /**
     * <p>Determine whether the operator needs parenthesis.</p>
     *
     * @param op    The operator.
     * @param level The level.
     * @return True if the operator needs parenthesis.
     */
    private boolean needsParen(Operator op, int level) {
        switch (op.getType()) {
            case Operator.TYPE_PREFIX:
                if ((level < op.getLevel()) ||
                        (level == op.getLevel() &&
                                (spez & SPEZ_LEFT) != 0 &&
                                op.getRight() == 0))
                    return true;
                return false;
            case Operator.TYPE_INFIX:
                if ((level < op.getLevel()) ||
                        (level == op.getLevel() &&
                                (spez & SPEZ_LEFT) != 0 &&
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
     * <p>Determine whether the infinx operator needs spaces.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param oper The infix operator.
     * @param sa   The call-site.
     * @param j    The argument index.
     * @return True if the infix operators needs spaces.
     */
    protected boolean needsSpaces(Operator oper, SkelAtom sa, int j) {
        /* comma etc.. */
        if ((oper.getBits() & Operator.MASK_OPER_NEWR) != 0) {
            return false;
            /* semicolon etc.. */
        } else if ((oper.getBits() & Operator.MASK_OPER_TABR) != 0) {
            if ((flags & FLAG_NEWL) != 0) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    /*********************************************************************/
    /* Special Compounds                                                 */
    /*********************************************************************/

    /**
     * <p>Write a set.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sc   The set skeleton.
     * @param ref  The set display.
     * @param j    The start index.
     * @param decl The predicate declaration.
     * @param mod  The module.
     * @param nsa  The call-site.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    private void writeSet(SkelCompound sc, Display ref,
                          int j, Object decl,
                          Object mod, SkelAtom nsa)
            throws IOException, EngineException, EngineMessage {
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        writeBreak(sc.sym, j, false);
        Object z = getArg(decl, backshift + j + modShift(mod, nsa));
        spez = WriteOpts.spezToMeta(z);
        offset = getOffset(z, backoffset);
        shift = WriteOpts.spezToShift(z);
        write(sc.args[j], ref, Operator.LEVEL_HIGH, null, null);
        spez = backspez;
        offset = backoffset;
        shift = backshift;
        writeBreak(sc.sym, sc.args.length, false);
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
    private void writeList(SkelCompound sc, Display ref,
                           Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object decl = predicateToMeta(cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        Object z = getArg(decl, backshift + modShift(mod, nsa));
        spez = WriteOpts.spezToMeta(z);
        offset = getOffset(z, backoffset);
        shift = WriteOpts.spezToShift(z);
        write(sc.args[0], ref, Operator.LEVEL_MIDDLE, null, null);
        z = getArg(decl, backshift + 1 + modShift(mod, nsa));
        spez = WriteOpts.spezToMeta(z);
        offset = getOffset(z, backoffset);
        shift = WriteOpts.spezToShift(z);
        Object term = sc.args[1];
        for (; ; ) {
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
                appendLink(",", cp);
                writeBreak(sc.sym, 1, true);
                backspez = spez;
                backoffset = offset;
                backshift = shift;
                z = getArg(decl, backshift);
                spez = WriteOpts.spezToMeta(z);
                offset = getOffset(z, backoffset);
                shift = WriteOpts.spezToShift(z);
                sc = (SkelCompound) term;
                write(sc.args[0], ref, Operator.LEVEL_MIDDLE, null, null);
                z = getArg(decl, backshift + 1);
                spez = WriteOpts.spezToMeta(z);
                offset = getOffset(z, backoffset);
                shift = WriteOpts.spezToShift(z);
                term = sc.args[1];
            } else if (!(term instanceof SkelAtom) ||
                    !((SkelAtom) term).fun.equals(Foyer.OP_NIL)) {
                cp = offsetToPredicate(term, null, null);
                appendLink("|", cp);
                writeBreak(sc.sym, 1, false);
                write(term, ref, Operator.LEVEL_MIDDLE, null, null);
                break;
            } else {
                break;
            }
        }
        spez = backspez;
        offset = backoffset;
        shift = backshift;
    }

    /**
     * <p>Write a compound.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sc   The compound skeleton.
     * @param ref  The compound display.
     * @param j    The start index.
     * @param decl The predicate declaration.
     * @param mod  The module.
     * @param nsa  The call-site.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    private void writeArgs(SkelCompound sc, Display ref,
                           int j, Object decl,
                           Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if (sc.args.length == j)
            return;
        writeBreak(sc.sym, j, false);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        Object z = getArg(decl, backshift + j + modShift(mod, nsa));
        spez = WriteOpts.spezToMeta(z);
        offset = getOffset(z, backoffset);
        shift = WriteOpts.spezToShift(z);
        write(sc.args[j], ref, Operator.LEVEL_MIDDLE, null, null);
        j++;
        for (; j < sc.args.length; j++) {
            z = getArg(decl, backshift + j + modShift(mod, nsa));
            spez = WriteOpts.spezToMeta(z);
            offset = getOffset(z, backoffset);
            shift = WriteOpts.spezToShift(z);
            append(',');
            writeBreak(sc.sym, j, true);
            Object mod2 = (j == 1 ? decodeQualification(sc, ref) : null);
            SkelAtom nsa2 = (mod2 != null ? sc.sym : null);
            write(sc.args[j], ref, Operator.LEVEL_MIDDLE, mod2, nsa2);
        }
        spez = backspez;
        offset = backoffset;
        shift = backshift;
        writeBreak(sc.sym, sc.args.length, false);
    }

    /**
     * <p>Decode a qualification.</p>
     *
     * @param sc  The compound skeleton.
     * @param ref The compound display.
     * @return The qualification.
     * @throws EngineMessage Shit happens.
     */
    Object decodeQualification(SkelCompound sc, Display ref)
            throws EngineMessage {
        if (sc.args.length == 2 &&
                sc.sym.fun.equals(EvaluableLogic.OP_COLON)) {
            return (engine != null ? EvaluableLogic.slashToClass(sc.args[0],
                    ref, CacheModule.MASK_MODULE_NERR, engine) : null);
        } else if (sc.args.length == 2 &&
                sc.sym.fun.equals(EvaluableLogic.OP_COLONCOLON)) {
            return (engine != null ? EvaluableLogic.slashToClass(sc.args[0],
                    ref, CacheModule.MASK_MODULE_CMPD |
                            CacheModule.MASK_MODULE_NERR, engine) : null);
        } else {
            return null;
        }
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
    final void write(Object term, Display ref, int level,
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
        SkelCompound sc = (SkelCompound) term;
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
        if (engine != null && (flags & PrologWriter.FLAG_IGNO) == 0) {
            if (sc.args.length == 1 && sc.sym.fun.equals(Foyer.OP_SET)) {
                CachePredicate cp = offsetToPredicate(sc, mod, nsa);
                Object decl = predicateToMeta(cp);
                appendLink(PrologReader.OP_LBRACE, cp);
                writeSet(sc, ref, 0, decl, mod, nsa);
                append(PrologReader.OP_RBRACE);
                return;
            }
            if (sc.args.length == 2 && sc.sym.fun.equals(Foyer.OP_CONS)) {
                if (level > Operator.LEVEL_MIDDLE)
                    indent += SPACES;
                append(PrologReader.OP_LBRACKET);
                writeBreak(sc.sym, 0, false);
                writeList(sc, ref, mod, nsa);
                writeBreak(sc.sym, 2, false);
                append(PrologReader.OP_RBRACKET);
                if (level > Operator.LEVEL_MIDDLE)
                    indent -= SPACES;
                return;
            }
            if (isIndex(sc)) {
                Operator oper = OperatorSearch.getOperQuick(sc.sym.scope, sc.sym.fun,
                        Operator.TYPE_POSTFIX, engine);
                if (oper != null && oper.getLevel() != 0 &&
                        (Foyer.OP_UNIT.equals(oper.getPortrayOrName()) ||
                                (Foyer.OP_SET.equals(oper.getPortrayOrName()) && sc.args.length == 2) ||
                                Foyer.OP_NIL.equals(oper.getPortrayOrName()))) {
                    if (oper.getLevel() <= Operator.LEVEL_MIDDLE &&
                            level > Operator.LEVEL_MIDDLE)
                        indent += SPACES;
                    writeIndex(sc, ref, level, mod, nsa, oper);
                    if (oper.getLevel() <= Operator.LEVEL_MIDDLE &&
                            level > Operator.LEVEL_MIDDLE)
                        indent -= SPACES;
                    return;
                }
            }
            if (isUnary(sc)) {
                Operator oper = OperatorSearch.getOperQuick(sc.sym.scope, sc.sym.fun,
                        Operator.TYPE_PREFIX, engine);
                if (oper != null && oper.getLevel() != 0) {
                    if (oper.getLevel() <= Operator.LEVEL_MIDDLE &&
                            level > Operator.LEVEL_MIDDLE)
                        indent += SPACES;
                    writePrefix(sc, ref, level, mod, nsa, oper);
                    if (oper.getLevel() <= Operator.LEVEL_MIDDLE &&
                            level > Operator.LEVEL_MIDDLE)
                        indent -= SPACES;
                    return;
                }
                oper = OperatorSearch.getOperQuick(sc.sym.scope, sc.sym.fun,
                        Operator.TYPE_POSTFIX, engine);
                if (oper != null && oper.getLevel() != 0) {
                    if (oper.getLevel() <= Operator.LEVEL_MIDDLE &&
                            level > Operator.LEVEL_MIDDLE)
                        indent += SPACES;
                    writePostfix(sc, ref, level, mod, nsa, oper);
                    if (oper.getLevel() <= Operator.LEVEL_MIDDLE &&
                            level > Operator.LEVEL_MIDDLE)
                        indent -= SPACES;
                    return;
                }
            }
            if (isBinary(sc)) {
                Operator oper = OperatorSearch.getOperQuick(sc.sym.scope,
                        sc.sym.fun, Operator.TYPE_INFIX, engine);
                if (oper != null && oper.getLevel() != 0) {
                    if (oper.getLevel() <= Operator.LEVEL_MIDDLE &&
                            level > Operator.LEVEL_MIDDLE)
                        indent += SPACES;
                    writeInfix(sc, ref, level, mod, nsa, oper);
                    if (oper.getLevel() <= Operator.LEVEL_MIDDLE &&
                            level > Operator.LEVEL_MIDDLE)
                        indent -= SPACES;
                    return;
                }
            }
        }
        if (level > Operator.LEVEL_MIDDLE)
            indent += SPACES;
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object decl = predicateToMeta(cp);
        String t = atomQuoted(sc.sym.fun, 0);
        safeSpace(t);
        appendLink(t, cp);
        append(PrologReader.OP_LPAREN);
        writeArgs(sc, ref, 0, decl, mod, nsa);
        append(PrologReader.OP_RPAREN);
        if (level > Operator.LEVEL_MIDDLE)
            indent -= SPACES;
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
        return false;
    }

    /**
     * <p>Check whether the operator name is an operator unit.</p>
     *
     * @param s The operator name.
     * @return If the operator name is an operator unit.
     */
    private boolean isOperUnit(String s) {
        return Foyer.OP_UNIT.equals(s);
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
    public static void toString(Object t, Display ref, Writer wr,
                                int flags, Engine en)
            throws EngineMessage, EngineException {
        PrologWriter pw;
        if (en != null) {
            pw = en.store.foyer.createWriter(Foyer.IO_TERM);
            pw.setDefaults(en.visor.peekStack());
            pw.setEngine(en);
        } else {
            pw = new PrologWriter();
        }
        pw.setFlags(flags);
        pw.setWriter(wr);
        pw.unparseStatement(t, ref);
    }

    /************************************************************/
    /* Navigation Code                                          */
    /************************************************************/

    /**
     * <p>Linkify a string and append it</p>
     *
     * @param t  The string.
     * @param cp The predicate.
     * @throws IOException     IO error.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    protected void appendLink(String t, CachePredicate cp)
            throws IOException, EngineMessage, EngineException {
        append(t);
    }

    /**
     * <p>Some testing.</p>
     * @param args Ignored.
     */
    /*
    public static void main(String[] args) {
        String var = "Salary#";
        System.out.println("var=" + var);
        System.out.println("variableNeedsQuotes(var)=" + variableNeedsQuotes(var));
        var = "🍊";
        System.out.println("var=" + var);
        System.out.println("variableNeedsQuotes(var)=" + variableNeedsQuotes(var));
    }
    */

}
