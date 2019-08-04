package jekpro.model.pretty;

import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Operator;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.regex.CodeType;
import matula.util.regex.CompLang;

import java.io.IOException;

/**
 * <p>This class provides the writing of annotated prolog terms.</p>
 * <p>The following parameters are recognized:</p>
 * <ul>
 * <li><b>flags:</b> FLAG_FILL and FLAG_NAVI.</li>
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
public class PrologWriterAnno extends PrologWriter {

    /************************************************************/
    /* Write Atom Hint                                          */
    /************************************************************/

    /**
     * <p>Write atomic or var.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param term The atomic or var.
     * @param ref  The display.
     * @param mod  The module context, or null.
     * @throws IOException     IO Error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected final void writeAtomicOrVar(Object term, Display ref,
                                          Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0 || !(term instanceof SkelAtom)) {
            super.writeAtomicOrVar(term, ref, mod, nsa);
            return;
        }
        SkelAtom sa = (SkelAtom) term;
        int quote = (sa instanceof SkelAtomAnno ? ((SkelAtomAnno) sa).getHint() : 0);
        switch (quote) {
            case CodeType.LINE_SINGLE:
            case CodeType.LINE_DOUBLE:
            case CodeType.LINE_BACK:
                StringBuilder buf = new StringBuilder();
                buf.appendCodePoint(quote);
                String t = CompLang.ISO_COMPLANG.escapeControl(sa.fun,
                        CodeType.ISO_CODETYPE, quote);
                buf.append(t);
                buf.appendCodePoint(quote);
                t = buf.toString();
                safeSpace(t);
                append(t);
                break;
            case CodeType.LINE_ZERO:
                if ((spez & SPEZ_MINS) != 0) {
                    append(' ');
                } else {
                    safeSpace(sa.fun);
                }
                append(sa.fun);
                break;
            default:
                writeAtom(sa, mod, nsa);
                break;
        }
    }

    /********************************************************/
    /* Operator Handling II                                 */
    /********************************************************/

    /**
     * <p>Write the operator.</p>
     *
     * @param oper The operator.
     * @param sa The atom.
     * @param cp The predicate or null.
     * @throws IOException     IO Error.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    protected final void writePrefix(Operator oper, SkelAtom sa,
                               CachePredicate cp)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0) {
            super.writePrefix(oper, sa, cp);
            return;
        }
        String t = atomQuoted(oper.getPortrayOrName(), 0);
        safeSpace(t);
        appendLink(t, cp);
        String[][] fillers = (sa instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sa).getFillers() : null);
        writeFiller(indent, MARGIN, fillers != null ? fillers[0] : null);
        if (!lastEol(fillers != null ? fillers[0] : null)) {
            if (MARGIN < getTextOffset() &&
                    (flags & FLAG_NEWL) != 0) {
                append(CodeType.LINE_EOL);
                for (int i = 0; i < indent; i++)
                    append(' ');
                spez &= ~SPEZ_FUNC;
                spez &= ~SPEZ_MINS;
            } else {
                if ((oper.getBits() & Operator.MASK_OPER_NSPR) == 0) {
                    append(' ');
                    spez &= ~SPEZ_FUNC;
                    spez &= ~SPEZ_MINS;
                } else {
                    spez |= SPEZ_FUNC;
                    if (sa.fun.equals(Foyer.OP_SUB)) {
                        spez |= SPEZ_MINS;
                    } else {
                        spez &= ~SPEZ_MINS;
                    }
                }
            }
        } else {
            spez &= ~SPEZ_FUNC;
            spez &= ~SPEZ_MINS;
        }
    }

    /**
     * <p>Write the operator.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param oper The operator.
     * @param sa   The call-site.
     * @param cp   The cache predicate or null.
     * @throws IOException     IO Error.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    protected final void writeInfix(Operator oper, SkelAtom sa,
                                    CachePredicate cp)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0) {
            super.writeInfix(oper, sa, cp);
            return;
        }
        if ((oper.getBits() & Operator.MASK_OPER_NSPL) == 0)
            append(' ');
        String t = atomQuoted(oper.getPortrayOrName(), MASK_ATOM_OPER);
        safeSpace(t);
        appendLink(t, cp);
        String[][] fillers = (sa instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sa).getFillers() : null);
        if ((oper.getBits() & Operator.MASK_OPER_NEWR) != 0) {
            writeFiller(indent, MARGIN, fillers != null ? fillers[1] : null);
            if (!lastEol(fillers != null ? fillers[1] : null)) {
                if ((flags & FLAG_NEWL) != 0) {
                    append(CodeType.LINE_EOL);
                    for (int i = 0; i < indent; i++)
                        append(' ');
                } else {
                    if ((oper.getBits() & Operator.MASK_OPER_NSPR) == 0)
                        append(' ');
                }
            }
        } else {
            writeFiller(indent, MARGIN, fillers != null ? fillers[1] : null);
            if (!lastEol(fillers != null ? fillers[1] : null)) {
                if (MARGIN < getTextOffset() &&
                        (flags & FLAG_NEWL) != 0) {
                    append(CodeType.LINE_EOL);
                    for (int i = 0; i < indent; i++)
                        append(' ');
                } else {
                    if ((oper.getBits() & Operator.MASK_OPER_NSPR) == 0)
                        append(' ');
                }
            }
        }
    }

    /**
     * <p>Check whether the compound is unary.</p>
     *
     * @param sc The compound.
     * @return True if unary, otherwise false.
     */
    protected final boolean isUnary(SkelCompound sc) {
        if ((flags & FLAG_FILL) == 0)
            return super.isUnary(sc);
        int quote = (sc.sym instanceof SkelAtomAnno ? ((SkelAtomAnno) sc.sym).getHint() : 0);
        switch (quote) {
            case '(':
                return false;
            default:
                return sc.args.length == 1;
        }
    }

    /**
     * <p>Check whether the compound is binary.</p>
     *
     * @param sc The compound.
     * @return True if binary, otherwise false.
     */
    protected boolean isBinary(SkelCompound sc) {
        if ((flags & FLAG_FILL) == 0)
            return sc.args.length == 2;
        int quote = (sc.sym instanceof SkelAtomAnno ? ((SkelAtomAnno) sc.sym).getHint() : 0);
        switch (quote) {
            case '(':
                return false;
            default:
                return sc.args.length == 2;
        }
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
    protected final void writeSet(SkelCompound sc, Display ref,
                                  Object mod, SkelAtom nsa)
            throws IOException, EngineException, EngineMessage {
        if ((flags & FLAG_FILL) == 0) {
            super.writeSet(sc, ref, mod, nsa);
            return;
        }
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object[] decl = predicateToMeta(cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        appendLink(PrologReader.OP_LBRACE, cp);
        Object z = getArg(decl, backshift + modShift(mod, nsa), backspez, cp);
        spez = getSpez(z);
        offset = getOffset(z, backoffset);
        shift = getShift(z);
        String[][] fillers = (sc.sym instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sc.sym).getFillers() : null);
        writeFiller(indent, MARGIN, fillers != null ? fillers[0] : null);
        if (!lastEol(fillers != null ? fillers[0] : null)) {
            if (MARGIN < getTextOffset() &&
                    (flags & FLAG_NEWL) != 0) {
                append(CodeType.LINE_EOL);
                for (int i = 0; i < indent; i++)
                    append(' ');
            }
        }
        write(sc.args[0], ref, Operator.LEVEL_HIGH, null, null);
        writeFiller(indent, MARGIN, fillers != null ? fillers[1] : null);
        if (!lastEol(fillers != null ? fillers[1] : null)) {
            if (MARGIN < getTextOffset() &&
                    (flags & FLAG_NEWL) != 0) {
                append(CodeType.LINE_EOL);
                for (int i = 0; i < indent; i++)
                    append(' ');
            }
        }
        spez = backspez;
        offset = backoffset;
        shift = backshift;
        append(PrologReader.OP_RBRACE);
    }

    /**
     * <p>Write a list.</p>
     *
     * @param sc  The list skeleton.
     * @param ref The list display.
     * @param mod The module.
     * @param nsa The call-site.
     * @throws IOException     IO Error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected final void writeList(SkelCompound sc, Display ref,
                                   Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0) {
            super.writeList(sc, ref, mod, nsa);
            return;
        }
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object[] decl = predicateToMeta(cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        appendLink(PrologReader.OP_LBRACKET, cp);
        Object z = getArg(decl, backshift + modShift(mod, nsa), backspez, cp);
        spez = getSpez(z);
        offset = getOffset(z, backoffset);
        shift = getShift(z);
        String[][] fillers = (sc.sym instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sc.sym).getFillers() : null);
        writeFiller(indent, MARGIN, fillers != null ? fillers[0] : null);
        if (!lastEol(fillers != null ? fillers[0] : null)) {
            if (MARGIN < getTextOffset() &&
                    (flags & FLAG_NEWL) != 0) {
                append(CodeType.LINE_EOL);
                for (int i = 0; i < indent; i++)
                    append(' ');
            }
        }
        write(sc.args[0], ref, Operator.LEVEL_MIDDLE, null, null);
        z = getArg(decl, backshift + 1 + modShift(mod, nsa), backspez, cp);
        spez = getSpez(z);
        offset = getOffset(z, backoffset);
        shift = getShift(z);
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
                sc = (SkelCompound) term;
                cp = offsetToPredicate(term, null, null);
                appendLink(",", cp);
                fillers = (sc.sym instanceof SkelAtomAnno ?
                        ((SkelAtomAnno) sc.sym).getFillers() : null);
                writeFiller(indent, MARGIN, fillers != null ? fillers[0] : null);
                if (!lastEol(fillers != null ? fillers[0] : null)) {
                    if (MARGIN < getTextOffset() &&
                            (flags & FLAG_NEWL) != 0) {
                        append(CodeType.LINE_EOL);
                        for (int i = 0; i < indent; i++)
                            append(' ');
                    } else {
                        append(' ');
                    }
                }
                decl = predicateToMeta(cp);
                backspez = spez;
                backoffset = offset;
                backshift = shift;
                z = getArg(decl, backshift, backspez, cp);
                spez = getSpez(z);
                offset = getOffset(z, backoffset);
                shift = getShift(z);
                write(sc.args[0], ref, Operator.LEVEL_MIDDLE, null, null);
                z = getArg(decl, backshift + 1, backspez, cp);
                spez = getSpez(z);
                offset = getOffset(z, backoffset);
                shift = getShift(z);
                term = sc.args[1];
            } else if (!(term instanceof SkelAtom) ||
                    !((SkelAtom) term).fun.equals(Foyer.OP_NIL)) {
                append('|');
                write(term, ref, Operator.LEVEL_MIDDLE, null, null);
                break;
            } else {
                writeFiller(indent, MARGIN, fillers != null ? fillers[1] : null);
                if (!lastEol(fillers != null ? fillers[1] : null)) {
                    if (MARGIN < getTextOffset() &&
                            (flags & FLAG_NEWL) != 0) {
                        append(CodeType.LINE_EOL);
                        for (int i = 0; i < indent; i++)
                            append(' ');
                    }
                }
                break;
            }
        }
        spez = backspez;
        offset = backoffset;
        shift = backshift;
        append(PrologReader.OP_RBRACKET);
    }

    /**
     * <p>Write a compound.</p>
     *
     * @param sc  The compound skeleton.
     * @param ref The compound display.
     * @param mod The module.
     * @param nsa The call-site.
     * @throws IOException     IO Error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected final void writeCompound(SkelCompound sc, Display ref,
                                       Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0) {
            super.writeCompound(sc, ref, mod, nsa);
            return;
        }
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object[] decl = predicateToMeta(cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        String t = atomQuoted(sc.sym.fun, 0);
        safeSpace(t);
        appendLink(t, cp);
        append(PrologReader.OP_LPAREN);
        int j = 0;
        Object z = getArg(decl, backshift + j + modShift(mod, nsa), backspez, cp);
        spez = getSpez(z);
        offset = getOffset(z, backoffset);
        shift = getShift(z);
        String[][] fillers = (sc.sym instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sc.sym).getFillers() : null);
        writeFiller(indent, MARGIN, fillers != null ? fillers[j] : null);
        if (!lastEol(fillers != null ? fillers[j] : null)) {
            if (MARGIN < getTextOffset() &&
                    (flags & FLAG_NEWL) != 0) {
                append(CodeType.LINE_EOL);
                for (int i = 0; i < indent; i++)
                    append(' ');
            }
        }
        write(sc.args[j], ref, Operator.LEVEL_MIDDLE, null, null);
        for (j = 1; j < sc.args.length; j++) {
            z = getArg(decl, backshift + j + modShift(mod, nsa), backspez, cp);
            spez = getSpez(z);
            offset = getOffset(z, backoffset);
            shift = getShift(z);
            append(',');
            writeFiller(indent, MARGIN, fillers != null ? fillers[j] : null);
            if (!lastEol(fillers != null ? fillers[j] : null)) {
                if (MARGIN < getTextOffset() &&
                        (flags & FLAG_NEWL) != 0) {
                    append(CodeType.LINE_EOL);
                    for (int i = 0; i < indent; i++)
                        append(' ');
                } else {
                    append(' ');
                }
            }
            Object mod2 = (j == 1 ? decodeQualification(sc, ref) : null);
            SkelAtom nsa2 = (mod2 != null ? sc.sym : null);
            write(sc.args[j], ref, Operator.LEVEL_MIDDLE, mod2, nsa2);
        }
        writeFiller(indent, MARGIN, fillers != null ? fillers[j] : null);
        if (!lastEol(fillers != null ? fillers[j] : null)) {
            if (MARGIN < getTextOffset() &&
                    (flags & FLAG_NEWL) != 0) {
                append(CodeType.LINE_EOL);
                for (int i = 0; i < indent; i++)
                    append(' ');
            }
        }
        spez = backspez;
        offset = backoffset;
        shift = backshift;
        append(PrologReader.OP_RPAREN);
    }

    /**
     * <p>Write an array index.</p>
     *
     * @param sc  The term.
     * @param ref The display.
     * @param mod The module.
     * @throws IOException     IO Error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected final void writeIndex(SkelCompound sc, Display ref,
                                    CachePredicate cp, Object[] decl,
                                    Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0) {
            super.writeIndex(sc, ref, cp, decl, mod, nsa);
            return;
        }
        int backtoff = getTextOffset() + SPACES;
        appendLink(PrologReader.OP_LBRACKET, cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        int j = 1;
        Object z = getArg(decl, backshift + j + modShift(mod, nsa), backspez, cp);
        spez = getSpez(z);
        offset = getOffset(z, backoffset);
        shift = getShift(z);
        SkelAtom sa = sc.sym;
        String[][] fillers = (sa instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sa).getFillers() : null);
        writeFiller(indent, MARGIN, fillers != null ? fillers[j] : null);
        write(sc.args[j], ref, Operator.LEVEL_MIDDLE, null, null);
        for (j = 2; j < sc.args.length; j++) {
            z = getArg(decl, backshift + j + modShift(mod, nsa), backspez, cp);
            spez = getSpez(z);
            offset = getOffset(z, backoffset);
            shift = getShift(z);
            append(',');
            append(' ');
            fillers = (sc.sym instanceof SkelAtomAnno ?
                    ((SkelAtomAnno) sc.sym).getFillers() : null);
            writeFiller(indent, MARGIN, fillers != null ? fillers[j] : null);
            write(sc.args[j], ref, Operator.LEVEL_MIDDLE, null, null);
        }
        append(PrologReader.OP_RBRACKET);
        spez = backspez;
        offset = backoffset;
        shift = backshift;
    }

    /**
     * <p>Write an array index.</p>
     *
     * @param sc  The term.
     * @param ref The display.
     * @param mod The module.
     * @throws IOException     IO Error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected final void writeStruct(SkelCompound sc, Display ref,
                                     CachePredicate cp, Object[] decl,
                                     Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0) {
            super.writeStruct(sc, ref, cp, decl, mod, nsa);
            return;
        }
        int backtoff = getTextOffset() + SPACES;
        appendLink(PrologReader.OP_LBRACE, cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        if (sc.args.length == 2) {
            Object z = getArg(decl, backshift + 1 + modShift(mod, nsa), backspez, cp);
            spez = getSpez(z);
            offset = getOffset(z, backoffset);
            shift = getShift(z);
            String[][] fillers = (sc.sym instanceof SkelAtomAnno ?
                    ((SkelAtomAnno) sc.sym).getFillers() : null);
            writeFiller(indent, MARGIN, fillers != null ? fillers[1] : null);
            write(sc.args[1], ref, Operator.LEVEL_HIGH, null, null);
        }
        append(PrologReader.OP_RBRACE);
        spez = backspez;
        offset = backoffset;
        shift = backshift;
    }

    /************************************************************/
    /* Unparse Large Fillers                                    */
    /************************************************************/

    /**
     * <p>Unparse end of file.</p>
     * <p>Can be ovrerridden by sub classes.</p>
     *
     * @param sa The atom.
     * @throws IOException IO Error.
     */
    protected final void unparseEndOfFile(SkelAtom sa) throws IOException {
        if ((flags & FLAG_FILL) == 0) {
            super.unparseEndOfFile(sa);
            return;
        }
        if ((flags & FLAG_CMMT) != 0) {
            String[][] fillers = (sa instanceof SkelAtomAnno ?
                    ((SkelAtomAnno) sa).getFillers() : null);
            writeFiller(indent, 0, fillers != null ? fillers[0] : null);
        }
    }

    /**
     * <p>Unparse period.</p>
     *
     * @param sa The atom.
     * @param t  The argument.
     * @throws IOException     IO Error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected final void unparsePeriod(SkelAtom sa, Object t, Display ref)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0) {
            super.unparsePeriod(sa, t, ref);
            return;
        }
        String[][] fillers = (sa instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sa).getFillers() : null);
        String[] filler = fillers != null ? fillers[0] : null;
        int k = predicateComments(filler);
        if ((flags & FLAG_CMMT) != 0) {
            if (filler != null)
                writeFiller(indent, 0, filler, 0, k);
        }
        if ((flags & FLAG_STMT) != 0) {
            if (filler != null)
                writeFiller(indent, 0, filler, k, filler.length);
            write(t, ref, lev, null, null);
            safeSpace(".");
            append(".");
            writeFiller(indent, MARGIN, fillers != null ? fillers[1] : null);
            if (!lastEol(fillers != null ? fillers[1] : null)) {
                append(CodeType.LINE_EOL);
                for (int i = 0; i < indent; i++)
                    append(' ');
            }
        }
    }

    /**
     * <p>Determine the predicate comments.</p>
     *
     * @param filler The fillers.
     * @return The predicate comment index.
     */
    private static int predicateComments(String[] filler) {
        if (filler == null)
            return 0;
        for (int i = filler.length - 1; i >= 0; i--) {
            String str = filler[i];
            if (str.startsWith(CompLang.ISO_COMPLANG.getLineComment()) ||
                    str.startsWith(CompLang.ISO_COMPLANG.getBlockCommentStart())) {
                /* */
            } else if (i == 0 || hasDoubleEol(str)) {
                return i + 1;
            }
        }
        return 0;
    }

    /**
     * <p>Check whether the filler has at least two eols.</p>
     *
     * @param str The filler.
     * @return True if the filler has at least two eols, otherwise false.
     */
    private static boolean hasDoubleEol(String str) {
        int k = str.indexOf(CodeType.LINE_EOL);
        if (k == -1)
            return false;
        k = str.indexOf(CodeType.LINE_EOL, k + 1);
        if (k == -1)
            return false;
        return true;
    }

    /************************************************************/
    /* Unparse Small Fillers                                    */
    /************************************************************/

    /**
     * <p>Write the filler.</p>
     *
     * @param pos    The newline position.
     * @param at     The comment position.
     * @param filler The filler.
     * @throws IOException IO error.
     */
    private void writeFiller(int pos, int at, String[] filler)
            throws IOException {
        if (filler == null)
            return;
        writeFiller(pos, at, filler, 0, filler.length);
    }

    /**
     * <p>Write a segment of a filler.</p>
     *
     * @param pos    The newline position.
     * @param at     The comment position.
     * @param filler The filler, non null.
     * @param from   The start index, inclusive.
     * @param to     The end index, exclusive.
     * @throws IOException IO error.
     */
    private void writeFiller(int pos, int at, String[] filler, int from, int to)
            throws IOException {
        for (int j = from; j < to; j++) {
            String str = filler[j];
            if (str.startsWith(CompLang.ISO_COMPLANG.getLineComment()) ||
                    str.startsWith(CompLang.ISO_COMPLANG.getBlockCommentStart())) {
                int i = getTextOffset();
                for (; i < at; i++)
                    append(' ');
                safeSpace(str);
            } else {
                at = 0;
            }
            append(str);
            if (str.length() > 0 && str.charAt(str.length() - 1) == CodeType.LINE_EOL) {
                for (int i = 0; i < pos; i++)
                    append(' ');
            }
        }
    }

    /**
     * <p>Check whether the filler ends with an eol.</p>
     *
     * @param filler The filler
     * @return True if the filler ends with an eol, otherwise false.
     */
    public static boolean lastEol(String[] filler) {
        if (filler == null)
            return false;
        String str = filler[filler.length - 1];
        if (str.length() > 0 && str.charAt(str.length() - 1) == CodeType.LINE_EOL)
            return true;
        return false;
    }

}
