package jekpro.model.pretty;

import jekpro.model.builtin.AbstractInformation;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.*;
import jekpro.model.rope.Operator;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.MapEntry;
import matula.util.regex.CodeType;
import matula.util.regex.CompLang;
import matula.util.system.ForeignFile;
import matula.util.system.ForeignUri;
import matula.util.system.ForeignXml;

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
public final class PrologWriterAnno extends PrologWriter {

    /************************************************************/
    /* Write Fillers                                            */
    /************************************************************/

    /**
     * <p>Write an atom.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param sa  The atom.
     * @param mod The module context, or null.
     * @throws IOException     IO Error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected void writeAtom(SkelAtom sa,
                             Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if ((spez & SPEZ_META) == 0 || (flags & FLAG_NAVI) == 0) {
            super.writeAtom(sa, mod, nsa);
            return;
        }
        CachePredicate cp = offsetToPredicate(sa, mod, nsa);
        if (engine != null && (flags & FLAG_IGNO) == 0 &&
                (spez & SPEZ_OPLE) != 0) {
            Operator op = OperatorSearch.getOper(sa.scope, sa.fun,
                    Operator.TYPE_PREFIX, engine);
            if (op != null) {
                if ((spez & SPEZ_FUNC) != 0)
                    append(' ');
                append(PrologReader.OP_LPAREN);
                writeLinkBegin(cp, offset >= 0);
                append(atomQuoted(sa.fun, 0));
                writeLinkEnd(cp);
                append(PrologReader.OP_RPAREN);
                return;
            }
        }
        if (Foyer.OP_UNIT.equals(sa.fun)) {
            if ((spez & SPEZ_FUNC) != 0)
                append(' ');
            writeLinkBegin(cp, offset >= 0);
            append(sa.fun);
            writeLinkEnd(cp);
            return;
        }
        String t = atomQuoted(sa.fun, 0);
        safeSpace(t);
        writeLinkBegin(cp, offset >= 0);
        append(t);
        writeLinkEnd(cp);
    }

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
    protected void writeAtomicOrVar(Object term, Display ref,
                                    Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0 && (flags & FLAG_NAVI) == 0) {
            super.writeAtomicOrVar(term, ref, mod, nsa);
            return;
        }
        if (!(term instanceof SkelAtom)) {
            super.writeAtomicOrVar(term, ref, mod, nsa);
            return;
        }
        SkelAtom sa = (SkelAtom) term;
        if ((flags & FLAG_FILL) != 0) {
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
                    safeSpace(sa.fun);
                    append(sa.fun);
                    break;
                default:
                    writeAtom(sa, mod, nsa);
                    break;
            }
        } else {
            writeAtom(sa, mod, nsa);
        }
    }

    /********************************************************/
    /* Operator Handling                                    */
    /********************************************************/

    /**
     * <p>Write the operator.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param op         The operator.
     * @param sa         The atom.
     * @param cp         The cache predicate or null.
     * @param backspez   The spezification.
     * @param backoffset The offset.
     * @throws IOException IO Error.
     */
    protected final void writePrefix(Operator op, SkelAtom sa,
                                     CachePredicate cp,
                                     int backspez, int backoffset)
            throws IOException {
        if ((flags & FLAG_FILL) == 0 && (flags & FLAG_NAVI) == 0) {
            super.writePrefix(op, sa, cp, backspez, backoffset);
            return;
        }
        /**
         * - spacing.
         * - anti specification
         */
        String t = atomQuoted(op.getPortrayOrName(), 0);
        safeSpace(t);
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkBegin(cp, backoffset >= 0);
        append(t);
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkEnd(cp);
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
     * @param cp         The cache predicate or null.
     * @param decl       The declaration or null.
     * @param backspez   The spezification.
     * @param backoffset The offset.
     * @throws IOException IO Error.
     */
    protected final void writeInfix(Operator op, SkelAtom sa,
                                    CachePredicate cp,
                                    Object[] decl, int indent,
                                    int backspez, int backoffset)
            throws IOException {
        if ((flags & FLAG_FILL) == 0 && (flags & FLAG_NAVI) == 0) {
            super.writeInfix(op, sa, cp, decl, indent, backspez, backoffset);
            return;
        }
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
                    writeLinkBegin(cp, backoffset >= 0);
                append(t);
                if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                    writeLinkEnd(cp);
                if ((op.getBits() & Operator.MASK_OPER_NSPR) == 0)
                    append(' ');
            } else if (op.getLevel() >= 1025 && !isLowr(decl)) {
                if ((flags & FLAG_FILL) != 0) {
                    String[][] fillers = (sa instanceof SkelAtomAnno ?
                            ((SkelAtomAnno) sa).getFillers() : null);
                    writeFiller(ENDLINE, (fillers != null ? fillers[0] : null));
                    if (!hasEol(fillers != null ? fillers[0] : null))
                        append(CodeType.LINE_EOL);
                }
                for (int i = 0; i < indent - SPACES; i++)
                    append(' ');
                String t = atomQuoted(op.getPortrayOrName(), MASK_ATOM_OPER);
                safeSpace(t);
                if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                    writeLinkBegin(cp, backoffset >= 0);
                append(t);
                if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                    writeLinkEnd(cp);
                for (int i = t.length(); i < SPACES; i++)
                    append(' ');
            } else {
                if ((op.getBits() & Operator.MASK_OPER_NSPL) == 0)
                    append(' ');
                String t = atomQuoted(op.getPortrayOrName(), MASK_ATOM_OPER);
                safeSpace(t);
                if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                    writeLinkBegin(cp, backoffset >= 0);
                append(t);
                if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                    writeLinkEnd(cp);
                if ((flags & FLAG_FILL) != 0) {
                    String[][] fillers = (sa instanceof SkelAtomAnno ?
                            ((SkelAtomAnno) sa).getFillers() : null);
                    writeFiller(ENDLINE, (fillers != null ? fillers[1] : null));
                    if (!hasEol(fillers != null ? fillers[1] : null))
                        append(CodeType.LINE_EOL);
                }
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
                writeLinkBegin(cp, backoffset >= 0);
            append(t);
            if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                writeLinkEnd(cp);
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
     * @param ref        The compound display.
     * @param cp         The cache predicate or null.
     * @param decl       The declaration or null.
     * @param backspez   The spezification.
     * @param backoffset The offset.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     * @throws IOException     IO Error.
     */
    protected final void writePostfix(Operator op, SkelCompound sc, Display ref,
                                      CachePredicate cp, Object[] decl,
                                      int backshift, int backspez, int backoffset,
                                      Object mod, SkelAtom nsa)
            throws IOException, EngineException, EngineMessage {
        if ((flags & FLAG_FILL) == 0 && (flags & FLAG_NAVI) == 0) {
            super.writePostfix(op, sc, ref, cp, decl,
                    backshift, backspez, backoffset, mod, nsa);
            return;
        }
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
                writeLinkBegin(cp, backoffset >= 0);
            append(t);
            if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                writeLinkEnd(cp);
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
    protected void writeSet(SkelCompound sc, Display ref,
                            Object mod, SkelAtom nsa)
            throws IOException, EngineException, EngineMessage {
        if ((flags & FLAG_FILL) == 0 && (flags & FLAG_NAVI) == 0) {
            super.writeSet(sc, ref, mod, nsa);
            return;
        }
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object[] decl = predicateToMeta(cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        int indent = getTextOffset() + SPACES;
        append(PrologReader.OP_LBRACE);
        Object z = getArg(decl, backshift + modShift(mod, nsa), backspez, cp);
        spez = getSpez(z);
        offset = getOffset(z, backoffset);
        shift = getShift(z);
        if ((flags & FLAG_FILL) != 0) {
            SkelAtom sa = sc.sym;
            String[][] fillers = (sa instanceof SkelAtomAnno ?
                    ((SkelAtomAnno) sa).getFillers() : null);
            if (hasEol(fillers != null ? fillers[0] : null)) {
                writeFiller(ENDLINE, fillers[0]);
                for (int i = 0; i < indent; i++)
                    append(' ');
            }
        }
        write(sc.args[0], ref, Operator.LEVEL_HIGH, null, null);
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
     * @throws IOException     IO Error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected final void writeList(SkelCompound sc, Display ref,
                                   Object mod, SkelAtom nsa,
                                   int indent, int backspez,
                                   int backoffset, int backshift)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0 && (flags & FLAG_NAVI) == 0) {
            super.writeList(sc, ref, mod, nsa, indent, backspez, backoffset, backshift);
            return;
        }
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
                sc = (SkelCompound) term;
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
                        writeLinkBegin(cp, backoffset >= 0);
                    append(',');
                    if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                        writeLinkEnd(cp);
                    if ((flags & FLAG_FILL) != 0) {
                        String[][] fillers = (sc.sym instanceof SkelAtomAnno ?
                                ((SkelAtomAnno) sc.sym).getFillers() : null);
                        writeFiller(ENDLINE, (fillers != null ? fillers[1] : null));
                        if (!hasEol(fillers != null ? fillers[1] : null))
                            append(CodeType.LINE_EOL);
                    }
                    for (int i = 0; i < indent; i++)
                        append(' ');
                } else {
                    if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                        writeLinkBegin(cp, backoffset >= 0);
                    append(',');
                    if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
                        writeLinkEnd(cp);
                    if ((backspez & SPEZ_META) != 0 &&
                            (backspez & SPEZ_EVAL) == 0)
                        append(' ');
                }
                write(sc.args[0], ref, Operator.LEVEL_MIDDLE, null, null);
                z = getArg(decl, backshift + 1, backspez, cp);
                spez = getSpez(z);
                offset = getOffset(z, backoffset);
                shift = getShift(z);
            } else if (!(term instanceof SkelAtom) ||
                    !((SkelAtom) term).fun.equals(Foyer.OP_NIL)) {
                append('|');
                write(term, ref, Operator.LEVEL_MIDDLE, null, null);
                break;
            } else {
                break;
            }
        }
    }

    /**
     * <p>Write a compound.</p>
     *
     * @param sc  The compound skeleton.
     * @param ref The compound display.
     * @param mod The module.
     * @throws IOException     IO Error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected final void writeCompound(SkelCompound sc, Display ref,
                                       Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0 && (flags & FLAG_NAVI) == 0) {
            super.writeCompound(sc, ref, mod, nsa);
            return;
        }
        CachePredicate cp = offsetToPredicate(sc, mod, nsa);
        Object[] decl = predicateToMeta(cp);
        int backspez = spez;
        int backoffset = offset;
        int backshift = shift;
        int indent = getTextOffset() + SPACES;
        String t = atomQuoted(sc.sym.fun, 0);
        safeSpace(t);
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkBegin(cp, backoffset >= 0);
        append(t);
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkEnd(cp);
        append('(');
        int j = 0;
        Object z = getArg(decl, backshift + j + modShift(mod, nsa), backspez, cp);
        spez = getSpez(z);
        offset = getOffset(z, backoffset);
        shift = getShift(z);
        if ((flags & FLAG_FILL) != 0) {
            SkelAtom sa = sc.sym;
            String[][] fillers = (sa instanceof SkelAtomAnno ?
                    ((SkelAtomAnno) sa).getFillers() : null);
            if (hasEol(fillers != null ? fillers[j] : null)) {
                writeFiller(ENDLINE, fillers[j]);
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
            if ((flags & FLAG_FILL) != 0) {
                SkelAtom sa = sc.sym;
                String[][] fillers = (sa instanceof SkelAtomAnno ?
                        ((SkelAtomAnno) sa).getFillers() : null);
                if (hasEol(fillers != null ? fillers[j] : null)) {
                    writeFiller(ENDLINE, fillers[j]);
                    for (int i = 0; i < indent; i++)
                        append(' ');
                } else {
                    if ((backspez & SPEZ_META) != 0 &&
                            (backspez & SPEZ_EVAL) == 0)
                        append(' ');
                }
            } else {
                if ((backspez & SPEZ_META) != 0 &&
                        (backspez & SPEZ_EVAL) == 0)
                    append(' ');
            }
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
        append(')');
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
    protected void writeIndex(SkelCompound sc, Display ref,
                              CachePredicate cp, Object[] decl,
                              int backshift, int backspez, int backoffset,
                              Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0 && (flags & FLAG_NAVI) == 0) {
            super.writeIndex(sc, ref, cp, decl,
                    backshift, backspez, backoffset, mod, nsa);
            return;
        }
        int indent = getTextOffset() + SPACES;
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkBegin(cp, backoffset >= 0);
        append(PrologReader.OP_LBRACKET);
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkEnd(cp);
        int j = 1;
        Object z = getArg(decl, backshift + j + modShift(mod, nsa), backspez, cp);
        spez = getSpez(z);
        offset = getOffset(z, backoffset);
        shift = getShift(z);
        if ((flags & FLAG_FILL) != 0) {
            SkelAtom sa = sc.sym;
            String[][] fillers = (sa instanceof SkelAtomAnno ?
                    ((SkelAtomAnno) sa).getFillers() : null);
            if (hasEol(fillers != null ? fillers[j] : null)) {
                writeFiller(ENDLINE, fillers[j]);
                for (int i = 0; i < indent; i++)
                    append(' ');
            }
        }
        write(sc.args[j], ref, Operator.LEVEL_MIDDLE, null, null);
        for (j = 2; j < sc.args.length; j++) {
            z = getArg(decl, backshift + j + modShift(mod, nsa), backspez, cp);
            spez = getSpez(z);
            offset = getOffset(z, backoffset);
            shift = getShift(z);
            append(',');
            if ((flags & FLAG_FILL) != 0) {
                SkelAtom sa = sc.sym;
                String[][] fillers = (sa instanceof SkelAtomAnno ?
                        ((SkelAtomAnno) sa).getFillers() : null);
                if (hasEol(fillers != null ? fillers[j] : null)) {
                    writeFiller(ENDLINE, fillers[j]);
                    for (int i = 0; i < indent; i++)
                        append(' ');
                } else {
                    if ((backspez & SPEZ_META) != 0 &&
                            (backspez & SPEZ_EVAL) == 0)
                        append(' ');
                }
            } else {
                if ((backspez & SPEZ_META) != 0 &&
                        (backspez & SPEZ_EVAL) == 0)
                    append(' ');
            }
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
     * <p>Write an array index.</p>
     *
     * @param sc  The term.
     * @param ref The display.
     * @param mod The module.
     * @throws IOException     IO Error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected void writeStruct(SkelCompound sc, Display ref,
                               CachePredicate cp, Object[] decl,
                               int backshift, int backspez, int backoffset,
                               Object mod, SkelAtom nsa)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0 && (flags & FLAG_NAVI) == 0) {
            super.writeStruct(sc, ref, cp, decl,
                    backshift, backspez, backoffset, mod, nsa);
            return;
        }
        int indent = getTextOffset() + SPACES;
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkBegin(cp, backoffset >= 0);
        append(PrologReader.OP_LBRACE);
        if ((backspez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkEnd(cp);
        if (sc.args.length == 2) {
            Object z = getArg(decl, backshift + 1 + modShift(mod, nsa), backspez, cp);
            spez = getSpez(z);
            offset = getOffset(z, backoffset);
            shift = getShift(z);
            if ((flags & FLAG_FILL) != 0) {
                SkelAtom sa = sc.sym;
                String[][] fillers = (sa instanceof SkelAtomAnno ?
                        ((SkelAtomAnno) sa).getFillers() : null);
                if (hasEol(fillers != null ? fillers[1] : null)) {
                    writeFiller(ENDLINE, fillers[1]);
                    for (int i = 0; i < indent; i++)
                        append(' ');
                }
            }
            write(sc.args[1], ref, Operator.LEVEL_HIGH, null, null);
        }
        append(PrologReader.OP_RBRACE);
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
        if ((flags & FLAG_FILL) == 0) {
            super.unparseEndOfFile(sa);
            return;
        }
        if ((flags & FLAG_CMMT) != 0) {
            String[][] fillers = (sa instanceof SkelAtomAnno ?
                    ((SkelAtomAnno) sa).getFillers() : null);
            writeFiller(PrologWriter.STANDALONE, (fillers != null ? fillers[0] : null));
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
    protected void unparsePeriod(SkelAtom sa, Object t, Display ref)
            throws IOException, EngineMessage, EngineException {
        if ((flags & FLAG_FILL) == 0 && (flags & FLAG_NAVI) == 0) {
            super.unparsePeriod(sa, t, ref);
            return;
        }
        if ((flags & FLAG_CMMT) != 0) {
            if ((flags & FLAG_FILL) != 0) {
                String[][] fillers = (sa instanceof SkelAtomAnno ?
                        ((SkelAtomAnno) sa).getFillers() : null);
                String[] filler = fillers != null ? fillers[0] : null;
                int k = predicateComments(filler);
                writeFiller(STANDALONE, filler, 0, k);
            }
        }
        if ((flags & FLAG_STMT) != 0) {
            if ((flags & FLAG_FILL) != 0) {
                String[][] fillers = (sa instanceof SkelAtomAnno ?
                        ((SkelAtomAnno) sa).getFillers() : null);
                String[] filler = fillers != null ? fillers[0] : null;
                int k = predicateComments(filler);
                writeFiller(STANDALONE, filler, k, (filler != null ? filler.length : 0));
            }
            write(t, ref, lev, null, null);
            safeSpace(".");
            append(".");
            if ((flags & FLAG_FILL) != 0) {
                String[][] fillers = (sa instanceof SkelAtomAnno ?
                        ((SkelAtomAnno) sa).getFillers() : null);
                writeFiller(ENDLINE, fillers != null ? fillers[1] : null);
                if (!hasEol(fillers != null ? fillers[1] : null))
                    append(CodeType.LINE_EOL);
            } else {
                append(CodeType.LINE_EOL);
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

    /**
     * <p>Write a segment of a filler.</p>
     *
     * @param at     The comment position.
     * @param filler The filler.
     * @param from   The start index, inclusive.
     * @param to     The end index, exclusive.
     * @throws IOException IO error.
     */
    private void writeFiller(int at, String[] filler, int from, int to)
            throws IOException {
        if (filler == null)
            return;
        for (int j = from; j < to; j++) {
            String str = filler[j];
            if (str.startsWith(CompLang.ISO_COMPLANG.getLineComment()) ||
                    str.startsWith(CompLang.ISO_COMPLANG.getBlockCommentStart())) {
                for (int i = getTextOffset(); i < at; i++)
                    append(' ');
                safeSpace(str);
                append(str);
            } else {
                append(str);
            }
        }
    }

    /**
     * <p>Write the filler.</p>
     *
     * @param at     The comment position.
     * @param filler The filler.
     * @throws IOException IO error.
     */
    private void writeFiller(int at, String[] filler)
            throws IOException {
        writeFiller(at, filler, 0, (filler != null ? filler.length : 0));
    }

    /************************************************************/
    /* Navigation Code                                          */
    /************************************************************/

    /**
     * <p>Write the navigation link begin comment.</p>
     *
     * @param cp   The predicate.
     * @param goal The goal flag.
     * @throws IOException IO Error.
     */
    private void writeLinkBegin(CachePredicate cp, boolean goal)
            throws IOException {
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0) {
            StringBuilder buf = new StringBuilder();
            buf.append("%<a href=\"");
            buf.append("__error__");
            buf.append("\">\n");
            getWriter().write(buf.toString());
            return;
        }
        AbstractSource src = getHome(cp.pick);
        if (src == null)
            throw new RuntimeException("shouldn't happen");
        StringBuilder buf;
//            if (goal) {
        String uri = ForeignUri.sysUriMake(getNavigation(src), "", getHash(src, cp.pick));
        buf = new StringBuilder();
        buf.append("%<a href=\"");
        buf.append(ForeignXml.sysTextEscape(ForeignUri.sysUriEncode(uri)));
        buf.append("\">\n");
//            } else {
//                String query = ForeignUri.sysQueryMake("words", getName(pick), "");
//                String uri = ForeignUri.sysUriMake("/found.jsp", query, "");
//                buf = new StringBuilder();
//                buf.append("%<a href=\"");
//                buf.append(ForeignXml.sysTextEscape(ForeignUri.sysUriEncode(uri)));
//                buf.append("\" class=\"navigation\">\n");
//            }
        /* bypass toff and lch */
        getWriter().write(buf.toString());
    }

    /**
     * <p>Write the navigation link end comment.</p>
     *
     * @param cp The predicate.
     * @throws IOException IO Error.
     */
    private void writeLinkEnd(CachePredicate cp)
            throws IOException {
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0) {
            getWriter().write("%</a>\n");
            return;
        }
        AbstractSource src = getHome(cp.pick);
        if (src == null)
            return;
        /* bypass toff and lch */
        getWriter().write("%</a>\n");
    }

    /************************************************************/
    /* Navigation Helper                                        */
    /************************************************************/

    /**
     * <p>Retrieve the path.</p>
     *
     * @param pick The predicate.
     * @return The path or null.
     */
    private AbstractSource getHome(Predicate pick) {
        if (getSource() != null && pick.getDef(getSource()) != null)
            return getSource();
        MapEntry<AbstractSource, Integer>[] snapshot = pick.snapshotDefs();
        return (snapshot.length > 0 ? snapshot[0].key : null);
    }

    /**
     * <p>Retrieve the navigation to the predicate.</p>
     *
     * @param dst The target source.
     * @return The navigation.
     */
    private String getNavigation(AbstractSource dst) {
        if (getSource() == null) {
            return dst.getHTMLPath();
        } else {
            String module = dst.getHTMLPath();
            String orig = getSource().getHTMLPath();
            return ForeignFile.sysPathRelative(orig, module);
        }
    }

    /**
     * <p>Retrieve the hash.</p>
     *
     * @param dst  The source.
     * @param pick The predicate.
     * @return The hash.
     */
    private static String getHash(AbstractSource dst, Predicate pick) {
        StringBuilder buf = new StringBuilder();
        String orig = AbstractInformation.getOrig(dst);
        String module = AbstractInformation.getModule(pick.getFun());
        if (!orig.equals(module)) {
            buf.append(module.replace(CachePackage.OP_CHAR_SEG, CacheModule.OP_CHAR_OS));
            buf.appendCodePoint(':');
        }
        buf.append(AbstractInformation.getName(pick.getFun()));
        buf.appendCodePoint(CacheModule.OP_CHAR_OS);
        buf.append(Integer.valueOf(pick.getArity()));
        return buf.toString();
    }

}
