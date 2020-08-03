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
 * <li><b>flags:</b> FLAG_FILL.</li>
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
        int quote = (sa instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sa).getHint() : 0);
        switch (quote & 0xFF) {
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
     * <p>Check whether the compound is unary.</p>
     *
     * @param sc The compound.
     * @return True if unary, otherwise false.
     */
    protected final boolean isUnary(SkelCompound sc) {
        if ((flags & FLAG_FILL) == 0)
            return super.isUnary(sc);
        int quote = (sc.sym instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sc.sym).getHint() : 0);
        switch (quote >> 8) {
            case '(':
                return false;
            default:
                return super.isUnary(sc);
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
            return super.isBinary(sc);
        int quote = (sc.sym instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sc.sym).getHint() : 0);
        switch (quote >> 8) {
            case '(':
                return false;
            default:
                return super.isBinary(sc);
        }
    }

    /**
     * <p>Check whether the compound is an index.</p>
     *
     * @param sc The compound.
     * @return True if the compound is an index, otherwise false.
     */
    protected boolean isIndex(SkelCompound sc) {
        if ((flags & FLAG_FILL) == 0)
            return super.isIndex(sc);
        int quote = (sc.sym instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sc.sym).getHint() : 0);
        switch (quote >> 8) {
            case '(':
                return false;
            default:
                return super.isIndex(sc);
        }
    }

    /*********************************************************************/
    /* Parenthesis Handling                                              */
    /*********************************************************************/

    /**
     * <p>Write a break if necessary.</p>
     *
     * @param sa    The call site.
     * @param j     The argument index.
     * @param space The space flag.
     * @throws IOException IO Error.
     */
    protected final void writeBreak(SkelAtom sa, int j, boolean space)
            throws IOException {
        if ((flags & FLAG_FILL) == 0) {
            super.writeBreak(sa, j, space);
            return;
        }
        String[][] fillers = (sa instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sa).getFillers() : null);
        String[] filler = (fillers != null && j < fillers.length ?
                fillers[j] : null);
        writeFiller(MARGIN, filler);
        if (!lastEol(filler)) {
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
    }

    /**
     * <p>Write always a break.</p>
     *
     * @param sa    The call site.
     * @param j     The argument index.
     * @param space The space flag.
     * @throws IOException IO Error.
     */
    protected final void writeBreakForce(SkelAtom sa, int j, boolean space)
            throws IOException {
        if ((flags & FLAG_FILL) == 0) {
            super.writeBreakForce(sa, j, space);
            return;
        }
        String[][] fillers = (sa instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sa).getFillers() : null);
        String[] filler = (fillers != null && j < fillers.length ?
                fillers[j] : null);
        writeFiller(MARGIN, filler);
        if (!lastEol(filler)) {
            if ((flags & FLAG_NEWL) != 0) {
                append(CodeType.LINE_EOL);
                for (int i = 0; i < indent; i++)
                    append(' ');
            } else {
                if (space)
                    append(' ');
            }
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
        if ((flags & FLAG_FILL) == 0) {
            super.writeAdjust(sa, j, t, space);
            return;
        }
        String[][] fillers = (sa instanceof SkelAtomAnno ?
                ((SkelAtomAnno) sa).getFillers() : null);
        String[] filler = (fillers != null && j < fillers.length ?
                fillers[j] : null);
        if (lastEol(filler) ||
                (flags & FLAG_NEWL) != 0) {
            for (int i = t.length(); i < SPACES; i++)
                append(' ');
        } else {
            if (space)
                append(' ');
        }
    }

    /**
     * <p>Determine whether the infinx operator needs spaces.</p>
     *
     * @param oper The infix operator.
     * @param sa   The call-site.
     * @param j    The argument index.
     * @return True if the infix operators needs spaces.
     */
    protected final boolean needsSpaces(Operator oper, SkelAtom sa, int j) {
        if ((flags & FLAG_FILL) == 0)
            return super.needsSpaces(oper, sa, j);
        /* comma etc.. */
        if ((oper.getBits() & Operator.MASK_OPER_NEWR) != 0) {
            return false;
            /* semicolon etc.. */
        } else if ((oper.getBits() & Operator.MASK_OPER_TABR) != 0) {
            String[][] fillers = (sa instanceof SkelAtomAnno ?
                    ((SkelAtomAnno) sa).getFillers() : null);
            String[] filler = (fillers != null && j < fillers.length ?
                    fillers[j] : null);
            if (lastEol(filler) ||
                    (flags & FLAG_NEWL) != 0) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
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
            writeFiller(0, fillers != null ? fillers[0] : null);
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
                writeFiller(0, filler, 0, k);
        }
        if ((flags & FLAG_STMT) != 0) {
            if (filler != null)
                writeFiller(0, filler, k, filler.length);
            write(t, ref, lev, null, null);
            safeSpace(".");
            append(".");
            writeFiller(MARGIN, fillers != null ? fillers[1] : null);
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
     * @param at     The comment position.
     * @param filler The filler.
     * @throws IOException IO error.
     */
    private void writeFiller(int at, String[] filler)
            throws IOException {
        if (filler == null)
            return;
        writeFiller(at, filler, 0, filler.length);
    }

    /**
     * <p>Write a segment of a filler.</p>
     *
     * @param at     The comment position.
     * @param filler The filler, non null.
     * @param from   The start index, inclusive.
     * @param to     The end index, exclusive.
     * @throws IOException IO error.
     */
    private void writeFiller(int at, String[] filler, int from, int to)
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
                for (int i = 0; i < indent; i++)
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
