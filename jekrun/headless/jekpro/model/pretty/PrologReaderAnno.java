package jekpro.model.pretty;

import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Operator;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.ListArray;
import matula.util.regex.CodeType;
import matula.util.regex.ScannerError;
import matula.util.regex.ScannerToken;

import java.io.IOException;

/**
 * <p>This class provides the reading of annotated prolog terms.</p>
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
public class PrologReaderAnno extends PrologReader {
    private ListArray<String> rtrn;

    /*********************************************************************/
    /* Special Operators                                                 */
    /*********************************************************************/

    /**
     * <p>Reads an prefix.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param help The functor.
     * @param oper The operator.
     * @return The preiîx.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected final Object readPrefix(SkelAtom help, Operator oper)
            throws EngineException, IOException, ScannerError, EngineMessage {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0)
            return super.readPrefix(help, oper);
        String[] filler = getFiller();
        if (filler != null)
            help = makeFillers(help, new String[][]{filler, null});
        Object jill = read(oper.getLevel() - oper.getRight());
        return new SkelCompound(help, jill);
    }

    /**
     * <p>Reads an infix.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param help The functor.
     * @param skel The left argument.
     * @param oper The operator.
     * @return The infix.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected final Object readInfix(SkelAtom help, Object skel, Operator oper)
            throws ScannerError, EngineMessage, EngineException, IOException {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0)
            return super.readInfix(help, skel, oper);
        String[] filler = getFiller();
        nextOperator();
        String[] filler2 = getFiller();
        if (filler != null || filler2 != null)
            help = makeFillers(help, new String[][]{filler, filler2, null});
        Object jack = read(oper.getLevel() - oper.getRight());
        return new SkelCompound(help, skel, jack);
    }

    /**
     * <p>Reads a postfix.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param help The functor.
     * @param skel The left argument.
     * @return The postfix.
     * @throws ScannerError    Error and position.
     * @throws IOException     IO Error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected final Object readPostfix(SkelAtom help, Object skel)
            throws ScannerError, IOException, EngineMessage, EngineException {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0)
            return super.readPostfix(help, skel);
        if (st.getHint() == 0 && OP_LBRACKET.equals(st.getData())) {
            nextToken();
            if (st.getHint() == 0 && OP_RBRACKET.equals(st.getData())) {
                nextToken();
                String[] filler = getFiller();
                if (filler != null)
                    help = makeFillers(help, new String[][]{null, filler});
                skel = new SkelCompound(help, skel);
            } else {
                String[] filler = getFiller();
                skel = readIndex(skel, filler, help);
                if (st.getHint() != 0 || !OP_RBRACKET.equals(st.getData()))
                    throw new ScannerError(ERROR_SYNTAX_BRACKET_BALANCE,
                            st.getTokenOffset());
                nextToken();
            }
        } else if (st.getHint() == 0 && OP_LBRACE.equals(st.getData())) {
            nextToken();
            if (st.getHint() == 0 && OP_RBRACE.equals(st.getData())) {
                nextToken();
                String[] filler = getFiller();
                if (filler != null)
                    help = makeFillers(help, new String[][]{null, filler});
                skel = new SkelCompound(help, skel);
            } else {
                String[] filler = getFiller();
                skel = readStruct(skel, filler, help);
                if (st.getHint() != 0 || !OP_RBRACE.equals(st.getData()))
                    throw new ScannerError(ERROR_SYNTAX_BRACE_BALANCE,
                            st.getTokenOffset());
                nextToken();
            }
        } else if (st.getHint() == 0 && OP_LPAREN.equals(st.getData())) {
            nextToken();
            if (st.getHint() == 0 && OP_RPAREN.equals(st.getData())) {
                nextToken();
                String[] filler = getFiller();
                if (filler != null)
                    help = makeFillers(help, new String[][]{null, filler});
                skel = new SkelCompound(help, skel);
            } else {
                throw new ScannerError(ERROR_SYNTAX_PARENTHESIS_BALANCE,
                        st.getTokenOffset());
            }
            return skel;
        } else {
            nextToken();
            String[] filler = getFiller();
            if (filler != null)
                help = makeFillers(help, new String[][]{null, filler});
            skel = new SkelCompound(help, skel);
        }
        return skel;
    }

    /*********************************************************************/
    /* Special Compounds                                                 */
    /*********************************************************************/

    /**
     * <p>Reads a set.</p>
     *
     * @param level The level.
     * @return The set.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     * @throws IOException     IO Error.
     */
    protected final Object readSet(int level)
            throws ScannerError, EngineMessage, EngineException, IOException {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0)
            return super.readSet(level);
        SkelAtom help = makePos(Foyer.OP_SET, getAtomPos());
        String[] filler = getFiller();
        Object arg = read(Operator.LEVEL_HIGH);
        if (st.getHint() != 0 || !OP_RBRACE.equals(st.getData()))
            throw new ScannerError(ERROR_SYNTAX_BRACE_BALANCE,
                    st.getTokenOffset());
        String[] filler2;
        if (level != -1) {
            nextToken();
            filler2 = getFiller();
        } else {
            filler2 = null;
        }
        if (filler != null || filler2 != null)
            help = makeFillers(help, new String[][]{filler, filler2});
        return new SkelCompound(help, arg);
    }

    /**
     * <p>Reads a list.</p>
     *
     * @param level The level.
     * @return The list.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     * @throws IOException     IO Error.
     */
    protected final Object readList(int level)
            throws ScannerError, EngineMessage, EngineException, IOException {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0)
            return super.readList(level);
        SkelAtom help = makePos(Foyer.OP_CONS, getAtomPos());
        String[] filler = getFiller();
        Object[] args = new Object[2];
        args[0] = read(Operator.LEVEL_MIDDLE);
        args[1] = null;
        SkelCompound back;
        Object t;
        for (; ; ) {
            if (st.getHint() == 0 && OP_COMMA.equals(st.getData())) {
                if (filler != null)
                    help = makeFillers(help, new String[][]{filler, null, null});
                back = new SkelCompound(help, args, null);
                help = makePos(Foyer.OP_CONS, getAtomPos());
                nextToken();
                filler = getFiller();
                if (filler != null)
                    help = makeFillers(help, new String[][]{filler, null, null});
                args = new Object[2];
                args[0] = read(Operator.LEVEL_MIDDLE);
                args[1] = back;
            } else if (st.getHint() == 0 && OP_BAR.equals(st.getData())) {
                nextToken();
                t = read(Operator.LEVEL_MIDDLE);
                break;
            } else {
                t = makePos(Foyer.OP_NIL, getAtomPos());
                break;
            }
        }
        if (st.getHint() != 0 || !OP_RBRACKET.equals(st.getData()))
            throw new ScannerError(ERROR_SYNTAX_BRACKET_BALANCE,
                    st.getTokenOffset());
        String[] filler2;
        if (level != -1) {
            nextToken();
            filler2 = getFiller();
        } else {
            filler2 = null;
        }
        if (filler != null || filler2 != null)
            help = makeFillers(help, new String[][]{filler, null, filler2});
        back = new SkelCompound(help, args, null);
        do {
            SkelCompound jack = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            back.var = SkelCompound.makeExtra(back.args);
            t = back;
            back = jack;
        } while (back != null);
        return t;
    }

    /**
     * <p>Mark the functor.</p>
     *
     * @param sa The skel atom.
     * @return The marked skel atom.
     */
    protected final SkelAtom markFunc(SkelAtom sa) {
        if ((getFlags() & PrologWriter.FLAG_HINT) == 0)
            return super.markFunc(sa);
        return makeHint(sa, '(');
    }

    /**
     * <p>Read a compound.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param help  The functor.
     * @param level The level.
     * @return The compound.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     * @throws IOException     IO Error.
     */
    protected final Object readCompound(SkelAtom help, int level)
            throws ScannerError, EngineMessage, EngineException, IOException {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0)
            return super.readCompound(help, level);
        ListArray<Object> vec = new ListArray<Object>();
        ListArray<String[]> fils = null;
        String[] filler = getFiller();
        if (filler != null) {
            fils = new ListArray<String[]>();
            fils.add(filler);
        }
        vec.add(read(Operator.LEVEL_MIDDLE));
        while (st.getHint() == 0 && OP_COMMA.equals(st.getData())) {
            nextToken();
            filler = getFiller();
            if (filler != null && fils == null) {
                fils = new ListArray<String[]>();
                for (int i = 0; i < vec.size(); i++)
                    fils.add(null);
            }
            if (fils != null)
                fils.add(filler);
            vec.add(read(Operator.LEVEL_MIDDLE));
        }
        if (st.getHint() != 0 || !OP_RPAREN.equals(st.getData()))
            throw new ScannerError(ERROR_SYNTAX_PARENTHESIS_BALANCE,
                    st.getTokenOffset());
        if (level != -1) {
            nextToken();
            filler = getFiller();
        } else {
            filler = null;
        }
        if (filler != null && fils == null) {
            fils = new ListArray<String[]>();
            for (int i = 0; i < vec.size(); i++)
                fils.add(null);
        }
        if (fils != null)
            fils.add(filler);
        Object[] args = new Object[vec.size()];
        vec.toArray(args);
        if (fils != null) {
            String[][] fillers = new String[fils.size()][];
            fils.toArray(fillers);
            help = makeFillers(help, fillers);
        }
        return new SkelCompound(help, args);
    }

    /**
     * <p>Reads an index.</p>
     *
     * @param skel   The indexed object.
     * @param filler The filler.
     * @return The compound.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     * @throws IOException     IO error.
     */
    protected final Object readIndex(Object skel, String[] filler, SkelAtom help)
            throws ScannerError, EngineMessage, EngineException, IOException {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0)
            return super.readIndex(skel, filler, help);
        ListArray<Object> vec = new ListArray<Object>();
        ListArray<String[]> fils = null;
        if (filler != null) {
            fils = new ListArray<String[]>();
            fils.add(filler);
        }
        vec.add(skel);
        filler = getFiller();
        if (filler != null && fils == null) {
            fils = new ListArray<String[]>();
            for (int i = 0; i < vec.size(); i++)
                fils.add(null);
        }
        if (fils != null)
            fils.add(filler);
        vec.add(read(Operator.LEVEL_MIDDLE));
        while (st.getHint() == 0 && OP_COMMA.equals(st.getData())) {
            nextToken();
            filler = getFiller();
            if (filler != null && fils == null) {
                fils = new ListArray<String[]>();
                for (int i = 0; i < vec.size(); i++)
                    fils.add(null);
            }
            if (fils != null)
                fils.add(filler);
            vec.add(read(Operator.LEVEL_MIDDLE));
        }
        Object[] args = new Object[vec.size()];
        vec.toArray(args);
        if (fils != null) {
            String[][] fillers = new String[fils.size()][];
            fils.toArray(fillers);
            help = makeFillers(help, fillers);
        }
        return new SkelCompound(help, args);
    }

    /**
     * <p>Reads an index.</p>
     *
     * @param skel   The indexed object.
     * @param filler The filler.
     * @return The compound.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     * @throws IOException     IO error.
     */
    protected final Object readStruct(Object skel, String[] filler, SkelAtom help)
            throws ScannerError, EngineMessage, EngineException, IOException {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0)
            return super.readStruct(skel, filler, help);
        ListArray<String[]> fils = null;
        if (filler != null) {
            fils = new ListArray<String[]>();
            fils.add(filler);
        }
        filler = getFiller();
        if (filler != null && fils == null) {
            fils = new ListArray<String[]>();
            fils.add(null);
        }
        if (fils != null)
            fils.add(filler);
        Object arg = read(Operator.LEVEL_HIGH);
        if (fils != null) {
            String[][] fillers = new String[fils.size()][];
            fils.toArray(fillers);
            help = makeFillers(help, fillers);
        }
        return new SkelCompound(help, skel, arg);
    }

    /**
     * <p>Factory for annotated atoms.</p>
     * <p>Will preserve the hint of the atom.</p>
     * <p>Can be overridden by a sub class.</p>
     *
     * @param sa The atom skeleton.
     * @param f  The fillers.
     * @return The result.
     */
    protected SkelAtom makeFillers(SkelAtom sa, String[][] f) {
        if (f == null)
            return sa;
        SkelAtomAnno sa2 = new SkelAtomAnno(sa.fun, sa.scope);
        if (sa instanceof SkelAtomAnno)
            sa2.setHint(((SkelAtomAnno) sa).getHint());
        sa2.setFillers(f);
        return sa2;
    }

    /***************************************************************/
    /* Special Numbers                                             */
    /***************************************************************/

    /**
     * <p>Read a number token.</p>
     *
     * @return The term.
     * @throws ScannerError Error and position.
     */
    protected final Object readNumber()
            throws ScannerError, IOException {
        if ((getFlags() & PrologWriter.FLAG_HINT) == 0)
            return super.readNumber();
        SkelAtom help;
        if (Foyer.OP_SUB.equals(st.getData())) {
            nextToken();
            help = new SkelAtom("-" + st.getData());
        } else {
            help = new SkelAtom(st.getData());
        }
        return makeHint(help, CodeType.LINE_ZERO);
    }

    /***************************************************************/
    /* Special Atomics                                             */
    /***************************************************************/

    /**
     * <p>Make a quoted token.</p>
     *
     * @param fun  The fun.
     * @param util The util.
     * @return The token.
     */
    protected final Object makeQuotedByUtil(String fun, int util)
            throws ScannerError {
        if ((getFlags() & PrologWriter.FLAG_HINT) == 0)
            return super.makeQuotedByUtil(fun, util);
        if (util == ReadOpts.UTIL_CODES || util == ReadOpts.UTIL_CHARS) {
            SkelAtom help = new SkelAtom(fun);
            return makeHint(help, st.getHint());
        } else {
            return atomByUtil(fun, util);
        }
    }

    /**
     * <p>Factory for annotated atoms.</p>
     * <p>Can be overridden by a sub class.</p>
     * s     *
     *
     * @param sa The atom skeleton.
     * @param h  The hint.
     * @return The result.
     */
    protected SkelAtom makeHint(SkelAtom sa, int h) {
        if (h == 0)
            return sa;
        SkelAtomAnno sa2 = new SkelAtomAnno(sa.fun, sa.scope);
        sa2.setHint(h);
        return sa2;
    }

    /***************************************************************/
    /* Main API                                                    */
    /***************************************************************/

    /**
     * <p>Fetch the first token.</p>
     *
     * @throws IOException  IO Error.
     * @throws ScannerError Error and position.
     */
    public final void firstToken() throws ScannerError, IOException {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0) {
            super.firstToken();
            return;
        }
        st.setFlags(ScannerToken.MASK_RTRN_ALL);
        st.firstToken();
        skipTokens();
    }

    /**
     * <p>Retrieve the molec token.</p>
     *
     * @throws IOException  IO Error.
     * @throws ScannerError Error and position
     */
    public final void nextToken()
            throws ScannerError, IOException {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0) {
            super.nextToken();
            return;
        }
        st.nextToken();
        skipTokens();
    }

    /**
     * <p>Skip the filler.</p>
     * <p>Line and block comments are kept as is.</p>
     * <p>Fillers are compresssed by to their newlines.</p>
     *
     * @throws IOException  IO Error.
     * @throws ScannerError Error and position.
     */
    private void skipTokens()
            throws ScannerError, IOException {
        rtrn = null;
        for (; ; ) {
            if (st.getHint() != 0)
                break;
            if (st.getData().startsWith(st.getRemark().getLineComment())
                    || st.getData().startsWith(st.getRemark().getBlockCommentStart())) {
                if (rtrn == null)
                    rtrn = new ListArray<String>();
                rtrn.add(st.getData());
                st.nextToken();
                continue;
            }
            if (OP_EOF.equals(st.getData()))
                break;
            String token = st.getData();
            int ch = token.codePointAt(0);
            if (st.getDelemiter().isLayout(ch)) {
                StringBuilder buf = null;
                int k = 0;
                while (k < token.length()) {
                    ch = token.codePointAt(k);
                    if (ch == CodeType.LINE_EOL) {
                        if (buf == null)
                            buf = new StringBuilder();
                        buf.appendCodePoint(ch);
                    }
                    k += Character.charCount(ch);
                }
                if (buf != null) {
                    if (rtrn == null)
                        rtrn = new ListArray<String>();
                    rtrn.add(buf.toString());
                }
                st.nextToken();
                continue;
            }
            break;
        }
    }

    /**
     * <p>Retrieve the molec token.</p>
     *
     * @throws IOException IO Error.
     */
    public final void nextTerminalSuffix()
            throws IOException {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0) {
            super.nextTerminalSuffix();
            return;
        }
        st.nextTerminalSuffix();
        skipTerminalSuffix();
    }

    /**
     * <p>Skip the filler.</p>
     * <p>Line and block comments are kept as is.</p>
     * <p>Fillers are compresssed to their newlines.</p>
     *
     * @throws IOException IO Error.
     */
    private void skipTerminalSuffix()
            throws IOException {
        rtrn = null;
        for (; ; ) {
            if (st.getHint() != 0)
                break;
            if (st.getData().startsWith(st.getRemark().getLineComment())) {
                if (rtrn == null)
                    rtrn = new ListArray<String>();
                rtrn.add(st.getData());
                st.nextTerminalSuffix();
                continue;
            }
            if (OP_EOLN.equals(st.getData())) {
                if (rtrn == null)
                    rtrn = new ListArray<String>();
                rtrn.add(OP_EOLN);
                break;
            }
            break;
        }
    }

    /**
     * <p>Retrieve the current filler.</p>
     *
     * @return The filler.
     */
    protected final String[] getFiller() {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0)
            return super.getFiller();
        if (rtrn == null)
            return null;
        String[] filler = new String[rtrn.size()];
        rtrn.toArray(filler);
        return filler;
    }

    /***************************************************************/
    /* Parse Statement & Internal                                  */
    /***************************************************************/

    /**
     * <p>Parse end of file.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @return The end of file.
     */
    protected final Object makeEof() {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0)
            return super.makeEof();
        if ((flags & FLAG_TEOF) != 0)
            return null;
        String[] filler = getFiller();
        SkelAtom help = makePos(AbstractSource.OP_END_OF_FILE, getAtomPos());
        if (filler != null)
            help = makeFillers(help, new String[][]{filler});
        return help;
    }

    /**
     * <p>Parse a period.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @return The period.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected final Object parsePeriodIncomplete()
            throws ScannerError, EngineMessage, EngineException, IOException {
        if ((getFlags() & PrologWriter.FLAG_FILL) == 0)
            return super.parsePeriodIncomplete();
        String[] filler = getFiller();
        Object jack = parseIncomplete();
        String[] filler2 = getFiller();
        SkelAtom help = new SkelAtom(Foyer.OP_CONS);
        if (filler != null || filler2 != null)
            help = makeFillers(help, new String[][]{filler, filler2});
        return new SkelCompound(help, jack);
    }

}
