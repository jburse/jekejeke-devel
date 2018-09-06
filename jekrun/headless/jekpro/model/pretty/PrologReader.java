package jekpro.model.pretty;

import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.molec.OperatorSearch;
import jekpro.model.rope.Named;
import jekpro.model.rope.Operator;
import jekpro.reference.arithmetic.EvaluableElem;
import jekpro.reference.structure.ForeignAtom;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.ListArray;
import matula.util.data.MapHashLink;
import matula.util.regex.*;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

/**
 * <p>This class provides the operator based reading of prolog
 * terms. Only the skeleton of a term is read.</p>
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
public class PrologReader {
    public final static int REP_CHARS = 0;
    public final static int REP_CODES = 1;

    public final static String OP_ANON = "_";
    public final static String OP_EOF = "";
    public final static String OP_EOLN = "\n";
    public final static String OP_PERIOD = ".";
    public final static String OP_COMMA = ",";
    public final static String OP_BAR = "|";
    public final static String OP_LPAREN = "(";
    public final static String OP_RPAREN = ")";
    public final static String OP_LBRACE = "{";
    public final static String OP_RBRACE = "}";
    public final static String OP_LBRACKET = "[";
    public final static String OP_RBRACKET = "]";

    /* only read opts */
    public final static int FLAG_SING = 0x00001000;
    public final static int FLAG_NEWV = 0x00002000;

    public final static String ERROR_SYNTAX_PARENTHESIS_BALANCE = "parenthesis_balance";
    public final static String ERROR_SYNTAX_CANNOT_START_TERM = "cannot_start_term"; /* SWI */
    public final static String ERROR_SYNTAX_BRACE_BALANCE = "brace_balance";
    public final static String ERROR_SYNTAX_BRACKET_BALANCE = "bracket_balance";
    public final static String ERROR_SYNTAX_OPERATOR_CLASH = "operator_clash"; /* SWI */

    private final static String[] noTermTempls = {OP_EOF, OP_PERIOD,
            OP_RPAREN, OP_RBRACE, OP_RBRACKET};
    private final static String[] noOperTempls = {OP_COMMA, OP_BAR};

    private Engine engine;
    protected AbstractSource source;
    public final ScannerToken st = new ScannerToken();
    private MapHashLink<String, SkelVar> vars; /* input order */
    private MapHashLink<String, SkelVar> anon; /* input order */
    private int gensym;
    private byte utildouble = ReadOpts.UTIL_CODES;
    private byte utilback = ReadOpts.UTIL_ERROR;
    private byte utilsingle = ReadOpts.UTIL_ATOM;
    private int flags;
    private int clausestart;

    /**
     * Construct a reader.
     */
    public PrologReader() {
        st.setDelemiter(CodeType.ISO_CODETYPE);
        st.setRemark(CompLang.ISO_COMPLANG);
    }

    /**
     * <p>Set the store.</p>
     *
     * @param en The engine.
     */
    public void setEngineRaw(Engine en) {
        engine = en;
    }

    /**
     * <p>Set the source.</p>
     *
     * @param s The source.
     */
    public void setSource(AbstractSource s) {
        source = s;
    }

    /**
     * <p>Retrieve the source.</p>
     *
     * @return The source.
     */
    public AbstractSource getSource() {
        return source;
    }

    /**
     * <p>Retrieve the scanner.</p>
     *
     * @return The scanner.
     */
    public ScannerToken getScanner() {
        return st;
    }

    /**
     * <p>Retrieve the clause lineno.</p>
     *
     * @return The clause lineno.
     */
    public int getClauseStart() {
        return clausestart;
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
     * @param u The back quotes utilization.
     */
    public void setUtilSingle(int u) {
        utilsingle = (byte) u;
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
     * <p>Set the flags.</p>
     *
     * @param f The flags.
     */
    public void setFlags(int f) {
        flags = f;
    }

    /**
     * <p>Retrieve the gensym.</p>
     *
     * @return The gensym.
     */
    public int getGensym() {
        return gensym;
    }

    /**
     * <p>Set the gensym.</p>
     *
     * @param g The gensym.
     */
    public void setGensym(int g) {
        gensym = g;
    }

    /**
     * <p>Retrieve the variable names.</p>
     *
     * @return The variable names.
     */
    public MapHashLink<String, SkelVar> getVars() {
        return vars;
    }

    /**
     * <p>Set the variable names.</p>
     *
     * @param a The variable names.
     */
    public void setVars(MapHashLink<String, SkelVar> a) {
        vars = a;
    }

    /**
     * <p>Retrieve the singletons.</p>
     *
     * @return The singletons.
     */
    public MapHashLink<String, SkelVar> getAnon() {
        return anon;
    }

    /**
     * <p>Set the singletons.</p>
     *
     * @param f The singletons.
     */
    public void setAnon(MapHashLink<String, SkelVar> f) {
        anon = f;
    }

    /**
     * <p>Set the util flags to the stor util flags.</p>
     *
     * @param store The store.
     */
    public void setReadUtil(AbstractStore store) {
        utildouble = (byte) store.foyer.getUtilDouble();
        utilback = (byte) store.foyer.getUtilBack();
        utilsingle = (byte) store.foyer.getUtilSingle();
    }

    /*******************************************************************/
    /* Read Method                                                     */
    /*******************************************************************/

    /**
     * <p>Read a term.</p>
     *
     * @param level The term level to respect.
     * @return The term.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    public Object read(int level)
            throws ScannerError, EngineMessage, EngineException, IOException {
        if (isTemplates(noTermTempls) || isTemplates(noOperTempls))
            throw new ScannerError(ERROR_SYNTAX_CANNOT_START_TERM,
                    st.getTokenOffset());
        int current = 0;
        Object skel;
        int h;
        if (st.getHint() != 0) {
            skel = readQuotedByUtil();
            if (!(skel instanceof SkelAtom) || ((SkelAtom) skel).getHint() != 0) {
                nextToken();
                h = -1;
            } else {
                h = 0;
            }
        } else if (OP_LPAREN.equals(st.getData())) {
            nextToken();
            skel = read(Operator.LEVEL_HIGH);
            if (st.getHint() != 0 || !OP_RPAREN.equals(st.getData()))
                throw new ScannerError(ERROR_SYNTAX_PARENTHESIS_BALANCE,
                        st.getTokenOffset());
            nextToken();
            h = -1;
        } else if (OP_LBRACE.equals(st.getData())) {
            nextToken();
            if (st.getHint() == 0 && OP_RBRACE.equals(st.getData())) {
                skel = makeAtom(Foyer.OP_SET);
                h = 0;
            } else {
                Object temp = read(Operator.LEVEL_HIGH);
                if (st.getHint() != 0 || !OP_RBRACE.equals(st.getData()))
                    throw new ScannerError(ERROR_SYNTAX_BRACE_BALANCE,
                            st.getTokenOffset());
                nextToken();
                skel = new SkelCompound(makeAtom(Foyer.OP_SET), temp);
                h = -1;
            }
        } else if (OP_LBRACKET.equals(st.getData())) {
            nextToken();
            if (st.getHint() == 0 && OP_RBRACKET.equals(st.getData())) {
                skel = makeAtom(Foyer.OP_NIL);
                h = 0;
            } else {
                skel = readList();
                if (st.getHint() != 0 || !OP_RBRACKET.equals(st.getData()))
                    throw new ScannerError(ERROR_SYNTAX_BRACKET_BALANCE,
                            st.getTokenOffset());
                nextToken();
                h = -1;
            }
        } else {
            h = st.getData().codePointAt(0);
            if (CodeType.ISO_CODETYPE.isUpper(h) || CodeType.ISO_CODETYPE.isUnderscore(h)) {
                skel = atomToVariable(st.getData());
                nextToken();
                h = -1;
            } else if (Character.isDigit(h) ||
                    (Foyer.OP_SUB.equals(st.getData()) && Character.isDigit(st.lookAhead()))) {
                skel = readNumber();
                nextToken();
                h = -1;
            } else if (CodeType.ISO_CODETYPE.isValid(h)) {
                skel = makeAtom(st.getData());
            } else {
                throw new ScannerError(CompLang.OP_SYNTAX_ILLEGAL_UNICODE,
                        st.getTokenOffset());
            }
        }
        if (h != -1) {
            if (st.lookAhead() == OP_LPAREN.codePointAt(0)) {
                nextToken();
                nextToken();
                skel = readCompound((SkelAtom) skel);
                if (st.getHint() != 0 || !OP_RPAREN.equals(st.getData()))
                    throw new ScannerError(ERROR_SYNTAX_PARENTHESIS_BALANCE,
                            st.getTokenOffset());
                nextToken();
            } else {
                nextToken();
                /* ISO 6.3.3.1 */
                if (isTemplates(noTermTempls) || isTemplates(noOperTempls)) {
                    /* */
                } else {
                    SkelAtom help = (SkelAtom) skel;
                    Operator op = engine != null ? OperatorSearch.getOper(help.scope,
                            help.fun, Operator.TYPE_PREFIX, engine) : null;
                    if (op != null) {
                        if (level < op.getLevel())
                            throw new ScannerError(ERROR_SYNTAX_OPERATOR_CLASH,
                                    st.getTokenOffset());
                        Object jill = read(op.getLevel() - op.getRight());
                        skel = new SkelCompound(help, jill);
                        current = op.getLevel();
                    } else {
                        skel = help;
                    }
                }
            }
        }
        for (; ; ) {
            if (isTemplates(noTermTempls) || isTemplate(OP_LPAREN))
                break;
            String fun;
            if (st.getHint() != 0) {
                int util = getUtilByQuote(st.getHint());
                if (util != ReadOpts.UTIL_ATOM)
                    break;
                fun = readQuoted();
            } else if (OP_LBRACKET.equals(st.getData())) {
                fun = Foyer.OP_INDEX;
            } else if (OP_LBRACE.equals(st.getData())) {
                fun = Foyer.OP_STRUCT;
            } else {
                h = st.getData().codePointAt(0);
                if (CodeType.ISO_CODETYPE.isUpper(h) || CodeType.ISO_CODETYPE.isUnderscore(h)) {
                    break;
                } else if (Character.isDigit(h)) {
                    break;
                } else if (CodeType.ISO_CODETYPE.isValid(h)) {
                    fun = st.getData();
                } else {
                    throw new ScannerError(CompLang.OP_SYNTAX_ILLEGAL_UNICODE,
                            st.getTokenOffset());
                }
            }
            Operator op = engine != null ? OperatorSearch.getOper(source,
                    fun, Operator.TYPE_INFIX, engine) : null;
            if (op != null && level >= op.getLevel()) {
                if (op.getLevel() - op.getLeft() < current)
                    throw new ScannerError(ERROR_SYNTAX_OPERATOR_CLASH,
                            st.getTokenOffset());
                skel = readInfix(makeAtom(fun), skel, op);
                current = op.getLevel();
                continue;
            }
            op = engine != null ? OperatorSearch.getOper(source,
                    fun, Operator.TYPE_POSTFIX, engine) : null;
            if (op != null && level >= op.getLevel()) {
                if (op.getLevel() - op.getLeft() < current)
                    throw new ScannerError(ERROR_SYNTAX_OPERATOR_CLASH,
                            st.getTokenOffset());
                skel = readPostfix(makeAtom(fun), skel);
                current = op.getLevel();
                continue;
            }
            break;
        }
        return skel;
    }

    /**
     * <p>Check whether the given token matches a template.</p>
     *
     * @param templ The template.
     * @return True if the token matches the template, otherwise false.
     * @throws ScannerError Scanning problem.
     */
    private boolean isTemplate(String templ) throws ScannerError, IOException {
        if (st.getHint() != 0 || !st.getData().equals(templ))
            return false;
        if (OP_PERIOD.equals(templ) && !st.isTerminalSuffix())
            return false;
        return true;
    }

    /**
     * <p>Check whether the given token matches one of the templates.</p>
     *
     * @param templs The templates.
     * @return True if the token matches one of the templates, otherwise false.
     * @throws ScannerError Scanning problem.
     */
    private boolean isTemplates(String[] templs) throws ScannerError, IOException {
        for (int i = 0; i < templs.length; i++) {
            if (isTemplate(templs[i]))
                return true;
        }
        return false;
    }

    /*********************************************************************/
    /* Special Operators                                                 */
    /*********************************************************************/

    /**
     * <p>Reads an infix.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param help The functor.
     * @param skel The left argument.
     * @param op   The operator.
     * @return The infix.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   IO Error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected Object readInfix(SkelAtom help, Object skel, Operator op)
            throws ScannerError, EngineMessage, EngineException, IOException {
        nextOperator();
        Object jack = read(op.getLevel() - op.getRight());
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
     * @throws EngineMessage   IO Error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected Object readPostfix(SkelAtom help, Object skel)
            throws ScannerError, IOException, EngineException, EngineMessage {
        if (st.getHint() == 0 && OP_LBRACKET.equals(st.getData())) {
            nextToken();
            if (st.getHint() == 0 && OP_RBRACKET.equals(st.getData())) {
                skel = new SkelCompound(help, skel);
                nextToken();
            } else {
                skel = readIndex(skel, null, help);
                if (st.getHint() != 0 || !OP_RBRACKET.equals(st.getData()))
                    throw new ScannerError(ERROR_SYNTAX_BRACKET_BALANCE,
                            st.getTokenOffset());
                nextToken();
            }
            return skel;
        } else if (st.getHint() == 0 && OP_LBRACE.equals(st.getData())) {
            nextToken();
            if (st.getHint() == 0 && OP_RBRACE.equals(st.getData())) {
                skel = new SkelCompound(help, skel);
                nextToken();
            } else {
                skel = readStruct(skel, null, help);
                if (st.getHint() != 0 || !OP_RBRACE.equals(st.getData()))
                    throw new ScannerError(ERROR_SYNTAX_BRACE_BALANCE,
                            st.getTokenOffset());
                nextToken();
            }
            return skel;
        } else {
            skel = new SkelCompound(help, skel);
            nextOperator();
            return skel;
        }
    }

    /**
     * <p>Advance the operator.</p>
     *
     * @throws ScannerError Error and position.
     * @throws IOException  IO Error.
     */
    protected final void nextOperator()
            throws ScannerError, IOException {
        if (st.getHint() == 0 && OP_LBRACKET.equals(st.getData())) {
            nextToken();
            if (st.getHint() == 0 && OP_RBRACKET.equals(st.getData())) {
                nextToken();
            } else {
                throw new ScannerError(ERROR_SYNTAX_BRACKET_BALANCE,
                        st.getTokenOffset());
            }
        } else if (st.getHint() == 0 && OP_LBRACE.equals(st.getData())) {
            nextToken();
            if (st.getHint() == 0 && OP_RBRACE.equals(st.getData())) {
                nextToken();
            } else {
                throw new ScannerError(ERROR_SYNTAX_BRACE_BALANCE,
                        st.getTokenOffset());
            }
        } else {
            nextToken();
        }
    }

    /*********************************************************************/
    /* Special Compounds                                                 */
    /*********************************************************************/

    /**
     * <p>Reads a list.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @return The list.
     * @throws ScannerError    Error and position.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected Object readList()
            throws ScannerError, EngineMessage, EngineException, IOException {
        SkelAtom help = makeAtom(Foyer.OP_CONS);
        Object[] args = new Object[2];
        args[0] = read(Operator.LEVEL_MIDDLE);
        args[1] = null;
        SkelCompound back = new SkelCompound(help, args, null);
        Object t;
        for (; ; ) {
            if (st.getHint() == 0 && OP_COMMA.equals(st.getData())) {
                nextToken();
                help = makeAtom(Foyer.OP_CONS);
                args = new Object[2];
                args[0] = read(Operator.LEVEL_MIDDLE);
                args[1] = back;
                back = new SkelCompound(help, args, null);
            } else if (st.getHint() == 0 && OP_BAR.equals(st.getData())) {
                nextToken();
                t = read(Operator.LEVEL_MIDDLE);
                break;
            } else {
                t = makeAtom(Foyer.OP_NIL);
                break;
            }
        }
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
     * <p>Reads a compound.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param help The functor.
     * @return The compound.
     * @throws ScannerError    Error and position.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected Object readCompound(SkelAtom help)
            throws ScannerError, EngineMessage, EngineException, IOException {
        ListArray<Object> vec = new ListArray<Object>();
        vec.add(read(Operator.LEVEL_MIDDLE));
        while (st.getHint() == 0 && OP_COMMA.equals(st.getData())) {
            nextToken();
            vec.add(read(Operator.LEVEL_MIDDLE));
        }
        Object[] args = new Object[vec.size()];
        vec.toArray(args);
        return new SkelCompound(help, args);
    }


    /**
     * <p>Reads an array index.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param skel   The indexed object.
     * @param filler The filler.
     * @param help   The functor.
     * @return The compound.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     * @throws IOException     IO error.
     */
    protected Object readIndex(Object skel, String[] filler, SkelAtom help)
            throws ScannerError, EngineMessage, EngineException, IOException {
        ListArray<Object> vec = new ListArray<Object>();
        vec.add(skel);
        vec.add(read(Operator.LEVEL_MIDDLE));
        while (st.getHint() == 0 && OP_COMMA.equals(st.getData())) {
            nextToken();
            vec.add(read(Operator.LEVEL_MIDDLE));
        }
        Object[] args = new Object[vec.size()];
        vec.toArray(args);
        return new SkelCompound(help, args);
    }

    /**
     * <p>Reads a struct.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param skel   The indexed object.
     * @param filler The filler.
     * @param help   The functor.
     * @return The compound.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     * @throws IOException     IO error.
     */
    protected Object readStruct(Object skel, String[] filler, SkelAtom help)
            throws ScannerError, EngineMessage, EngineException, IOException {
        Object arg = read(Operator.LEVEL_HIGH);
        return new SkelCompound(help, skel, arg);
    }

    /***************************************************************/
    /* Special Numbers                                             */
    /***************************************************************/

    /**
     * <p>Read a number token.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @return The term.
     * @throws ScannerError Error and position.
     */
    protected Object readNumber()
            throws ScannerError, IOException {
        Number num;
        if (st.getHint() == 0 && Foyer.OP_SUB.equals(st.getData())) {
            nextToken();
            num = ForeignAtom.toNumber(st.getData(), st.getTokenOffset(),
                    ForeignAtom.MASK_NUMB_USCR);
            num = EvaluableElem.neg(num);
        } else {
            num = ForeignAtom.toNumber(st.getData(), st.getTokenOffset(),
                    ForeignAtom.MASK_NUMB_USCR);
        }
        return num;
    }

    /***************************************************************/
    /* Special Atomics                                             */
    /***************************************************************/

    /**
     * <p>Read a quoted token.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @return The term.
     * @throws ScannerError Error and position.
     */
    protected Object readQuotedByUtil()
            throws ScannerError {
        int util = getUtilByQuote(st.getHint());
        String fun = readQuoted();
        return atomByUtil(fun, util);
    }

    /**
     * <p>Retrieve the util by quote.</p>
     *
     * @param quote The quote.
     * @return The util.
     */
    public final int getUtilByQuote(int quote) {
        switch (quote) {
            case CodeType.LINE_SINGLE:
                return utilsingle;
            case CodeType.LINE_DOUBLE:
                return utildouble;
            case CodeType.LINE_BACK:
                return utilback;
            default:
                throw new IllegalArgumentException("illegal quote");
        }
    }

    /**
     * <p>Convert an atom by util.</p>
     *
     * @param s The atom.
     * @param u The util.
     * @return The converted atom.
     */
    public final Object atomByUtil(String s, int u) throws ScannerError {
        switch (u) {
            case ReadOpts.UTIL_ERROR:
                throw new ScannerError(ERROR_SYNTAX_CANNOT_START_TERM, st.getTokenOffset());
            case ReadOpts.UTIL_CODES:
                return atomToList(s, REP_CODES);
            case ReadOpts.UTIL_CHARS:
                return atomToList(s, REP_CHARS);
            case ReadOpts.UTIL_ATOM:
                return makeAtom(s);
            case ReadOpts.UTIL_VARIABLE:
                return atomToVariable(s);
            default:
                throw new IllegalArgumentException("illegal util");
        }
    }

    /**
     * <p>Convert a string to a code list.</p>
     *
     * @param s   The atom.
     * @param rep The representation.
     * @return The code list.
     */
    private Object atomToList(String s, int rep) {
        Object res = makeAtom(Foyer.OP_NIL);
        if (s.length() == 0)
            return res;
        SkelAtom help = makeAtom(Foyer.OP_CONS);
        for (int i = s.length(); i > 0; i--) {
            int ch = s.codePointBefore(i);
            Object val;
            switch (rep) {
                case PrologReader.REP_CHARS:
                    val = new SkelAtom(SkelAtom.valueOf(ch));
                    break;
                case PrologReader.REP_CODES:
                    val = Integer.valueOf(ch);
                    break;
                default:
                    throw new IllegalArgumentException("illegal representation");
            }
            res = new SkelCompound(help, val, res);
            i -= Character.charCount(ch) - 1;
        }
        return res;
    }

    /**
     * <p>Convert an atom to a variable.</p>
     *
     * @param key The atom.
     * @return The variable.
     */
    public SkelVar atomToVariable(String key) {
        SkelVar mv;
        if (OP_ANON.equals(key)) {
            mv = new SkelVar(gensym);
            gensym++;
            return mv;
        }
        if (vars == null) {
            vars = new MapHashLink<String, SkelVar>();
            mv = null;
        } else {
            mv = vars.get(key);
        }
        if (mv == null) {
            if ((flags & PrologReader.FLAG_NEWV) != 0) {
                mv = new SkelVar(gensym);
            } else {
                mv = SkelVar.valueOf(gensym);
            }
            gensym++;
            vars.add(key, mv);
            if ((flags & FLAG_SING) != 0) {
                if (anon == null)
                    anon = new MapHashLink<String, SkelVar>();
                anon.add(key, mv);
            }
        } else {
            if ((flags & FLAG_SING) != 0) {
                if (anon != null) {
                    anon.remove(key);
                    if (anon.size == 0)
                        anon = null;
                }
            }
        }
        return mv;
    }

    /**
     * <p>Read a quoted token of type atom.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @return The string, or null.
     * @throws ScannerError Error and position.
     */
    protected final String readQuoted()
            throws ScannerError {
        return CompLang.resolveEscape(
                CodeType.ISO_CODETYPE.resolveDouble(st.getData(),
                        st.getHint(), st.getTokenOffset()),
                st.getHint(), true, st.getTokenOffset(), CodeType.ISO_CODETYPE);
    }

    /**
     * <p>Create an atom to the current source.</p>
     * <p>Can be overridden by a sub class.</p>
     *
     * @param f The name.
     * @return The atom.
     */
    protected SkelAtom makeAtom(String f) {
        return new SkelAtom(f, source);
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
    public void firstToken() throws ScannerError, IOException {
        Reader lr = st.getReader();
        clausestart = OpenOpts.getLineNumber(lr);
        st.setFlags(0);
        st.firstToken();
    }

    /**
     * <p>Retrieve the molec token.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @throws IOException  IO Error.
     * @throws ScannerError Error and position
     */
    protected void nextToken() throws ScannerError, IOException {
        st.nextToken();
    }

    /**
     * <p>Retrieve the molec token.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @throws IOException IO Error.
     */
    protected void nextTerminalSuffix()
            throws IOException {
        st.nextTerminalSuffix();
    }

    /***************************************************************/
    /* Parse Statement & Internal                                  */
    /***************************************************************/

    /**
     * <p>Convenience method that will read a term.</p>
     * <p>The line will not be completed.</p>
     * <p>Returns end_of_file upon eof.</p>
     *
     * @return The term skeleton.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    public final Object parseHeadStatement()
            throws ScannerError, EngineMessage, EngineException {
        try {
            firstToken();
            Reader lr = st.getReader();
            clausestart = OpenOpts.getLineNumber(lr);
            setVars(null);
            setAnon(null);
            setGensym(0);
            if (st.getHint() == 0 && OP_EOF.equals(st.getData()))
                return makeEof();
            if ((getFlags() & PrologWriter.FLAG_MKDT) != 0)
                return parsePeriodIncomplete(OP_PERIOD);
            return parseIncomplete(OP_PERIOD);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Parse end of file.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @return The end of file.
     */
    protected Object makeEof() {
        return new SkelAtom(AbstractSource.OP_END_OF_FILE);
    }

    /**
     * <p>Convenience method that will read a term.</p>
     * <p>Returns null upon eof.</p>
     *
     * @return The term skeleton.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    public final Object parseHeadInternal()
            throws ScannerError, EngineMessage, EngineException {
        try {
            firstToken();
            Reader lr = st.getReader();
            clausestart = OpenOpts.getLineNumber(lr);
            setVars(null);
            setAnon(null);
            setGensym(0);
            if (st.getHint() == 0 && OP_EOF.equals(st.getData()))
                return null;
            if ((getFlags() & PrologWriter.FLAG_MKDT) != 0)
                return parsePeriodIncomplete(OP_EOF);
            return parseIncomplete(OP_EOF);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Convenience method that will read a term.</p>
     * <p>Complete the line.</p>
     *
     * @param templ The template.
     * @param e     The current error or null.
     * @throws EngineMessage Shit happens.
     */
    public final void parseTailError(String templ, ScannerError e)
            throws EngineMessage {
        if (e != null && (ScannerToken.OP_SYNTAX_END_OF_LINE_IN_STRING.equals(e.getError()) ||
                ScannerToken.OP_SYNTAX_END_OF_LINE_IN_CHARACTER.equals(e.getError()) ||
                ScannerToken.OP_SYNTAX_CONT_ESC_IN_CHARACTER.equals(e.getError())))
            return;
        try {
            while (!isTemplate(templ) && (st.getHint() != 0 || !OP_EOF.equals(st.getData())))
                nextToken();
            while (st.getHint() != 0 || (!OP_EOLN.equals(st.getData()) && !OP_EOF.equals(st.getData())))
                nextTerminalSuffix();
            if (st.getHint() != 0 || !OP_EOLN.equals(st.getData()))
                st.pushBack();
        } catch (ScannerError z) {
            /* do nothing */
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Parse a term and wrap with period.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param templ The template.
     * @return term wrapped in a period.
     * @throws ScannerError    Error and position.
     * @throws IOException     IO error.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    protected Object parsePeriodIncomplete(String templ)
            throws ScannerError, EngineMessage, EngineException, IOException {
        Object jack = parseIncomplete(templ);
        SkelAtom help = new SkelAtom(Foyer.OP_CONS);
        return new SkelCompound(help, jack);
    }

    /**
     * <p>Parse a term.</p>
     * <p>To allow retrieval of error position, the tail is not parsed
     * if the main read fails. In this case parseTailError() needs to
     * be additionally called to sync the parser with alleged end of
     * the clause.</p>
     *
     * @param templ The template.
     * @return The term.
     * @throws ScannerError    Error and position.
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    public final Object parseIncomplete(String templ)
            throws ScannerError, EngineException, EngineMessage, IOException {
        Object jack = read(Operator.LEVEL_HIGH);
        if (!isTemplate(templ))
            throw new ScannerError(AbstractCompiler.ERROR_SYNTAX_END_OF_CLAUSE_EXPECTED,
                    st.getTokenOffset());
        parseTailError(PrologReader.OP_PERIOD, null);
        return jack;
    }

    /***************************************************************/
    /* Validation Helpers                                          */
    /***************************************************************/

    /**
     * <p>Check whether the object is a text input stream.</p>
     *
     * @param obj The object.
     * @throws EngineMessage Shit happens.
     */
    public static void checkTextRead(Object obj) throws EngineMessage {
        if (!(obj instanceof Reader)) {
            if (!(obj instanceof InputStream)) {
                throw new EngineMessage(EngineMessage.permissionError(
                        EngineMessage.OP_PERMISSION_INPUT,
                        EngineMessage.OP_PERMISSION_STREAM, obj));
            } else {
                throw new EngineMessage(EngineMessage.permissionError(
                        EngineMessage.OP_PERMISSION_INPUT,
                        EngineMessage.OP_PERMISSION_BINARY_STREAM, obj));
            }
        }
    }

    /**
     * <p>Check whether the object is a binary input stream.</p>
     *
     * @param obj The object.
     * @throws EngineMessage Shit happens.
     */
    public static void checkBinaryRead(Object obj) throws EngineMessage {
        if (!(obj instanceof InputStream)) {
            if (!(obj instanceof Reader)) {
                throw new EngineMessage(EngineMessage.permissionError(
                        EngineMessage.OP_PERMISSION_INPUT,
                        EngineMessage.OP_PERMISSION_STREAM, obj));
            } else {
                throw new EngineMessage(EngineMessage.permissionError(
                        EngineMessage.OP_PERMISSION_INPUT,
                        EngineMessage.OP_PERMISSION_TEXT_STREAM, obj));
            }
        }
    }

    /********************************************************************/
    /* Style Checks                                                     */
    /********************************************************************/

    /**
     * <p>Perform the singletons style check.</p>
     *
     * @param anon The anonymous variables.
     * @param pos  The position.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void checkSingleton(Named[] anon,
                                      PositionKey pos,
                                      Engine en)
            throws EngineMessage, EngineException {
        try {
            if (anon != null && anon.length > 0) {
                Object val = singToMolec(anon, en.store);
                throw new EngineMessage(EngineMessage.syntaxError(
                        EngineMessage.OP_SYNTAX_SINGLETON_VAR, val));
            }
        } catch (EngineMessage x) {
            EngineException y = new EngineException(x,
                    EngineException.fetchLoc(EngineException.fetchStack(en),
                            pos, en), EngineException.OP_WARNING
            );
            y.printStackTrace(en);
        }
    }

    /**
     * <p>Create a list of the singletons.</p>
     *
     * @param anon  The anonymous variables.
     * @param store The store.
     * @return The list of the singletons names.
     */
    private static Object singToMolec(Named[] anon, AbstractStore store) {
        Object end = store.foyer.ATOM_NIL;
        for (int i = anon.length - 1; i >= 0; i--)
            end = new SkelCompound(store.foyer.ATOM_CONS,
                    new SkelAtom(anon[i].getName()), end);
        return end;
    }

}
