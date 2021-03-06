package jekpro.model.pretty;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.builtin.Flag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Operator;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.SpecialOper;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>This class provides read options.</p>
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
public final class ReadOpts {
    public final static String OP_VARIABLES = "variables";
    public final static String OP_VARIABLE_NAMES = "variable_names";
    private final static String OP_SINGLETONS = "singletons";
    final static String OP_SOURCE = "source";
    final static String OP_ANNOTATION = "annotation";
    public final static String OP_LINE_NO = "line_no";

    public final static String OP_VALUE_ERROR = "error";
    private final static String OP_VALUE_CODES = "codes";
    private final static String OP_VALUE_CHARS = "chars";
    private final static String OP_VALUE_VARIABLE = "variable";
    private final static String OP_VALUE_ATOM = "atom";
    private final static String OP_VALUE_STRING = "string";

    public final static int UTIL_ERROR = 0;
    public final static int UTIL_CODES = 1;
    public final static int UTIL_CHARS = 2;
    public final static int UTIL_VARIABLE = 3;
    public final static int UTIL_ATOM = 4;
    public final static int UTIL_STRING = 5;

    final static String OP_TERMINATOR = "terminator";
    private final static String OP_TERMINATOR_PERIOD = "period";
    private final static String OP_TERMINATOR_END_OF_FILE = "end_of_file";
    public final static String OP_TERMINATOR_NONE = "none";

    private final static int TERMINATOR_EOF = 0x00000001;
    private final static int TERMINATOR_NONE = 0x00000002;

    public int flags;
    public int lev = Operator.LEVEL_HIGH;
    public byte utildouble;
    public byte utilback;
    public byte utilsingle;
    public AbstractSource source;

    /***************************************************************/
    /* Read Options                                                */
    /***************************************************************/

    /**
     * <p>Create some read options.</p>
     *
     * @param s The source.
     * @param f The flags.
     */
    private ReadOpts(AbstractSource s, int f) {
        source = s;
        utildouble = s.utildouble;
        utilback = s.utilback;
        utilsingle = s.utilsingle;
        flags = f;
    }

    /**
     * <p>Decode the given read parameters.</p>
     *
     * @param t     The parameters skeleton.
     * @param d     The parameters display.
     * @param flags The default flags.
     * @throws EngineMessage Shit happens.
     */
    public static ReadOpts decodeReadParameter(Object t, Display d,
                                               int flags, Engine en)
            throws EngineMessage {
        ReadOpts res = null;
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        while (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONS)) {
            Object[] sc = ((SkelCompound) t).args;
            en.skel = sc[0];
            en.deref();
            if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_VARIABLES)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_VARIABLE_NAMES)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_SINGLETONS)) {
                if (res == null)
                    res = new ReadOpts(en.visor.peekStack(), flags);
                res.flags |= PrologReader.FLAG_SING;
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_DOUBLE_QUOTES)) {
                if (res == null)
                    res = new ReadOpts(en.visor.peekStack(), flags);
                res.utildouble = (byte) ReadOpts.atomToUtil(((SkelCompound) en.skel).args[0], en.display);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_BACK_QUOTES)) {
                if (res == null)
                    res = new ReadOpts(en.visor.peekStack(), flags);
                res.utilback = (byte) ReadOpts.atomToUtil(((SkelCompound) en.skel).args[0], en.display);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_SINGLE_QUOTES)) {
                if (res == null)
                    res = new ReadOpts(en.visor.peekStack(), flags);
                res.utilsingle = (byte) ReadOpts.atomToUtil(((SkelCompound) en.skel).args[0], en.display);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(WriteOpts.OP_PRIORITY)) {
                Number num = SpecialEval.derefAndCastInteger(((SkelCompound) en.skel).args[0], en.display);
                SpecialEval.checkNotLessThanZero(num);
                int k = SpecialEval.castIntValue(num);
                SpecialOper.checkOperatorLevel(k);
                if (res == null)
                    res = new ReadOpts(en.visor.peekStack(), flags);
                res.lev = k;
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_ANNOTATION)) {
                int anno = WriteOpts.termToAnno(((SkelCompound) en.skel).args[0], en.display, en);
                if (res == null)
                    res = new ReadOpts(en.visor.peekStack(), flags);
                if ((anno & WriteOpts.ANNO_MKDT) != 0) {
                    res.flags |= PrologWriter.FLAG_MKDT;
                } else {
                    res.flags &= ~PrologWriter.FLAG_MKDT;
                }
                if ((anno & WriteOpts.ANNO_FILL) != 0) {
                    res.flags |= PrologWriter.FLAG_FILL;
                } else {
                    res.flags &= ~PrologWriter.FLAG_FILL;
                }
                if ((anno & WriteOpts.ANNO_HINT) != 0) {
                    res.flags |= PrologWriter.FLAG_HINT;
                } else {
                    res.flags &= ~PrologWriter.FLAG_HINT;
                }
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_TERMINATOR)) {
                int terminator = atomToTerminator(((SkelCompound) en.skel).args[0], en.display);
                if (res == null)
                    res = new ReadOpts(en.visor.peekStack(), flags);
                if ((terminator & TERMINATOR_EOF) != 0) {
                    res.flags |= PrologReader.FLAG_TEOF;
                } else {
                    res.flags &= ~PrologReader.FLAG_TEOF;
                }
                if ((terminator & TERMINATOR_NONE) != 0) {
                    res.flags |= PrologReader.FLAG_TNON;
                } else {
                    res.flags &= ~PrologReader.FLAG_TNON;
                }
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_SOURCE)) {
                SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(
                        ((SkelCompound) en.skel).args[0], en.display);
                AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                src = src.getStore().getSource(sa.fun);
                AbstractSource.checkExistentSource(src, sa);
                if (res == null)
                    res = new ReadOpts(en.visor.peekStack(), flags);
                res.source = src;
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_LINE_NO)) {
                /* do nothing */
            } else {
                EngineMessage.checkInstantiated(en.skel);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_READ_OPTION,
                        en.skel), en.display);
            }
            en.skel = sc[1];
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
        }
        if (t instanceof SkelAtom &&
                ((SkelAtom) t).fun.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_LIST, t), d);
        }
        return res;
    }

    /**
     * <p>Set the read options.</p>
     *
     * @param pr The Prolog reader.
     */
    public void setReadOpts(PrologReader pr) {
        pr.setSource(source);
        pr.setUtilDouble(utildouble);
        pr.setUtilBack(utilback);
        pr.setUtilSingle(utilsingle);
        pr.setFlags(flags);
        pr.setLevel(lev);
    }

    /**
     * <p>Decode the given read options.</p>
     *
     * @param t  The options skel.
     * @param d  The options display.
     * @param t2 The read term skel.
     * @param d2 The read term display.
     * @param en The engine.
     * @return True if the options could be unified.
     * @throws EngineException Auto load problem.
     */
    public static boolean decodeReadResults(Object t, Display d,
                                            Object t2, Display d2,
                                            Engine en,
                                            PrologReader rd)
            throws EngineException {
        en.skel = t;
        en.display = d;
        en.deref();
        while (en.skel instanceof SkelCompound &&
                ((SkelCompound) en.skel).args.length == 2 &&
                ((SkelCompound) en.skel).sym.fun.equals(Foyer.OP_CONS)) {
            Object[] mc = ((SkelCompound) en.skel).args;
            d = en.display;
            en.skel = mc[0];
            en.deref();
            if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_VARIABLES)) {
                if (!en.unify(termToList(t2, en.store), d2, ((SkelCompound) en.skel).args[0], en.display))
                    return false;
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_VARIABLE_NAMES)) {
                if (!en.unify(SpecialLoad.hashToAssoc(rd.getVars(), d2, en), d2, ((SkelCompound) en.skel).args[0], en.display))
                    return false;
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_SINGLETONS)) {
                if (!en.unify(SpecialLoad.hashToAssoc(rd.getAnon(), d2, en), d2, ((SkelCompound) en.skel).args[0], en.display))
                    return false;
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_DOUBLE_QUOTES)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(WriteOpts.OP_PRIORITY)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_BACK_QUOTES)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_SINGLE_QUOTES)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_ANNOTATION)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_TERMINATOR)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_SOURCE)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_LINE_NO)) {
                if (!en.unify(Integer.valueOf(rd.getClauseStart()), Display.DISPLAY_CONST, ((SkelCompound) en.skel).args[0], en.display))
                    return false;
            } else {
                throw new RuntimeException("internal error");
            }
            en.skel = mc[1];
            en.display = d;
            en.deref();
        }
        if (en.skel instanceof SkelAtom &&
                ((SkelAtom) en.skel).fun.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            throw new RuntimeException("internal error");
        }
        return true;
    }

    /**
     * <p>Make a list of the term variables.</p>
     *
     * @param val   The term.
     * @param store The store
     * @return The term variables.
     */
    private static Object termToList(Object val, Store store) {
        Object end = store.foyer.ATOM_NIL;
        Object var = SupervisorCopy.getVar(val);
        if (var == null)
            return end;
        if (var instanceof SkelVar) {
            end = new SkelCompound(store.foyer.ATOM_CONS, var, end);
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            for (int i = 0; i < temp.length; i++)
                end = new SkelCompound(store.foyer.ATOM_CONS, temp[i], end);
        }
        return end;
    }

    /**
     * <p>Convert an atom to an util value.</p>
     * <p>The following values are accepted:</p>
     * <ul>
     * <li><b>atom:</b> UTIL_ATOM.</li>
     * <li><b>codes:</b> UTIL_CODES.</li>
     * <li><b>variable:</b> UTIL_VARIABLE.</li>
     * </ul>
     *
     * @param m The util value skel.
     * @param d The util value display.
     * @return The util value.
     * @throws EngineMessage Shit happens.
     */
    public static int atomToUtil(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(OP_VALUE_ERROR)) {
            return UTIL_ERROR;
        } else if (fun.equals(OP_VALUE_CODES)) {
            return UTIL_CODES;
        } else if (fun.equals(OP_VALUE_CHARS)) {
            return UTIL_CHARS;
        } else if (fun.equals(OP_VALUE_VARIABLE)) {
            return UTIL_VARIABLE;
        } else if (fun.equals(OP_VALUE_ATOM)) {
            return UTIL_ATOM;
        } else if (fun.equals(OP_VALUE_STRING)) {
            return UTIL_STRING;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Convert an util to an atom.</p>
     *
     * @param u The util.
     * @return The atom.
     */
    public static SkelAtom utilToAtom(int u) {
        String res;
        switch (u) {
            case UTIL_ERROR:
                res = OP_VALUE_ERROR;
                break;
            case UTIL_CODES:
                res = OP_VALUE_CODES;
                break;
            case UTIL_CHARS:
                res = OP_VALUE_CHARS;
                break;
            case UTIL_VARIABLE:
                res = OP_VALUE_VARIABLE;
                break;
            case UTIL_ATOM:
                res = OP_VALUE_ATOM;
                break;
            case UTIL_STRING:
                res = OP_VALUE_STRING;
                break;
            default:
                throw new IllegalArgumentException("illegal util");
        }
        return new SkelAtom(res);
    }

    /**
     * <p>Convert an atom to a terminator.</p>
     *
     * @param m The terminator skel.
     * @param d The terminator display.
     * @return The terminator.
     * @throws EngineMessage Shit happens.
     */
    private static int atomToTerminator(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(OP_TERMINATOR_PERIOD)) {
            return 0;
        } else if (fun.equals(OP_TERMINATOR_END_OF_FILE)) {
            return TERMINATOR_EOF;
        } else if (fun.equals(OP_TERMINATOR_NONE)) {
            return TERMINATOR_NONE;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

}
