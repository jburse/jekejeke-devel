package jekpro.model.pretty;

import jekpro.model.builtin.AbstractFlag;
import jekpro.model.builtin.Flag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ReadOpts {
    public final static String OP_LINE_NO = "line_no";
    public final static String OP_VARIABLES = "variables";
    public final static String OP_VARIABLE_NAMES = "variable_names";
    private final static String OP_SINGLETONS = "singletons";
    final static String OP_SOURCE = "source";
    final static String OP_ANNOTATION = "annotation";

    public final static String OP_VALUE_ERROR = "error";
    private final static String OP_VALUE_CODES = "codes";
    private final static String OP_VALUE_CHARS = "chars";
    private final static String OP_VALUE_ATOM = "atom";
    private final static String OP_VALUE_VARIABLE = "variable";

    public final static int UTIL_CHARS = 2;
    public final static int UTIL_VARIABLE = 3;
    public final static int UTIL_CODES = 1;
    public final static int UTIL_ERROR = 0;
    public final static int UTIL_ATOM = 4;

    public int flags = PrologWriter.FLAG_STMT;
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
     * @param en The engine.
     */
    public ReadOpts(Engine en) {
        utildouble = (byte) en.store.foyer.getUtilDouble();
        utilback = (byte) en.store.foyer.getUtilBack();
        utilsingle = (byte) en.store.foyer.getUtilSingle();
        source = en.store.user;
    }

    /**
     * <p>Decode the given read parameters.</p>
     *
     * @param t The parameters skeleton.
     * @param d The parameters display.
     * @throws EngineMessage Shit happens.
     */
    public void decodeReadParameter(Object t, Display d, Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        while (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONS)) {
            SkelCompound sc = (SkelCompound) t;
            en.skel = sc.args[0];
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
                /* do nothing */
                flags |= PrologWriter.FLAG_SING;
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_FLAG_DOUBLE_QUOTES)) {
                utildouble = (byte) ReadOpts.atomToUtil(((SkelCompound) en.skel).args[0], en.display, en);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_FLAG_BACK_QUOTES)) {
                utilback = (byte) ReadOpts.atomToUtil(((SkelCompound) en.skel).args[0], en.display, en);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_FLAG_SINGLE_QUOTES)) {
                utilsingle = (byte) ReadOpts.atomToUtil(((SkelCompound) en.skel).args[0], en.display, en);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_ANNOTATION)) {
                int anno = WriteOpts.termToAnno(((SkelCompound) en.skel).args[0], en.display, en);
                if ((anno & WriteOpts.ANNO_MKDT) != 0) {
                    flags |= PrologWriter.FLAG_MKDT;
                } else {
                    flags &= ~PrologWriter.FLAG_MKDT;
                }
                if ((anno & WriteOpts.ANNO_FILL) != 0) {
                    flags |= PrologWriter.FLAG_FILL;
                } else {
                    flags &= ~PrologWriter.FLAG_FILL;
                }
                if ((anno & WriteOpts.ANNO_HINT) != 0) {
                    flags |= PrologWriter.FLAG_HINT;
                } else {
                    flags &= ~PrologWriter.FLAG_HINT;
                }
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_SOURCE)) {
                en.skel = ((SkelCompound) en.skel).args[0];
                en.deref();
                String fun = EngineMessage.castString(en.skel, en.display);
                AbstractSource src = en.store.getSource(fun);
                AbstractSource.checkExistentSource(src, fun);
                source = src;
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_LINE_NO)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(WriteOpts.OP_PART)) {
                int part = atomToReadPart(((SkelCompound) en.skel).args[0], en.display, en);
                if ((part & WriteOpts.PART_STMT) != 0) {
                    flags |= PrologWriter.FLAG_STMT;
                } else {
                    flags &= ~PrologWriter.FLAG_STMT;
                }
            } else {
                EngineMessage.checkInstantiated(en.skel);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_READ_OPTION,
                        en.skel), en.display);
            }
            en.skel = sc.args[1];
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
    }

    /**
     * <p>Set the read options.</p>
     *
     * @param pr The Prolog reader.
     */
    public void setReadOpts(PrologReader pr) {
        pr.setFlags(flags);
        pr.setUtilDouble(utildouble);
        pr.setUtilBack(utilback);
        pr.setUtilSingle(utilsingle);
        pr.setSource(source);
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
     * @throws EngineMessage   Auto load problem.
     * @throws EngineException Auto load problem.
     */
    public static boolean decodeReadOptions(Object t, Display d,
                                            Object t2, Display d2,
                                            Engine en,
                                            PrologReader rd)
            throws EngineMessage, EngineException {
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
                if (!en.unifyTerm(((SkelCompound) en.skel).args[0], en.display,
                        termToList(t2, en.store), d2))
                    return false;
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_VARIABLE_NAMES)) {
                if (!en.unifyTerm(((SkelCompound) en.skel).args[0], en.display,
                        makeAssoc(rd.getVars(), en.store), d2))
                    return false;
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_SINGLETONS)) {
                if (!en.unifyTerm(((SkelCompound) en.skel).args[0], en.display,
                        makeAssoc(rd.getAnon(), en.store), d2))
                    return false;
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_FLAG_DOUBLE_QUOTES)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_FLAG_BACK_QUOTES)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_FLAG_SINGLE_QUOTES)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_ANNOTATION)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_SOURCE)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(OP_LINE_NO)) {
                if (!en.unifyTerm(((SkelCompound) en.skel).args[0], en.display,
                        Integer.valueOf(rd.getClauseStart()), Display.DISPLAY_CONST))
                    return false;
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(WriteOpts.OP_PART)) {
                /* do nothing */
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
     * <p>Convert a named list to a Prolog association list.</p>
     *
     * @param vars  The named list.
     * @param store The store.
     * @return The Prolog association list.
     */
    private static Object makeAssoc(MapHashLink<String, SkelVar> vars, AbstractStore store) {
        Object end = store.foyer.ATOM_NIL;
        if (vars == null)
            return end;
        for (MapEntry<String, SkelVar> entry = vars.getLastEntry();
             entry != null; entry = vars.predecessor(entry))
            end = new SkelCompound(store.foyer.ATOM_CONS,
                    new SkelCompound(store.foyer.ATOM_EQUAL,
                            new SkelAtom(entry.key), entry.value), end);
        return end;
    }

    /**
     * <p>Make a list of the term variables.</p>
     *
     * @param val   The term.
     * @param store The store
     * @return The term variables.
     */
    private static Object termToList(Object val, AbstractStore store) {
        Object end = store.foyer.ATOM_NIL;
        if (val instanceof SkelVar) {
            end = new SkelCompound(store.foyer.ATOM_CONS, val, end);
        } else if (val instanceof SkelCompound) {
            SkelVar[] vars = ((SkelCompound) val).vars;
            if (vars == null)
                return end;
            for (int i = 0; i < vars.length; i++)
                end = new SkelCompound(store.foyer.ATOM_CONS, vars[i], end);
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
     * @param m  The util value skel.
     * @param d  The util value display.
     * @param en The engine.
     * @return The util value.
     * @throws EngineMessage Shit happens.
     */
    public static int atomToUtil(Object m, Display d, Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        String fun = EngineMessage.castString(en.skel, en.display);
        if (fun.equals(OP_VALUE_ERROR)) {
            return UTIL_ERROR;
        } else if (fun.equals(OP_VALUE_CODES)) {
            return UTIL_CODES;
        } else if (fun.equals(OP_VALUE_CHARS)) {
            return UTIL_CHARS;
        } else if (fun.equals(OP_VALUE_ATOM)) {
            return UTIL_ATOM;
        } else if (fun.equals(OP_VALUE_VARIABLE)) {
            return UTIL_VARIABLE;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, en.skel));
        }
    }

    /**
     * <p>Convert an util to an atom.</p>
     *
     * @param u The util.
     * @return The atom.
     */
    public static SkelAtom utilToAtom(int u) {
        switch (u) {
            case UTIL_ERROR:
                return new SkelAtom(OP_VALUE_ERROR);
            case UTIL_CODES:
                return new SkelAtom(OP_VALUE_CODES);
            case UTIL_CHARS:
                return new SkelAtom(OP_VALUE_CHARS);
            case UTIL_ATOM:
                return new SkelAtom(OP_VALUE_ATOM);
            case UTIL_VARIABLE:
                return new SkelAtom(OP_VALUE_VARIABLE);
            default:
                throw new IllegalArgumentException("illegal util");
        }
    }

    /**
     * <p>Convert an atom to a write part.</p>
     *
     * @param m  The annotation mode skel.
     * @param d  The annotation mode display.
     * @param en The engine.
     * @return The annotation mode.
     * @throws EngineMessage Shit happens.
     */
    public static int atomToReadPart(Object m, Display d, Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        String fun = EngineMessage.castString(en.skel, en.display);
        if (fun.equals(AbstractFlag.OP_FALSE)) {
            return 0;
        } else if (fun.equals(Foyer.OP_TRUE)) {
            return WriteOpts.PART_STMT;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, en.skel));
        }
    }

}
