package jekpro.model.pretty;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.builtin.Flag;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Operator;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.reflect.SpecialOper;
import jekpro.reference.runtime.EvaluableLogic;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.Types;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

/**
 * <p>This class provides write options.</p>
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
public final class WriteOpts {
    public final static String OP_FLAG_QUOTED = "quoted";
    private final static String OP_IGNORE_OPS = "ignore_ops";
    private final static String OP_NUMBERVARS = "numbervars";
    private final static String OP_IGNORE_MOD = "ignore_mod";
    final static String OP_PRIORITY = "priority";
    private final static String OP_CONTEXT = "context";

    private final static String OP_ANNO_MKDT = "makedot";
    private final static String OP_ANNO_FILL = "filler";
    private final static String OP_ANNO_HINT = "hint";

    private final static String OP_FORMAT = "format";
    private final static String OP_FORMAT_NEWL = "newline";
    private final static String OP_FORMAT_NAVI = "navigation";

    private final static String OP_PART = "part";
    private final static String OP_PART_CMMT = "comment";
    private final static String OP_PART_STMT = "statement";

    public static final int FORMAT_NEWL = 1;
    public static final int FORMAT_NAVI = 2;

    public static final int ANNO_MKDT = 1;
    public static final int ANNO_FILL = 2;
    public static final int ANNO_HINT = 4;

    public static final int PART_CMMT = 1;
    public static final int PART_STMT = 2;

    public int flags = PrologWriter.FLAG_DFLT;
    public int lev = Operator.LEVEL_HIGH;
    public int spez;
    public int offset;
    public int shift;
    public byte utildouble;
    public byte utilback;
    public byte utilsingle;
    public AbstractSource source;
    public MapHash<BindUniv, String> printmap;

    /***************************************************************/
    /* Write Options                                               */
    /***************************************************************/

    /**
     * <p>Create some write options.</p>
     *
     * @param s The source.
     */
    public WriteOpts(AbstractSource s) {
        source = s;
        utildouble = s.utildouble;
        utilback = s.utilback;
        utilsingle = s.utilsingle;
    }

    /**
     * <p>Decode the given write options.</p>
     *
     * @param t  The options skel.
     * @param d  The options display.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public void decodeWriteOptions(Object t, Display d, Engine en)
            throws EngineMessage {
        try {
            en.skel = t;
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            while (t instanceof SkelCompound &&
                    ((SkelCompound) t).args.length == 2 &&
                    ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONS)) {
                SkelCompound mc = (SkelCompound) t;
                en.skel = mc.args[0];
                en.deref();
                if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(OP_FLAG_QUOTED)) {
                    if (atomToBool(((SkelCompound) en.skel).args[0], en.display)) {
                        flags |= PrologWriter.FLAG_QUOT;
                    } else {
                        flags &= ~PrologWriter.FLAG_QUOT;
                    }
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(OP_NUMBERVARS)) {
                    if (atomToBool(((SkelCompound) en.skel).args[0], en.display)) {
                        flags |= PrologWriter.FLAG_NUMV;
                    } else {
                        flags &= ~PrologWriter.FLAG_NUMV;
                    }
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(OP_IGNORE_OPS)) {
                    if (atomToBool(((SkelCompound) en.skel).args[0], en.display)) {
                        flags |= PrologWriter.FLAG_IGNO;
                    } else {
                        flags &= ~PrologWriter.FLAG_IGNO;
                    }
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(OP_IGNORE_MOD)) {
                    if (atomToBool(((SkelCompound) en.skel).args[0], en.display)) {
                        flags |= PrologWriter.FLAG_IGNM;
                    } else {
                        flags &= ~PrologWriter.FLAG_IGNM;
                    }
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(OP_PRIORITY)) {
                    Number num = SpecialEval.derefAndCastInteger(((SkelCompound) en.skel).args[0], en.display);
                    SpecialEval.checkNotLessThanZero(num);
                    int k = SpecialEval.castIntValue(num);
                    SpecialOper.checkOperatorLevel(k);
                    lev = k;
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(OP_FORMAT)) {
                    int form = atomToFormat(((SkelCompound) en.skel).args[0], en.display);
                    if ((form & FORMAT_NEWL) != 0) {
                        flags |= PrologWriter.FLAG_NEWL;
                    } else {
                        flags &= ~PrologWriter.FLAG_NEWL;
                    }
                    if ((form & FORMAT_NAVI) != 0) {
                        flags |= PrologWriter.FLAG_NAVI;
                    } else {
                        flags &= ~PrologWriter.FLAG_NAVI;
                    }
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(OP_PART)) {
                    int part = atomToWritePart(((SkelCompound) en.skel).args[0], en.display);
                    if ((part & PART_CMMT) != 0) {
                        flags |= PrologWriter.FLAG_CMMT;
                    } else {
                        flags &= ~PrologWriter.FLAG_CMMT;
                    }
                    if ((part & PART_STMT) != 0) {
                        flags |= PrologWriter.FLAG_STMT;
                    } else {
                        flags &= ~PrologWriter.FLAG_STMT;
                    }
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(OP_CONTEXT)) {
                    Object obj = Predicate.checkMetaSpezArg(
                            ((SkelCompound) en.skel).args[0], en.display, en);
                    if (spezToMeta(obj)) {
                        spez |= PrologWriter.SPEZ_META;
                    } else {
                        spez &= ~PrologWriter.SPEZ_META;
                    }
                    offset = spezToOffset(obj);
                    shift = spezToShift(obj);
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(ReadOpts.OP_VARIABLE_NAMES)) {
                    printmap = SupervisorCopy.assocToMapUniv(((SkelCompound) en.skel).args[0], en.display, en);
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_DOUBLE_QUOTES)) {
                    utildouble = (byte) ReadOpts.atomToUtil(
                            ((SkelCompound) en.skel).args[0], en.display);
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_BACK_QUOTES)) {
                    utilback = (byte) ReadOpts.atomToUtil(
                            ((SkelCompound) en.skel).args[0], en.display);
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_SINGLE_QUOTES)) {
                    utilsingle = (byte) ReadOpts.atomToUtil(
                            ((SkelCompound) en.skel).args[0], en.display);
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(ReadOpts.OP_ANNOTATION)) {
                    int anno = termToAnno(((SkelCompound) en.skel).args[0], en.display, en);
                    if ((anno & ANNO_MKDT) != 0) {
                        flags |= PrologWriter.FLAG_MKDT;
                    } else {
                        flags &= ~PrologWriter.FLAG_MKDT;
                    }
                    if ((anno & ANNO_FILL) != 0) {
                        flags |= PrologWriter.FLAG_FILL;
                    } else {
                        flags &= ~PrologWriter.FLAG_FILL;
                    }
                    if ((anno & ANNO_HINT) != 0) {
                        flags |= PrologWriter.FLAG_HINT;
                    } else {
                        flags &= ~PrologWriter.FLAG_HINT;
                    }
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(ReadOpts.OP_SOURCE)) {
                    SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(
                            ((SkelCompound) en.skel).args[0], en.display);
                    AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
                    src = src.getStore().getSource(sa.fun);
                    AbstractSource.checkExistentSource(src, sa);
                    source = src;
                } else {
                    EngineMessage.checkInstantiated(en.skel);
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_WRITE_OPTION,
                            en.skel), en.display);
                }
                en.skel = mc.args[1];
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
            validatePrintMap();
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

    /**
     * <p>Set the write options.</p>
     *
     * @param pw The Prolog writer.
     */
    public void setWriteOpts(PrologWriter pw) {
        pw.setFlags(flags);
        pw.setSource(source);
        pw.setUtilDouble(utildouble);
        pw.setUtilBack(utilback);
        pw.setUtilSingle(utilsingle);
        pw.setLevel(lev);
        pw.setSpez(spez);
        pw.setOffset(offset);
        pw.setShift(shift);
        pw.setPrintMap(printmap);
    }

    /**
     * <p>Convert an atom to a bool.</p>
     * <p>The following values are accepted:</p>
     * <ul>
     * <li><b>true:</b> true.</li>
     * <li><b>false:</b> false.</li>
     * </ul>
     *
     * @param m The bool skel.
     * @param d The bool display.
     * @return The bool value.
     * @throws EngineMessage Shit happens.
     */
    public static boolean atomToBool(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(AbstractFlag.OP_FALSE)) {
            return false;
        } else if (fun.equals(Foyer.OP_TRUE)) {
            return true;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Convert an atom to a format.</p>
     * <p>The following values are accepted:</p>
     * <ul>
     * <li><b>false:</b> 0.</li>
     * <li><b>newline:</b> FORMAT_NEWL.</li>
     * <li><b>navigation:</b> FORMAT_NAVI.</li>
     * <li><b>true:</b> FORMAT_NEWL | FORMAT_NAVI.</li>
     * </ul>
     *
     * @param m The bool skel.
     * @param d The bool display.
     * @return The bool value.
     * @throws EngineMessage Shit happens.
     */
    private static int atomToFormat(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(AbstractFlag.OP_FALSE)) {
            return 0;
        } else if (fun.equals(OP_FORMAT_NEWL)) {
            return FORMAT_NEWL;
        } else if (fun.equals(OP_FORMAT_NAVI)) {
            return FORMAT_NAVI;
        } else if (fun.equals(Foyer.OP_TRUE)) {
            return FORMAT_NEWL | FORMAT_NAVI;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Convert a term to an annotation mode.</p>
     * <p>The following values are accepted:</p>
     * <ul>
     * <li><b>none:</b> ANNO_NONE.</li>
     * <li><b>filler:</b> ANNO_FILL.</li>
     * <li><b>hint:</b> ANNO_HINT.</li>
     * </ul>
     *
     * @param m  The term skel.
     * @param d  The term display.
     * @param en The engine.
     * @return The annotation mode.
     * @throws EngineMessage Shit happens.
     */
    public static int termToAnno(Object m, Display d, Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if ((m instanceof SkelCompound) &&
                ((SkelCompound) m).args.length == 2 &&
                ((SkelCompound) m).sym.fun.equals(PrologReader.OP_BAR)) {
            SkelCompound sc = (SkelCompound) m;
            int val1 = termToAnno(sc.args[0], d, en);
            int val2 = termToAnno(sc.args[1], d, en);
            return val1 | val2;
        } else if (m instanceof SkelAtom) {
            String fun = ((SkelAtom) m).fun;
            if (fun.equals(AbstractFlag.OP_FALSE)) {
                return 0;
            } else if (fun.equals(OP_ANNO_MKDT)) {
                return ANNO_MKDT;
            } else if (fun.equals(OP_ANNO_FILL)) {
                return ANNO_FILL;
            } else if (fun.equals(OP_ANNO_HINT)) {
                return ANNO_HINT;
            } else if (fun.equals(Foyer.OP_TRUE)) {
                return ANNO_MKDT + ANNO_FILL + ANNO_HINT;
            } else {
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_FLAG_VALUE, m));
            }
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_ATOM, m), d);
        }
    }

    /**
     * <p>Convert an annotation mode to a term.</p>
     *
     * @param anno The annotation mode.
     * @return The term.
     */
    public static Object annoToTerm(int anno) {
        ListArray<String> list = new ListArray<String>();
        if ((anno & ANNO_HINT) != 0)
            list.add(OP_ANNO_HINT);
        if ((anno & ANNO_FILL) != 0)
            list.add(OP_ANNO_FILL);
        if ((anno & ANNO_MKDT) != 0)
            list.add(OP_ANNO_MKDT);
        if (list.size() == 3)
            return new SkelAtom(Foyer.OP_TRUE);
        if (list.size() == 0)
            return new SkelAtom(AbstractFlag.OP_FALSE);
        Object res = new SkelAtom(list.get(0));
        for (int i = 1; i < list.size(); i++) {
            res = new SkelCompound(new SkelAtom(PrologReader.OP_BAR),
                    new SkelAtom(list.get(i)), res);
        }
        return res;
    }

    /**
     * <p>Convert an atom to a write part.</p>
     * <p>The following values are accepted:</p>
     * <ul>
     * <li><b>comment:</b> PART_CMMT.</li>
     * <li><b>statement:</b> PART_STMT.</li>
     * </ul>
     *
     * @param m The annotation mode skel.
     * @param d The annotation mode display.
     * @return The annotation mode.
     * @throws EngineMessage Shit happens.
     */
    public static int atomToWritePart(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(AbstractFlag.OP_FALSE)) {
            return 0;
        } else if (fun.equals(OP_PART_CMMT)) {
            return PART_CMMT;
        } else if (fun.equals(OP_PART_STMT)) {
            return PART_STMT;
        } else if (fun.equals(Foyer.OP_TRUE)) {
            return PART_CMMT + PART_STMT;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Validate the var map.</p>
     */
    public void validatePrintMap() throws EngineMessage {
        if (utilsingle == ReadOpts.UTIL_VARIABLE ||
                utilback == ReadOpts.UTIL_VARIABLE ||
                utildouble == ReadOpts.UTIL_VARIABLE)
            return;
        if (printmap == null)
            return;
        for (MapEntry<BindUniv, String> entry = printmap.getLastEntry();
             entry != null; entry = printmap.predecessor(entry)) {
            if (PrologWriter.variableNeedsQuotes(entry.value))
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_VARIABLE_NAME, new SkelAtom(entry.value)));
        }
    }

    /******************************************************************/
    /* Meta Spezifier Handling                                        */
    /******************************************************************/

    /**
     * <p>Retrieve the meta flag.</p>
     * <p>The default is question mark.</p>
     *
     * @param obj The meta spezifier, can be null.
     * @return The meta flag.
     */
    static boolean spezToMeta(Object obj) {
        if (obj instanceof Integer) {
            return true;
        } else if (obj == null || (obj instanceof SkelAtom &&
                ((SkelAtom) obj).fun.equals(Predicate.OP_QUESTION))) {
            return false;
        } else if (obj instanceof SkelCompound &&
                ((SkelCompound) obj).args.length == 1 &&
                ((SkelCompound) obj).sym.fun.equals(EvaluableLogic.OP_COLONCOLON)) {
            return true;
        } else {
            throw new IllegalArgumentException("illegal meta spec");
        }
    }

    /**
     * <p>Retrieve the meta offset.</p>
     * <p>The default is zero.</p>
     *
     * @param obj The meta spezifier, can be null.
     * @return The meta offset.
     */
    static int spezToOffset(Object obj) {
        if (obj instanceof Integer) {
            return ((Integer) obj).intValue();
        } else if (obj == null || (obj instanceof SkelAtom &&
                ((SkelAtom) obj).fun.equals(Predicate.OP_QUESTION))) {
            return 0;
        } else if (obj instanceof SkelCompound &&
                ((SkelCompound) obj).args.length == 1 &&
                ((SkelCompound) obj).sym.fun.equals(EvaluableLogic.OP_COLONCOLON)) {
            int k = spezToOffset2(((SkelCompound) obj).args[0]);
            return (k >= 0 ? k + 1 : k - 1);
        } else if (obj == AbstractSkel.VOID_OBJ || (obj instanceof SkelCompound &&
                ((SkelCompound) obj).args.length == 1 &&
                ((SkelCompound) obj).sym.fun.equals(Predicate.OP_HASH))) {
            return (obj == AbstractSkel.VOID_OBJ ? 1 : spezToOffset3(((SkelCompound) obj).args[0]));
        } else {
            throw new IllegalArgumentException("illegal meta spec");
        }
    }

    /**
     * @param obj The meta spezifier, non null.
     * @return The meta offset.
     */
    private static int spezToOffset2(Object obj) {
        if (obj instanceof Integer) {
            return ((Integer) obj).intValue();
        } else if (obj instanceof SkelCompound &&
                ((SkelCompound) obj).args.length == 1 &&
                ((SkelCompound) obj).sym.fun.equals(EvaluableLogic.OP_COLONCOLON)) {
            int k = spezToOffset(((SkelCompound) obj).args[0]);
            return (k >= 0 ? k + 1 : k - 1);
        } else {
            throw new IllegalArgumentException("illegal meta spec");
        }
    }


    /**
     * @param obj The meta spezifier, non null.
     * @return The meta offset.
     */
    private static int spezToOffset3(Object obj) {
        if (obj instanceof Integer) {
            return ((Integer) obj).intValue();
        } else {
            throw new IllegalArgumentException("illegal meta spec");
        }
    }

    /**
     * <p>Retrieve the meta shift.</p>
     * <p>The default is zero.</p>
     *
     * @param obj The meta spezifier, can be null.
     * @return The meta shift.
     */
    static int spezToShift(Object obj) {
        if (obj instanceof Integer) {
            return 0;
        } else if (obj == null || (obj instanceof SkelAtom &&
                ((SkelAtom) obj).fun.equals(Predicate.OP_QUESTION))) {
            return 0;
        } else if (obj instanceof SkelCompound &&
                ((SkelCompound) obj).args.length == 1 &&
                ((SkelCompound) obj).sym.fun.equals(EvaluableLogic.OP_COLONCOLON)) {
            return spezToShift2(((SkelCompound) obj).args[0]) + 1;
        } else if (obj == AbstractSkel.VOID_OBJ || (obj instanceof SkelCompound &&
                ((SkelCompound) obj).args.length == 1 &&
                ((SkelCompound) obj).sym.fun.equals(Predicate.OP_HASH))) {
            return 0;
        } else {
            throw new IllegalArgumentException("illegal meta spec");
        }
    }

    /**
     * @param obj The meta spezifier, non null.
     * @return The meta shif.
     */
    private static int spezToShift2(Object obj) {
        if (obj instanceof Integer) {
            return 0;
        } else if (obj instanceof SkelCompound &&
                ((SkelCompound) obj).args.length == 1 &&
                ((SkelCompound) obj).sym.fun.equals(EvaluableLogic.OP_COLONCOLON)) {
            return spezToShift(((SkelCompound) obj).args[0]) + 1;
        } else {
            throw new IllegalArgumentException("illegal meta spec");
        }
    }

}
