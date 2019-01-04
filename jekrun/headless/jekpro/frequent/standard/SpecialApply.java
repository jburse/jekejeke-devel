package jekpro.frequent.standard;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Clause;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>Provides built-in predicates for the module apply.</p>
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
public final class SpecialApply extends AbstractSpecial {
    private final static int SPECIAL_SYS_MODEXT_ARGS_ANY = 0;
    private final static int SPECIAL_SYS_CALL_ANY = 1;

    /**
     * <p>Create a meta special.</p>
     *
     * @param i The id.
     */
    public SpecialApply(int i) {
        super(i);
        switch (i) {
            case SPECIAL_SYS_MODEXT_ARGS_ANY:
                break;
            case SPECIAL_SYS_CALL_ANY:
                subflags |= MASK_DELE_VIRT;
                break;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_SYS_MODEXT_ARGS_ANY:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                boolean multi = moduleExtendGoal(temp[0], ref, temp, ref, temp.length - 1, en);
                Display d = en.display;
                if (!en.unifyTerm(temp[temp.length - 1], ref, en.skel, d))
                    return false;
                if (multi)
                    BindCount.remTab(d.bind, en);
                return en.getNext();
            case SPECIAL_SYS_CALL_ANY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                boolean ext = moduleExtendGoal(temp[0], ref, temp, ref, temp.length, en);
                d = en.display;
                multi = en.wrapGoal();
                if (multi && ext)
                    BindCount.remTab(d.bind, en);
                ref = en.display;
                Clause clause = en.store.foyer.CLAUSE_CONT;
                DisplayClause ref2 = new DisplayClause();
                ref2.bind = DisplayClause.newBindClause(clause.dispsize);
                ref2.addArgument(en.skel, ref, en);
                if (multi || ext)
                    BindCount.remTab(ref.bind, en);
                ref2.setEngine(en);
                en.contskel = clause.getNextRaw(en);
                en.contdisplay = ref2;
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Extend the given goal.</p>
     * <p>The argument is passed in skel and display.</p>
     * <p>Result is returned in skel and display.</p>
     *
     * @param t2    The arguments skeleton.
     * @param d2    The arguments display.
     * @param slice The slice length.
     * @param en    The engine.
     * @return True if new display is returned, otherwise false.
     */
    private static boolean moduleExtendGoal(Object t, Display d,
                                            Object[] t2, Display d2,
                                            int slice, Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(SpecialQuali.OP_COLON)) {
            SkelCompound sc = (SkelCompound) t;
            boolean ext = moduleExtendGoal(sc.args[1], d, t2, d2, slice, en);
            Object t4 = en.skel;
            d2 = en.display;
            en.skel = sc.args[0];
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            boolean multi = pairCount(t, d, t4, d2, en);
            en.skel = pairAlloc(sc.sym, t, d, t4, d2, multi, en);
            if (multi && ext)
                BindCount.remTab(d2.bind, en);
            return (multi || ext);
        } else if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(SpecialQuali.OP_COLONCOLON)) {
            SkelCompound sc = (SkelCompound) t;
            boolean ext = moduleExtendGoal(sc.args[1], d, t2, d2, slice, en);
            Object t4 = en.skel;
            d2 = en.display;
            en.skel = sc.args[0];
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            boolean multi = pairCount(t, d, t4, d2, en);
            en.skel = pairAlloc(sc.sym, t, d, t4, d2, multi, en);
            if (multi && ext)
                BindCount.remTab(d2.bind, en);
            return (multi || ext);
        } else {
            SkelAtom sa;
            if (t instanceof SkelCompound) {
                sa = ((SkelCompound) t).sym;
            } else if (t instanceof SkelAtom) {
                sa = (SkelAtom) t;
            } else {
                EngineMessage.checkInstantiated(t);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_CALLABLE, t), d);
            }
            boolean multi = extendCount(t, d, t2, d2, slice, en);
            en.skel = extendAlloc(sa, t, d, t2, d2, slice, multi, en);
            return multi;
        }
    }

    /***************************************************************/
    /* Pair Univ                                                   */
    /***************************************************************/

    /**
     * <p>Count the needed variable place holders.</p>
     * <p>The reused or new display is returned in the engine copy display.</p>
     *
     * @param t  The goal skel.
     * @param d  The goal display.
     * @param t2 The extend skel.
     * @param d2 The extend display.
     * @param en The engine.
     * @return True if new display is returned, otherwise false.
     */
    private static boolean pairCount(Object t, Display d, Object t2,
                                     Display d2, Engine en) {
        int countvar = 0;
        Display last = Display.DISPLAY_CONST;
        boolean multi = false;
        if (EngineCopy.getVar(t) != null) {
            countvar++;
            if (last == Display.DISPLAY_CONST) {
                last = d;
            } else if (last != d) {
                multi = true;
            }
        }
        if (EngineCopy.getVar(t2) != null) {
            countvar++;
            if (last == Display.DISPLAY_CONST) {
                last = d2;
            } else if (last != d2) {
                multi = true;
            }
        }
        if (multi)
            last = new Display(Display.newBind(countvar));
        en.display = last;
        return multi;
    }

    /**
     * <p>Copy the arguments.</p>
     * <p>The reused or new display is passed via the engine display</p>
     * <p>The reused or new display is returned in the engine display.</p>
     *
     * @param sa    The symbol.
     * @param t     The goal skel.
     * @param d     The goal display.
     * @param t2    The extend skel.
     * @param d2    The extend display.
     * @param multi The multi flag.
     * @param en    The engine copy.
     * @return The new compound.
     */
    private static SkelCompound pairAlloc(SkelAtom sa,
                                          Object t, Display d,
                                          Object t2, Display d2,
                                          boolean multi, Engine en) {
        Display d3 = en.display;
        SkelVar[] vars;
        if (multi) {
            vars = SkelVar.valueOfArray(d3.bind.length);
        } else {
            vars = null;
        }
        Object[] args = new Object[2];
        int countvar = 0;
        if (multi && EngineCopy.getVar(t) != null) {
            SkelVar sv = vars[countvar];
            countvar++;
            d3.bind[sv.id].bindVar(t, d, en);
            args[0] = sv;
        } else {
            args[0] = t;
        }
        if (multi && EngineCopy.getVar(t2) != null) {
            SkelVar sv = vars[countvar];
            // countvar++;
            d3.bind[sv.id].bindVar(t2, d2, en);
            args[1] = sv;
        } else {
            args[1] = t2;
        }
        en.display = d3;
        if (multi) {
            return new SkelCompound(sa, args, vars);
        } else {
            return new SkelCompound(sa, args);
        }
    }

    /***************************************************************/
    /* Extend Univ                                                 */
    /***************************************************************/

    /**
     * <p>Count the needed variable place holders.</p>
     * <p>The reused or new display is returned in the engine display.</p>
     *
     * @param t     The goal skel.
     * @param d     The goal display.
     * @param t2    The extend arguments.
     * @param d2    The extend display.
     * @param slice The slice length.
     * @param en    The engine.
     * @return True if new display is returned, otherwise false.
     */
    private static boolean extendCount(Object t, Display d,
                                       Object[] t2, Display d2,
                                       int slice, Engine en) {
        int countvar = 0;
        Display last = Display.DISPLAY_CONST;
        boolean multi = false;
        if (t instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) t;
            for (int i = 0; i < sc.args.length; i++) {
                en.skel = sc.args[i];
                en.display = d;
                en.deref();
                if (EngineCopy.getVar(en.skel) != null) {
                    countvar++;
                    if (last == Display.DISPLAY_CONST) {
                        last = en.display;
                    } else if (last != en.display) {
                        multi = true;
                    }
                }
            }
        }
        for (int i = 1; i < slice; i++) {
            en.skel = t2[i];
            en.display = d2;
            en.deref();
            if (EngineCopy.getVar(en.skel) != null) {
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = en.display;
                } else if (last != en.display) {
                    multi = true;
                }
            }
        }
        if (multi)
            last = new Display(Display.newBind(countvar));
        en.display = last;
        return multi;
    }

    /**
     * <p>Copy the arguments.</p>
     * <p>The reused or new display is passed via the engine display</p>
     * <p>The reused or new display is returned in the engine display.</p>
     *
     * @param sa    The symbol.
     * @param t     The goal skel.
     * @param d     The goal display.
     * @param t2    The extend arguments.
     * @param d2    The extend display.
     * @param slice The slice length.
     * @param multi The multi flag.
     * @param en    The engine.
     * @return The new compound.
     */
    private static SkelCompound extendAlloc(SkelAtom sa, Object t, Display d,
                                            Object[] t2, Display d2,
                                            int slice,
                                            boolean multi, Engine en) {
        Display d3 = en.display;
        SkelVar[] vars;
        if (multi) {
            vars = SkelVar.valueOfArray(d3.bind.length);
        } else {
            vars = null;
        }
        int len;
        if (t instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) t;
            len = sc.args.length - 1;
        } else {
            len = -1;
        }
        Object[] args = new Object[len + slice];
        int countvar = 0;
        if (t instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) t;
            for (int i = 0; i < sc.args.length; i++) {
                en.skel = sc.args[i];
                en.display = d;
                en.deref();
                if (multi && EngineCopy.getVar(en.skel) != null) {
                    SkelVar sv = vars[countvar];
                    countvar++;
                    d3.bind[sv.id].bindVar(en.skel, en.display, en);
                    args[i] = sv;
                } else {
                    args[i] = en.skel;
                }
            }
        }
        for (int i = 1; i < slice; i++) {
            en.skel = t2[i];
            en.display = d2;
            en.deref();
            if (multi && EngineCopy.getVar(en.skel) != null) {
                SkelVar sv = vars[countvar];
                countvar++;
                d3.bind[sv.id].bindVar(en.skel, en.display, en);
                args[len + i] = sv;
            } else {
                args[len + i] = en.skel;
            }
        }
        en.display = d3;
        if (multi) {
            return new SkelCompound(sa, args, vars);
        } else {
            return new SkelCompound(sa, args);
        }
    }

}
