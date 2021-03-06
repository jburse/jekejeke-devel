package jekpro.frequent.standard;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Directive;
import jekpro.reference.runtime.EvaluableLogic;
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
    private final static int SPECIAL_SYS_CALLN = 0;
    private final static int SPECIAL_SUBSUMES_TERM = 1;
    private final static int SPECIAL_SUBSUMES = 2;

    /**
     * <p>Create a meta special.</p>
     *
     * @param i The id.
     */
    public SpecialApply(int i) {
        super(i);
        switch (i) {
            case SPECIAL_SYS_CALLN:
                subflags |= MASK_DELE_VIRT;
                break;
            case SPECIAL_SUBSUMES_TERM:
            case SPECIAL_SUBSUMES:
                break;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Logically evaluate a term in a list of goals for the first time.</p>
     * <p>The term is passed via the skel and display of the engine.</p>
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
            case SPECIAL_SYS_CALLN:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                moduleExtendGoal(temp, ref, temp.length, en);

                Directive dire = SupervisorCall.callGoal2(0, en);
                Display d2 = en.display;

                CallFrame ref2 = CallFrame.getFrame(d2, dire, en);
                en.contskel = dire;
                en.contdisplay = ref2;
                return true;
            case SPECIAL_SUBSUMES_TERM:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!SpecialApply.subsumesTerm(temp[0], ref, temp[1], ref, en))
                    return false;
                return true;
            case SPECIAL_SUBSUMES:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!SpecialApply.subsumes(temp[0], ref, temp[1], ref, temp[1], ref, en))
                    return false;
                return true;

            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Extend the given term.</p>
     * <p>The term is passed in skel and display.</p>
     * <p>Result is returned in skel and display.</p>
     *
     * @param t2    The arguments skeleton.
     * @param d2    The arguments display.
     * @param slice The slice length.
     * @param en    The engine.
     */
    private static void moduleExtendGoal(Object[] t2, Display d2,
                                         int slice, Engine en)
            throws EngineMessage {
        Object t = en.skel;
        Display d = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(EvaluableLogic.OP_COLON)) {
            SkelCompound sc = (SkelCompound) t;
            en.skel = sc.args[1];
            en.display = d;
            en.deref();
            moduleExtendGoal(t2, d2, slice, en);
            Object t4 = en.skel;
            d2 = en.display;
            en.skel = sc.args[0];
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            boolean multi = pairCount(t, d, t4, d2, en);
            en.skel = pairAlloc(sc.sym, t, d, t4, d2, multi, en);
        } else if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(EvaluableLogic.OP_COLONCOLON)) {
            SkelCompound sc = (SkelCompound) t;
            en.skel = sc.args[1];
            en.display = d;
            en.deref();
            moduleExtendGoal(t2, d2, slice, en);
            Object t4 = en.skel;
            d2 = en.display;
            en.skel = sc.args[0];
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            boolean multi = pairCount(t, d, t4, d2, en);
            en.skel = pairAlloc(sc.sym, t, d, t4, d2, multi, en);
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
        }
    }

    /***************************************************************/
    /* Pair Univ                                                   */
    /***************************************************************/

    /**
     * <p>Count the needed variable place holders.</p>
     * <p>The reused or new display is returned in the engine copy display.</p>
     *
     * @param t  The term skel.
     * @param d  The term display.
     * @param t2 The extend skel.
     * @param d2 The extend display.
     * @param en The engine.
     * @return True if new display is returned, otherwise false.
     */
    private static boolean pairCount(Object t, Display d,
                                     Object t2, Display d2, Engine en) {
        int countvar = 0;
        Display last = Display.DISPLAY_CONST;
        boolean multi = false;
        if (SupervisorCopy.getVar(t) != null) {
            countvar++;
            if (last == Display.DISPLAY_CONST) {
                last = d;
            } else if (last != d) {
                multi = true;
            }
        }
        if (SupervisorCopy.getVar(t2) != null) {
            countvar++;
            if (last == Display.DISPLAY_CONST) {
                last = d2;
            } else if (last != d2) {
                multi = true;
            }
        }
        if (multi)
            last = new DisplayMarkable(countvar);
        en.display = last;
        return multi;
    }

    /**
     * <p>Copy the arguments.</p>
     * <p>The reused or new display is passed via the engine display</p>
     * <p>The reused or new display is returned in the engine display.</p>
     *
     * @param sa    The symbol.
     * @param t     The term skel.
     * @param d     The term display.
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
        if (multi && SupervisorCopy.getVar(t) != null) {
            SkelVar sv = vars[countvar];
            countvar++;
            d3.bind[sv.id].bindUniv(t, d, en);
            args[0] = sv;
        } else {
            args[0] = t;
        }
        if (multi && SupervisorCopy.getVar(t2) != null) {
            SkelVar sv = vars[countvar];
            // countvar++;
            boolean ext = d2.getAndReset();
            d3.bind[sv.id].bindUniv(t2, d2, en);
            if (ext)
                d2.remTab(en);
            args[1] = sv;
        } else {
            args[1] = t2;
        }
        en.display = d3;
        if (multi) {
            SkelCompound sc = new SkelCompound(args, sa);
            sc.var = (vars.length > 1 ? vars : vars[0]);
            return sc;
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
     * @param t     The term skel.
     * @param d     The term display.
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
            Object[] args = ((SkelCompound) t).args;
            for (int i = 0; i < args.length; i++) {
                en.skel = args[i];
                en.display = d;
                en.deref();
                if (SupervisorCopy.getVar(en.skel) != null) {
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
            if (SupervisorCopy.getVar(en.skel) != null) {
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = en.display;
                } else if (last != en.display) {
                    multi = true;
                }
            }
        }
        if (multi)
            last = new DisplayMarkable(countvar);
        en.display = last;
        return multi;
    }

    /**
     * <p>Copy the arguments.</p>
     * <p>The reused or new display is passed via the engine display</p>
     * <p>The reused or new display is returned in the engine display.</p>
     *
     * @param sa    The symbol.
     * @param t     The term skel.
     * @param d     The term display.
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
            Object[] args2 = ((SkelCompound) t).args;
            for (int i = 0; i < args2.length; i++) {
                en.skel = args2[i];
                en.display = d;
                en.deref();
                if (multi && SupervisorCopy.getVar(en.skel) != null) {
                    SkelVar sv = vars[countvar];
                    countvar++;
                    d3.bind[sv.id].bindUniv(en.skel, en.display, en);
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
            if (multi && SupervisorCopy.getVar(en.skel) != null) {
                SkelVar sv = vars[countvar];
                countvar++;
                d3.bind[sv.id].bindUniv(en.skel, en.display, en);
                args[len + i] = sv;
            } else {
                args[len + i] = en.skel;
            }
        }
        en.display = d3;
        if (multi) {
            SkelCompound sc = new SkelCompound(args, sa);
            sc.var = (vars.length > 1 ? vars : vars[0]);
            return sc;
        } else {
            return new SkelCompound(sa, args);
        }
    }

    /*****************************************************************/
    /* Subsumes Term                                                 */
    /*****************************************************************/

    /**
     * <p>>Subsumes term check. There is no bindings side effect.</p
     * <p>The verify hooks of attribute variables are ignored.</p>
     *
     * @param alfa The first skeleton.
     * @param d1   The first display.
     * @param beta The second skeleton.
     * @param d2   The second display.
     * @param en   The engine.
     * @return True if the two terms unify, otherwise false.
     */
    private static boolean subsumesTerm(Object alfa, Display d1,
                                        Object beta, Display d2,
                                        Engine en) {
        AbstractUndo mark = en.bind;
        boolean res = subsumes(alfa, d1, beta, d2, beta, d2, en);
        en.fault = null;
        en.releaseBind(mark);
        if (en.fault != null)
            throw new RuntimeException("shouldn't happen");
        return res;
    }

    /**
     * <p>>Subsumes term check. There is no bindings side effect.</p
     * <p>The verify hooks of attribute variables are ignored.</p>
     * <p>http://www.picat-lang.org/bprolog/publib/metutl.html</p>
     *
     * @param alfa  The first skeleton.
     * @param d1    The first display.
     * @param beta  The second skeleton.
     * @param d2    The second display.
     * @param gamma The third skeleton.
     * @param d3    The third display.
     * @param en    The engine.
     * @return True if the two terms unify, otherwise false.
     */
    private static boolean subsumes(Object alfa, Display d1,
                                    Object beta, Display d2,
                                    Object gamma, Display d3,
                                    Engine en) {
        for (; ; ) {
            if (alfa instanceof SkelVar) {
                BindUniv b1;
                if ((b1 = d1.bind[((SkelVar) alfa).id]).display != null) {
                    alfa = b1.skel;
                    d1 = b1.display;
                    continue;
                }
                for (; ; ) {
                    if (beta instanceof SkelVar) {
                        BindUniv b2;
                        if ((b2 = d2.bind[((SkelVar) beta).id]).display != null) {
                            beta = b2.skel;
                            d2 = b2.display;
                            continue;
                        }
                        if (b1 == b2)
                            return true;
                    }
                    if (b1.hasVar(gamma, d3, d1))
                        return false;
                    b1.bindUniv(beta, d2, en);
                    return true;
                }
            }
            for (; ; ) {
                if (beta instanceof SkelVar) {
                    BindUniv b;
                    if ((b = d2.bind[((SkelVar) beta).id]).display != null) {
                        beta = b.skel;
                        d2 = b.display;
                        continue;
                    }
                    return false;
                }
                break;
            }
            if (!(alfa instanceof SkelCompound))
                return alfa.equals(beta);
            if (!(beta instanceof SkelCompound))
                return false;
            Object[] t1 = ((SkelCompound) alfa).args;
            Object[] t2 = ((SkelCompound) beta).args;
            if (t1.length != t2.length)
                return false;
            if (!((SkelCompound) alfa).sym.equals(((SkelCompound) beta).sym))
                return false;
            int i = 0;
            for (; i < t1.length - 1; i++) {
                if (!subsumes(t1[i], d1, t2[i], d2, gamma, d3, en))
                    return false;
            }
            alfa = t1[i];
            beta = t2[i];
        }
    }

}
