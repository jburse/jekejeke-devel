package jekpro.model.molec;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.Engine;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import jekpro.tools.term.TermVar;
import matula.util.data.AbstractMap;

/**
 * <p>This class provides a reference counted variable binder.</p>
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
public class BindUniv extends AbstractUndo {
    public final static int MASK_REFS_LINK = 0x00008000;
    public final static int MASK_REFS_FRSH = 0x00007FFF;
    public final static int MASK_REFS_WEAK = 0x40000000;

    public final static BindUniv[] BIND_CONST = new BindUniv[0];

    public Object skel;
    public Display display;
    public int refs = MASK_REFS_LINK;

    /**
     * <p>Restore state as desired and remove bind from the engine.</p>
     * <p>The current exception is passed via the engine skel.</p>
     * <p>The new current exception is returned via the engine skel.</p>
     *
     * @param en The engine.
     */
    public void unbind(Engine en) {
        if (refs < MASK_REFS_WEAK) {
            BindUniv.unbind(this, en);
        } else {
            refs -= MASK_REFS_WEAK;
            BindUniv.unbindWeak(this, en);
        }
    }

    /**
     * <p>Remove the variable from the binding list and unbind the term from it.</p>
     * <p>Decrement the dependent counts, and if zero recursively unbind.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param bc The bind univ.
     * @param en The engine.
     */
    private static void unbind(BindUniv bc, Engine en) {
        for (; ; ) {
            /* unbind variable */
            Display d = bc.display;
            Object t = bc.skel;
            bc.removeBind(en);
            bc.skel = null;
            bc.display = null;

            Object var = SupervisorCopy.getVar(t);
            if (var == null)
                break;
            BindUniv[] b = d.bind;
            SkelVar v;
            if (var instanceof SkelVar) {
                v = (SkelVar) var;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                int i = 0;
                for (; i < temp.length - 1; i++) {
                    v = temp[i];
                    bc = b[v.id];
                    int j = bc.refs - 1;
                    if (j == 0 || j == MASK_REFS_WEAK) {
                        b[v.id] = null;
                        if (bc.display != null)
                            bc.unbind(en);
                    } else {
                        bc.refs = j;
                    }
                }
                v = temp[i];
            }
            bc = b[v.id];
            int j = bc.refs - 1;
            if (j == 0 || j == MASK_REFS_WEAK) {
                b[v.id] = null;
                if (bc.display != null)
                    if (j == 0) {
                        continue;
                    } else {
                        bc.unbind(en);
                    }
            } else {
                bc.refs = j;
            }
            break;
        }
    }

    /**
     * <p>Remove the variable from the binding list and unbind the term from it.</p>
     * <p>Decrement the dependent counts, and if zero recursively unbind.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param bc The bind univ.
     * @param en The engine.
     */
    private static void unbindWeak(BindUniv bc, Engine en) {
        for (; ; ) {
            /* unbind variable */
            Display d = bc.display;
            Object t = bc.skel;
            bc.removeBind(en);
            bc.skel = null;
            bc.display = null;

            Object var = SupervisorCopy.getVar(t);
            if (var == null)
                break;
            BindUniv[] b = d.bind;
            SkelVar v;
            if (var instanceof SkelVar) {
                v = (SkelVar) var;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                int i = 0;
                for (; i < temp.length - 1; i++) {
                    v = temp[i];
                    bc = b[v.id];
                    int j = bc.refs - MASK_REFS_LINK;
                    if (j == 0 || j == MASK_REFS_WEAK) {
                        b[v.id] = null;
                        if (bc.display != null)
                            bc.unbind(en);
                    } else {
                        bc.refs = j;
                    }
                }
                v = temp[i];
            }
            bc = b[v.id];
            int j = bc.refs - MASK_REFS_LINK;
            if (j == 0 || j == MASK_REFS_WEAK) {
                b[v.id] = null;
                if (bc.display != null) {
                    if (j == MASK_REFS_WEAK) {
                        continue;
                    } else {
                        bc.unbind(en);
                    }
                }
            } else {
                bc.refs = j;
            }
            break;
        }
    }

    /**
     * <p>Bind this variable with a term.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param t  The first term skeleton.
     * @param d  The first term display.
     * @param en The engine.
     * @throws EngineException Shit happens.
     */
    public boolean bindAttr(Object t, Display d, Engine en)
            throws EngineException {
        bindUniv(t, d, en);
        return true;
    }

    /**
     * <p>Bind this variable with a term.</p>
     *
     * @param t  The term to bind to.
     * @param d  The display of the term.
     * @param en The engine.
     */
    public final void bindUniv(Object t, Display d, Engine en) {
        /* bind variable */
        skel = t;
        display = d;
        addBind(en);

        Object var = SupervisorCopy.getVar(t);
        if (var == null)
            return;

        BindUniv[] b = d.bind;
        if (var instanceof SkelVar) {
            SkelVar v = (SkelVar) var;
            BindUniv bc = b[v.id];
            bc.refs++;
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            int n = temp.length;
            for (int j = 0; j < n; j++) {
                SkelVar v = temp[j];
                BindUniv bc = b[v.id];
                bc.refs++;
            }
        }
    }

    /**
     * <p>Bind this variable with a term.</p>
     *
     * @param t  The term to bind to.
     * @param d  The display of the term.
     * @param en The engine.
     */
    public final void bindUnivWeak(Object t, Display d, Engine en) {
        /* bind variable */
        skel = t;
        display = d;
        addBind(en);

        Object var = SupervisorCopy.getVar(t);
        if (var == null)
            return;

        BindUniv[] b = d.bind;
        if (var instanceof SkelVar) {
            SkelVar v = (SkelVar) var;
            BindUniv bc = b[v.id];
            bc.refs += MASK_REFS_LINK;
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            int n = temp.length;
            for (int j = 0; j < n; j++) {
                SkelVar v = temp[j];
                BindUniv bc = b[v.id];
                bc.refs += MASK_REFS_LINK;
            }
        }
    }

    /**
     * <p>Retrieve the attributed variable.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @return The attributed variable or null.
     */
    public TermVar getAttr() {
        return null;
    }

    /**
     * <p>Retrieve the serial number of a variable.</p>
     *
     * @param en The engine, or null.
     * @return The serial number.
     */
    public final int getValue(Engine en) {
        AbstractMap<BindUniv, Integer> map = en.visor.varmap;
        Integer val = map.get(this);
        if (val == null)
            val = UndoSerno.bindSerno(this, en);
        return val.intValue();
    }

    /****************************************************************/
    /* Unification Routines                                         */
    /****************************************************************/

    /**
     * <p>>Unify two terms. As a side effect bindings are established.</p
     * <p>Occurs check is performed depending on occurs check flag.</p>
     * <p>Bindings are only created when the occurs check fails.<p>
     * <p>The verify hooks of attribute variables are called.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param alfa The first skeleton.
     * @param d1   The first display.
     * @param beta The second skeleton.
     * @param d2   The second display.
     * @param en   The engine.
     * @return True if the two terms unify, otherwise false.
     * @throws EngineException Shit happens.
     */
    public static boolean unify(Object alfa, Display d1,
                                Object beta, Display d2,
                                Engine en)
            throws EngineException {
        for (; ; ) {
            if (alfa instanceof SkelVar) {
                // combined check and deref
                BindUniv b1;
                if ((b1 = d1.bind[((SkelVar) alfa).id]).display != null) {
                    alfa = b1.skel;
                    d1 = b1.display;
                    continue;
                }
                for (; ; ) {
                    if (beta instanceof SkelVar) {
                        // combined check and deref
                        BindUniv b2;
                        if ((b2 = d2.bind[((SkelVar) beta).id]).display != null) {
                            beta = b2.skel;
                            d2 = b2.display;
                            continue;
                        }
                        if (b1 == b2)
                            return true;
                        return b2.bindAttr(alfa, d1, en);
                    }
                    return b1.bindAttr(beta, d2, en);
                }
            }
            for (; ; ) {
                // combined check and deref
                if (beta instanceof SkelVar) {
                    BindUniv bc;
                    if ((bc = d2.bind[((SkelVar) beta).id]).display != null) {
                        beta = bc.skel;
                        d2 = bc.display;
                        continue;
                    }
                    return bc.bindAttr(alfa, d1, en);
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
                if (!unify(t1[i], d1, t2[i], d2, en))
                    return false;
            }
            alfa = t1[i];
            beta = t2[i];
        }
    }

    /**
     * <p>>Unify two terms. As a side effect bindings are established.</p
     * <p>Bindings are only created when the occurs check fails.<p>
     * <p>The verify hooks of attribute variables are called.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param alfa The first skeleton.
     * @param d1   The first display.
     * @param beta The second skeleton.
     * @param d2   The second display.
     * @param en   The engine.
     * @return True if the two terms unify, otherwise false.
     */
    public static boolean unifyChecked(Object alfa, Display d1,
                                       Object beta, Display d2,
                                       Engine en)
            throws EngineException {
        for (; ; ) {
            if (alfa instanceof SkelVar) {
                // combined check and deref
                BindUniv b1;
                if ((b1 = d1.bind[((SkelVar) alfa).id]).display != null) {
                    alfa = b1.skel;
                    d1 = b1.display;
                    continue;
                }
                for (; ; ) {
                    if (beta instanceof SkelVar) {
                        // combined check and deref
                        BindUniv b2;
                        if ((b2 = d2.bind[((SkelVar) beta).id]).display != null) {
                            beta = b2.skel;
                            d2 = b2.display;
                            continue;
                        }
                        if (b1 == b2)
                            return true;
                        if (b2.hasVar(alfa, d1, d2))
                            return false;
                        return b2.bindAttr(alfa, d1, en);
                    }
                    if (b1.hasVar(beta, d2, d1))
                        return false;
                    return b1.bindAttr(beta, d2, en);
                }
            }
            for (; ; ) {
                // combined check and deref
                if (beta instanceof SkelVar) {
                    BindUniv bc;
                    if ((bc = d2.bind[((SkelVar) beta).id]).display != null) {
                        beta = bc.skel;
                        d2 = bc.display;
                        continue;
                    }
                    if (bc.hasVar(alfa, d1, d2))
                        return false;
                    return bc.bindAttr(alfa, d1, en);
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
                if (!unifyChecked(t1[i], d1, t2[i], d2, en))
                    return false;
            }
            alfa = t1[i];
            beta = t2[i];
        }
    }

    /**
     * <p>>Unify two terms. As a side effect bindings are established.</p
     *
     * @param alfa The first skeleton.
     * @param d1   The first display.
     * @param beta The second skeleton.
     * @param d2   The second display.
     * @param en   The engine.
     */
    public static void unifyVariable(Object alfa, Display d1,
                                     Object beta, Display d2,
                                     Engine en) {
        BindUniv b1;
        while (alfa instanceof SkelVar &&
                (b1 = d1.bind[((SkelVar) alfa).id]).display != null) {
            alfa = b1.skel;
            d1 = b1.display;
        }
        b1 = d2.bind[((SkelVar) beta).id];
        b1.bindUniv(alfa, d1, en);
    }

    /**
     * <p>>Unify two terms. As a side effect bindings are established.</p
     *
     * @param alfa The first skeleton.
     * @param d1   The first display.
     * @param beta The second skeleton.
     * @param d2   The second display.
     * @param en   The engine.
     */
    public static void unifyVariableWeak(Object alfa, Display d1,
                                         Object beta, Display d2,
                                         Engine en) {
        BindUniv b1;
        while (alfa instanceof SkelVar &&
                (b1 = d1.bind[((SkelVar) alfa).id]).display != null) {
            alfa = b1.skel;
            d1 = b1.display;
        }
        b1 = d2.bind[((SkelVar) beta).id];
        b1.refs += MASK_REFS_WEAK;
        b1.bindUnivWeak(alfa, d1, en);
    }

    /**
     * <p>Check whether a variable occurs in a term.</p>
     * <p>Check is done from skeleton and display.</p>
     * <p>Uses the vars speed up structure of skel compouned.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param m  The term.
     * @param d  The display of the term.
     * @param d2 The display of the variable.
     * @return True when the variable occurs in the term, false otherwise.
     */
    public boolean hasVar(Object m, Display d, Display d2) {
        if ((refs & MASK_REFS_FRSH) != 0) {
            return hasVar(m, d, this);
        } else {
            return hasVarFresh(m, d, d2, this);
        }
    }

    /**
     * <p>Check whether a variable occurs in a term.</p>
     * <p>Check is done from skeleton and display.</p>
     * <p>Uses the vars speed up structure of skel compouned.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param m  The term.
     * @param d  The display of the term.
     * @return True when the variable occurs in the term, false otherwise.
     */
    private static boolean hasVar(Object m, Display d, BindUniv bc) {
        for (; ; ) {
            Object var = SupervisorCopy.getVar(m);
            if (var == null)
                return false;
            SkelVar v;
            if (var instanceof SkelVar) {
                v = (SkelVar) var;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                int i = 0;
                for (; i < temp.length - 1; i++) {
                    v = temp[i];
                    BindUniv b = d.bind[v.id];
                    if (b.display != null) {
                        if (hasVar(b.skel, b.display, bc))
                            return true;
                    } else {
                        if (b == bc)
                            return true;
                    }
                }
                v = temp[i];
            }
            BindUniv b = d.bind[v.id];
            if (b.display != null) {
                m = b.skel;
                d = b.display;
            } else {
                return (b == bc);
            }
        }
    }

    /**
     * <p>Check whether a variable occurs in a term.</p>
     * <p>Check is done from skeleton and display.</p>
     * <p>Uses the vars speed up structure of skel compouned.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param m  The term.
     * @param d  The display of the term.
     * @param d2 The display of the variable.
     * @return True when the variable occurs in the term, false otherwise.
     */
    private static boolean hasVarFresh(Object m, Display d, Display d2, BindUniv bc) {
        for (; ; ) {
            if (d != d2)
                return false;
            Object var = SupervisorCopy.getVar(m);
            if (var == null)
                return false;
            SkelVar v;
            if (var instanceof SkelVar) {
                v = (SkelVar) var;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                int i = 0;
                for (; i < temp.length - 1; i++) {
                    v = temp[i];
                    BindUniv b = d.bind[v.id];
                    if (b.display != null) {
                        if (hasVarFresh(b.skel, b.display, d2, bc))
                            return true;
                    } else {
                        if (b == bc)
                            return true;
                    }
                }
                v = temp[i];
            }
            BindUniv b = d.bind[v.id];
            if (b.display != null) {
                m = b.skel;
                d = b.display;
            } else {
                return (b == bc);
            }
        }
    }

}
