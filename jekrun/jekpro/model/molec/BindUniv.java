package jekpro.model.molec;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.Engine;
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
    public static final int MASK_VAR_WEAK = 0x40000000;
    public static final int MASK_VAR_FRSH = 0x3FFFFFFF;

    public final static BindUniv[] BIND_CONST = new BindUniv[0];

    public Object skel;
    public Display display;
    public int refs = MASK_VAR_WEAK;

    /**
     * <p>Restore state as desired and remove bind from the engine.</p>
     * <p>The current exception is passed via the engine skel.</p>
     * <p>The new current exception is returned via the engine skel.</p>
     *
     * @param en The engine.
     */
    public void unbind(Engine en) {
        BindUniv.unbind(this, en);
    }

    /**
     * <p>Remove the variable from the binding list and unbind the term from it.</p>
     * <p>Decrement the dependent counts, and if zero recursively unbind.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param bc The bind univ.
     * @param en The engine.
     */
    public static void unbind(BindUniv bc, Engine en) {
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
            int k;
            if (var instanceof SkelVar) {
                k = ((SkelVar) var).id;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                int i = 0;
                for (; i < temp.length - 1; i++) {
                    k = temp[i].id;
                    bc = b[k];
                    int j = bc.refs - 1;
                    if (j == 0) {
                        b[k] = null;
                        if (bc.display != null)
                            BindUniv.unbind(bc, en);
                    } else {
                        bc.refs = j;
                    }
                }
                k = temp[i].id;
            }
            bc = b[k];
            int j = bc.refs - 1;
            if (j == 0) {
                b[k] = null;
                if (bc.display != null)
                    continue;
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
            BindUniv bc = b[((SkelVar) var).id];
            bc.refs++;
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            int n = temp.length;
            for (int j = 0; j < n; j++) {
                BindUniv bc = b[temp[j].id];
                bc.refs++;
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
        for (; ; ) {
            if ((refs & MASK_VAR_FRSH) == 0 && d != d2)
                return false;
            Object var = SupervisorCopy.getVar(m);
            if (var == null)
                return false;
            BindUniv[] b = d.bind;
            BindUniv bc;
            if (var instanceof SkelVar) {
                bc = b[((SkelVar) var).id];
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                int i = 0;
                for (; i < temp.length - 1; i++) {
                    bc = b[temp[i].id];
                    if (bc.display != null) {
                        if (hasVar(bc.skel, bc.display, d2))
                            return true;
                    } else {
                        if (bc == this)
                            return true;
                    }
                }
                bc = b[temp[i].id];
            }
            if (bc.display != null) {
                m = bc.skel;
                d = bc.display;
            } else {
                return (bc == this);
            }
        }
    }

}
