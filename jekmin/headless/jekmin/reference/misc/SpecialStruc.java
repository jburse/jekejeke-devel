package jekmin.reference.misc;

import jekpro.frequent.standard.SpecialSort;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.reference.structure.EngineVars;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>The built-ins for the module struc.</p>
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
public final class SpecialStruc extends AbstractSpecial {
    private final static int SPECIAL_SYS_TERM_KERNEL = 0;
    private final static int SPECIAL_SYS_TERM_GLOBALS = 1;

    private final static String OP_UNIVERSAL = "#";

    /**
     * <p>Create a quantified terms special.</p>
     *
     * @param i The id.
     */
    public SpecialStruc(int i) {
        super(i);
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
            case SPECIAL_SYS_TERM_KERNEL:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                SpecialStruc.termKernel(temp[0], ref, en);
                if (!en.unifyTerm(temp[1], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_TERM_GLOBALS:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                EngineVars ev = new EngineVars();
                SpecialStruc.termGlobals(temp[0], ref, ev);
                en.skel = en.store.foyer.ATOM_NIL;
                en.display = Display.DISPLAY_CONST;
                SpecialSort.createSet(ev.vars, en);
                Display d = en.display;
                boolean multi = d.getAndReset();
                if (!en.unifyTerm(temp[1], ref, en.skel, d))
                    return false;
                if (multi)
                    BindUniv.remTab(d.bind, en);
                return en.getNext();
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /*******************************************************************/
    /* Universal Quantifier                                            */
    /*******************************************************************/

    /**
     * <p>Strip the goal from its quantifiers.</p>
     * <p>Result is return in skel and display of the engine.</p>
     *
     * @param t  The goal skeleton.
     * @param d  The goal display.
     * @param en The engine.
     */
    private static void termKernel(Object t, Display d, Engine en) {
        while (t instanceof SkelVar) {
            BindVar b;
            if ((b = d.bind[((SkelVar) t).id]).display == null)
                break;
            t = b.skel;
            d = b.display;
        }
        while (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(SpecialStruc.OP_UNIVERSAL)) {
            SkelCompound sc = (SkelCompound) t;
            t = sc.args[1];
            while (t instanceof SkelVar) {
                BindVar b;
                if ((b = d.bind[((SkelVar) t).id]).display == null)
                    break;
                t = b.skel;
                d = b.display;
            }
        }
        en.skel = t;
        en.display = d;
    }

    /**
     * <p>Compute the free variables of a goal.</p>
     * <p>The result is returned in the engine copy vars.</p>
     *
     * @param t The goal skeleton.
     * @param d The goal display.
     */
    private static void termGlobals(Object t, Display d, EngineVars ev) {
        while (t instanceof SkelVar) {
            BindVar b;
            if ((b = d.bind[((SkelVar) t).id]).display == null)
                return;
            t = b.skel;
            d = b.display;
        }
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(SpecialStruc.OP_UNIVERSAL)) {
            SkelCompound sc = (SkelCompound) t;
            termGlobals(sc.args[1], d, ev);
            ev.varExclude(sc.args[0], d);
        } else {
            ev.varInclude(t, d);
        }
    }

}
