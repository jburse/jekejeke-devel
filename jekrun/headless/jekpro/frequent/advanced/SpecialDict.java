package jekpro.frequent.advanced;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for the module dict.</p>
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
public final class SpecialDict extends AbstractSpecial {
    private final static int SPECIAL_DICT_GET = 0;
    private final static int SPECIAL_DICT_PUT = 1;

    /**
     * <p>Create a dict special.</p>
     *
     * @param i The id.
     */
    public SpecialDict(int i) {
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
            case SPECIAL_DICT_GET:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                if (!(en.skel instanceof SkelCompound))
                    return false;
                SkelCompound sc = (SkelCompound) en.skel;
                Display d = en.display;

                SkelAtom k = SpecialUniv.derefAndCastStringWrapped(temp[1], ref);

                int i = dictIndex(sc.args, d, k, en);
                if (i < 0)
                    return false;
                i++;

                if (!en.unifyTerm(temp[2], ref, sc.args[i], d))
                    return false;
                return en.getNext();
            case SPECIAL_DICT_PUT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                if (!(en.skel instanceof SkelCompound))
                    return false;
                sc = (SkelCompound) en.skel;
                d = en.display;

                k = SpecialUniv.derefAndCastStringWrapped(temp[1], ref);

                i = dictIndex(sc.args, d, k, en);
                if (i < 0)
                    return false;
                i++;

                en.skel = temp[2];
                en.display = ref;
                en.deref();
                Object t2 = en.skel;
                Display d2 = en.display;

                boolean multi = SpecialUniv.setCount(sc.args, d, t2, d2, i, en);
                sc = SpecialUniv.setAlloc(sc.sym, sc.args, d, t2, d2, i, multi, en);
                d = en.display;
                if (!en.unifyTerm(temp[3], ref, sc, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return en.getNext();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Find a key position.</p>
     *
     * @param t  The args skeleton array.
     * @param d  The args display.
     * @param k  The key atom.
     * @param en The engine.
     * @return The key position or -1.
     */
    private static int dictIndex(Object[] t, Display d,
                                 SkelAtom k, Engine en) {
        for (int i = 0; i < t.length; i += 2) {
            en.skel = t[i];
            en.display = d;
            en.deref();
            if (k.equals(en.skel))
                return i;
        }
        return -1;
    }

}
