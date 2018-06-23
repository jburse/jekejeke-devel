package jekpro.frequent.advanced;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.AbstractBind;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Goal;
import jekpro.reference.arithmetic.EvaluableElem;
import jekpro.reference.arithmetic.SpecialCompare;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for dict ops.</p>
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
    private final static int SPECIAL_BETWEEN = 2;

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
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkCallable(en.skel, en.display);
                Object t = en.skel;
                Display d = en.display;

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                SkelAtom k = EngineMessage.castStringWrapped(en.skel, en.display);

                if (!(t instanceof SkelCompound) || !dictGet((SkelCompound) t, d, k, en))
                    return false;
                if (!en.unifyTerm(temp[2], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_DICT_PUT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkCallable(en.skel, en.display);
                t = en.skel;
                d = en.display;

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                k = EngineMessage.castStringWrapped(en.skel, en.display);

                en.skel = temp[2];
                en.display = ref;
                en.deref();
                Object t2 = en.skel;
                Display d2 = en.display;

                if (!(t instanceof SkelCompound) || !dictPut((SkelCompound) t, d, k, t2, d2, en))
                    return false;
                if (!en.unifyTerm(temp[3], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_BETWEEN:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                Number num1=EngineMessage.castInteger(en.skel,en.display);

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                Number num2=EngineMessage.castInteger(en.skel,en.display);

                AbstractBind mark = en.bind;
                while (SpecialCompare.computeCmp(num1, num2)<0) {
                    if (en.unifyTerm(temp[2],ref,num1,Display.DISPLAY_CONST)) {
                        /* create choice point */
                        en.choices = new ChoiceBetween(en.choices, num1,
                                (Goal) en.contskel, en.contdisplay, mark);
                        en.number++;
                        return en.getNext();
                    }

                    /* undo bindings */
                    en.skel = null;
                    en.releaseBind(mark);
                    if (en.skel != null)
                        throw (EngineException) en.skel;

                    num1= EvaluableElem.add(num1, Integer.valueOf(1));
                }
                return false;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Retrieve a dict value.</p>
     * <p>The dict value is returned in engine skel and display.</p>
     *
     * @param t The dict skeleton.
     * @param d The dict display.
     * @param k The key atom.
     * @return True if a dict value was found, otherwise false.
     */
    private static boolean dictGet(SkelCompound t, Display d,
                                   SkelAtom k, Engine en) {
        int i = dictIndex(t.args, d, k, en);
        if (i < 0)
            return false;
        i++;

        en.skel = t.args[i];
        en.display = d;
        return true;
    }

    /**
     * <p>Set a dict value.</p>
     * <p>The new dict is returned in engine skel and display.</p>
     *
     * @param t  The dict skeleton.
     * @param d  The dict display.
     * @param k  The key atom.
     * @param t2 The value skeleton.
     * @param d2 The value display.
     * @return True if a dict value was found, otherwise false.
     */
    private static boolean dictPut(SkelCompound t, Display d, SkelAtom k,
                                   Object t2, Display d2, Engine en) {
        int i = dictIndex(t.args, d, k, en);
        if (i < 0)
            return false;
        i++;

        boolean multi = SpecialUniv.setCount(t.args, d, t2, d2, i, en);
        en.skel = new SkelCompound(t.sym, SpecialUniv.setAlloc(t.args, d, t2, d2, i, multi, en));
        return true;
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
