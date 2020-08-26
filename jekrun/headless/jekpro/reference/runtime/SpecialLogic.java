package jekpro.reference.runtime;

import jekpro.frequent.basic.SpecialProxy;
import jekpro.frequent.standard.SupervisorCall;
import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.*;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.ListArray;

/**
 * <p>Provides built-in predicates for the module logic.</p>
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
public final class SpecialLogic extends AbstractSpecial {
    private final static int SPECIAL_FINDALL = 0;
    private final static int SPECIAL_FINDALL_END = 1;
    private final static int SPECIAL_CALL_COLON = 2;
    private final static int SPECIAL_CALL_COLONCOLON = 3;
    private final static int SPECIAL_SYS_REPLACE_SITE = 4;

    /**
     * <p>Create a logic special.</p>
     *
     * @param i The id.
     */
    public SpecialLogic(int i) {
        super(i);
        switch (i) {
            case SPECIAL_FINDALL:
            case SPECIAL_FINDALL_END:
                break;
            case SPECIAL_CALL_COLON:
            case SPECIAL_CALL_COLONCOLON:
                subflags |= MASK_DELE_VIRT;
                break;
            case SPECIAL_SYS_REPLACE_SITE:
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
     * @throws EngineException Shit happens.
     * @throws EngineMessage Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        switch (id) {
            case SPECIAL_FINDALL:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                ListArray<Object> list = iterFindAll(temp[0], ref, en);

                en.skel = en.store.foyer.ATOM_NIL;
                en.display = Display.DISPLAY_CONST;
                createList(list, en);

                Display d = en.display;
                boolean multi = d.getAndReset();
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_FINDALL_END:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                list = iterFindAll(temp[0], ref, en);

                en.skel = temp[3];
                en.display = ref;
                en.deref();
                createList(list, en);

                d = en.display;
                multi = d.getAndReset();
                if (!en.unifyTerm(temp[2], ref, en.skel, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return true;
            case SPECIAL_CALL_COLON:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                SkelAtom sym=((SkelCompound) en.skel).sym;

                Object obj = EvaluableLogic.slashToClass(temp[0], ref, false, true, en);
                SkelAtom mod = modToAtom(obj, temp[0], ref, en);
                colonToCallable(temp[1], ref, true, en);
                EvaluableLogic.colonToRoutine(mod, sym, true, en);

                Directive dire = SupervisorCall.callGoal2(AbstractDefined.MASK_DEFI_TRAN, en);
                d = en.display;

                CallFrame ref2 = CallFrame.getFrame(d, dire, en);
                en.contskel = dire;
                en.contdisplay = ref2;
                return true;
            case SPECIAL_CALL_COLONCOLON:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                sym=((SkelCompound) en.skel).sym;

                en.skel = temp[0];
                en.display = ref;
                en.deref();
                Object recv = en.skel;
                Display d2 = en.display;

                obj = EvaluableLogic.slashToClass(recv, d2, true, true, en);
                mod = objToAtom(obj, recv, d2, en);
                colonToCallable(temp[1], ref, true, en);
                EvaluableLogic.colonToMethod(mod, sym, recv, d2, true, en);

                dire = SupervisorCall.callGoal2(AbstractDefined.MASK_DEFI_TRAN, en);
                d = en.display;

                ref2 = CallFrame.getFrame(d, dire, en);
                en.contskel = dire;
                en.contdisplay = ref2;
                return true;
            case SPECIAL_SYS_REPLACE_SITE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                en.skel = temp[2];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                obj = en.skel;
                d = en.display;

                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SkelAtom sa2 = StackElement.callableToName(en.skel);

                SkelAtom sa = StackElement.callableToName(obj);
                sa = EvaluableLogic.makeAtom(sa.fun, en, sa2);
                obj = StackElement.callableFromName(obj, sa);
                if (!en.unifyTerm(temp[0], ref, obj, d))
                    return false;
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Find all solutions.</p>
     * <p>The term is passed in skel and display of the engine.</p>
     *
     * @param t2 The template term skel.
     * @param d2 The template term display.
     * @param en The engine.
     * @return The list of solutions.
     * @throws EngineException Shit happens.
     */
    private static ListArray<Object> iterFindAll(Object t2, Display d2,
                                                 Engine en)
            throws EngineException {
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        ListArray<Object> temp = null;
        AbstractUndo mark = en.bind;
        int snap = en.number;
        try {
            Directive dire = SupervisorCall.callGoal(AbstractDefined.MASK_DEFI_CALL, en);
            Display d3 = en.display;

            CallFrame ref2 = CallFrame.getFrame(d3, dire, en);
            en.contskel = dire;
            en.contdisplay = ref2;
            boolean found = en.runLoop(snap, true);
            while (found) {
                Object val = AbstractSkel.copySkel(t2, d2, en);
                if (temp == null)
                    temp = new ListArray<Object>();
                temp.add(val);
                found = en.runLoop(snap, false);
            }
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            throw en.fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            throw en.fault;
        }
        en.contskel = r;
        en.contdisplay = u;
        en.fault = null;
        en.releaseBind(mark);
        if (en.fault != null)
            throw en.fault;
        return temp;
    }

    /**
     * <p>Create the result list.</p>
     * <p>The end is passed in skel and display of the engine.</p>
     * <p>Result is returned in skel and display of the engine.</p>
     *
     * @param temp The list of solutions or null.
     * @param en   The engine.
     */
    private static void createList(ListArray<Object> temp, Engine en) {
        if (temp == null)
            return;
        for (int i = temp.size() - 1; i >= 0; i--) {
            Object t = en.skel;
            Display d = en.display;
            Object val = temp.get(i);
            Display ref = AbstractSkel.createMarker(val);
            pairValue(en.store.foyer.CELL_CONS,
                    val, ref, t, d, en);
        }
    }

    /**
     * <p>Cons the value to the given term.</p>
     * <p>The result is returned in skeleton and display.</p>
     *
     * @param t2 The term skeleton.
     * @param d2 The term display.
     * @param t  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     */
    public static void pairValue(SkelCompound sc,
                                 Object t2, Display d2,
                                 Object t, Display d, Engine en) {
        Object v2 = SupervisorCopy.getVar(t2);
        Object v = SupervisorCopy.getVar(t);
        if (v2 == null) {
            Object[] args = new Object[2];
            args[0] = t2;
            args[1] = t;
            en.skel = new SkelCompound(sc.sym, args, v);
            en.display = d;
        } else if (v == null) {
            Object[] args = new Object[2];
            args[0] = t2;
            args[1] = t;
            en.skel = new SkelCompound(sc.sym, args, v2);
            en.display = d2;
        } else {
            Display d3 = new Display(2);
            d3.marker = true;
            boolean ext = d2.getAndReset();
            d3.bind[0].bindUniv(t2, d2, en);
            if (ext)
                d2.remTab(en);
            ext = d.getAndReset();
            d3.bind[1].bindUniv(t, d, en);
            if (ext)
                d.remTab(en);
            en.skel = sc;
            en.display = d3;
        }
    }

    /****************************************************************/
    /* Name Helper                                                  */
    /****************************************************************/

    /**
     * <p>Retrieve the module name.</p>
     *
     * @param mod The object.
     * @param t   The slash skeleton.
     * @param d   The slash display.
     * @param en  The engine.
     * @return The nodule name.
     * @throws EngineMessage Shit happens.
     */
    static SkelAtom objToAtom(Object mod, Object t, Display d,
                              Engine en)
            throws EngineMessage {
        if (!(mod instanceof AbstractSkel) &&
                !(mod instanceof Number)) {
            /* reference */
            mod = SpecialProxy.refClassOrProxy(mod);
            if (mod == null)
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, t), d);
            mod = SpecialProxy.classOrProxyName(mod, en);
            if (mod == null)
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_CLASS, t), d);
        } else {
            /* atom */
        }
        return (SkelAtom) mod;
    }

    /**
     * <p>Retrieve the module atom.</p>
     *
     * @param mod The object.
     * @param t   The slash skeleton.
     * @param d   The slash display.
     * @param en  The engine.
     * @return The nodule atom.
     * @throws EngineMessage Shit happens.
     */
    public static SkelAtom modToAtom(Object mod, Object t, Display d, Engine en)
            throws EngineMessage {
        if (!(mod instanceof AbstractSkel) &&
                !(mod instanceof Number)) {
            /* reference */
            mod = SpecialProxy.classOrProxyName(mod, en);
            if (mod == null)
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_CLASS, t), d);
        } else {
            /* atom */
        }
        return (SkelAtom) mod;
    }


    /************************************************************/
    /* Callable Colon                                           */
    /************************************************************/

    /**
     * <p>Convert a colon to a callable.</p>
     * <p>The result is return in skel and display of the engine.</p>
     * <p>A qualified callable has the following syntax.</p>
     * <pre>
     *     colon --> module ":" colon
     *             | receiver "::" colon
     *             | term.
     * </pre>
     *
     * @param t    The colon skeleton.
     * @param d    The colon display.
     * @param comp The compound flag.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void colonToCallable(Object t, Display d,
                                       boolean comp,
                                       Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(EvaluableLogic.OP_COLON)) {
            SkelCompound temp = (SkelCompound) t;
            Object obj = EvaluableLogic.slashToClass(temp.args[0], d, false, true, en);
            SkelAtom mod = modToAtom(obj, temp.args[0], d, en);
            colonToCallable(temp.args[1], d, comp, en);
            EvaluableLogic.colonToRoutine(mod, temp.sym, comp, en);
        } else if (comp && t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(EvaluableLogic.OP_COLONCOLON)) {
            SkelCompound temp = (SkelCompound) t;

            en.skel = temp.args[0];
            en.display = d;
            en.deref();
            Object recv = en.skel;
            Display d2 = en.display;

            Object obj = EvaluableLogic.slashToClass(recv, d2, true, true, en);
            SkelAtom mod = objToAtom(obj, recv, d2, en);
            colonToCallable(temp.args[1], d, comp, en);
            EvaluableLogic.colonToMethod(mod, temp.sym, recv, d2, comp, en);
        }
    }

}