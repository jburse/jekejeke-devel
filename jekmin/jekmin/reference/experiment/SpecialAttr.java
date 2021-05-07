package jekmin.reference.experiment;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>Provides built-in predicates for the attribute variable predicates.</p>
 *
 * @author Copyright 2013-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Minlog 0.6.6 (minimal logic extension)
 */
public final class SpecialAttr extends AbstractSpecial {
    private final static int SPECIAL_SYS_ENSURE_SERNO = 0;
    private final static int SPECIAL_SYS_COMPILE_HOOK = 1;
    private final static int SPECIAL_SYS_CLAUSE_HOOK = 2;

    public final static String OP_TYPE_VAR = "var";

    /**
     * <p>Create a special attribute.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialAttr(int i) {
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
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        switch (id) {
            case SPECIAL_SYS_ENSURE_SERNO:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof SkelVar))
                    throw new EngineMessage(EngineMessage.typeError(
                            SpecialAttr.OP_TYPE_VAR, en.skel), en.display);
                BindUniv bc = en.display.bind[((SkelVar) en.skel).id];
                if (!(bc instanceof BindAttr)) {
                    BindAttr bc2 = BindAttr.createAttr();
                    bc.bindUniv(bc2.attrskel, bc2.attrdisplay, en);
                    bc2.attrdisplay.remTab(en);
                    UndoSerno.bindSerno(bc2, en);
                } else {
                    bc.getValue(en);
                }
                return true;
            case SPECIAL_SYS_COMPILE_HOOK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof SkelVar))
                    throw new EngineMessage(EngineMessage.typeError(
                            SpecialAttr.OP_TYPE_VAR, en.skel), en.display);
                bc = en.display.bind[((SkelVar) en.skel).id];
                if (!(bc instanceof BindAttr)) {
                    BindAttr bc2 = BindAttr.createAttr();
                    bc.bindUniv(bc2.attrskel, bc2.attrdisplay, en);
                    bc2.attrdisplay.remTab(en);
                    bc = bc2;
                }
                Object val = AbstractSkel.copySkel(temp[1], ref, en);
                Hook hook = new Hook(val, (BindAttr) bc);
                if (!en.unify(hook, Display.DISPLAY_CONST, temp[2], ref))
                    return false;
                return true;
            case SPECIAL_SYS_CLAUSE_HOOK:
                return SpecialAttr.searchHook(en);
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * @param en The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static boolean searchHook(Engine en)
            throws EngineMessage, EngineException {
        Object[] temp = ((SkelCompound) en.skel).args;
        Display ref = en.display;
        en.skel = temp[0];
        en.display = ref;
        en.deref();
        if (!(en.skel instanceof SkelVar))
            throw new EngineMessage(EngineMessage.typeError(
                    SpecialAttr.OP_TYPE_VAR, en.skel), en.display);
        BindUniv bv = en.display.bind[((SkelVar) en.skel).id];
        if (!(bv instanceof BindAttr))
            return false;

        /* find rope */
        Hook[] hks = ((BindAttr) bv).hooks;
        if (hks.length == 0)
            return false;
        AbstractUndo mark = en.bind;
        int at = 0;
        Hook hook;
        /* search rope */
        int size;
        Display ref1 = null;
        for (; ; ) {
            hook = hks[at];
            Object m = hook.term;
            size = SupervisorCopy.displaySize(m);
            if (ref1 == null) {
                ref1 = new Display(size);
            } else {
                ref1.setSize(size);
            }
            at++;
            if (en.unify(m, ref1, temp[1], ref) &&
                    en.unify(hook, Display.DISPLAY_CONST, temp[2], ref))
                break;

            /* end of cursor */
            if (at == hks.length)
                return false;

            /* undo bindings */
            en.fault = null;
            en.releaseBind(mark);
            if (en.fault != null)
                throw en.fault;
        }

        if (size != 0)
            ref1.remTab(en);

        if (at != hks.length) {
            /* create choice point */
            en.choices = new ChoiceHook(en.choices, at, hks,
                    en.contskel, en.contdisplay, ref1, mark);
            en.number++;
        }
        /* succeed */
        return true;
    }

}
