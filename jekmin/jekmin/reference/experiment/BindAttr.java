package jekmin.reference.experiment;

import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Intermediate;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelVar;
import jekpro.tools.term.TermVar;

/**
 * <p>This class provides a variable binder for attributes.</p>
 * <p>Will invoke the hooks before bind.</p>
 *
 * @author Copyright 2013-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Minlog 0.6.6 (minimal logic extension)
 */
final class BindAttr extends BindUniv {
    private static final Hook[] LIST_VOID = new Hook[0];

    Hook[] hooks = LIST_VOID;
    final SkelVar attrskel;
    final Display attrdisplay;

    /**
     * <p>Create a new attributed variable.</p>
     *
     * @param v The attr skel.
     * @param d The attr display.
     */
    BindAttr(SkelVar v, Display d) {
        attrskel = v;
        attrdisplay = d;
    }

    /**
     * <p>Create an attribute variable.</p>
     * <p>
     * <p>The result is returned in skeleton and display.</p>.
     */
    static BindAttr createAttr() {
        Display newref = new Display();
        newref.bind = new BindUniv[1];
        BindAttr bc2 = new BindAttr(SkelVar.valueOf(0), newref);
        newref.bind[0] = bc2;
        return bc2;
    }

    /*************************************************************/
    /* Variation Points                                          */
    /*************************************************************/

    /**
     * <p>Bind this variable with a term.</p>
     *
     * @param t  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return True if binding was allowed, otherwise false.
     * @throws EngineException Shit happens.
     */
    public final boolean bindAttr(Object t, Display d, Engine en)
            throws EngineException {
        /* var reorder */
        BindUniv bv;
        if (t instanceof SkelVar &&
                !((bv = d.bind[((SkelVar) t).id]) instanceof BindAttr)) {
            bv.bindUniv(attrskel, attrdisplay, en);
            return true;
        }
        /* execute hooks */
        Hook[] hks = hooks;
        for (int i = 0; i < hks.length; i++) {
            Hook h = hks[i];
            if (!invokeHook(t, d, h.term, en))
                return false;
        }
        /* continue with binding */
        bindUniv(t, d, en);
        return true;
    }

    /**
     * <p>Invoke a hook with the verify flag set to false.</p>
     *
     * @param t  The first term skeleton.
     * @param d  The first term display.
     * @param m2 The hook template.
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false
     * @throws EngineException Shit happens.
     */
    private boolean invokeHook(Object t, Display d,
                               Object m2, Engine en)
            throws EngineException {
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        boolean backverify = en.visor.setVerify(false);
        int snap = en.number;
        try {
            Display d3 = AbstractSkel.createMarker(m2);
            boolean multi = d3.getAndReset();
            Directive dire = en.store.foyer.CLAUSE_HOOK;
            Display d4 = new Display(3);
            d4.bind[0].bindUniv(m2, d3, en);
            if (multi)
                d3.remTab(en);
            d4.bind[1].bindUniv(attrskel, attrdisplay, en);
            d4.bind[2].bindUniv(t, d, en);
            CallFrame ref = CallFrame.getFrame(d4, dire, en);
            en.contskel = dire;
            en.contdisplay = ref;
            if (!en.runLoop(snap, true)) {
                en.contskel = r;
                en.contdisplay = u;
                en.visor.setVerify(backverify);
                return false;
            }
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            en.visor.setVerify(backverify);
            throw en.fault;
        } catch (EngineMessage y) {
            EngineException x = new EngineException(y,
                    EngineException.fetchStack(en));
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.cutChoices(snap);
            en.visor.setVerify(backverify);
            throw en.fault;
        }
        en.contskel = r;
        en.contdisplay = u;
        en.fault = null;
        en.cutChoices(snap);
        en.visor.setVerify(backverify);
        if (en.fault != null)
            throw en.fault;
        return true;
    }

    /*************************************************************/
    /* Assert Retract                                            */
    /*************************************************************/

    /**
     * <p>Set a hook.</p>
     *
     * @param a     The hook.
     * @param flags The flags.
     * @return True if success.
     */
    boolean assertHook(Hook a, int flags) {
        if ((a.flags & Hook.MASK_HOOK_ASSE) != 0)
            return false;
        a.flags |= Hook.MASK_HOOK_ASSE;
        Hook[] hks = hooks;
        Hook[] hksnew;
        if (hks.length == 0) {
            hksnew = new Hook[1];
            hksnew[0] = a;
        } else {
            hksnew = new Hook[hks.length + 1];
            if ((flags & AbstractDefined.OPT_ACTI_BOTT) != 0) {
                System.arraycopy(hks, 0, hksnew, 0, hks.length);
                hksnew[hks.length] = a;
            } else {
                System.arraycopy(hks, 0, hksnew, 1, hks.length);
                hksnew[0] = a;
            }
        }
        hooks = hksnew;
        return true;
    }

    /**
     * <p>Erase a hook.</p>
     *
     * @param a The hook.
     * @return True if success.
     */
    boolean retractHook(Hook a) {
        if ((a.flags & Hook.MASK_HOOK_ASSE) == 0)
            return false;
        a.flags &= ~Hook.MASK_HOOK_ASSE;
        Hook[] hks = hooks;
        if (hks.length == 1) {
            hooks = LIST_VOID;
        } else {
            int i;
            for (i = 0; i < hks.length && hks[i] != a; i++) ;
            Hook[] hksnew = new Hook[hks.length - 1];
            System.arraycopy(hks, 0, hksnew, 0, i);
            System.arraycopy(hks, i + 1, hksnew, i, hks.length - 1 - i);
            hooks = hksnew;
        }
        return true;
    }

    /**
     * <p>Retrieve the attributed variable.</p>
     *
     * @return The attributed variable or null.
     */
    public TermVar getAttr() {
        /* already garbage collected? */
        if (attrdisplay.bind[attrskel.id] == null)
            return null;
        /* already instantiated? */
        if (display != null)
            return null;
        return (TermVar) AbstractTerm.createMolec(attrskel, attrdisplay);
    }

}
