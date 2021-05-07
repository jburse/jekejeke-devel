package jekmin.reference.experiment;

import jekpro.frequent.experiment.InterfaceReference;
import jekpro.model.inter.Engine;
import jekpro.tools.term.AbstractSkel;

/**
 * <p>This class represents a hook.</p>
 * <p>Does not provide hash, equal or lexical comparison.</p>
 *
 * @author Copyright 2013-2018, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Minlog 0.6.6 (minimal logic extension)
 */
public final class Hook implements InterfaceReference {
    public final static int MASK_HOOK_ASSE = 0x00000001;

    final Object term;
    final BindAttr attr;
    int flags;

    /**
     * <p>Create a new hook.</p>
     *
     * @param v The template skeleton.
     * @param a The attributed variable.
     */
    public Hook(Object v, BindAttr a) {
        term = v;
        attr = a;
    }

    /**********************************************************/
    /* Reference Protocol                                     */
    /**********************************************************/

    /**
     * <p>Assert this reference.</p>
     *
     * @param flags The flags.
     * @param en    The engine.
     * @return True if the reference was asserted, otherwise false.
     */
    public boolean assertRef(int flags, Engine en) {
        return attr.assertHook(this, flags);
    }

    /**
     * <p>Retract this reference.</p>
     *
     * @param en The engine.
     * @return True if the reference was retracted, otherwise false.
     */
    public boolean retractRef(Engine en) {
        return attr.retractHook(this);
    }

    /**
     * <p>Clause this reference.</p>
     * <p>The result is returned in the skeleton and display.</p>
     *
     * @param en The engine.
     */
    public void clauseRef(Engine en) {
        Object val = term;
        en.skel = val;
        en.display = AbstractSkel.createMarker(val);
    }

}
