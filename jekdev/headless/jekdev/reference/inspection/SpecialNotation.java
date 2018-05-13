package jekdev.reference.inspection;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.AbstractStore;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapEntry;

/**
 * <p>Provides built-in predicates for direct source access.</p>
 *
 * @author Copyright 2015-2018, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.9 (a fast and small prolog interpreter)
 */
public final class SpecialNotation extends AbstractSpecial {
    private final static int SPECIAL_SYS_ATOM_SLASH = 0;
    private final static int SPECIAL_SYS_CALLABLE_COLON = 1;
    private final static int SPECIAL_SYS_INDICATOR_COLON = 2;

    /**
     * <p>Create a base direct access builtin.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialNotation(int i) {
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
            case SPECIAL_SYS_ATOM_SLASH:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof SkelVar)) {
                    SkelAtom sa = EngineMessage.castStringWrapped(en.skel, en.display);
                    if (!en.unifyTerm(temp[1], ref,
                            SpecialQuali.moduleToSlashSkel(sa.fun, sa.scope,
                                    sa.getPosition(), en), en.display))
                        return false;
                    return en.getNext();
                }
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                SpecialQuali.slashToModule(en);
                if (!en.unifyTerm(temp[0], ref, en.skel, Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_CALLABLE_COLON:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof SkelVar)) {
                    if (!en.unifyTerm(temp[1], ref,
                            SpecialQuali.callableToColonSkel(en.skel, en),
                            en.display))
                        return false;
                    return en.getNext();
                }
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                SpecialQuali.colonToCallable(en);
                if (!en.unifyTerm(temp[0], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_INDICATOR_COLON:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof SkelVar)) {
                    Integer arity = StoreKey.propToIndicator(en);
                    if (!en.unifyTerm(temp[1], ref,
                            SpecialQuali.indicatorToColonSkel((SkelAtom) en.skel, arity.intValue(), en),
                            Display.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                }
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                Integer arity = SpecialQuali.colonToIndicator(en);
                Object val = new SkelCompound(new SkelAtom(Foyer.OP_SLASH), en.skel, arity);
                if (!en.unifyTerm(temp[0], ref, val, Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

}
