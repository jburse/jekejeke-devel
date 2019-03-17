package jekdev.model.bugger;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.StoreKey;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.SkelAtom;
import matula.util.data.MapHash;

/**
 * <p>This class provides trace predicate properties.</p>
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
 * Only to be distributed with programs that add sgnificant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class PropertyTracePredicate extends AbstractProperty<Predicate> {
    private final static String OP_SYS_NOINSTRUMENT = "sys_noinstrument";
    private final static String OP_SYS_NOWAKEUP = "sys_nowakeup";

    private static final int PROP_SYS_NOINSTRUMENT = 0;
    private static final int PROP_SYS_NOWAKEUP = 1;

    /**
     * <p>Create a predicate property.</p>
     *
     * @param i The id of the predicate property.
     */
    private PropertyTracePredicate(int i) {
        super(i);
    }


    /**
     * <p>Define the predicate properties.</p>
     *
     * @return The predicate properties.
     */
    public static MapHash<StoreKey, AbstractProperty<Predicate>> definePredProps() {
        MapHash<StoreKey, AbstractProperty<Predicate>> predprops = new MapHash<StoreKey, AbstractProperty<Predicate>>();
        predprops.add(new StoreKey(OP_SYS_NOINSTRUMENT, 0), new PropertyTracePredicate(PROP_SYS_NOINSTRUMENT));
        predprops.add(new StoreKey(OP_SYS_NOWAKEUP, 0), new PropertyTracePredicate(PROP_SYS_NOWAKEUP));
        return predprops;
    }

    /**
     * <p>Retrieve all the predicate properties.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @return The predicate properties.
     * @throws EngineMessage Shit happens.
     */
    public Object[] getObjProps(Predicate pick, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_SYS_NOINSTRUMENT:
                AbstractDelegate fun = pick.del;
                if (fun instanceof AbstractDefined &&
                        (fun.subflags & AbstractDefined.MASK_DEFI_NIST) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NOINSTRUMENT)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_SYS_NOWAKEUP:
                fun = pick.del;
                if (fun instanceof AbstractDefined &&
                        (fun.subflags & AbstractDefined.MASK_DEFI_NHWK) != 0) {
                    return new Object[]{new SkelAtom(OP_SYS_NOWAKEUP)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Set a predicate property.</p>
     *
     * @param pick The predicate.
     * @param m    The property skeleton.
     * @param d    The property display.
     * @param en   The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setObjProp(Predicate pick, Object m, Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_SYS_NOINSTRUMENT:
                AbstractDelegate fun = pick.del;
                AbstractDefined.checkDefinedWrite(fun, pick, en);
                fun.subflags |= AbstractDefined.MASK_DEFI_NIST;
                return true;
            case PROP_SYS_NOWAKEUP:
                fun = pick.del;
                AbstractDefined.checkDefinedWrite(fun, pick, en);
                fun.subflags |= AbstractDefined.MASK_DEFI_NHWK;
                return true;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Reset a predicate property.</p>
     *
     * @param pick The predicate.
     * @param m    The property skeleton.
     * @param d    The property display.
     * @param en   The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean resetObjProp(Predicate pick, Object m, Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_SYS_NOINSTRUMENT:
                AbstractDelegate fun = pick.del;
                AbstractDefined.checkDefinedWrite(fun, pick, en);
                fun.subflags &= ~AbstractDefined.MASK_DEFI_NIST;
                return true;
            case PROP_SYS_NOWAKEUP:
                fun = pick.del;
                AbstractDefined.checkDefinedWrite(fun, pick, en);
                fun.subflags &= ~AbstractDefined.MASK_DEFI_NHWK;
                return true;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

}