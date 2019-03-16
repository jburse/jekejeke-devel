package jekdev.model.bugger;

import jekdev.model.pretty.LocatorTrace;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractLocator;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

/**
 * <p>This class provides trace source properties.</p>
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
public final class PropertyTraceSource extends AbstractProperty<AbstractSource> {
    private final static String OP_SYS_FIRST_LOCATION = "sys_first_location";
    private final static String OP_SYS_LOCATION = "sys_location";

    private static final int PROP_SYS_FIRST_LOCATION = 0;
    private static final int PROP_SYS_LOCATION = 1;

    /**
     * <p>Create a source property.</p>
     *
     * @param i The id of the source property.
     */
    private PropertyTraceSource(int i) {
        super(i);
    }

    /**
     * <p>Define the source properties.</p>
     *
     * @return The source properties.
     */
    public static MapHash<StoreKey, AbstractProperty<AbstractSource>> defineSrcProps() {
        MapHash<StoreKey, AbstractProperty<AbstractSource>> srcprops = new MapHash<StoreKey, AbstractProperty<AbstractSource>>();
        srcprops.add(new StoreKey(OP_SYS_FIRST_LOCATION, 3), new PropertyTraceSource(PROP_SYS_FIRST_LOCATION));
        srcprops.add(new StoreKey(OP_SYS_LOCATION, 3), new PropertyTraceSource(PROP_SYS_LOCATION));
        return srcprops;
    }

    /**
     * <p>Retrieve all the object properties.</p>
     *
     * @param src The object.
     * @param en  The engine.
     * @return The properties.
     * @throws EngineMessage Shit happens.
     */
    public Object[] getObjProps(AbstractSource src, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_SYS_FIRST_LOCATION:
                AbstractLocator locator = src.locator;
                if (locator == null)
                    return AbstractBranch.FALSE_PROPERTY;
                MapEntry<PositionKey, Predicate[]>[] snapshot = ((LocatorTrace) locator).allFirstPositions();
                if (snapshot.length == 0)
                    return AbstractBranch.FALSE_PROPERTY;
                SkelAtom sa = new SkelAtom(OP_SYS_FIRST_LOCATION);
                return snapshotToVals(sa, snapshot, en);
            case PROP_SYS_LOCATION:
                locator = src.locator;
                if (locator == null)
                    return AbstractBranch.FALSE_PROPERTY;
                snapshot = ((LocatorTrace) locator).allPositions();
                if (snapshot.length == 0)
                    return AbstractBranch.FALSE_PROPERTY;
                sa = new SkelAtom(OP_SYS_LOCATION);
                return snapshotToVals(sa, snapshot, en);
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Set a object property.</p>
     *
     * @param obj The object.
     * @param m   The property skeleton.
     * @param d   The property display.
     * @param en  The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setObjProp(AbstractSource obj, Object m, Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_SYS_FIRST_LOCATION:
                /* can't modify */
                return false;
            case PROP_SYS_LOCATION:
                /* can't modify */
                return false;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Reset a object property.</p>
     *
     * @param obj The object.
     * @param m   The property skeleton.
     * @param d   The property display.
     * @param en  The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean resetObjProp(AbstractSource obj, Object m, Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_SYS_FIRST_LOCATION:
                /* can't modify */
                return false;
            case PROP_SYS_LOCATION:
                /* can't modify */
                return false;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /*********************************************************/
    /* Collection Helper                                     */
    /*********************************************************/

    /**
     * <p>Convert a snapshot to a values.</p>
     *
     * @param sa       The property name.
     * @param snapshot The snapshot.
     * @param en       The engine.
     * @return The values.
     */
    private static Object[] snapshotToVals(SkelAtom sa,
                                           MapEntry<PositionKey, Predicate[]>[] snapshot,
                                           Engine en)
            throws EngineMessage {
        ListArray<Object> list = new ListArray<Object>();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<PositionKey, Predicate[]> entry = snapshot[i];
            Predicate[] snapshot2 = entry.value;
            for (int j = 0; j < snapshot2.length; j++) {
                Predicate pick = snapshot2[j];
                Object skel = SpecialQuali.indicatorToColonSkel(pick.getFun(),
                        pick.getSource().getStore().user,
                        pick.getArity(), en);
                PositionKey pos = entry.key;
                list.add(AbstractTerm.createMolec(new SkelCompound(
                        sa, skel,
                        new SkelAtom(pos.getOrigin()),
                        Integer.valueOf(pos.getLineNo())), Display.DISPLAY_CONST));
            }
        }
        Object[] vals = new Object[list.size()];
        list.toArray(vals);
        return vals;
    }

}