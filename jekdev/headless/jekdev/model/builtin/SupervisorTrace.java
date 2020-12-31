package jekdev.model.builtin;

import jekdev.model.pretty.SetTable;
import jekdev.reference.debug.SpecialDefault;
import jekpro.frequent.standard.SupervisorCall;
import jekpro.model.inter.StackElement;
import jekpro.model.inter.Supervisor;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.StoreKey;
import jekpro.tools.term.PositionKey;
import matula.util.data.ListArray;
import matula.util.data.SetEntry;
import matula.util.data.SetHash;

/**
 * <p>The class provides a supervisor trace.</p>
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
public final class SupervisorTrace extends Supervisor {
    public static StoreKey[] VOID_KEYS = new StoreKey[0];

    public StackElement skipframe;
    private SetTable tstab;
    private SetHash<PositionKey> tbreakpoints;
    public long lastmod;

    /**
     * <p>Create a supervisor for a store.</p>
     *
     * @param f The foyer.
     */
    public SupervisorTrace(Foyer f) {
        super(f);
        flags = SpecialDefault.MASK_DEBG_INHR;
    }

    /**
     * <p>Retrieve a supervisor call service object.</p>
     *
     * @return The supervisor call service object.
     */
    public SupervisorCall getCall() {
        SupervisorCall ec = call;
        if (ec == null) {
            ec = new SupervisorCallTrace();
            call = ec;
        }
        return ec;
    }

    /***************************************************************/
    /* Thread Mode Handling                                        */
    /***************************************************************/

    /**
     * <p>Set the engine debug mode.</p>
     *
     * @param m The debug mode.
     */
    public void setThreadMode(int m) {
        synchronized (this) {
            flags = (flags & ~SpecialDefault.MASK_MODE_DEBG) | m;
        }
    }

    /**
     * <p>Set the engine leash flags.</p>
     *
     * @param f The leash flags.
     */
    public void setThreadLeash(int f) {
        synchronized (this) {
            flags = (flags & ~SpecialDefault.MASK_MODE_LESH) | f;
        }
    }

    /**
     * <p>Set the leash flags.</p>
     *
     * @param f The leash flags.
     */
    public void setThreadVisible(int f) {
        synchronized (this) {
            flags = (flags & ~SpecialDefault.MASK_MODE_VIBL) | f;
        }
    }

    /***************************************************************/
    /* Spypoint Handling                                           */
    /***************************************************************/

    /**
     * <p>Check presence of a spy point.</p>
     *
     * @param arity The arity.
     * @param fun   The name.
     * @return True if the spy point is present, otherwise false.
     */
    public boolean containsThreadSpyPoint(int arity, String fun) {
        if (tstab == null)
            return false;
        return tstab.contains(fun, arity);
    }

    /**
     * <p>Add a spy point.</p>
     *
     * @param arity The arity.
     * @param fun   The name.
     */
    public void addThreadSpyPoint(int arity, String fun) {
        if (tstab == null)
            tstab = new SetTable();
        if (tstab.contains(fun, arity))
            return;
        tstab.add(fun, arity);
    }

    /**
     * <p>Remove a spy point.</p>
     *
     * @param arity The arity.
     * @param fun   The name.
     */
    public void removeThreadSpyPoint(int arity, String fun) {
        if (tstab == null)
            return;
        if (!tstab.contains(fun, arity))
            return;
        tstab.remove(fun, arity);
    }

    /**
     * <p>Perform a snapshot of the spy points.</p>
     *
     * @return The spy points.
     */
    public StoreKey[] snapshotThreadSpyPoints() {
        if (tstab == null)
            return VOID_KEYS;
        StoreKey[] keys = new StoreKey[tstab.deepSize()];
        tstab.toDeepArray(keys);
        return keys;
    }

    /***************************************************************/
    /* Breakpoint Handling                                         */
    /***************************************************************/

    /**
     * <p>Check presence of a break point.</p>
     *
     * @param pk The break point.
     * @return True if the break point is present, otherwise false.
     */
    public boolean containsThreadBreakPoint(PositionKey pk) {
        if (tbreakpoints == null)
            return false;
        return tbreakpoints.getKey(pk) != null;
    }

    /**
     * <p>Add a break point.</p>
     *
     * @param pk The break point.
     */
    public void addThreadBreakPoint(PositionKey pk) {
        if (tbreakpoints == null) {
            tbreakpoints = new SetHash<>();
            tbreakpoints.add(pk);
        } else if (tbreakpoints.getKey(pk) == null) {
            tbreakpoints.add(pk);
        }
    }

    /**
     * <p>Remove a break point.</p>
     *
     * @param pk The break point.
     */
    public void removeThreadBreakPoint(PositionKey pk) {
        if (tbreakpoints == null)
            return;
        tbreakpoints.remove(pk);
        if (tbreakpoints.size == 0)
            tbreakpoints = null;
    }

    /**
     * <p>Perform a snapshot of the break points.</p>
     *
     * @return The break points.
     */
    public ListArray<PositionKey> snapshotThreadBreakPoints() {
        ListArray<PositionKey> res = new ListArray<>();
        if (tbreakpoints == null)
            return res;
        for (SetEntry<PositionKey> entry = tbreakpoints.getFirstEntry();
             entry != null; entry = tbreakpoints.successor(entry)) {
            res.add(entry.value);
        }
        return res;
    }

    /***************************************************************/
    /* Ignore Handling                                             */
    /***************************************************************/

    /**
     * <p>Set the ignore flag.</p>
     * <p>Needs to be synchronize, since flags is shared.</p>
     *
     * @param f The new ignore flag.
     * @return The old ignore flag.
     */
    public boolean setIgnore(boolean f) {
        synchronized (this) {
            boolean h = (flags & SpecialDefault.MASK_DEBG_NOFL) == 0;
            if (f) {
                flags &= ~SpecialDefault.MASK_DEBG_NOFL;
            } else {
                flags |= SpecialDefault.MASK_DEBG_NOFL;
            }
            return h;
        }
    }

}
