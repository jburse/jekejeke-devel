package jekdev.model.pretty;

import jekdev.model.builtin.SupervisorTrace;
import jekdev.reference.debug.SpecialDefault;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.tools.term.PositionKey;
import matula.util.data.ListArray;
import matula.util.data.SetEntry;
import matula.util.data.SetHash;

/**
 * <p>An object that encapsulates a knowledge base. An knowledge base
 * is a set of source, resource and canoncache. Source contain predicates
 * and operators.</p>
 * <p>Debugger version of it.</p>
 *
 * @author Copyright 2018, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.3.1 (a fast and small prolog interpreter)
 */
public class StoreTrace extends Store {
    private SetTable stab;
    private SetHash<PositionKey> breakpoints;
    private int maxstack = Store.DEFAULT_MAX_STACK;
    private long lastmod;
    public int flags;

    /**
     * <p>Create a new store.</p>
     *
     * @param f The foyer.
     * @param p The parent store.
     */
    StoreTrace(Foyer f, Store p) {
        super(f, p);
    }

    /***************************************************************/
    /* Mode Handling                                               */
    /***************************************************************/

    /**
     * <p>Set the debug mode.</p>
     *
     * @param m The mode.
     */
    public void setMode(int m) {
        synchronized (this) {
            flags = (flags & ~SpecialDefault.MASK_MODE_DEBG) | m;
        }
    }

    /**
     * <p>Set the engine leash flags.</p>
     *
     * @param f The leash flags.
     */
    public void setLeash(int f) {
        synchronized (this) {
            flags = (flags & ~SpecialDefault.MASK_MODE_LESH) | f;
        }
    }

    /**
     * <p>Set the leash flags.</p>
     *
     * @param f The leash flags.
     */
    public void setVisible(int f) {
        synchronized (this) {
            flags = (flags & ~SpecialDefault.MASK_MODE_VIBL) | f;
        }
    }

    /**
     * <p>Retrieve the last modified.</p>
     *
     * @return The last modified.
     */
    public long getLastModified() {
        return lastmod;
    }

    /**
     * <p>Set the last modified.</p>
     *
     * @param l The last modified.
     */
    public void setLastModified(long l) {
        lastmod = l;
    }

    /***************************************************************/
    /* Spypoint Handling                                           */
    /***************************************************************/

    /**
     * <p>Check presence of a spy point.</p>
     *
     * @param fun   The functor.
     * @param arity The arity.
     * @param mod The module.
     * @return True if the spy point is present, otherwise false.
     */
    public boolean containsSpyPoint(String fun, int arity, String mod) {
        if (stab == null)
            return false;
        synchronized (this) {
            if (stab == null)
                return false;
            return stab.contains(fun, arity, mod);
        }
    }

    /**
     * <p>Add a spy point.</p>
     *
     * @param fun   The functor.
     * @param arity The arity.
     * @param mod The module
     */
    public void addSpyPoint(String fun, int arity, String mod) {
        synchronized (this) {
            if (stab == null)
                stab = new SetTable();
            if (stab.contains(fun, arity, mod))
                return;
            stab.add(fun, arity, mod);
        }
    }

    /**
     * <p>Remove a spy point.</p>
     *
     * @param fun   The functor.
     * @param arity The arity.
     * @param mod The module.
     */
    public void removeSpyPoint(String fun, int arity, String mod) {
        if (stab == null)
            return;
        synchronized (this) {
            if (stab == null)
                return;
            if (!stab.contains(fun, arity, mod))
                return;
            stab.remove(fun, arity, mod);
        }
    }

    /**
     * <p>Perform a snapshot of the spy points.</p>
     *
     * @return The spy points.
     */
    public StoreKey[] snapshotSpyPoints() {
        if (stab == null)
            return SupervisorTrace.VOID_KEYS;
        synchronized (this) {
            if (stab == null)
                return SupervisorTrace.VOID_KEYS;
            StoreKey[] keys = new StoreKey[stab.deepSize()];
            stab.toDeepArray(keys);
            return keys;
        }
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
    public boolean containsBreakPoint(PositionKey pk) {
        if (breakpoints == null)
            return false;
        synchronized (this) {
            if (breakpoints == null)
                return false;
            return breakpoints.getKey(pk) != null;
        }
    }

    /**
     * <p>Add a break point.</p>
     *
     * @param pk The break point.
     */
    public void addBreakPoint(PositionKey pk) {
        synchronized (this) {
            if (breakpoints == null) {
                breakpoints = new SetHash<>();
                breakpoints.add(pk);
            } else if (breakpoints.getKey(pk) == null) {
                breakpoints.add(pk);
            }
        }
    }

    /**
     * <p>Remove a break point.</p>
     *
     * @param pk The break point.
     */
    public void removeBreakPoint(PositionKey pk) {
        if (breakpoints == null)
            return;
        synchronized (this) {
            if (breakpoints == null)
                return;
            breakpoints.remove(pk);
            if (breakpoints.size == 0)
                breakpoints = null;
        }
    }

    /**
     * <p>Perform a snapshot of the break points.</p>
     *
     * @return The break points.
     */
    public ListArray<PositionKey> snapshotBreakPoints() {
        ListArray<PositionKey> res = new ListArray<>();
        if (breakpoints == null)
            return res;
        synchronized (this) {
            if (breakpoints == null)
                return res;
            for (SetEntry<PositionKey> entry = breakpoints.getFirstEntry();
                 entry != null; entry = breakpoints.successor(entry)) {
                res.add(entry.value);
            }
        }
        return res;
    }

    /*******************************************************************/
    /* Max Store                                                       */
    /*******************************************************************/

    /**
     * <p>Retrieve the max stack setting.</p>
     *
     * @return The max stack setting.
     */
    public int getMaxStack() {
        return maxstack;
    }

    /**
     * <p>Set the max stack setting.</p>
     *
     * @param m The max stack setting.
     */
    public void setMaxStack(int m) {
        maxstack = m;
    }

}