package jekpro.model.pretty;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.molec.SkelAtomQuali;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Location;
import jekpro.model.rope.Operator;
import jekpro.model.rope.Resource;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.comp.sharik.AbstractBundle;
import matula.comp.sharik.AbstractTracking;
import matula.comp.sharik.Enforced;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.system.CacheBounded;
import matula.util.wire.Fence;
import matula.util.wire.LivestockEvent;
import matula.util.wire.LivestockEventClose;
import matula.util.wire.LivestockEventMemory;

import java.util.Locale;
import java.util.Random;

/**
 * <p>Shared by all stores.</p>
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
public abstract class Foyer extends Enforced {
    public final static String OP_COMMA = ",";
    public final static String OP_SLASH = "/";
    public final static String OP_NIL = "[]";
    public final static String OP_CONS = ".";
    public final static String OP_SUB = "-";
    public final static String OP_TRUE = "true";
    public final static String OP_EQUAL = "=";
    public final static String OP_LESS = "<";
    public final static String OP_GREATER = ">";
    public final static String OP_SET = "{}";
    public final static String OP_UNIT = "()";

    public final static String OP_INDEX = "sys_index";
    public final static String OP_STRUCT = "sys_struct";

    public final static int MASK_STORE_NIST = 0x00000010;
    public final static int MASK_STORE_NHWK = 0x00000020;

    public final static int MASK_STORE_NBDY = 0x00000100;
    public final static int MASK_STORE_NLST = 0x00000200;
    public final static int MASK_STORE_NHED = 0x00000400;

    public final static int MASK_STORE_NIDX = 0x00002000;
    public final static int MASK_STORE_NBCV = 0x00004000;
    public final static int MASK_STORE_CEXP = 0x00008000;

    public final static int MASK_STORE_SMRY = 0x00010000;
    public final static int MASK_STORE_DTLS = 0x00020000;

    public final static int IO_TERM = 0;
    public final static int IO_ANNO = 1;

    public static final int HINT_CMD = 1;
    public static final int HINT_GUI = 2;
    public static final int HINT_WEB = 3;

    private final ListArray<Integer> hole = new ListArray<Integer>();
    private int nextseqid;
    public Object belongsto;
    public Locale locale = Locale.getDefault();
    public Object proxy;

    private byte utildouble = ReadOpts.UTIL_CODES;
    private byte utilback = ReadOpts.UTIL_ERROR;
    private byte utilsingle = ReadOpts.UTIL_ATOM;
    public long timeout = 30000; /* half minute */
    public Random random = new Random();
    private int flags;
    //    private int gensym;
    private int hint;

    private final CacheBounded<String, Object> canoncache = new CacheBounded<String, Object>();
    private final ListArray<Store> regs = new ListArray<Store>();
    private Store[] cacheregs;

    public final SkelAtom ATOM_COMMA = new SkelAtom(Foyer.OP_COMMA);
    public final SkelAtom ATOM_SLASH = new SkelAtom(Foyer.OP_SLASH);
    public final SkelAtom ATOM_NIL = new SkelAtom(Foyer.OP_NIL);
    public final SkelAtom ATOM_CONS = new SkelAtom(Foyer.OP_CONS);
    public final SkelAtom ATOM_SUB = new SkelAtom(Foyer.OP_SUB);
    public final SkelAtom ATOM_TRUE = new SkelAtom(Foyer.OP_TRUE);
    public final SkelAtom ATOM_EQUAL = new SkelAtom(Foyer.OP_EQUAL);
    public final SkelAtom ATOM_LESS = new SkelAtom(Foyer.OP_LESS);
    public final SkelAtom ATOM_GREATER = new SkelAtom(Foyer.OP_GREATER);

    public final SkelCompound CELL_CONS = new SkelCompound(ATOM_CONS,
            SkelVar.valueOf(0), SkelVar.valueOf(1));
    public final SkelCompound CELL_SUB = new SkelCompound(ATOM_SUB,
            SkelVar.valueOf(0), SkelVar.valueOf(1));

    public Clause CLAUSE_CONT;
    public Clause CLAUSE_CALL;
    public Clause CLAUSE_HOOK;

    /**
     * <p>Retrieve the factory.</p>
     *
     * @return The factory.
     */
    public AbstractFactory getFactory() {
        return (AbstractFactory) getFramework();
    }

    /************************************************************/
    /* Session Id                                               */
    /************************************************************/

    /**
     * <p>Generatea a new sym.</p>
     *
     * @return The new sym.
     */
//    public int genSym() {
//        int res;
//        synchronized (this) {
//            res = gensym;
//            gensym = res + 1;
//        }
//        return res;
//    }

    /************************************************************/
    /* Thread Locale Identifiers                                */
    /************************************************************/

    /**
     * <p>Acquire a hole.</p>
     *
     * @return The hole.
     */
    public final int acquireHole() {
        int s;
        synchronized (this) {
            int n = hole.size();
            if (n != 0) {
                s = hole.get(n - 1).intValue();
                hole.remove(n - 1);
            } else {
                s = nextseqid;
                nextseqid++;
            }
        }
        return s;
    }

    /**
     * <p>Release a hole.</p>
     *
     * @param id The hole.
     */
    public final void releaseHole(int id) {
        Integer val = Integer.valueOf(id);
        synchronized (this) {
            hole.add(val);
        }
    }

    /************************************************************/
    /* Quotes                                                   */
    /************************************************************/

    /**
     * <p>Retrieve the double quotes utilization.</p>
     *
     * @return The utilization.
     */
    public int getUtilDouble() {
        return utildouble;
    }

    /**
     * <p>Set the double quotes utilization.</p>
     *
     * @param u The utilization.
     */
    public void setUtilDouble(int u) {
        utildouble = (byte) u;
    }

    /**
     * <p>Retrieve the back quotes utilization.</p>
     *
     * @return The utilization.
     */
    public int getUtilBack() {
        return utilback;
    }

    /**
     * <p>Set the back quotes utilization.</p>
     *
     * @param u The utilization.
     */
    public void setUtilBack(int u) {
        utilback = (byte) u;
    }

    /**
     * <p>Retrieve the single quotes utilization.</p>
     *
     * @return The utilization.
     */
    public int getUtilSingle() {
        return utilsingle;
    }

    /**
     * <p>Set the single quotes utilization.</p>
     *
     * @param u The utilization.
     */
    public void setUtilSingle(int u) {
        utilsingle = (byte) u;
    }

    /**************************************************************/
    /* Flags                                                      */
    /**************************************************************/

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public int getBits() {
        return flags;
    }

    /**
     * <p>Set a flag.</p>
     *
     * @param mask The flag mask.
     */
    public void setBit(int mask) {
        synchronized (this) {
            flags |= mask;
        }
    }

    /**
     * <p>Reset a flag.</p>
     *
     * @param mask The flag mask.
     */
    public void resetBit(int mask) {
        synchronized (this) {
            flags &= ~mask;
        }
    }

    /**********************************************************/
    /* Foyer Livecycle                                        */
    /**********************************************************/

    /**
     * <p>Init the foyer.</p>
     *
     * @param en     The engine.
     * @param prompt The prompt flag.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void initFoyer(Engine en, boolean prompt)
            throws EngineMessage, EngineException {
        AbstractBranch[] branches = getFactory().getInitBranches();
        for (int i = 0; i < branches.length; i++)
            branches[i].initBranch(en, prompt, true);
    }

    /**
     * <p>Fini the foyer.</p>
     *
     * @param store The store.
     * @throws EngineMessage Shit happens.
     */
    public void finiFoyer(Store store)
            throws EngineMessage, EngineException {
        try {
            Fence.DEFAULT.fireEvent(new LivestockEventClose(this));
            Fence.DEFAULT.waitInactive(this);
        } catch (InterruptedException x) {
            throw new RuntimeException(x);
        }
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = snapshotTrackings();
        for (int i = snapshot.length - 1; i >= 0; i--) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractBranch branch = (AbstractBranch) entry.key;
            branch.finiBranch(store, true);
        }
    }

    /**
     * <p>Fire a memory event to the worst thread.</p>
     *
     * @throws InterruptedException Shit happens.
     */
    public final void fireEventMemory() throws InterruptedException {
        LivestockEvent e = new LivestockEventMemory(this);
        Fence.DEFAULT.fireEventWorstOffender(e);
    }

    /*********************************************************/
    /* I/O Object Factories                                  */
    /*********************************************************/

    /**
     * <p>Factory for Prolog readers.</p>
     *
     * @return The Prolog reader.
     */
    public PrologReader createReader(int io) {
        switch (io) {
            case IO_TERM:
                return new PrologReader();
            case IO_ANNO:
                return new PrologReaderAnno();
            default:
                throw new IllegalArgumentException("illegal io");
        }
    }

    /**
     * <p>Factory for Prolog writers.</p>
     *
     * @return The Prolog writer.
     */
    public static PrologWriter createWriter(int io) {
        switch (io) {
            case IO_TERM:
                return new PrologWriter();
            case IO_ANNO:
                return new PrologWriterAnno();
            default:
                throw new IllegalArgumentException("illegal io");
        }
    }

    /*********************************************************/
    /* Store etc.. Object Factories                          */
    /*********************************************************/

    /**
     * <p>Factory for stores.</p>
     *
     * @return The store.
     */
    public Store createStore() {
        return new Store(this);
    }

    /**
     * <p>Factory for stores.</p>
     *
     * @param c The class.
     * @return The store.
     */
    public Store createStore(Class c) {
        return new Store(this, c);
    }

    /**
     * <p>Factory for supervisors.</p>
     *
     * @return The supervisor.
     */
    public Supervisor createSupervisor() {
        return new Supervisor(this);
    }

    /**
     * <p>Factory for resources by source key.</p>
     *
     * @param key The source key.
     * @return The resource.
     */
    public Resource createResource(String key) {
        return new Resource(key);
    }

    /*********************************************************/
    /* Atom etc.. Object Factories                           */
    /*********************************************************/

    /**
     * <p>Factory for atoms by name and source.</p>
     *
     * @param f The name.
     * @param s The source.
     * @param m The mask.
     */
    public SkelAtom createAtom(String f, AbstractSource s, int m) {
        if ((m & SkelAtom.MASK_ATOM_ANNO) != 0)
            return new SkelAtomAnno(f, s);
        if ((m & SkelAtom.MASK_ATOM_QALI) != 0)
            return new SkelAtomQuali(f, s);
        return new SkelAtom(f, s);
    }

    /**
     * <p>Factory for clause.</p>
     *
     * @param copt The clause option flags.
     * @return The clause.
     */
    public Clause createClause(int copt) {
        return new Clause(copt);
    }

    /**
     * <p>Create a location</p>
     *
     * @return The location.
     */
    public Location createLocation() {
        return new Location();
    }

    /**
     * <p>Factory for operators by type and key.</p>
     *
     * @param t The type.
     * @param k The key.
     * @return The operator.
     */
    public Operator createOperator(int t, String k) {
        return new Operator(t, k);
    }

    /**********************************************************/
    /* Canon Cache                                            */
    /**********************************************************/

    /**
     * <p>Retrieve the canon cache result.</p>
     *
     * @param path The path.
     * @return The result.
     */
    public Object getCanonCache(String path) {
        synchronized (this) {
            return canoncache.get(path);
        }
    }

    /**
     * <p>Set the canon cache result.</p>
     *
     * @param path The path.
     * @param res  The result.
     */
    public void setCanonCache(String path, Object res) {
        if (res == null)
            throw new NullPointerException("result missing");
        synchronized (this) {
            if (canoncache.get(path) == null)
                canoncache.add(path, res);
        }
    }

    /**
     * <p>Clear the canon cache.</p>
     */
    public void clearCanonCache() {
        synchronized (this) {
            canoncache.clear();
        }
    }

    /**********************************************************/
    /* Store Registry                                         */
    /**********************************************************/

    /**
     * <p>Add a store to the registry.</p>
     *
     * @param store The store.
     */
    public void addStore(Store store) {
        synchronized (this) {
            regs.add(store);
            cacheregs = null;
        }
    }

    /**
     * <p>Remove a store from the registry.</p>
     *
     * @param store The store.
     */
    public void removeStore(Store store) {
        synchronized (this) {
            int k = regs.indexOf(store);
            if (k < 0)
                return;
            regs.remove(k);
            cacheregs = null;
        }
    }

    /**
     * <p>Take a snapshot of the registered stores.</p>
     *
     * @return The registered stores.
     */
    public Store[] snapshotStores() {
        Store[] res = cacheregs;
        if (res != null)
            return res;
        synchronized (this) {
            res = cacheregs;
            if (res != null)
                return res;
            res = new Store[regs.size];
            regs.toArray(res);
            cacheregs = res;
        }
        return res;
    }

    /**********************************************************/
    /* Cildren Notification                                   */
    /**********************************************************/

    /**
     * <p>Notify the sources of a new fix version.</p>
     *
     * @param what The abstract store.
     */
    public void notifyFixvers(Store what) {
        Object o = new Object();
        Store[] stores = snapshotStores();
        for (int j = 0; j < stores.length; j++) {
            Store store = stores[j];
            if (!store.ancestorStore(what))
                continue;
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int i = 0; i < sources.length; i++) {
                AbstractSource source = sources[i].value;
                source.fixvers = o;
            }
        }
    }

    /**
     * <p>Notify the sources of a new import version.</p>
     *
     * @param what The abstract store.
     */
    public void notifyImportvers(Store what) {
        Object o = new Object();
        Store[] stores = snapshotStores();
        for (int j = 0; j < stores.length; j++) {
            Store store = stores[j];
            if (!store.ancestorStore(what))
                continue;
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int i = 0; i < sources.length; i++) {
                AbstractSource source = sources[i].value;
                source.importvers = o;
            }
        }
    }

    /***********************************************************/
    /* Advanced Configuration                                  */
    /***********************************************************/

    /**
     * <p>Retrieve the hint.</p>
     *
     * @return The hint.
     */
    public int getHint() {
        return hint;
    }

    /**
     * <p>Set the hint.</p>
     *
     * @param h The hint.
     */
    public void setHint(int h) {
        hint = h;
    }

}