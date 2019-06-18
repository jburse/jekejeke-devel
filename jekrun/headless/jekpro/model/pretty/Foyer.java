package jekpro.model.pretty;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.molec.SkelAtomQuali;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Operator;
import jekpro.model.rope.Resource;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.comp.sharik.AbstractTracking;
import matula.comp.sharik.Enforced;
import matula.util.config.AbstractBundle;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
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
public class Foyer extends Enforced {
    public final static String OP_COMMA = ",";
    public final static String OP_SLASH = "/";
    public final static String OP_NIL = "[]";
    public final static String OP_CONS = ".";
    public final static String OP_SUB = "-";
    public final static String OP_TRUE = "true";
    public final static String OP_EQUAL = "=";
    private final static String OP_LESS = "<";
    private final static String OP_GREATER = ">";
    private final static String OP_JEKEJEKE = "jekejeke";
    public final static String OP_SET = "{}";
    public final static String OP_UNIT = "()";
    public final static String OP_SEMICOLON = ";";
    public final static String OP_CONDITION = "->";
    public final static String OP_SOFT_CONDITION = "*->";

    public final static String OP_SYS_ALTER = "sys_alter";
    public final static String OP_SYS_GUARD = "sys_guard";
    public final static String OP_SYS_BEGIN = "sys_begin";
    public final static String OP_SYS_COMMIT = "sys_commit";
    public final static String OP_SYS_SOFT_BEGIN = "sys_soft_begin";
    public final static String OP_SYS_SOFT_COMMIT = "sys_soft_commit";
    public final static String OP_CALL = "call";

    public final static String OP_INDEX = "sys_index";
    public final static String OP_STRUCT = "sys_struct";

    public final static int MASK_FOYER_NIST = 0x00000010;
    public final static int MASK_FOYER_CEXP = 0x00000020;
    public final static int MASK_FOYER_NIDX = 0x00000040;

    public final static int MASK_FOYER_NBDY = 0x00000100;
    public final static int MASK_FOYER_NSTK = 0x00000200;
    public final static int MASK_FOYER_NHED = 0x00000400;

    public final static int MASK_FOYER_SMRY = 0x00001000;
    public final static int MASK_FOYER_DTLS = 0x00002000;

    public final static int IO_TERM = 0;
    public final static int IO_ANNO = 1;

    public static final int HINT_CMD = 1;
    public static final int HINT_GUI = 2;
    public static final int HINT_WEB = 3;

    private final ListArray<Integer> hole = new ListArray<Integer>();
    private int nextseqid;
    public Object goodfor;
    public Locale locale = Locale.getDefault();
    public Object proxy;

    public long timeout = 30000; /* half minute */
    public Random random = new Random();
    private int flags;
    //    private int gensym;
    private int hint;

    private final ListArray<Store> regs = new ListArray<Store>();
    private Store[] cacheregs;

    public final SkelAtom ATOM_NIL = new SkelAtom(Foyer.OP_NIL);
    public final SkelAtom ATOM_CONS = new SkelAtom(Foyer.OP_CONS);
    public final SkelAtom ATOM_SUB = new SkelAtom(Foyer.OP_SUB);
    public final SkelAtom ATOM_EQUAL = new SkelAtom(Foyer.OP_EQUAL);
    public final SkelAtom ATOM_LESS = new SkelAtom(Foyer.OP_LESS);
    public final SkelAtom ATOM_GREATER = new SkelAtom(Foyer.OP_GREATER);
    public final SkelAtom ATOM_JEKEJEKE = new SkelAtom(Foyer.OP_JEKEJEKE);

    public final SkelCompound CELL_CONS = new SkelCompound(ATOM_CONS,
            SkelVar.valueOf(0), SkelVar.valueOf(1));
    public final SkelCompound CELL_SUB = new SkelCompound(ATOM_SUB,
            SkelVar.valueOf(0), SkelVar.valueOf(1));

    public SkelCompound CELL_COMMA;

    public SkelAtom ATOM_COMMA;
    public SkelAtom ATOM_TRUE;
    public SkelAtom ATOM_SLASH;
    public SkelAtom ATOM_SEMICOLON;
    public SkelAtom ATOM_CONDITION;
    public SkelAtom ATOM_SOFT_CONDITION;

    public SkelAtom ATOM_SYS_ALTER;
    public SkelAtom ATOM_SYS_GUARD;
    public SkelAtom ATOM_SYS_BEGIN;
    public SkelAtom ATOM_SYS_COMMIT;
    public SkelAtom ATOM_SYS_SOFT_BEGIN;
    public SkelAtom ATOM_SYS_SOFT_COMMIT;
    public SkelAtom ATOM_CALL;

    public Directive CLAUSE_CONT;
    public Directive CLAUSE_CALL;
    public Directive CLAUSE_TRAN;

    public Directive CLAUSE_HOOK;

    /**
     * <p>Retrieve the factory.</p>
     *
     * @return The factory.
     */
    public AbstractFactory getFactory() {
        return (AbstractFactory) getFramework();
    }

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
    public PrologWriter createWriter(int io) {
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
     * <p>Can be overridden by subclasses.</p>
     *
     * @return The store.
     */
    public Store createStore() {
        Store store = new Store(this, null);
        store.loader = getClass().getClassLoader();
        return store;
    }

    /**
     * <p>Factory for stores.</p>
     * <p>Can be overridden by subclasses.</p>
     *
     * @param c The class.
     * @return The store.
     */
    public Store createStore(Class c) {
        Store store = new Store(this, null);
        store.loader = c.getClassLoader();
        return store;
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
     * <p>Factory for directives.</p>
     *
     * @param copt The directives option flags.
     * @return The directives.
     */
    public Directive createDirective(int copt) {
        return new Directive(copt);
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
     * <p>Create a new locator.</p>
     *
     * @param src The source.
     * @return The new locator, or null.
     */
    public AbstractLocator createLocator(AbstractSource src) {
        return null;
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
    /* Store Registry                                         */
    /**********************************************************/

    /**
     * <p>Add a store to the registry.</p>
     *
     * @param store The store.
     */
    public void addStore(Store store) {
        if (store.parent == null)
            setRoot(store);
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
        if (store.parent == null)
            setRoot(null);
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
    public final void setHint(int h) {
        hint = h;
    }

}