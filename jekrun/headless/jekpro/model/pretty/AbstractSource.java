package jekpro.model.pretty;

import jekpro.frequent.system.ForeignLocale;
import jekpro.model.builtin.Branch;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.CacheSubclass;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.*;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.foreign.LookupBinary;
import jekpro.tools.foreign.LookupResource;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.comp.sharik.AbstractBundle;
import matula.util.data.*;
import matula.util.misc.AbstractLock;
import matula.util.misc.Lock;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignFile;
import matula.util.system.ForeignUri;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.Comparator;

/**
 * <p>This class represents a source. Besides the predicates and operators,
 * a source contains also the uses and imports. Predicates and operators
 * are only assigned when the full name is qualified. Otherwise the
 * module user is used. The class also contains a completion lock.</p>
 * <p>The protocol consists of the following methods.</p>
 * <ul>
 * <li>openReader: Open a read stream.</li>
 * <li>closeReader: Close a read stream and move attributes.</li>
 * <li>initSource: Compute default package, name and parent.</li>
 * <li>clearModule: Clear the module before a reconsult or purge.</li>
 * <li>loadModule: Consult a stream.</li>
 * <li>checkModule: Check the module.</li>
 * </ul>
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
public abstract class AbstractSource implements Comparator<StoreKey> {
    public final static String OP_SYS_SOURCE_PRELOAD = "sys_source_preload";
    public final static String OP_SHORT_NAME = "short_name";
    public final static String OP_LAST_MODIFIED = "last_modified";
    public final static String OP_VERSION_TAG = "version_tag";
    public final static String OP_EXPIRATION = "expiration";
    public final static String OP_SYS_TIMING = "sys_timing";
    public final static String OP_SYS_SOURCE_NAME = "sys_source_name";

    public final static String OP_SYS_SOURCE_VISIBLE = "sys_source_visible";

    public final static String OP_PUBLIC = "public";
    public final static String OP_PRIVATE = "private";

    public final static String OP_END_OF_FILE = "end_of_file";

    private static final String PREFIX_FOREIGN = "foreign:";
    private static final String EXTENSION_HTML = ".html";

    /* source flags */
    public static final int MASK_SRC_NOTR = 0x00000001;
    public static final int MASK_SRC_PREL = 0x00000002;
    public static final int MASK_SRC_VSPR = 0x00000004;
    public static final int MASK_SRC_VSPU = 0x00000008;

    public static final int MASK_SRC_MKDT = 0x00000010;
    public static final int MASK_SRC_FILL = 0x00000020;
    public static final int MASK_SRC_HINT = 0x00000040;
    public static final int MASK_SRC_SCND = 0x00000080;

    /* combined source flags */
    public static final int MASK_SRC_VSNP = MASK_SRC_VSPR | MASK_SRC_VSPU;

    /* import relationship flags */
    public static final int MASK_IMPT_AUTO = 0x00000001;
    public static final int MASK_IMPT_MODL = 0x00000002;
    public static final int MASK_IMPT_REEX = 0x00000004;

    public static final int MASK_IMPT_RSCS = 0x00000010;
    public static final int MASK_IMPT_HOFL = 0x00000020;
    public static final int MASK_IMPT_PAIM = 0x00000040;

    /* combined import relationship flags */
    public static final int MASK_IMPT_INVM = MASK_IMPT_MODL | MASK_IMPT_PAIM;
    public static final int MASK_IMPT_VISI = MASK_IMPT_REEX | MASK_IMPT_INVM;

    /* prefix relationship flags */
    public static final int MASK_PCKG_LIBR = 0x00000001;
    public static final int MASK_PCKG_FRGN = 0x00000002;
    public static final int MASK_USES_LIBR = 0x00000004;
    public static final int MASK_USES_FRGN = 0x00000008;

    /* suffix relationship flags */
    public static final int MASK_USES_TEXT = 0x00000010;
    public static final int MASK_USES_BNRY = 0x00000020;
    public static final int MASK_USES_RSCS = 0x00000040;

    private final String path;
    private int flags = MASK_SRC_VSPU;
    public Object importvers;
    public Object fixvers;
    private AbstractBundle branch;
    private Store store;
    private final MapHashLink<AbstractSource, Integer> imports = new MapHashLink<AbstractSource, Integer>();
    private final MapHashLink<AbstractSource, Integer> importsinv = new MapHashLink<AbstractSource, Integer>();
    private MapEntry<AbstractSource, Integer>[] cacheimports;
    private MapEntry<AbstractSource, Integer>[] cacheimportsinv;
    private final MapHashLink<String, Integer> fixes = new MapHashLink<String, Integer>();
    private MapEntry<String, Integer>[] cachefixes;
    public final MapHashLink<Predicate, Location> locs = new MapHashLink<Predicate, Location>();
    private MapEntry<Predicate, Location>[] cachelocs;
    public final MapHashLink<Predicate, Integer> predsinv = new MapHashLink<Predicate, Integer>();
    public MapEntry<Predicate, Integer>[] cachepredsinv;
    public final SetHashLink<Operator> opsinv = new SetHashLink<Operator>();
    public Operator[] cacheopsinv;
    private final Lock lock = new Lock();
    private final MapHash<String, AssocArray<Integer, Predicate>> preds = new MapHash<String, AssocArray<Integer, Predicate>>();
    private Predicate[] cachepreds;
    private final MapHash<String, AssocArray<Integer, Operator>> ops = new MapHash<String, AssocArray<Integer, Operator>>();
    private Operator[] cacheops;
    private final ListArray<Resource> resources = new ListArray<Resource>();
    private Resource[] cacheresources;
    private String name;
    protected String fullname = Branch.OP_USER;
    private long timing;
    public byte utildouble = ReadOpts.UTIL_CODES;
    public byte utilback = ReadOpts.UTIL_ERROR;
    public byte utilsingle = ReadOpts.UTIL_ATOM;

    /**
     * <p>Create a source from path.</p>
     *
     * @param p The path.
     */
    public AbstractSource(String p) {
        path = p;
    }

    /**
     * <p>Retrieve the path.</p>
     *
     * @return The path.
     */
    public final String getPath() {
        return path;
    }

    /**
     * <p>Retrieve the branch.</p>
     *
     * @return The branch.
     */
    public AbstractBundle getBranch() {
        return branch;
    }

    /**
     * <p>Set the branch.</p>
     *
     * @param b The branch.
     */
    public void setBranch(AbstractBundle b) {
        branch = b;
    }

    /**
     * <p>Retrieve the store.</p>
     *
     * @return The store.
     */
    public Store getStore() {
        return store;
    }

    /**
     * <p>Set the store.</p>
     *
     * @param s The store.
     */
    void setStore(Store s) {
        store = s;
    }

    /**
     * <p>Retrieve the timing.</p>
     *
     * @return The timing.
     */
    public long getTiming() {
        return timing;
    }

    /**
     * <p>Set the timing.</p>
     *
     * @param t The timing.
     */
    public void setTiming(long t) {
        timing = t;
    }

    /**
     * <p>Retrieve the write lock.</p>
     *
     * @return The write lock.
     */
    public AbstractLock getWrite() {
        return lock.getWrite();
    }

    /**
     * <p>Retrieve the read lock.</p>
     *
     * @return The read lock.
     */
    public AbstractLock getRead() {
        return lock.getRead();
    }

    /**
     * <p>Compare two store keys.</p>
     *
     * @param o1 The first store key.
     * @param o2 The second store key.
     * @return < 0 if less than, 0 if equal, > 0 if greater than.
     */
    public int compare(StoreKey o1, StoreKey o2) {
        return o1.compareTo(o2);
    }

    /**************************************************************/
    /* Hierarchical Atom                                         */
    /**************************************************************/

    /**
     * <p>Retrieve a path atom</p>
     *
     * @return The path atom.
     */
    public SkelAtom getPathAtom() {
        return new SkelAtom(getPath(), getStore().user);
    }

    /**************************************************************/
    /* Open & Close Reader                                        */
    /**************************************************************/

    /**
     * <p>Open a read stream.</p>
     *
     * @param if_modified The if modified flag.
     * @param opts        The options.
     * @param en          The engine.
     * @return The reader or null.
     * @throws EngineMessage Shit happens.
     */
    public abstract Reader openReader(boolean if_modified,
                                      LoadOpts opts, Engine en)
            throws EngineMessage;

    /**
     * <p>Close a read stream and move attributes.</p>
     *
     * @param reader The reader.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public void closeReader(Reader reader)
            throws EngineException, EngineMessage {
        try {
            reader.close();
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**************************************************************/
    /* Init & Clear Module                                        */
    /**************************************************************/

    /**
     * <p>Compute default package, name and parent.</p>
     */
    public abstract void initSource()
            throws EngineMessage;

    /**
     * <p>Clear the module before a reconsult or purge.</p>
     *
     * @throws EngineMessage Shit happens.
     */
    public void clearModule()
            throws EngineMessage {
        /* remove predicates */
        MapEntry<Predicate, Integer>[] preds = snapshotPredsInv();
        for (int i = 0; i < preds.length; i++) {
            Predicate pick = preds[i].key;
            pick.clearPredicate(this);
        }

        /* remove operators */
        Operator[] opers = snapshotOpersInv();
        for (int i = 0; i < opers.length; i++) {
            Operator oper = opers[i];
            oper.clearOper(this);
        }

        /* remove resources */
        Resource[] rscs = snapshotResources();
        for (int i = 0; i < rscs.length; i++) {
            Resource res = rscs[i];
            removeResource(res);
        }

        clearLocation();
        resetBit(AbstractSource.MASK_SRC_MKDT);
        resetBit(AbstractSource.MASK_SRC_FILL);
        utildouble = ReadOpts.UTIL_CODES;
        utilback = ReadOpts.UTIL_ERROR;
        utilsingle = ReadOpts.UTIL_ATOM;
    }

    /**************************************************************/
    /* Load & Check Module                                        */
    /**************************************************************/

    /**
     * <p>Consult a stream.</p>
     *
     * @param lr  The buffered reader.
     * @param en  The interpreter.
     * @param rec The recursion flag.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void loadModule(Reader lr, Engine en, boolean rec)
            throws EngineMessage, EngineException {
        PrologReader rd = null;
        PositionKey pos = null;
        for (; ; ) {
            try {
                AbstractSource src = en.visor.peekStack();
                Object val;
                int flags = 0;
                if ((src.getBits() & AbstractSource.MASK_SRC_FILL) != 0)
                    flags |= PrologWriter.FLAG_FILL;
                if ((src.getBits() & AbstractSource.MASK_SRC_MKDT) != 0)
                    flags |= PrologWriter.FLAG_MKDT;
                flags |= PrologReader.FLAG_SING;
                if ((en.store.foyer.getBits() & Foyer.MASK_FOYER_CEXP) == 0 &&
                        (en.store.foyer.getBits() & Foyer.MASK_FOYER_NBCV) != 0)
                    flags |= PrologReader.FLAG_NEWV;
                if (rd == null ||
                        (rd.getFlags() & PrologWriter.FLAG_FILL) !=
                                (flags & PrologWriter.FLAG_FILL)) {
                    if ((flags & PrologWriter.FLAG_FILL) == 0) {
                        rd = en.store.foyer.createReader(Foyer.IO_TERM);
                    } else {
                        rd = en.store.foyer.createReader(Foyer.IO_ANNO);
                    }
                    rd.getScanner().setReader(lr);
                    rd.setEngineRaw(en);
                }
                rd.setFlags(flags);
                rd.setSource(en.visor.peekStack());
                try {
                    try {
                        val = rd.parseHeadStatement();
                    } catch (ScannerError y) {
                        String line = ScannerError.linePosition(OpenOpts.getLine(lr), y.getErrorOffset());
                        rd.parseTailError(y);
                        EngineMessage x = new EngineMessage(EngineMessage.syntaxError(y.getMessage()));
                        pos = (OpenOpts.getPath(lr) != null ?
                                new PositionKey(OpenOpts.getPath(lr), OpenOpts.getLineNumber(lr)) : null);
                        throw new EngineException(x,
                                EngineException.fetchPos(EngineException.fetchLoc(
                                        EngineException.fetchStack(en),
                                        pos, en), line, en)
                        );
                    }
                } catch (IOException y) {
                    throw EngineMessage.mapIOProblem(y);
                }
                pos = (OpenOpts.getPath(lr) != null ?
                        new PositionKey(OpenOpts.getPath(lr), rd.getClauseStart()) : null);
                if (val instanceof SkelAtom &&
                        ((SkelAtom) val).fun.equals(AbstractSource.OP_END_OF_FILE))
                    break;
                ListArray<PreClause> res = FileText.expandTermAndWrap(rd, val, pos, en);
                if (res == null)
                    continue;
                for (int i = res.size() - 1; i >= 0; i--) { /* trick for yfx */
                    PreClause pre = res.get(i);
                    val = pre.molec;
                    if (val instanceof SkelCompound &&
                            ((SkelCompound) val).args.length == 1 &&
                            ((SkelCompound) val).sym.fun.equals(PreClause.OP_TURNSTILE)) {
                        FileText.executeDirective(pre, pos, en);
                    } else {
                        PrologReader.checkSingleton(pre.anon, pos, en);
                        Clause clause = PreClause.determineCompiled(AbstractDefined.OPT_PERF_CNLT, pre.molec, en);
                        clause.vars = pre.vars;
                        clause.assertRef(AbstractDefined.OPT_PERF_CNLT, en);
                    }
                }
            } catch (EngineMessage x) {
                EngineException y = new EngineException(x,
                        EngineException.fetchLoc(
                                EngineException.fetchStack(en), pos, en));
                if (SpecialLoad.systemConsultBreak(y, en, rec))
                    break;
            } catch (EngineException x) {
                if (SpecialLoad.systemConsultBreak(x, en, rec))
                    break;
            }
        }
    }

    /**
     * <p>Check the module.</p>
     *
     * @param en The interpreter.
     * @return True if module is empty, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public boolean checkModule(Engine en)
            throws EngineMessage, EngineException {
        boolean empty = true;

        /* check predicates */
        MapEntry<Predicate, Integer>[] preds = snapshotPredsInv();
        for (int i = 0; i < preds.length; i++) {
            Predicate pred = preds[i].key;
            empty = false;
            Predicate.checkPredicateInit(pred, this, en);
        }

        /* check operators */
        Operator[] opers = snapshotOpersInv();
        for (int i = 0; i < opers.length; i++) {
            Operator oper = opers[i];
            empty = false;
            Operator.checkOperInit(oper, en);
        }

        return empty;
    }

    /**
     * <p>Perform a style check on the given module.</p>
     * <p>This check is performed at the end of loading a module.</p>
     *
     * @param src The module.
     * @param en  The engine.
     * @throws EngineMessage Printing error.
     */
    public static void checkModuleInit(AbstractSource src,
                                       Engine en)
            throws EngineMessage, EngineException {
        try {
            checkModuleNonEmpty(src);
        } catch (EngineMessage x) {
            EngineException y = new EngineException(x,
                    EngineException.fetchStack(en),
                    EngineException.OP_WARNING);
            y.printStackTrace(en);
        }
    }

    /**
     * <p>Perform the non-empty check.</p>
     *
     * @param src The module.
     * @throws EngineMessage The warning.
     */
    private static void checkModuleNonEmpty(AbstractSource src)
            throws EngineMessage {
        /* has resources? */
        Resource[] rscs = src.snapshotResources();
        if (rscs.length > 0)
            return;

        /* has file loads */
        MapEntry<AbstractSource, Integer>[] deps = src.snapshotDeps();
        for (int j = 0; j < deps.length; j++) {
            MapEntry<AbstractSource, Integer> dep = deps[j];
            if ((dep.value.intValue() & MASK_IMPT_MODL) == 0)
                continue;
            return;
        }

        /* has locale module? */
        deps = src.snapshotDepsInv();
        for (int j = 0; j < deps.length; j++) {
            MapEntry<AbstractSource, Integer> dep = deps[j];
            if ((dep.value.intValue() & MASK_IMPT_PAIM) == 0)
                continue;
            return;
        }

        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_MODULE_EMPTY,
                new SkelAtom(ForeignLocale.shortName(src.getPath()))));
    }

    /**************************************************************/
    /* AbstractSource Listing                                     */
    /**************************************************************/

    /**
     * <p>Show the short name of the source key.</p>
     *
     * @param wr  The writer.
     * @param src The source.
     */
    public static void showShortName(Writer wr, AbstractSource src)
            throws EngineMessage {
        try {
            wr.write("% ");
            wr.write(ForeignLocale.shortName(src.getPath()));
            wr.write('\n');
            wr.flush();
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**************************************************************/
    /* Dependency Handling                                        */
    /**************************************************************/

    /**
     * <p>Add a source import.</p>
     *
     * @param s The source import.
     * @param f The import flags.
     * @return The diff flags.
     */
    public int addDep(AbstractSource s, int f) {
        if (f == 0)
            throw new IllegalArgumentException("zero deps");
        int back;
        synchronized (this) {
            Integer im = imports.get(s);
            if (im == null) {
                back = f;
                im = Integer.valueOf(f);
                imports.add(s, im);
            } else {
                int f1 = im.intValue();
                int f2 = f1 | f;
                back = f1 ^ f2;
                if (back == 0)
                    return 0;
                imports.remove(s);
                im = Integer.valueOf(f2);
                imports.add(s, im);
            }
            cacheimports = null;
        }
        s.addDepInv(this, f);
        return back;
    }

    /**
     * <p>Add an inv source import.</p>
     *
     * @param s The inv source import.
     * @param f The import flags.
     */
    private void addDepInv(AbstractSource s, int f) {
        synchronized (this) {
            Integer im = importsinv.get(s);
            if (im == null) {
                im = Integer.valueOf(f);
                importsinv.add(s, im);
            } else {
                int f1 = im.intValue();
                int f2 = f1 | f;
                if (f1 == f2)
                    return;
                importsinv.remove(s);
                im = Integer.valueOf(f2);
                importsinv.add(s, im);
            }
            cacheimportsinv = null;
        }
    }

    /**
     * <p>Remove a source import.</p>
     *
     * @param s The source import.
     * @param f The import flags.
     * @return The diff flags..
     */
    public int removeDep(AbstractSource s, int f) {
        if (f == 0)
            throw new IllegalArgumentException("zero deps");
        int back;
        synchronized (this) {
            Integer im = imports.get(s);
            if (im == null) {
                return 0;
            } else {
                int f1 = im.intValue();
                int f2 = f1 & ~f;
                back = f1 ^ f2;
                if (back == 0)
                    return 0;
                imports.remove(s);
                if (f2 != 0) {
                    im = Integer.valueOf(f2);
                    imports.add(s, im);
                }
            }
            cacheimports = null;
        }
        s.removeDepInv(this, f);
        return back;
    }

    /**
     * <p>Remove an inv source import.</p>
     *
     * @param s The inv source import.
     * @param f The import flags.
     */
    private void removeDepInv(AbstractSource s, int f) {
        synchronized (this) {
            Integer im = importsinv.get(s);
            if (im == null) {
                return;
            } else {
                int f1 = im.intValue();
                int f2 = f1 & ~f;
                if (f1 == f2)
                    return;
                importsinv.remove(s);
                if (f2 != 0) {
                    im = Integer.valueOf(f2);
                    importsinv.add(s, im);
                }
            }
            cacheimportsinv = null;
        }
    }

    /**
     * <p>Remove all load deps.</p>
     *
     * @param f The import flags.
     * @return The union of the import flags.
     */
    public int clearDeps(int f) {
        int back = 0;
        MapEntry<AbstractSource, Integer>[] deps = snapshotDeps();
        for (int i = deps.length - 1; i >= 0; i--) {
            MapEntry<AbstractSource, Integer> dep = deps[i];
            back = removeDep(dep.key, f) | back;
        }
        return back;
    }

    /**
     * <p>Retrieve the source imports.</p>
     *
     * @return The source imports.
     */
    public MapEntry<AbstractSource, Integer>[] snapshotDeps() {
        MapEntry<AbstractSource, Integer>[] res = cacheimports;
        if (res != null)
            return res;
        synchronized (this) {
            res = cacheimports;
            if (res != null)
                return res;
            res = new MapEntry[imports.size];
            imports.toArray(res);
            cacheimports = res;
        }
        return res;
    }

    /**
     * <p>Retrieve the inv source imports.</p>
     *
     * @return The inv source imports.
     */
    public MapEntry<AbstractSource, Integer>[] snapshotDepsInv() {
        MapEntry<AbstractSource, Integer>[] res = cacheimportsinv;
        if (res != null)
            return res;
        synchronized (this) {
            res = cacheimportsinv;
            if (res != null)
                return res;
            res = new MapEntry[importsinv.size];
            importsinv.toArray(res);
            cacheimportsinv = res;
        }
        return res;
    }

    /**************************************************************/
    /* Predicates Inverse Usage Index                             */
    /**************************************************************/

    /**
     * <p>Add an inv predicate definition.</p>
     *
     * @param s The inv predicate definition.
     * @param f The definition flags.
     */
    public void addPredInv(Predicate s, int f) {
        synchronized (this) {
            Integer im = predsinv.get(s);
            if (im == null) {
                im = Integer.valueOf(f);
                predsinv.add(s, im);
            } else {
                int f1 = im.intValue();
                int f2 = f1 | f;
                if (f1 == f2)
                    return;
                predsinv.remove(s);
                im = Integer.valueOf(f2);
                predsinv.add(s, im);
            }
            cachepredsinv = null;
        }
    }

    /**
     * <p>Remove an inv predicate definition.</p>
     *
     * @param s The inv predicate definition.
     * @param f The definition flags.
     */
    public void removePredInv(Predicate s, int f) {
        synchronized (this) {
            Integer im = predsinv.get(s);
            if (im == null) {
                return;
            } else {
                int f1 = im.intValue();
                int f2 = f1 & ~f;
                if (f1 == f2)
                    return;
                predsinv.remove(s);
                if (f2 != 0) {
                    im = Integer.valueOf(f2);
                    predsinv.add(s, im);
                }
            }
            cachepredsinv = null;
        }
    }

    /**
     * <p>Retrieve the inv predicate definitions.</p>
     *
     * @return The inv predicate definitions.
     */
    public MapEntry<Predicate, Integer>[] snapshotPredsInv() {
        MapEntry<Predicate, Integer>[] res = cachepredsinv;
        if (res != null)
            return res;
        synchronized (this) {
            res = cachepredsinv;
            if (res != null)
                return res;
            res = new MapEntry[predsinv.size];
            predsinv.toArray(res);
            cachepredsinv = res;
        }
        return res;
    }

    /**************************************************************/
    /* Operator Inverse Usage Index                               */
    /**************************************************************/

    /**
     * <p>Add an inv operator definition.</p>
     *
     * @param s The inv operator definition.
     */
    public void addOperInv(Operator s) {
        synchronized (this) {
            if (opsinv.getKey(s) != null)
                return;
            opsinv.add(s);
            cacheopsinv = null;
        }
    }

    /**
     * <p>Remove an inv operator definition.</p>
     *
     * @param s The inv operator definition.
     */
    public void removeOperInv(Operator s) {
        synchronized (this) {
            if (opsinv.getKey(s) == null)
                return;
            opsinv.remove(s);
            cacheopsinv = null;
        }
    }

    /**
     * <p>Retrieve the inv operator definitions.</p>
     *
     * @return The inv operator definitions.
     */
    public Operator[] snapshotOpersInv() {
        Operator[] res = cacheopsinv;
        if (res != null)
            return res;
        synchronized (this) {
            res = cacheopsinv;
            if (res != null)
                return res;
            res = new Operator[opsinv.size];
            opsinv.toArray(res);
            cacheopsinv = res;
        }
        return res;
    }

    /**************************************************************/
    /* Fullname Computation                                       */
    /**************************************************************/

    /**
     * <p>Set the module name.</p>
     *
     * @param n The module name.
     * @return True if the module name was not changed, otherwise false.
     */
    public boolean setName(String n) {
        String back;
        synchronized (this) {
            back = name;
            name = n;
        }
        return (back != null ? back.equals(n) : null == n);
    }

    /**
     * <p>Retrieve the module name.</p>
     *
     * @return The module name.
     */
    public String getName() {
        return name;
    }

    /**
     * <p>Set the fullname.</p>
     *
     * @param f The fullname.
     */
    public void setFullName(String f) {
        fullname = f;
    }

    /**
     * <p>Retrieve the full name.</p>
     *
     * @return The full name.
     */
    public String getFullName() {
        return fullname;
    }

    /**************************************************************/
    /* Path Fixes                                                 */
    /**************************************************************/

    /**
     * <p>Add a path fix.</p>
     *
     * @param p The path fix.
     * @param f The flags.
     * @return The diff flags.
     */
    public int addFix(String p, int f) {
        if (f == 0)
            throw new IllegalArgumentException("zero fixes");
        int back;
        synchronized (this) {
            Integer pf = fixes.get(p);
            if (pf == null) {
                back = f;
                pf = Integer.valueOf(f);
                fixes.add(p, pf);
            } else {
                int f1 = pf.intValue();
                int f2 = f1 | f;
                back = f1 ^ f2;
                if (back == 0)
                    return back;
                fixes.remove(p);
                pf = Integer.valueOf(f2);
                fixes.add(p, pf);
            }
            cachefixes = null;
        }
        return back;
    }

    /**
     * <p>Remove a path fix.</p>
     *
     * @param p The path fix.
     * @param f The flags.
     * @return The diff flags.
     */
    public int removeFix(String p, int f) {
        if (f == 0)
            throw new IllegalArgumentException("zero fixes");
        int back;
        synchronized (this) {
            Integer pf = fixes.get(p);
            if (pf == null) {
                return 0;
            } else {
                int f1 = pf.intValue();
                int f2 = f1 & ~f;
                back = f1 ^ f2;
                if (back == 0)
                    return 0;
                fixes.remove(p);
                if (f2 != 0) {
                    pf = Integer.valueOf(f2);
                    fixes.add(p, pf);
                }
            }
            cachefixes = null;
        }
        return back;
    }

    /**
     * <p>Clear the path fixes.</p>
     *
     * @param f The flags.
     * @return The diff flags.
     */
    public int clearFixes(int f) {
        int back = 0;
        MapEntry<String, Integer>[] fixes = snapshotFixes();
        for (int i = fixes.length - 1; i >= 0; i--) {
            MapEntry<String, Integer> fix = fixes[i];
            back = removeFix(fix.key, f) | back;
        }
        return back;
    }

    /**
     * <p>Retrieve the path fixes.</p>
     *
     * @return The path fixes.
     */
    public MapEntry<String, Integer>[] snapshotFixes() {
        MapEntry<String, Integer>[] res = cachefixes;
        if (res != null)
            return res;
        synchronized (this) {
            res = cachefixes;
            if (res != null)
                return res;
            res = new MapEntry[fixes.size];
            fixes.toArray(res);
            cachefixes = res;
        }
        return res;
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

    /**************************************************************/
    /* Location Set                                               */
    /**************************************************************/

    /**
     * <p>Define the locations for a predicate.</p>
     * <p>Can be overridden by subclasses.</p>
     *
     * @param pick The predicate.
     */
    public Location defineLocation(Predicate pick) {
        Location location;
        synchronized (this) {
            location = locs.get(pick);
            if (location != null)
                return location;
            location = store.foyer.createLocation();
            locs.add(pick, location);
        }
        return location;
    }

    /**
     * <p>Clear the location information.</p>
     */
    public void clearLocation() {
        synchronized (this) {
            locs.clear();
        }
    }

    /**
     * <p>Retrieve the first position for a source.</p>
     * <p>Can be overridden by subclasses.</p>
     *
     * @param pick The predicate.
     * @return The first position.
     */
    public PositionKey getLocationFirst(Predicate pick) {
        Location location;
        synchronized (this) {
            location = locs.get(pick);
        }
        return (location != null ? location.firstPosition() : null);
    }

    /**
     * <p>Compute a snapshot of the locations.</p>
     *
     * @return The snapshot of the locations.
     */
    public final MapEntry<Predicate, Location>[] snapshotLocation() {
        MapEntry<Predicate, Location>[] res = cachelocs;
        if (res != null)
            return res;
        synchronized (this) {
            res = cachelocs;
            if (res != null)
                return res;
            res = new MapEntry[locs.size];
            locs.toArray(res);
            cachelocs = res;
        }
        return res;
    }

    /**************************************************************/
    /* Routine Access                                             */
    /**************************************************************/

    /**
     * <p>Retrieve predicate for the given store key.</p>
     *
     * @param fun   The name.
     * @param arity The arity.
     * @return The predicate.
     */
    public final Predicate getRoutine(int arity, String fun) {
        synchronized (this) {
            AssocArray<Integer, Predicate> map = preds.get(fun);
            if (map == null)
                return null;
            return map.get(Integer.valueOf(arity));
        }
    }

    /**
     * <p>Define a neutral predicate and register usage.</p>
     *
     * @param arity The arity.
     * @param fun   The name.
     * @param scope The sourcel.
     * @param en    The engine.
     * @param copt  The create flag.
     * @return The predicate.
     * @throws EngineMessage Shit happens.
     */
    public final Predicate defineRoutine(int arity, String fun,
                                         AbstractSource scope,
                                         Engine en, int copt)
            throws EngineMessage {
        Predicate pick = checkRoutine(arity, fun, scope);
        pick.usagePredicate(scope, en, copt);
        return pick;
    }

    /**
     * <p>Define a neutral predicate.</p>
     *
     * @param arity The arity.
     * @param fun   The name.
     * @param scope The call-site, not null.
     * @return Some previous predicate or the new neutral predicate.
     */
    public Predicate checkRoutine(int arity, String fun,
                                  AbstractSource scope) {
        Predicate pick;
        synchronized (this) {
            AssocArray<Integer, Predicate> map = preds.get(fun);
            if (map != null) {
                pick = map.get(Integer.valueOf(arity));
                if (pick != null)
                    return pick;
            }
            pick = new Predicate(fun, arity);
            if ((scope.getBits() & AbstractSource.MASK_SRC_VSPR) != 0)
                pick.setBit(Predicate.MASK_PRED_VSPR);
            if ((scope.getBits() & AbstractSource.MASK_SRC_VSPU) != 0)
                pick.setBit(Predicate.MASK_PRED_VSPU);
            pick.setSource(this);
            if (map == null) {
                map = new AssocArray<Integer, Predicate>();
                preds.put(fun, map);
            }
            map.add(Integer.valueOf(arity), pick);
            cachepreds = null;
        }
        return pick;
    }

    /**
     * <p>Remove a predicate.</p>
     *
     * @param arity The arity.
     * @param fun   The name.
     */
    public final void removeRoutine(int arity, String fun) {
        Predicate pick;
        synchronized (this) {
            MapEntry<String, AssocArray<Integer, Predicate>> entry = preds.getEntry(fun);
            if (entry == null)
                return;
            AssocArray<Integer, Predicate> map = entry.value;
            int k = map.indexOf(Integer.valueOf(arity));
            if (!(k >= 0))
                return;
            pick = map.getValue(k);
            map.removeEntry(k);
            if (map.size == 0) {
                preds.removeEntry(entry);
                preds.resize();
            } else {
                map.resize();
            }
            cachepreds = null;
        }
        pick.updateImport((pick.getBits() & Predicate.MASK_PRED_VSPR) != 0);
    }

    /**
     * <p>Compute a snapshot of the routines.</p>
     *
     * @return The snapshot of the routines.
     */
    public final Predicate[] snapshotRoutine() {
        Predicate[] res = cachepreds;
        if (res != null)
            return res;
        synchronized (this) {
            res = cachepreds;
            if (res != null)
                return res;
            int size = 0;
            for (MapEntry<String, AssocArray<Integer, Predicate>> entry = preds.getFirstEntry();
                 entry != null; entry = preds.successor(entry))
                size += entry.value.size;
            res = new Predicate[size];
            size = 0;
            for (MapEntry<String, AssocArray<Integer, Predicate>> entry = preds.getFirstEntry();
                 entry != null; entry = preds.successor(entry)) {
                AssocArray<Integer, Predicate> map = entry.value;
                map.toArrayValues(res, size);
                size += map.size;
            }
            cachepreds = res;
        }
        return res;
    }

    /**************************************************************/
    /* Operator Access                                            */
    /**************************************************************/

    /**
     * <p>Retrieve an operator.</p>
     *
     * @param type The type.
     * @param fun  The name.
     * @return The operator or null.
     */
    public Operator getOper(int type, String fun) {
        synchronized (this) {
            AssocArray<Integer, Operator> map = ops.get(fun);
            if (map == null)
                return null;
            return map.get(Integer.valueOf(type));
        }
    }

    /**
     * <p>Define a neutral operator and register usage.</p>
     *
     * @param type  The type.
     * @param fun   The name.
     * @param scope The call-site, not null.
     * @param en    The engine.
     * @return The operator.
     * @throws EngineMessage Shit happens.
     */
    public Operator defineOper(int type, String fun,
                               AbstractSource scope,
                               Engine en)
            throws EngineMessage {
        Operator oper = checkOper(type, fun, scope);
        oper.addDef(scope, en);
        return oper;
    }

    /**
     * <p>Define a neutral operator.</p>
     *
     * @param type  The type.
     * @param fun   The name.
     * @param scope The call-site, not null.
     * @return The operator.
     */
    public Operator checkOper(int type, String fun,
                              AbstractSource scope) {
        Operator oper;
        synchronized (this) {
            AssocArray<Integer, Operator> map = ops.get(fun);
            if (map != null) {
                oper = map.get(Integer.valueOf(type));
                if (oper != null)
                    return oper;
            }
            oper = store.foyer.createOperator(type, fun);
            if ((scope.getBits() & AbstractSource.MASK_SRC_VSPR) != 0)
                oper.setBit(Operator.MASK_OPER_VSPR);
            if ((scope.getBits() & AbstractSource.MASK_SRC_VSPU) != 0)
                oper.setBit(Operator.MASK_OPER_VSPU);
            oper.setSource(this);
            if (map == null) {
                map = new AssocArray<Integer, Operator>();
                ops.put(fun, map);
            }
            map.add(Integer.valueOf(type), oper);
            cacheops = null;
        }
        return oper;
    }

    /**
     * <p>Remove an operator.</p>
     *
     * @param type The type.
     * @param fun  The name.
     */
    public void removeOper(int type, String fun) {
        synchronized (this) {
            MapEntry<String, AssocArray<Integer, Operator>> entry = ops.getEntry(fun);
            if (entry == null)
                return;
            AssocArray<Integer, Operator> map = entry.value;
            int k = map.indexOf(Integer.valueOf(type));
            if (!(k >= 0))
                return;
            map.removeEntry(k);
            if (map.size == 0) {
                ops.removeEntry(entry);
                ops.resize();
            } else {
                map.resize();
            }
            cacheops = null;
        }
    }

    /**
     * <p>Retrieve a snapshot of the syntax operators.</p>
     *
     * @return The syntax operators.
     */
    public Operator[] snapshotOper() {
        Operator[] res = cacheops;
        if (res != null)
            return res;
        synchronized (this) {
            res = cacheops;
            if (res != null)
                return res;
            int size = 0;
            for (MapEntry<String, AssocArray<Integer, Operator>> entry = ops.getFirstEntry();
                 entry != null; entry = ops.successor(entry))
                size += entry.value.size;
            res = new Operator[size];
            size = 0;
            for (MapEntry<String, AssocArray<Integer, Operator>> entry = ops.getFirstEntry();
                 entry != null; entry = ops.successor(entry)) {
                AssocArray<Integer, Operator> map = entry.value;
                map.toArrayValues(res, size);
                size += map.size;
            }
            cacheops = res;
        }
        return res;
    }

    /**********************************************************************/
    /* Resources Access                                                   */
    /**********************************************************************/

    /**
     * <p>Add a resource entry to the resources list.</p>
     *
     * @param rsc The resource entry.
     */
    public void addResource(Resource rsc) {
        synchronized (this) {
            resources.add(rsc);
            cacheresources = null;
        }
    }

    /**
     * <p>Remove a resource entry to the resources list.</p>
     *
     * @param rsc The resource entry.
     */
    public void removeResource(Resource rsc) {
        synchronized (this) {
            int k = resources.indexOf(rsc);
            if (k < 0)
                return;
            resources.remove(k);
            cacheresources = null;
        }
    }

    /**
     * <p>Take a snapshot of the resources list.</p>
     *
     * @return The resources list.
     */
    public Resource[] snapshotResources() {
        Resource[] res = cacheresources;
        if (res != null)
            return res;
        synchronized (this) {
            res = cacheresources;
            if (res != null)
                return res;
            res = new Resource[resources.size];
            resources.toArray(res);
            cacheresources = res;
        }
        return res;
    }

    /**********************************************************************/
    /* Cross Referencer                                                   */
    /**********************************************************************/

    /**
     * <p>Retrieve the HTML path.</p></op>
     *
     * @return The HTML path.
     */
    public String getHTMLPath() {
        if (ForeignUri.sysUriIsRelative(path)) {
            return PREFIX_FOREIGN + ForeignFile.sysNameBase(path) + EXTENSION_HTML;
        } else {
            return ForeignFile.sysNameBase(path) + EXTENSION_HTML;
        }
    }

    /**
     * <p>Make a source by source key.</p>
     *
     * @param key   The source key.
     * @param rsc   The rscs flag.
     * @param store The store.
     * @return The source.
     */
    public static AbstractSource makeSource(String key, boolean rsc, Store store) {
        AbstractSource src;
        if (ForeignUri.sysUriIsRelative(key) || CacheSubclass.isLocal(key)) {
            AbstractFactory factory = store.foyer.getFactory();
            src = factory.getReflection().createSynth(key, store);
        } else {
            src = AbstractFile.createSourceFile(key, rsc, store);
        }
        return src;
    }

    /**
     * <p>Retrieve a source by full name.</p>
     *
     * @param fullname The full name.
     * @param store    The store.
     * @return The source.
     */
    public static AbstractSource getModule(String fullname, Store store) {
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int i = 0; i < sources.length; i++) {
                AbstractSource src = sources[i].value;
                String s = src.getFullName();
                if (Branch.OP_USER.equals(s))
                    continue;
                if (fullname.equals(s))
                    return src;
            }
            store = store.parent;
        }
        return null;
    }

    /**
     * <p>Check whether a store has a key.</p>
     *
     * @param key   The key.
     * @param store The store.
     * @return True if the store has the key, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public static boolean hasKey(String key, Store store)
            throws EngineMessage {
        if (ForeignUri.sysUriIsRelative(key)) {
            if (key.equals(Branch.OP_USER) || key.equals(Branch.OP_SYSTEM))
                return false;
            String keyrel = LookupBinary.removeClassExt(key);
            if (keyrel != null) {
                return LookupBinary.keyToClass(key, store) != null;
            } else {
                return store.getSourceDeclared(key) != null;
            }
        } else {
            return LookupResource.hasSourceFile(key, store);
        }
    }

    /**
     * <p>Check if source exists.</p>
     *
     * @param source The source.
     * @param sa     The source atom.
     * @throws EngineMessage Shit happens.
     */
    public static void checkExistentSource(AbstractSource source, SkelAtom sa)
            throws EngineMessage {
        if (source == null)
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_SOURCE, sa));
    }

    /**
     * <p>Some test cases.</p>
     *
     * @param args The arguments.
     */
    /*
    public static void main(String[] args) {
        AbstractSource fst = new SourceLocal("foo/bar");
        AbstractSource snd = new SourceLocal("foo/jill");
        System.out.println("fst=" + fst.getPath());
        System.out.println("snd=" + snd.getPath());
        boolean flag = samePackage(fst, snd);
        System.out.println("same_package(fst,snd)=" + flag);

        System.out.println();

        fst = new SourceLocal("foo/bar");
        snd = new SourceLocal("jack/jill");
        System.out.println("fst=" + fst.getPath());
        System.out.println("snd=" + snd.getPath());
        flag = samePackage(fst, snd);
        System.out.println("same_package(fst,snd)=" + flag);

        System.out.println();

        fst = new SourceLocal("foo/bar");
        snd = new SourceLocal("fooz/bar");
        System.out.println("fst=" + fst.getPath());
        System.out.println("snd=" + snd.getPath());
        flag = samePackage(fst, snd);
        System.out.println("same_package(fst,snd)=" + flag);

        System.out.println();

        fst = new SourceLocal("foo/bar$jack");
        snd = new SourceLocal("foo/bar$jill");
        System.out.println("fst=" + fst.getPath());
        System.out.println("snd=" + snd.getPath());
        flag = sameHome(fst, snd);
        System.out.println("same_home(fst,snd)=" + flag);

        System.out.println();

        fst = new SourceLocal("foo/bar");
        snd = new SourceLocal("foo/bar$jill");
        System.out.println("fst=" + fst.getPath());
        System.out.println("snd=" + snd.getPath());
        flag = sameHome(fst, snd);
        System.out.println("same_home(fst,snd)=" + flag);

        System.out.println();

        fst = new SourceLocal("foo/jack");
        snd = new SourceLocal("foo/bar$jill");
        System.out.println("fst=" + fst.getPath());
        System.out.println("snd=" + snd.getPath());
        flag = sameHome(fst, snd);
        System.out.println("same_home(fst,snd)=" + flag);

        System.out.println();

        fst = new SourceLocal("foo/bar$jill");
        snd = new SourceLocal("foo/baz$jill");
        System.out.println("fst=" + fst.getPath());
        System.out.println("snd=" + snd.getPath());
        flag = sameHome(fst, snd);
        System.out.println("same_home(fst,snd)=" + flag);

        System.out.println();

        fst = new SourceLocal("foo$jill");
        snd = new SourceLocal("fooz$jill");
        System.out.println("fst=" + fst.getPath());
        System.out.println("snd=" + snd.getPath());
        flag = sameHome(fst, snd);
        System.out.println("same_home(fst,snd)=" + flag);
    }
    */

}