package jekdev.reference.debug;

import jekpro.frequent.system.ForeignThread;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologWriter;
import jekpro.model.rope.Bouquet;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Index;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.reflect.SpecialPred;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.*;
import matula.util.regex.IgnoreCase;

import java.io.IOException;
import java.io.Writer;
import java.util.concurrent.locks.ReadWriteLock;

/**
 * <p>Provides a special predicates for index dump.</p>
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
public final class SpecialDump extends AbstractSpecial {
    private final static int SPECIAL_SYS_JITI_INSPECT = 0;
    private final static int SPECIAL_SYS_JITI_RECAP = 1;
    private final static int SPECIAL_SYS_AVERAGER_NEW = 2;
    private final static int SPECIAL_SYS_AVERAGER_SHOW = 3;
    private final static int SPECIAL_SYS_AVERAGER_HAS = 4;

    /**
     * <p>Create a index dump special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialDump(int i) {
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
            case SPECIAL_SYS_JITI_INSPECT:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Predicate pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                if (pick == null)
                    return false;
                if (!(pick.del instanceof AbstractDefined))
                    return false;

                Number beta = SpecialEval.derefAndCastInteger(temp[1], ref);
                SpecialEval.checkNotLessThanZero(beta);
                int flags = SpecialEval.castIntValue(beta);

                SpecialDump.inspectClauses((AbstractDefined) pick.del, flags, en);
                return true;
            case SPECIAL_SYS_JITI_RECAP:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                if (pick == null)
                    return false;
                if (!(pick.del instanceof AbstractDefined))
                    return false;

                AssocSorted<String, DumpReport> map = SpecialFriendly.derefAndCastMap(temp[1], ref);
                SpecialDump.recapClauses((AbstractDefined) pick.del, map, en);
                return true;
            case SPECIAL_SYS_AVERAGER_NEW:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                map = new AssocSorted<>(IgnoreCase.DEFAULT_TERTIARY);
                if (!BindUniv.unifyTerm(map, Display.DISPLAY_CONST, temp[0], ref, en))
                    return false;
                return true;
            case SPECIAL_SYS_AVERAGER_SHOW:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                map = SpecialFriendly.derefAndCastMap(temp[0], ref);

                SpecialDump.averagerShow(map, en);
                return true;
            case SPECIAL_SYS_AVERAGER_HAS:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                map = SpecialFriendly.derefAndCastMap(temp[0], ref);
                if (averagerEmpty(map))
                    return false;
                return true;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Show a statistics map.</p>
     *
     * @param map The statistics map.
     * @param en  The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void averagerShow(AssocSorted<String, DumpReport> map, Engine en)
            throws EngineMessage {
        try {
            Object obj = en.visor.curoutput;
            LoadOpts.checkTextWrite(obj);
            Writer wr = (Writer) obj;

            DumpReport.show(map, wr);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Check whether the statistics map is empty.</p>
     *
     * @param map The statistics map.
     * @return True if the map is empty, otherwise false.
     */
    private static boolean averagerEmpty(AssocSorted<String, DumpReport> map) {
        return DumpReport.empty(map);
    }

    /*********************************************************/
    /* Index Inspection                                      */
    /*********************************************************/

    /**
     * <p>Inspect the indexes of a predicate.</p>
     *
     * @param def   The abstract defined.
     * @param flags The flags.
     * @param en    The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void inspectClauses(AbstractDefined def, int flags,
                                       Engine en)
            throws EngineException, EngineMessage {
        try {
            Object obj = en.visor.curoutput;
            LoadOpts.checkTextWrite(obj);
            Writer wr = (Writer) obj;

            PrologWriter pw = en.store.foyer.createWriter(Foyer.IO_TERM);
            pw.setDefaults(en.visor.peekStack());
            pw.setEngine(en);
            pw.setFlags(pw.getFlags() | PrologWriter.FLAG_QUOT);
            pw.setWriter(wr);

            DumpPrinter dp = new DumpPrinter();
            dp.flags = flags;
            dp.pw = pw;

            ReadWriteLock lock = def.getLock(en);
            if (lock != null) {
                try {
                    lock.readLock().lockInterruptibly();
                } catch (InterruptedException x) {
                    throw (EngineMessage) ForeignThread.sysThreadClear();
                }
                try {
                    Bouquet cr = def.getBouquet(en);
                    int len = getLengthScope(cr.set, en);
                    SpecialDump.inspectBouquet(cr, dp, 0, len, en);
                } finally {
                    lock.readLock().unlock();
                }
            } else {
                Bouquet cr = def.getBouquet(en);
                int len = getLengthScope(cr.set, en);
                SpecialDump.inspectBouquet(cr, dp, 0, len, en);
            }
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Inspect the indexes of a bouquet.</p>
     *
     * @param cr    The bouquet.
     * @param dp    The dump printer.
     * @param start The start position.
     * @param len   The length.
     * @param en    The engine copy.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void inspectBouquet(Bouquet cr, DumpPrinter dp,
                                       int start, int len, Engine en)
            throws IOException, EngineMessage, EngineException {
        Writer wr = dp.pw.getWriter();
        wr.write("length=");
        wr.write(Integer.toString(len));
        wr.write("\n");
        Index[] help = cr.args;
        if (help == null)
            return;
        for (int j = 0; j < help.length; j++) {
            Index ci = help[j];
            if (ci == null)
                continue;
            if ((dp.flags & DumpPrinter.INSPECT_MASK_IRRELEVANT) == 0 &&
                    (ci.map == null && ci.guard == null))
                continue;
            dp.dumpOffset();
            wr.write("at=");
            wr.write(Integer.toString(start + j + 1));
            dp.level++;
            SpecialDump.inspectIndex(ci, dp, start + j + 1, en);
            dp.level--;
        }
    }

    /**
     * <p>Inspect the indexes of an index.</p>
     *
     * @param idx   The index.
     * @param dp    The dump printer.
     * @param start The start position.
     * @param en    The engine copy.
     * @throws IOException     IO error.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void inspectIndex(Index idx, DumpPrinter dp,
                                     int start, Engine en)
            throws IOException, EngineMessage, EngineException {
        SpecialDump.inspectPairs(idx.map, dp, start, en);
        if (idx.nonguard != null) {
            int len = getLengthScope(idx.nonguard.set, en);
            if (len != 0) {
                dp.dumpOffset();
                Writer wr = dp.pw.getWriter();
                wr.write("nonguard, ");
                dp.level++;
                SpecialDump.inspectBouquet(idx.nonguard,
                        dp, start, len, en);
                dp.level--;
            }
        }
        if (idx.guard != null) {
            int len = getLengthScope(idx.guard.set, en);
            if (len != 0) {
                dp.dumpOffset();
                Writer wr = dp.pw.getWriter();
                wr.write("guard, ");
                dp.level++;
                SpecialDump.inspectBouquet(idx.guard,
                        dp, start, len, en);
                dp.level--;
            }
        }
    }

    /*********************************************************/
    /* Index Recap                                           */
    /*********************************************************/

    /**
     * <p>Recap the indexes of a predicate.</p>
     *
     * @param def The abstract defined.
     * @param map The statistics map.
     * @param en  The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void recapClauses(AbstractDefined def,
                                     AssocSorted<String, DumpReport> map,
                                     Engine en)
            throws EngineException, EngineMessage {
        try {
            ReadWriteLock lock = def.getLock(en);
            if (lock != null) {
                try {
                    lock.readLock().lockInterruptibly();
                } catch (InterruptedException x) {
                    throw (EngineMessage) ForeignThread.sysThreadClear();
                }
                try {
                    Bouquet cr = def.getBouquet(en);
                    SpecialDump.recapBouquet(cr, map, "", 0, en);
                } finally {
                    lock.readLock().unlock();
                }
            } else {
                Bouquet cr = def.getBouquet(en);
                SpecialDump.recapBouquet(cr, map, "", 0, en);
            }
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Recap the indexes of a bouquet.</p>
     *
     * @param cr    The bouquet.
     * @param map   The dump report.
     * @param path  The path.
     * @param start The start position.
     * @param en    The engine copy.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void recapBouquet(Bouquet cr, AssocSorted<String, DumpReport> map,
                                     String path, int start, Engine en)
            throws IOException, EngineMessage, EngineException {
        Index[] help = cr.args;
        if (help == null)
            return;
        for (int j = 0; j < help.length; j++) {
            Index ci = help[j];
            if (ci == null)
                continue;
            if (ci.map == null && ci.guard == null)
                continue;
            String path2 = path + Integer.toString(start + j + 1);
            SpecialDump.recapIndex(ci, map, path2, start + j + 1, en);
        }
    }

    /**
     * <p>Recap the indexes of an index.</p>
     *
     * @param idx   The index.
     * @param map   The dump report.
     * @param path  The path.
     * @param start The start position.
     * @param en    The engine copy.
     * @throws IOException     IO error.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void recapIndex(Index idx, AssocSorted<String, DumpReport> map,
                                   String path, int start, Engine en)
            throws IOException, EngineMessage, EngineException {
        SpecialDump.recapPairs(idx.map, map, path, start, en);
        if (idx.nonguard != null) {
            int len = getLengthScope(idx.nonguard.set, en);
            if (len != 0) {
                String path2 = path + "+";
                DumpReport.add(map, path, len);
                SpecialDump.recapBouquet(idx.nonguard,
                        map, path2, start, en);
            }
        }
        if (idx.guard != null) {
            int len = getLengthScope(idx.guard.set, en);
            if (len != 0) {
                String path2 = path + "+";
                DumpReport.add(map, path, len);
                SpecialDump.recapBouquet(idx.guard,
                        map, path2, start, en);
            }
        }
    }

    /*********************************************************/
    /* Pairs Inspect                                         */
    /*********************************************************/

    /**
     * <p>Inspect the indexes of pairs.</p>
     *
     * @param pairs The pairs.
     * @param dp    The dump printer.
     * @param start The start position.
     * @param en    The engine copy.
     * @throws IOException     IO error.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void inspectPairs(AbstractAssoc<Object, Bouquet> pairs,
                                     DumpPrinter dp, int start, Engine en)
            throws IOException, EngineMessage, EngineException {
        if (pairs != null) {
            if (pairs instanceof AssocArray) {
                SpecialDump.inspectPairsArray((AssocArray) pairs, dp, start, en);
            } else {
                SpecialDump.inspectPairsHash((MapHash) pairs, dp, start, en);
            }
        } else {
            Writer wr = dp.pw.getWriter();
            wr.write("\n");
        }
    }

    /**
     * <p>Inspect the indexes of array pairs.</p>
     *
     * @param pairs The pairs.
     * @param dp    The dump printer.
     * @param start The start position.
     * @param en    The engine copy.
     * @throws IOException     IO error.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void inspectPairsArray(AssocArray<Object, Bouquet> pairs,
                                          DumpPrinter dp, int start, Engine en)
            throws IOException, EngineMessage, EngineException {
        Writer wr = dp.pw.getWriter();
        wr.write("\n");
        for (int j = 0; j < pairs.size(); j++) {
            Bouquet cp = pairs.getValue(j);
            int len = getLengthScope(cp.set, en);
            if (len == 0)
                continue;
            dp.dumpOffset();
            wr.write("key=");
            Object val = pairs.getKey(j);
            if (val instanceof String)
                val = new SkelAtom((String) val);
            dp.pw.unparseStatement(val, Display.DISPLAY_CONST);
            wr.write(", ");
            dp.level++;
            SpecialDump.inspectBouquet(cp, dp, start, len, en);
            dp.level--;
        }
    }

    /**
     * <p>Inspect the indexes of hash pairs.</p>
     *
     * @param pairs The pairs.
     * @param dp    The dump printer.
     * @param start The start position.
     * @param en    The engine copy.
     * @throws IOException     IO error.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void inspectPairsHash(MapHash<Object, Bouquet> pairs,
                                         DumpPrinter dp, int start, Engine en)
            throws IOException, EngineMessage, EngineException {
        Writer wr = dp.pw.getWriter();
        wr.write(", map=");
        wr.write(Integer.toString(pairs.length()));
        wr.write("\n");
        for (MapEntry<Object, Bouquet> entry = pairs.getFirstEntry();
             entry != null; entry = pairs.successor(entry)) {
            Bouquet cp = entry.value;
            int len = getLengthScope(cp.set, en);
            if (len == 0)
                continue;
            dp.dumpOffset();
            wr.write("key=");
            Object val = entry.key;
            if (val instanceof String)
                val = new SkelAtom((String) val);
            dp.pw.unparseStatement(val, Display.DISPLAY_CONST);
            wr.write(", hash=");
            wr.write(Integer.toString(pairs.index(entry.key)));
            wr.write(", ");
            dp.level++;
            SpecialDump.inspectBouquet(cp, dp, start, len, en);
            dp.level--;
        }
    }

    /*********************************************************/
    /* Pairs Recap                                           */
    /*********************************************************/

    /**
     * <p>Inspect the indexes of pairs.</p>
     *
     * @param pairs The pairs.
     * @param map   The dump report.
     * @param path  The path.
     * @param start The start position.
     * @param en    The engine copy.
     * @throws IOException     IO error.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void recapPairs(AbstractAssoc<Object, Bouquet> pairs,
                                   AssocSorted<String, DumpReport> map, String path,
                                   int start, Engine en)
            throws IOException, EngineMessage, EngineException {
        if (pairs != null) {
            if (pairs instanceof AssocArray) {
                SpecialDump.recapPairsArray((AssocArray) pairs,
                        map, path, start, en);
            } else {
                SpecialDump.recapPairsHash((MapHash) pairs,
                        map, path, start, en);
            }
        }
    }

    /**
     * <p>Inspect the indexes of array pairs.</p>
     *
     * @param pairs The pairs.
     * @param map   The dump report.
     * @param start The start position.
     * @param en    The engine copy.
     * @throws IOException     IO error.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void recapPairsArray(AssocArray<Object, Bouquet> pairs,
                                        AssocSorted<String, DumpReport> map, String path,
                                        int start, Engine en)
            throws IOException, EngineMessage, EngineException {
        String path2 = path + "+";
        for (int j = 0; j < pairs.size(); j++) {
            Bouquet cp = pairs.getValue(j);
            int len = getLengthScope(cp.set, en);
            if (len == 0)
                continue;
            DumpReport.add(map, path, len);
            SpecialDump.recapBouquet(cp, map, path2, start, en);
        }
    }

    /**
     * <p>Inspect the indexes of hash pairs.</p>
     *
     * @param pairs The pairs.
     * @param map   The dump report.
     * @param start The start position.
     * @param en    The engine copy.
     * @throws IOException     IO error.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void recapPairsHash(MapHash<Object, Bouquet> pairs,
                                       AssocSorted<String, DumpReport> map, String path,
                                       int start, Engine en)
            throws IOException, EngineMessage, EngineException {
        String path2 = path + "+";
        for (MapEntry<Object, Bouquet> entry = pairs.getFirstEntry();
             entry != null; entry = pairs.successor(entry)) {
            Bouquet cp = entry.value;
            int len = getLengthScope(cp.set, en);
            if (len == 0)
                continue;
            DumpReport.add(map, path, len);
            SpecialDump.recapBouquet(cp, map, path2, start, en);
        }
    }

    /*********************************************************/
    /* Rope Polymorphism                                     */
    /*********************************************************/

    /**
     * <p>Retrieve the clauses length for the given scope.</p>
     *
     * @param rope The rope.
     * @param en   The engine.
     * @return The length for the scope.
     */
    private static int getLengthScope(AbstractList<Clause> rope, Engine en) {
        if (rope != null) {
            if (rope instanceof ListArray) {
                return getLengthScopeArray((ListArray) rope, en);
            } else {
                return getLengthScopeHash((SetHashLink) rope, en);
            }
        } else {
            return 0;
        }
    }

    /**
     * <p>Retrieve the clauses length for the given scope.</p>
     *
     * @param rope The rope.
     * @param en   The engine.
     * @return The length for the scope.
     */
    private static int getLengthScopeArray(ListArray<Clause> rope, Engine en) {
        int len = 0;
        for (int i = 0; i < rope.size(); i++) {
            SkelAtom sa = StackElement.callableToName(rope.get(i).head);
            if (Clause.ancestorSource(sa.scope, en))
                len++;
        }
        return len;
    }

    /**
     * <p>Retrieve the clauses length for the given scope.</p>
     *
     * @param rope The rope.
     * @param en   The engine.
     * @return The length for the scope.
     */
    private static int getLengthScopeHash(SetHashLink<Clause> rope, Engine en) {
        int len = 0;
        for (SetEntry<Clause> entry = rope.getFirstEntry();
             entry != null; entry = rope.successor(entry)) {
            SkelAtom sa = StackElement.callableToName(entry.value.head);
            if (Clause.ancestorSource(sa.scope, en))
                len++;
        }
        return len;
    }

}
