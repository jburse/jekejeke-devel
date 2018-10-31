package jekpro.frequent.standard;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindCount;
import jekpro.model.molec.BindVar;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.structure.EngineLexical;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.*;

import java.util.Comparator;

/**
 * <p>Provides built-in predicates for the set theory.</p>
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
public final class SpecialSort extends AbstractSpecial {
    private final static int SPECIAL_SORT = 0;
    private final static int SPECIAL_SYS_DISTINCT = 1;
    private final static int SPECIAL_KEYSORT = 2;
    private final static int SPECIAL_SYS_KEYGROUP = 3;
    private final static int SPECIAL_HASH_CODE = 4;
    private final static int SPECIAL_SYS_GROUND = 5;
    private final static int SPECIAL_SYS_HASH_CODE = 6;
    private final static int SPECIAL_LOCALE_SORT = 7;
    private final static int SPECIAL_LOCALE_KEYSORT = 8;

    /**
     * <p>Create a sort special.</p>
     *
     * @param i The id.
     */
    public SpecialSort(int i) {
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
        try {
            switch (id) {
                case SPECIAL_SORT:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    BindCount[] ref = en.display;
                    boolean multi = SpecialSort.sort(en, temp[0], ref, en);
                    BindCount[] d = en.display;
                    if (!en.unifyTerm(temp[1], ref, en.skel, d))
                        return false;
                    if (multi)
                        BindCount.remTab(d, en);
                    return en.getNext();
                case SPECIAL_SYS_DISTINCT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = SpecialSort.distinct(temp[0], ref, en);
                    d = en.display;
                    if (!en.unifyTerm(temp[1], ref, en.skel, d))
                        return false;
                    if (multi)
                        BindCount.remTab(d, en);
                    return en.getNext();
                case SPECIAL_KEYSORT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = SpecialSort.keySort(en, temp[0], ref, en);
                    d = en.display;
                    if (!en.unifyTerm(temp[1], ref, en.skel, d))
                        return false;
                    if (multi)
                        BindCount.remTab(d, en);
                    return en.getNext();
                case SPECIAL_SYS_KEYGROUP:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    multi = SpecialSort.keyGroup(temp[0], ref, en);
                    d = en.display;
                    if (!en.unifyTerm(temp[1], ref, en.skel, d))
                        return false;
                    if (multi)
                        BindCount.remTab(d, en);
                    return en.getNext();
                case SPECIAL_HASH_CODE:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Number val = Integer.valueOf(hashCode(temp[0], ref, 0));
                    if (!en.unifyTerm(temp[1], ref, val, BindCount.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                case SPECIAL_SYS_GROUND:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    val = SpecialEval.derefAndCastInteger(temp[1], ref);
                    if (!termGround(temp[0], ref, SpecialEval.castIntValue(val)))
                        return false;
                    return en.getNext();
                case SPECIAL_SYS_HASH_CODE:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    val = SpecialEval.derefAndCastInteger(temp[1], ref);
                    val = Integer.valueOf(termHash(temp[0], ref,
                            SpecialEval.castIntValue(val), 0));
                    if (!en.unifyTerm(temp[2], ref, val, BindCount.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                case SPECIAL_LOCALE_SORT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Comparator cmp = EngineLexical.comparatorAtom(temp[0], ref);
                    multi = SpecialSort.sort(new EngineLexical(cmp, en), temp[1], ref, en);
                    d = en.display;
                    if (!en.unifyTerm(temp[2], ref, en.skel, d))
                        return false;
                    if (multi)
                        BindCount.remTab(d, en);
                    return en.getNext();
                case SPECIAL_LOCALE_KEYSORT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    cmp = EngineLexical.comparatorAtom(temp[0], ref);
                    multi = SpecialSort.keySort(new EngineLexical(cmp, en), temp[1], ref, en);
                    d = en.display;
                    if (!en.unifyTerm(temp[2], ref, en.skel, en.display))
                        return false;
                    if (multi)
                        BindCount.remTab(d, en);
                    return en.getNext();
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

    /************************************************************************/
    /* Sort Predicate                                                       */
    /************************************************************************/

    /**
     * <p>Sort a list.</p>
     *
     * @param c  The comparator.
     * @param m  The skeleton.
     * @param d  The display.
     * @param en The engine.
     */
    private static boolean sort(Comparator<Object> c,
                                Object m, BindCount[] d, Engine en)
            throws EngineMessage {
        AbstractSet<Object> set = new SetTree<Object>(c);
        SpecialSort.sortSet(set, m, d, en);
        return createSet(en.store.foyer.ATOM_NIL,
                BindCount.DISPLAY_CONST, set, en);
    }

    /**
     * <p>Distinct a list.</p>
     *
     * @param m  The skeleton.
     * @param d  The display.
     * @param en The engine.
     */
    private static boolean distinct(Object m, BindCount[] d, Engine en)
            throws EngineMessage {
        AbstractSet<Object> set = new SetHashLink<Object>();
        SpecialSort.sortSet(set, m, d, en);
        return createSet(en.store.foyer.ATOM_NIL,
                BindCount.DISPLAY_CONST, set, en);
    }

    /**
     * <p>Sort the list.</p>
     *
     * @param set The abstract set.
     * @param m   The list skel.
     * @param d   The list display.
     * @param en  The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void sortSet(AbstractSet<Object> set,
                                Object m, BindCount[] d, Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        while (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 2 &&
                ((SkelCompound) m).sym.fun.equals(Foyer.OP_CONS)) {
            SkelCompound sc = (SkelCompound) m;
            en.skel = sc.args[0];
            en.display = d;
            en.deref();
            Object elem = AbstractTerm.createMolec(en.skel, en.display);
            try {
                if (set.getKey(elem) == null)
                    set.add(elem);
            } catch (ArithmeticException x) {
                throw new EngineMessage(EngineMessage.evaluationError(
                        x.getMessage()));
            }
            en.skel = sc.args[1];
            en.display = d;
            en.deref();
            m = en.skel;
            d = en.display;
        }
        if (m instanceof SkelAtom &&
                ((SkelAtom) m).fun.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_LIST, m), d);
        }
    }

    /**
     * <p>Create the set.</p>
     *
     * @param t4  The difference skeleton.
     * @param d2  The difference display.
     * @param set The abstract map.
     * @param en  The engine.
     */
    public static <T> boolean createSet(Object t4, BindCount[] d2,
                                        AbstractSet<T> set, Engine en) {
        en.skel = t4;
        en.display = d2;
        boolean multi = false;
        if (set == null)
            return multi;
        for (SetEntry<T> entry = set.getLastEntry();
             entry != null; entry = set.predecessor(entry)) {
            t4 = en.skel;
            d2 = en.display;
            boolean ext = multi;
            T elem = entry.key;
            Object val = AbstractTerm.getSkel(elem);
            BindCount[] ref = AbstractTerm.getDisplay(elem);
            multi = SpecialFind.pairValue(en.store.foyer.CELL_CONS,
                    val, ref, t4, d2, en);
            if (multi && ext)
                BindCount.remTab(d2, en);
            multi = (multi || ext);
        }
        return multi;
    }

    /************************************************************************/
    /* Key Sort Predicate                                                   */
    /************************************************************************/

    /**
     * <p>Key sort a pair list.</p>
     * <p>The result is returned in skel and display of the engine.</p>
     *
     * @param c  The comparator.
     * @param m  The skeletion.
     * @param d  The display.
     * @param en The engine.
     */
    private static boolean keySort(Comparator<Object> c,
                                   Object m, BindCount[] d, Engine en)
            throws EngineMessage {
        AbstractMap<Object, ListArray<Object>> map = new MapTree<Object, ListArray<Object>>(c);
        SpecialSort.sortMap(map, m, d, en);
        return SpecialSort.createMap(map, en);
    }

    /**
     * <p>Key group a pair list.</p>
     * <p>The result is returned in skel and display of the engine.</p>
     *
     * @param m  The skeletion.
     * @param d  The display.
     * @param en The engine.
     */
    private static boolean keyGroup(Object m, BindCount[] d, Engine en)
            throws EngineMessage {
        AbstractMap<Object, ListArray<Object>> map = new MapHashLink<Object, ListArray<Object>>();
        SpecialSort.sortMap(map, m, d, en);
        return SpecialSort.createMap(map, en);
    }

    /**
     * <p>Key the list.</p>
     *
     * @param map The abstract map.
     * @param m   The list skeleton.
     * @param d   The list display.
     * @param en  The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void sortMap(AbstractMap<Object, ListArray<Object>> map,
                                Object m, BindCount[] d, Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        while (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 2 &&
                ((SkelCompound) m).sym.fun.equals(Foyer.OP_CONS)) {
            SkelCompound sc = (SkelCompound) m;
            en.skel = sc.args[0];
            en.display = d;
            en.deref();
            Object m2 = en.skel;
            BindCount[] d2 = en.display;
            if (m2 instanceof SkelCompound &&
                    ((SkelCompound) m2).args.length == 2 &&
                    ((SkelCompound) m2).sym.fun.equals(Foyer.OP_SUB)) {
                /* */
            } else {
                EngineMessage.checkInstantiated(m2);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_PAIR, m2), d2);
            }
            SkelCompound sc2 = (SkelCompound) m2;
            en.skel = sc2.args[0];
            en.display = d2;
            en.deref();
            Object elem = AbstractTerm.createMolec(en.skel, en.display);
            ListArray<Object> found;
            try {
                found = map.get(elem);
                if (found == null) {
                    found = new ListArray<Object>();
                    map.add(elem, found);
                }
            } catch (ArithmeticException x) {
                throw new EngineMessage(EngineMessage.evaluationError(
                        x.getMessage()));
            }
            en.skel = sc2.args[1];
            en.display = d2;
            en.deref();
            Object elem2 = AbstractTerm.createMolec(en.skel, en.display);
            found.add(elem2);
            en.skel = sc.args[1];
            en.display = d;
            en.deref();
            m = en.skel;
            d = en.display;
        }
        if (m instanceof SkelAtom &&
                ((SkelAtom) m).fun.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_LIST, m), d);
        }
    }

    /**
     * <p>Create the map.</p>
     *
     * @param map The abstract map.
     * @param en  The engine.
     */
    private static <K, V> boolean createMap(AbstractMap<K, ListArray<V>> map,
                                            Engine en) {
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = BindCount.DISPLAY_CONST;
        boolean multi = false;
        for (MapEntry<K, ListArray<V>> entry = map.getLastEntry();
             entry != null; entry = map.predecessor(entry)) {
            K elem2 = entry.key;
            Object val2 = AbstractTerm.getSkel(elem2);
            BindCount[] ref2 = AbstractTerm.getDisplay(elem2);
            ListArray<V> list = entry.value;
            for (int i = list.size() - 1; i >= 0; i--) {
                Object t4 = en.skel;
                BindCount[] d2 = en.display;
                boolean ext = multi;
                V elem = list.get(i);
                Object val = AbstractTerm.getSkel(elem);
                BindCount[] ref = AbstractTerm.getDisplay(elem);
                boolean ext2 = SpecialFind.pairValue(en.store.foyer.CELL_SUB,
                        val2, ref2, val, ref, en);
                val = en.skel;
                ref = en.display;
                multi = SpecialFind.pairValue(en.store.foyer.CELL_CONS,
                        val, ref, t4, d2, en);
                if (multi && ext)
                    BindCount.remTab(d2, en);
                if (multi && ext2)
                    BindCount.remTab(ref, en);
                multi = (multi || ext || ext2);
            }
        }
        return multi;
    }

    /********************************************************************/
    /* Hash Functions                                                   */
    /********************************************************************/

    /**
     * <p>Compute the hash code.</p>
     * <p>Teil recursive solution.</p>
     *
     * @param t   The term.
     * @param d   The display of the term.
     * @param res The preceding hash.
     * @return The hash value.
     */
    public static int hashCode(Object t, BindCount[] d, int res) {
        for (; ; ) {
            BindVar b1;
            while (t instanceof SkelVar &&
                    (b1 = d[((SkelVar) t).id]).display != null) {
                t = b1.skel;
                d = b1.display;
            }
            if (t instanceof SkelVar)
                return res * 31 + ((SkelVar) t).hashCode(d);
            if (!(t instanceof SkelCompound))
                return res * 31 + t.hashCode();
            Object[] tc = ((SkelCompound) t).args;
            res = res * 31 + ((SkelCompound) t).sym.hashCode();
            int i = 0;
            for (; i < tc.length - 1; i++)
                res = hashCode(tc[i], d, res);
            t = tc[i];
        }
    }

    /**
     * <p>Check groundness up to a depth.</p>
     * <p>Cant use our speed-up structure here.</p>
     * <p>Teil recursive solution.</p>
     *
     * @param t     The term.
     * @param d     The display of the term.
     * @param depth The requested depth.
     * @return True if ground up to depth, otherwise false.
     */
    private static boolean termGround(Object t, BindCount[] d, int depth) {
        for (; ; ) {
            if (depth == 0)
                return true;
            BindVar b1;
            while (t instanceof SkelVar &&
                    (b1 = d[((SkelVar) t).id]).display != null) {
                t = b1.skel;
                d = b1.display;
            }
            if (t instanceof SkelVar)
                return false;
            if (!(t instanceof SkelCompound))
                return true;
            Object[] tc = ((SkelCompound) t).args;
            depth--;
            int i = 0;
            for (; i < tc.length - 1; i++)
                if (!termGround(tc[i], d, depth))
                    return false;
            t = tc[i];
        }
    }

    /**
     * <p>Compute the hash code up to a depth.</p>
     * <p>Teil recursive solution.</p>
     *
     * @param t     The term.
     * @param d     The display of the term.
     * @param depth The requested depth.
     * @param res   The preceding hash.
     * @return The hash value.
     */
    private static int termHash(Object t, BindCount[] d, int depth, int res) {
        for (; ; ) {
            if (depth == 0)
                return res;
            BindVar b1;
            while (t instanceof SkelVar &&
                    (b1 = d[((SkelVar) t).id]).display != null) {
                t = b1.skel;
                d = b1.display;
            }
            if (t instanceof SkelVar)
                return res * 31 + ((SkelVar) t).hashCode(d);
            if (!(t instanceof SkelCompound))
                return res * 31 + t.hashCode();
            Object[] tc = ((SkelCompound) t).args;
            res = res * 31 + ((SkelCompound) t).sym.hashCode();
            depth--;
            int i = 0;
            for (; i < tc.length - 1; i++)
                res = termHash(tc[i], d, depth, res);
            t = tc[i];
        }
    }

}
