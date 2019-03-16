package jekpro.model.rope;

import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologWriter;
import jekpro.tools.term.SkelAtom;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>This class provides a hash pairs implementation.</p>
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
public final class PairsHash
        extends MapHash<Object, Bouquet>
        implements InterfacePairs {
    private static final int MIN_LARGE = 3;

    /**
     * <p>Create a pairs hash.</p>
     */
    public PairsHash() {
        super();
    }

    /**
     * <p>Create a pairs hash.</p>
     *
     * @param capa The ahead capacity.
     */
    public PairsHash(int capa) {
        super(capa);
    }

    /**
     * <p>Add the clause to the list of pairs.</p>
     *
     * @param clause The clause to be added.
     */
    public void addClause(Clause clause) {
        for (MapEntry<Object, Bouquet> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            Bouquet cp = entry.value;
            cp.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
        }
    }

    /**
     * <p>Add the clause to the list of pairs.</p>
     *
     * @param m      The key.
     * @param clause The clause to be added.
     * @param b      The nonguard.
     */
    public InterfacePairs addClause(Object m, Clause clause, Bouquet b) {
        MapEntry<Object, Bouquet> entry = getEntry(m);
        Bouquet cp;
        if (entry == null) {
            if (b != null) {
                cp = b.copyBouquet();
            } else {
                cp = new Bouquet();
            }
            add(m, cp);
        } else {
            cp = entry.value;
        }
        cp.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
        return null;
    }

    /**
     * <p>Assert the clause to the list of pairs.</p>
     *
     * @param clause The clause to be added.
     * @param at     The position.
     * @param flags  The flags.
     */
    public void assertClause(Clause clause, int at, int flags) {
        for (MapEntry<Object, Bouquet> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            Bouquet cp = entry.value;
            cp.assertClause(at + 1, clause, flags);
        }
    }

    /**
     * <p>Assert the clause to the list of pairs.</p>
     *
     * @param m      The key.
     * @param clause The clause to be added.
     * @param at     The position.
     * @param flags  The flags.
     * @param b      The nonguard.
     * @return The new interface pairs, or null.
     */
    public InterfacePairs assertClause(Object m, Clause clause,
                                       int at, int flags, Bouquet b) {
        MapEntry<Object, Bouquet> entry = getEntry(m);
        Bouquet cp;
        if (entry == null) {
            if (b != null) {
                cp = b.copyBouquet();
            } else {
                cp = new Bouquet();
            }
            add(m, cp);
        } else {
            cp = entry.value;
        }
        cp.assertClause(at + 1, clause, flags);
        return null;
    }

    /**
     * <p>Retract the clause to the list of pairs.</p>
     *
     * @param clause The clause to be added.
     * @param at     The position.
     * @return The new interface pairs, or null.
     */
    public InterfacePairs retractClause(Clause clause, int at) {
        boolean dirty = false;
        for (MapEntry<Object, Bouquet> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            Bouquet cp = entry.value;
            cp.retractClause(at + 1, clause);
            if (cp.set == null) {
                removeEntry(entry);
                dirty = true;
            }
        }
        if (dirty)
            resize();
        if (size() < MIN_LARGE)
            return toSmall();
        return null;
    }

    /**
     * <p>Retract the clause to the list of pairs.</p>
     *
     * @param m      The key.
     * @param clause The clause to be added.
     * @param at     The position.
     * @return The new interface pairs, or null.
     */
    public InterfacePairs retractClause(Object m, Clause clause, int at) {
        MapEntry<Object, Bouquet> entry = getEntry(m);
        if (entry == null)
            return null;
        Bouquet cp = entry.value;
        cp.retractClause(at + 1, clause);
        if (cp.set == null) {
            removeEntry(entry);
            resize();
        }
        if (size() < MIN_LARGE)
            return toSmall();
        return null;
    }

    /**
     * <p>Create a small map from this large map.</p>
     * <p>Carry over the bouquets.</p>
     *
     * @return The small map.
     */
    private InterfacePairs toSmall() {
        PairsArray res = new PairsArray(size());
        for (MapEntry<Object, Bouquet> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            Object m = entry.key;
            Bouquet cp = entry.value;
            res.add(m, cp);
        }
        return res;
    }

    /**
     * <p>Dump the index.</p>
     *
     * @param wr    The writer.
     * @param off   The left indentation.
     * @param start The start position.
     * @param en    The engine copy.
     * @throws IOException     IO error.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void inspectIndex(Writer wr, int off, int start,
                             Engine en)
            throws IOException, EngineMessage, EngineException {
        wr.write(", map=");
        wr.write(Integer.toString(length()));
        wr.write("\n");
        PrologWriter pw = en.store.foyer.createWriter(Foyer.IO_TERM);
        pw.setSource(en.visor.peekStack());
        pw.setEngineRaw(en);
        for (MapEntry<Object, Bouquet> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            Bouquet cp = entry.value;
            InterfaceRope set = cp.set;
            int len = (set != null ? set.getLengthScope(en) : 0);
            if (len == 0)
                continue;
            for (int i = 0; i < off; i++)
                wr.write(" ");
            wr.write("key=");
            Object val = entry.key;
            if (val instanceof String)
                val = new SkelAtom((String) val);
            pw.setWriter(wr);
            pw.unparseStatement(val, Display.DISPLAY_CONST);
            wr.write(", hash=");
            wr.write(Integer.toString(index(entry.key)));
            wr.write(", ");
            cp.inspectPaths(wr, off + 2, start, len, en);
        }
    }

}