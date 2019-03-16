package jekpro.model.rope;

import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologWriter;
import jekpro.tools.term.SkelAtom;
import matula.util.data.AssocArray;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>This class provides an array pairs implementation.</p>
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
public final class PairsArray
        extends AssocArray<Object, Bouquet>
        implements InterfacePairs {
    private static final int MAX_SMALL = 6;

    /**
     * <p>Create a pairs array.</p>
     */
    public PairsArray() {
        super();
    }

    /**
     * <p>Create a pairs array.</p>
     *
     * @param capa The ahead capacity.
     */
    public PairsArray(int capa) {
        super(capa);
    }

    /**
     * <p>Add the clause to the list of pairs.</p>
     *
     * @param clause The clause to be added.
     */
    public void addClause(Clause clause) {
        for (int j = 0; j < size(); j++) {
            Bouquet cp = getValue(j);
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
        int j = indexOf(m);
        Bouquet cp;
        if (j < 0) {
            if (b != null) {
                cp = b.copyBouquet();
            } else {
                cp = new Bouquet();
            }
            add(m, cp);
        } else {
            cp = getValue(j);
        }
        cp.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
        if (size() > MAX_SMALL)
            return toLarge();
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
        for (int j = 0; j < size(); j++) {
            Bouquet cp = getValue(j);
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
        int j = indexOf(m);
        Bouquet cp;
        if (j < 0) {
            if (b != null) {
                cp = b.copyBouquet();
            } else {
                cp = new Bouquet();
            }
            add(m, cp);
        } else {
            cp = getValue(j);
        }
        cp.assertClause(at + 1, clause, flags);
        if (size() > MAX_SMALL)
            return toLarge();
        return null;
    }

    /**
     * <p>Create a large map from this small map.</p>
     * <p>Carry over the bouquets.</p>
     *
     * @return The large map.
     */
    private InterfacePairs toLarge() {
        PairsHash res = new PairsHash(size());
        for (int j = 0; j < size(); j++) {
            Object m = getKey(j);
            Bouquet cp = getValue(j);
            res.add(m, cp);
        }
        return res;
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
        for (int j = size() - 1; j >= 0; j--) {
            Bouquet cp = getValue(j);
            cp.retractClause(at + 1, clause);
            if (cp.set == null) {
                removeEntry(j);
                dirty = true;
            }
        }
        if (dirty)
            resize();
        return null;
    }

    /**
     * <p>Retract the clause to the list of pairs.</p>
     *
     * @param m      The key.
     * @param clause The clause to be added.
     * @param at     The position.
     */
    public InterfacePairs retractClause(Object m, Clause clause, int at) {
        int j = indexOf(m);
        if (j < 0)
            return null;
        Bouquet cp = getValue(j);
        cp.retractClause(at + 1, clause);
        if (cp.set == null) {
            removeEntry(j);
            resize();
        }
        return null;
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
        wr.write("\n");
        PrologWriter pw = en.store.foyer.createWriter(Foyer.IO_TERM);
        pw.setSource(en.visor.peekStack());
        pw.setEngineRaw(en);
        for (int j = 0; j < size(); j++) {
            Bouquet cp = getValue(j);
            InterfaceRope set = cp.set;
            int len = (set != null ? set.getLengthScope(en) : 0);
            if (len == 0)
                continue;
            for (int i = 0; i < off; i++)
                wr.write(" ");
            wr.write("key=");
            Object val = getKey(j);
            if (val instanceof String)
                val = new SkelAtom((String) val);
            pw.setWriter(wr);
            pw.unparseStatement(val, Display.DISPLAY_CONST);
            wr.write(", ");
            cp.inspectPaths(wr, off + 2, start, len, en);
        }
    }

}