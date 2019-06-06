package jekpro.model.molec;

import jekpro.model.inter.Engine;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

/**
 * <p>The class provides a varmap number undo.</p>
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
public final class UndoSerno extends AbstractUndo {
    private final Display display;

    /**
     * <p>Create a varmap undo.</p>
     *
     * @param d The display.
     */
    private UndoSerno(Display d) {
        display = d;
    }

    /**
     * <p>Reset the varmap.</p>
     *
     * @param en The engine.
     */
    public void unbind(Engine en) {
        MapHash<Display, Integer> m = en.visor.varmap;
        MapEntry<Display, Integer> e = m.getEntry(display);
        if (e == null)
            throw new IllegalStateException("value missing");
        m.removeEntry(e);
        if (m.size() == 0) {
            en.visor.varmap = null;
        } else {
            m.resize();
        }
        en.visor.serno -= display.bind.length;

        removeBind(en);
    }

    /**
     * <p>Set a new varmap.</p>
     *
     * @param d  The display.
     * @param en The engine.
     * @return The new varmap.
     */
    public static Integer bindVarmap(Display d, Engine en) {
        MapHash<Display, Integer> m = en.visor.varmap;
        if (m == null) {
            m = new MapHash<Display, Integer>();
            en.visor.varmap = m;
        }
        Integer val = Integer.valueOf(en.visor.serno);
        m.add(d, val);
        en.visor.serno += d.bind.length;

        UndoSerno bs = new UndoSerno(d);
        bs.addBind(en);

        return val;
    }

}