package jekpro.model.molec;

import jekpro.model.inter.Engine;
import matula.util.data.AbstractMap;
import matula.util.data.MapEntry;

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
    private final BindUniv univ;

    /**
     * <p>Create a varmap undo.</p>
     *
     * @param bc The bind univ.
     */
    private UndoSerno(BindUniv bc) {
        univ = bc;
    }

    /**
     * <p>Retrieve the univ.</p>
     *
     * @return The univ.
     */
    public BindUniv getUniv() {
        return univ;
    }

    /**
     * <p>Reset the varmap.</p>
     *
     * @param en The engine.
     */
    public void unbind(Engine en) {
        AbstractMap<BindUniv, Integer> map = en.visor.varmap;
        MapEntry<BindUniv, Integer> entry = map.getEntry(univ);
        if (entry == null)
            throw new IllegalStateException("value missing");
        map.removeEntry(entry);
        map.resize();

        removeBind(en);
    }

    /**
     * <p>Set a new varmap.</p>
     *
     * @param bc The bind univ.
     * @param en The engine.
     * @return The new varmap.
     */
    public static Integer bindSerno(BindUniv bc, Engine en) {
        AbstractMap<BindUniv, Integer> map = en.visor.varmap;
        Integer val = Integer.valueOf(map.totalSize());
        map.add(bc, val);

        UndoSerno bs = new UndoSerno(bc);
        bs.addBind(en);

        return val;
    }

}