package jekpro.model.rope;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>The class provides the base class for intermediate code.</p>
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
public abstract class Intermediate {
    public Intermediate next;
    public int flags;

    /**
     * <p>Retrieve the next term depending on debug mode.</p>
     * <p>Should be implemented by subclasses.</p>
     *
     * @param en The engine.
     * @return The next term.
     */
    public Intermediate getNextRaw(Engine en) {
        return next;
    }

    /**
     * <p>Resolve the current term.</p>
     *
     * @param en The engine.
     * @return True if success, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public abstract boolean resolveNext(Engine en)
            throws EngineException, EngineMessage;

    /**
     * <p>Set the structure and minarg of the variables in the given term.</p>
     *
     * @param molec  The head skeleton.
     * @param helper The helper.
     */
    public static void setHead(Object molec, Optimization[] helper) {
        if (!(molec instanceof SkelCompound))
            return;
        SkelCompound mc = (SkelCompound) molec;
        for (int i = mc.args.length - 1; i >= 0; i--) {
            Object a = mc.args[i];
            if (a instanceof SkelVar) {
                SkelVar mv = (SkelVar) a;
                Optimization ov = helper[mv.id];
                ov.flags = (ov.flags & ~Optimization.MASK_VAR_MARG) | i;
            } else if (a instanceof SkelCompound) {
                Object var = ((SkelCompound) a).var;
                if (var == null)
                    continue;
                if (var instanceof SkelVar) {
                    SkelVar mv = (SkelVar) var;
                    Optimization ov = helper[mv.id];
                    ov.flags |= Optimization.MASK_VAR_HSTR;
                } else {
                    SkelVar[] temp = (SkelVar[]) var;
                    for (int j = 0; j < temp.length; j++) {
                        SkelVar mv = temp[j];
                        Optimization ov = helper[mv.id];
                        ov.flags |= Optimization.MASK_VAR_HSTR;
                    }
                }
            }
        }
    }

    /**
     * <p>Set the goals structure flag.</p>
     *
     * @param term   The body skeleton.
     * @param helper The helper.
     */
    public static void setBody(Object term, Optimization[] helper) {
        Object var = SupervisorCopy.getVar(term);
        if (var == null)
            return;
        if (var instanceof SkelVar) {
            SkelVar mv = (SkelVar) var;
            Optimization ov = helper[mv.id];
            ov.flags |= Optimization.MASK_VAR_BODY;
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            for (int j = 0; j < temp.length; j++) {
                SkelVar mv = temp[j];
                Optimization ov = helper[mv.id];
                ov.flags |= Optimization.MASK_VAR_BODY;
            }
        }
    }

}
