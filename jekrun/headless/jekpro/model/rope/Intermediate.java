package jekpro.model.rope;

import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.AbstractList;

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
     * <p>Check whether the given term is an alternative.</p>
     *
     * @param term The term.
     * @return True if the term is an alterantive.
     */
    public static boolean isAlter(Object term) {
        if (term instanceof SkelCompound &&
                ((SkelCompound) term).args.length == 2 &&
                ((SkelCompound) term).sym.fun.equals(Foyer.OP_SYS_ALTER)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Check whether the given term is an alternative.</p>
     *
     * @param term The term.
     * @return True if the term is an alterantive.
     */
    public static boolean isGuard(Object term) {
        if (term instanceof SkelCompound &&
                ((SkelCompound) term).args.length == 1 &&
                ((SkelCompound) term).sym.fun.equals(Foyer.OP_SYS_GUARD)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Check whether the given term is a sequent.</p>
     *
     * @param term The term.
     * @return True if the term is a sequent.
     */
    public static boolean isSequen(Object term) {
        if (term instanceof SkelCompound &&
                ((SkelCompound) term).args.length == 1 &&
                ((SkelCompound) term).sym.fun.equals(Foyer.OP_SYS_SEQUEN)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Determine the control type.</p>
     *
     * @param term The term.
     * @return The control type.
     */
    public static int controlType(Object term) {
        if (term instanceof SkelAtom &&
                ((SkelAtom) term).fun.equals(Foyer.OP_SYS_BEGIN)) {
            return Directive.TYPE_CTRL_BEGN;
        } else if (term instanceof SkelAtom &&
                ((SkelAtom) term).fun.equals(Foyer.OP_SYS_COMMIT)) {
            return Directive.TYPE_CTRL_CMMT;
        } else if (term instanceof SkelAtom &&
                ((SkelAtom) term).fun.equals(Foyer.OP_SYS_SOFT_BEGIN)) {
            return Directive.TYPE_CTRL_SBGN;
        } else if (term instanceof SkelAtom &&
                ((SkelAtom) term).fun.equals(Foyer.OP_SYS_SOFT_COMMIT)) {
            return Directive.TYPE_CTRL_SCMT;
        } else {
            return Directive.TYPE_CTRL_NONE;
        }
    }

}
