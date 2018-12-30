package jekpro.model.builtin;

import jekpro.model.inter.Engine;
import jekpro.model.molec.BindCount;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;

/**
 * <p>Abstract class for toolkit and capability flags.</p>
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
public abstract class AbstractFlag {
    public final static String OP_ON = "on";
    public final static String OP_OFF = "off";
    public final static String OP_FALSE = "false";
    public final static String OP_NULL = "null";

    protected final int id;

    /**
     * <p>Create a flag.</p>
     *
     * @param i The id of the flag.
     */
    protected AbstractFlag(int i) {
        id = i;
    }

    /************************************************************/
    /* Prolog Flags                                             */
    /************************************************************/

    /**
     * <p>Retrieve the value of this Prolog flag.</p>
     *
     * @param en The engine.
     * @return The value.
     */
    public Object getFlag(Engine en) {
        throw new IllegalArgumentException("not implemented");
    }

    /**
     * <p>Set the value of a this Prolog flag.</p>
     *
     * @param m  The value skel.
     * @param d  The value display.
     * @param en The engine.
     * @return True if flag could be changed, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setFlag(Object m, BindCount[] d, Engine en)
            throws EngineMessage {
        throw new IllegalArgumentException("not implemented");
    }

    /************************************************************/
    /* Thread Flags                                             */
    /************************************************************/

    /**
     * <p>Retrieve the value of this thread flag.</p>
     *
     * @param t  The thread.
     * @param en The engine.
     * @return The value.
     */
    public Object getThreadFlag(Thread t, Engine en) {
        throw new IllegalArgumentException("not implemented");
    }

    /**
     * <p>Set the value of a this flag.</p>
     *
     * @param m  The value skel.
     * @param d  The value display.
     * @param t  The thread.
     * @param en The engine.
     * @return True if flag could be changed, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setThreadFlag(Object m, BindCount[] d,
                                 Thread t, Engine en)
            throws EngineMessage {
        throw new IllegalArgumentException("not implemented");
    }

    /************************************************************/
    /* On & Off Atom                                            */
    /************************************************************/

    /**
     * <p>Convert an atom to a switch. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param m The switch skel.
     * @param d The switch display.
     * @return The switch value.
     * @throws EngineMessage Shit happens.
     */
    public static boolean atomToSwitch(Object m, BindCount[] d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(OP_ON)) {
            return true;
        } else if (fun.equals(OP_OFF)) {
            return false;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

    /**
     * <p>Convert a switch to an atom.</p>
     * <p>A switch has the following syntax:</p>
     * <pre>
     *      switch = "on" | "off".
     * </pre>
     *
     * @param s The switch value.
     * @return The switch skel.
     */
    public static SkelAtom switchToAtom(boolean s) {
        return new SkelAtom(s ? OP_ON : OP_OFF);
    }

}