package jekpro.model.builtin;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Store;
import jekpro.tools.term.SkelAtom;
import matula.comp.sharik.AbstractActivator;
import matula.util.data.MapHash;

import java.io.File;

/**
 * <p>Session flags on runtime library level.</p>
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
public final class FlagSession extends AbstractFlag<Store> {
    public final static MapHash<String, AbstractFlag<Store>> DEFAULT
            = new MapHash<String, AbstractFlag<Store>>();

    public final static String OP_USER_DIR = "user_dir";

    private static final int FLAG_USER_DIR = 0;

    static {
        DEFAULT.add(OP_USER_DIR, new FlagSession(FLAG_USER_DIR));
    }

    /**
     * <p>Create a session flag.</p>
     *
     * @param i The id of the session flag.
     */
    private FlagSession(int i) {
        super(i);
    }

    /**
     * <p>Retrieve the value of this flag.</p>
     *
     * @param en The engine.
     * @return The value.
     */
    public Object getObjFlag(Store obj, Engine en) {
        switch (id) {
            case FLAG_USER_DIR:
                AbstractActivator activator = obj.foyer.getFramework().getActivator();
                File dir = activator.getUserDir(obj.foyer.getApplication());
                return new SkelAtom(dir.toString());
            default:
                throw new IllegalArgumentException("illegal flag");
        }
    }

    /**
     * <p>Set the value of a this flag.</p>
     *
     * @param m  The value skel.
     * @param d  The value display.
     * @param en The engine.
     * @return True if flag could be changed, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setObjFlag(Store obj, Object m, Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case FLAG_USER_DIR:
                /* can't modify */
                return false;
            default:
                throw new IllegalArgumentException("illegal flag");
        }
    }

}