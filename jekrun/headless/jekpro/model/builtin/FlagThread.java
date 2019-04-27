package jekpro.model.builtin;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import matula.util.data.MapHash;

/**
 * <p>Thread flags on runtime library level.</p>
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
public final class FlagThread extends AbstractFlag<Thread> {
    public final static MapHash<String, AbstractFlag<Thread>> DEFAULT
            = new MapHash<String, AbstractFlag<Thread>>();

    public final static String OP_FLAG_SYS_THREAD_NAME = "sys_thread_name";
    public final static String OP_FLAG_SYS_THREAD_STATE = "sys_thread_state";
    public final static String OP_FLAG_SYS_THREAD_GROUP = "sys_thread_group";

    private static final int FLAG_SYS_THREAD_NAME = 0;
    private static final int FLAG_SYS_THREAD_STATE = 1;
    private static final int FLAG_SYS_THREAD_GROUP = 2;

    static {
        DEFAULT.add(OP_FLAG_SYS_THREAD_NAME, new FlagThread(FLAG_SYS_THREAD_NAME));
        DEFAULT.add(OP_FLAG_SYS_THREAD_STATE, new FlagThread(FLAG_SYS_THREAD_STATE));
        DEFAULT.add(OP_FLAG_SYS_THREAD_GROUP, new FlagThread(FLAG_SYS_THREAD_GROUP));
    }

    /**
     * <p>Create a thread flag.</p>
     *
     * @param i The id of the thread flag.
     */
    private FlagThread(int i) {
        super(i);
    }

    /**
     * <p>Retrieve the value of this thread flag.</p>
     *
     * @param t  The thread.
     * @param en The engine.
     * @return The value.
     */
    public Object getObjFlag(Thread t, Engine en) {
        switch (id) {
            case FLAG_SYS_THREAD_NAME:
                return t.getName();
            case FLAG_SYS_THREAD_STATE:
                return t.getState().name();
            case FLAG_SYS_THREAD_GROUP:
                return t.getThreadGroup();
            default:
                throw new IllegalArgumentException("illegal flag");
        }
    }

    /**
     * <p>Set the value of a this flag.</p>
     *
     * @param t  The thread.
     * @param m  The value skel.
     * @param d  The value display.
     * @param en The engine.
     * @return True if flag could be changed, otherwise false.
     */
    public boolean setObjFlag(Thread t, Object m, Display d,
                              Engine en) {
        switch (id) {
            case FLAG_SYS_THREAD_NAME:
                /* can't modify */
                return false;
            case FLAG_SYS_THREAD_STATE:
                /* can't modify */
                return false;
            case FLAG_SYS_THREAD_GROUP:
                /* can't modify */
                return false;
            default:
                throw new IllegalArgumentException("illegal flag");
        }
    }

}