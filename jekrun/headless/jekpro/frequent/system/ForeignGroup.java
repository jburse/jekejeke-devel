package jekpro.frequent.system;

import jekpro.tools.call.ArrayEnumeration;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.InterpreterMessage;

/**
 * The foreign predicates for the module system/group.
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
public final class ForeignGroup {

    private final static String OP_SYS_GROUP_NAME = "sys_group_name";

    private final static String[] OP_PROPS = {
            OP_SYS_GROUP_NAME};

    /* For autonumbering anonymous groups. */
    private static int groupInitNumber;

    private static synchronized int nextGroupNum() {
        return groupInitNumber++;
    }

    /****************************************************************/
    /* Group Creation                                               */
    /****************************************************************/

    /**
     * <p>Create a new annoymous thread group.</p>
     *
     * @return The new annonymous trhead group.
     */
    public static ThreadGroup sysGroupNew() {
        ThreadGroup tg = new ThreadGroup("Group-" + nextGroupNum());
        tg.setDaemon(true);
        return tg;
    }

    /****************************************************************/
    /* Group Inspection                                             */
    /****************************************************************/

    /**
     * <p>Retrieve the threads of a thread group.</p>
     *
     * @param co The call out.
     * @param tg The thread group.
     * @return The oldest thread or null.
     */
    public static Thread sysCurrentGroupThread(CallOut co, ThreadGroup tg) {
        ArrayEnumeration<Thread> dc;
        if (co.getFirst()) {
            dc = new ArrayEnumeration<Thread>(snapshotThreads(tg));
            co.setData(dc);
        } else {
            dc = (ArrayEnumeration<Thread>) co.getData();
        }
        if (!dc.hasMoreElements())
            return null;
        Thread res = dc.nextElement();
        co.setRetry(dc.hasMoreElements());
        return res;
    }

    /**
     * <p>Compute a snapshot of the threads a thread group.</p>
     *
     * @param tg The thread group.
     * @return The snapshot of the threads of the thread group.
     */
    private static Thread[] snapshotThreads(ThreadGroup tg) {
        Thread[] threads = new Thread[4];
        int num = tg.enumerate(threads, false);
        while (num == threads.length) {
            threads = new Thread[threads.length * 2];
            num = tg.enumerate(threads, false);
        }
        Thread[] res = new Thread[num];
        System.arraycopy(threads, 0, res, 0, num);
        return res;
    }

    /**
     * <p>Retrieve the known group properties.</p>
     *
     * @param co The call out.
     * @return The known group property.
     */
    public static String sysCurrentGroupFlag(CallOut co) {
        ArrayEnumeration<String> dc;
        if (co.getFirst()) {
            dc = new ArrayEnumeration<String>(OP_PROPS);
            co.setData(dc);
        } else {
            dc = (ArrayEnumeration<String>) co.getData();
        }
        if (!dc.hasMoreElements())
            return null;
        String res = dc.nextElement();
        co.setRetry(dc.hasMoreElements());
        return res;
    }

    /**
     * <p>Retrieve a group property.</p>
     *
     * @param t    The group.
     * @param name The group property name.
     * @return The group property value, or null.
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysGetGroupFlag(ThreadGroup t, String name)
            throws InterpreterMessage {
        if (OP_SYS_GROUP_NAME.equals(name)) {
            return t.getName();
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    "prolog_flag", name));
        }
    }

}