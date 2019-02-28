package jekpro.frequent.system;

import jekpro.model.builtin.AbstractFlag;
import jekpro.tools.call.*;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Lobby;
import jekpro.tools.term.SkelAtom;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.wire.AbstractLivestock;
import matula.util.wire.Fence;

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
    private final static String OP_SYS_GROUP_GROUP = "sys_group_group";

    private final static String[] OP_PROPS = {
            OP_SYS_GROUP_NAME,
            OP_SYS_GROUP_GROUP};

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

    /**
     * <p>Make a copy of the given term and create a thread
     * running the term once.</p>
     *
     * @param inter The interpreter.
     * @param tg    The thread group.
     * @param t     The term.
     * @return The new thread.
     */
    public static Thread sysThreadNew(Interpreter inter, ThreadGroup tg, AbstractTerm t)
            throws InterpreterMessage {
        Object obj = AbstractTerm.copyMolec(inter, t);
        final Interpreter inter2 = ForeignThread.makeInterpreter(inter);
        final CallIn callin = inter2.iterator(obj);
        Thread thread = new Thread(tg, ForeignThread.makeRunnable(callin, inter2));
        inter2.getController().setFence(thread);
        return thread;
    }

    /****************************************************************/
    /* Group Enumeration                                            */
    /****************************************************************/

    /**
     * <p>Retrieve the oldest threads of a thread group.</p>
     *
     * @param tg The thread group.
     * @return The oldest thread or null.
     */
    public static Thread sysGroupThread(ThreadGroup tg) {
        Thread[] threads = new Thread[1];
        int num = tg.enumerate(threads, false);
        return (num != 0 ? threads[0] : null);
    }

    /**
     * <p>Retrieve the threads of a thread group.</p>
     *
     * @param co The call out.
     * @param tg The thread group.
     * @return The threads.
     */
    public static Thread sysCurrentThread(CallOut co, ThreadGroup tg) {
        ArrayEnumeration<Thread> dc;
        if (co.getFirst()) {
            dc = new ArrayEnumeration<Thread>(snapshotThreadsOfGroup(tg));
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
     * <p>Compute a snapshot of the threads of a thread group.</p>
     *
     * @param tg The thread group.
     * @return The snapshot of the threads of the thread group.
     */
    private static Thread[] snapshotThreadsOfGroup(ThreadGroup tg) {
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
     * <p>Retrieve the groups of a thread group.</p>
     *
     * @param co The call out.
     * @param tg The thread group.
     * @return The groups.
     */
    public static ThreadGroup sysCurrentGroup(CallOut co, ThreadGroup tg) {
        ArrayEnumeration<ThreadGroup> dc;
        if (co.getFirst()) {
            dc = new ArrayEnumeration<ThreadGroup>(snapshotGroupsOfGroup(tg));
            co.setData(dc);
        } else {
            dc = (ArrayEnumeration<ThreadGroup>) co.getData();
        }
        if (!dc.hasMoreElements())
            return null;
        ThreadGroup res = dc.nextElement();
        co.setRetry(dc.hasMoreElements());
        return res;
    }

    /**
     * <p>Compute a snapshot of the groups of a thread group.</p>
     *
     * @param tg The thread group.
     * @return The snapshot of the groups of the thread group.
     */
    private static ThreadGroup[] snapshotGroupsOfGroup(ThreadGroup tg) {
        ThreadGroup[] groups = new ThreadGroup[4];
        int num = tg.enumerate(groups, false);
        while (num == groups.length) {
            groups = new ThreadGroup[groups.length * 2];
            num = tg.enumerate(groups, false);
        }
        ThreadGroup[] res = new ThreadGroup[num];
        System.arraycopy(groups, 0, res, 0, num);
        return res;
    }

    /****************************************************************/
    /* Group Flags                                                  */
    /****************************************************************/

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
     * @param tg   The group.
     * @param name The group property name.
     * @return The group property value, or null.
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysGetGroupFlag(ThreadGroup tg, String name)
            throws InterpreterMessage {
        if (OP_SYS_GROUP_NAME.equals(name)) {
            return tg.getName();
        } else if (OP_SYS_GROUP_GROUP.equals(name)) {
            ThreadGroup val = tg.getParent();
            return (val != null ? val : new SkelAtom(AbstractFlag.OP_NULL));
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    "prolog_flag", name));
        }
    }

    /****************************************************************/
    /* Managed Threads                                              */
    /****************************************************************/

    /**
     * <p>Retrieve the managed threads.</p>
     *
     * @param co The call out.
     * @param inter The interpreter.
     * @return The Prolog list of managed threads.
     */
    public static Object sysCurrentThread(CallOut co, Interpreter inter) {
        ArrayEnumeration<Thread> dc;
        if (co.getFirst()) {
            Lobby lobby = inter.getKnowledgebase().getLobby();
            dc = new ArrayEnumeration<Thread>(snapshotManagedThreads(lobby));
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
     * <p>Compute a snapshot of the managed threads.</p>
     *
     * @param lobby The lobby.
     * @return The managed thread.
     */
    private static Thread[] snapshotManagedThreads(Lobby lobby) {
        ListArray<Thread> list = new ListArray<Thread>();
        MapEntry<Thread, AbstractLivestock>[] snapshot = Fence.DEFAULT.snapshotLivestocks();
        for (int i = snapshot.length - 1; i >= 0; i--) {
            AbstractLivestock al = snapshot[i].value;
            if (al.source != lobby.getFoyer())
                continue;
            list.add(snapshot[i].key);
        }
        Thread[] res=new Thread[list.size()];
        list.toArray(res);
        return res;
    }

    /**
     * <p>Check whether the thread is managed.</p>
     *
     * @param inter The interpreter.
     * @param t     The thread.
     * @return True if the thread is managed, otherwise false.
     */
    public static boolean sysCurrentThreadChk(Interpreter inter, Thread t) {
        Lobby lobby = inter.getKnowledgebase().getLobby();
        AbstractLivestock al = Fence.DEFAULT.getLivestock(t);
        return (al != null && al.source == lobby.getFoyer());
    }

    /****************************************************************/
    /* Debug Threads                                                */
    /****************************************************************/

    /**
     * <p>Retrieve the JVM stack as a Prolog term.</p>
     *
     * @param t The thread.
     * @return The JVM stack.
     */
    /*
    public static Object sysThreadStack(Interpreter inter, Thread t) {
        Lobby lobby = inter.getKnowledgebase().getLobby();
        Object res = lobby.ATOM_NIL;
        StackTraceElement[] stacks = t.getStackTrace();
        for (int i = stacks.length - 1; i >= 0; i--) {
            StackTraceElement stack=stacks[i];
            String fn=stack.getFileName();
            Object val=new TermCompound("stack", stack.getClassName(),
                      stack.getMethodName(), (fn!=null?fn:""), stack.getLineNumber());
            res = new TermCompound(lobby.ATOM_CONS, val, res);
        }
        return res;
    }
    */

}