package jekpro.model.inter;

import jekpro.frequent.experiment.SpecialRef;
import jekpro.frequent.standard.EngineCopy;
import jekpro.model.molec.*;
import jekpro.model.pretty.*;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Goal;
import jekpro.model.rope.PreClause;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapHashLink;

import java.io.Writer;

/**
 * <p>The base delegate class for a defined delegate.</p>
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
public abstract class AbstractDefined extends AbstractDelegate {
    public final static String OP_STATIC = "static";
    public final static String OP_DYNAMIC = "dynamic";
    public final static String OP_THREAD_LOCAL = "thread_local";
    public final static String OP_GROUP_LOCAL = "group_local";

    /* predicate type */
    public final static int MASK_DEFI_STAT = 0x00000010;
    public final static int MASK_DEFI_DYNA = 0x00000020;
    public final static int MASK_DEFI_THLC = 0x00000040;
    public final static int MASK_DEFI_GRLC = 0x00000080;

    public final static int MASK_DEFI_MASK = MASK_DEFI_STAT |
            MASK_DEFI_DYNA | MASK_DEFI_THLC | MASK_DEFI_GRLC;
    public final static int MASK_DEFI_ASSE =
            MASK_DEFI_DYNA | MASK_DEFI_THLC | MASK_DEFI_GRLC;

    /* clause compilation */
    public final static int MASK_DEFI_NOBR = 0x00000100;
    public final static int MASK_DEFI_STOP = 0x00000200;
    public final static int MASK_DEFI_NIST = 0x00000400;
    public final static int MASK_DEFI_NHWK = 0x00000800;

    public final static int MASK_DEFI_NBDY = 0x00001000;
    public final static int MASK_DEFI_NLST = 0x00002000;
    public final static int MASK_DEFI_NHED = 0x00004000;

    /* predicate check flags */
    public final static int OPT_CHCK_MASK = 0x0000000F;
    public final static int OPT_CHCK_DEFN = 0x00000001;
    public final static int OPT_CHCK_ASSE = 0x00000002;

    /* predicate creation flags */
    public final static int OPT_PROM_MASK = 0x000000F0;
    public final static int OPT_PROM_STAT = 0x00000010;
    public final static int OPT_PROM_DYNA = 0x00000020;
    public final static int OPT_PROM_THLC = 0x00000040;

    /* predicate style flags */
    public final static int OPT_STYL_MASK = 0x00000F00;
    public final static int OPT_STYL_DECL = 0x00000100;

    /* operation arguments flags */
    public final static int OPT_ARGS_ASOP = 0x00001000;

    /* operation action flags */
    public final static int OPT_ACTI_BOTT = 0x00010000;
    public final static int OPT_ACTI_WRIT = 0x00020000;

    /* operation result flags */
    public final static int OPT_RSLT_CREF = 0x00100000;
    public final static int OPT_RSLT_FRME = 0x00200000;

    /* combined operation flags */
    public final static int OPT_PERF_CNLT = AbstractDefined.OPT_PROM_STAT |
            AbstractDefined.OPT_CHCK_DEFN | AbstractDefined.OPT_STYL_DECL |
            AbstractDefined.OPT_ACTI_BOTT;

    /**
     * <p>Create a delegate defined.</p>
     *
     * @param flags The store flags.
     */
    AbstractDefined(int flags) {
        if ((flags & Foyer.MASK_STORE_NBDY) != 0)
            subflags |= AbstractDefined.MASK_DEFI_NBDY;
        if ((flags & Foyer.MASK_STORE_NLST) != 0)
            subflags |= AbstractDefined.MASK_DEFI_NLST;
        if ((flags & Foyer.MASK_STORE_NHED) != 0)
            subflags |= AbstractDefined.MASK_DEFI_NHED;
        if ((flags & Foyer.MASK_STORE_NIST) != 0)
            subflags |= AbstractDefined.MASK_DEFI_NIST;
        if ((flags & Foyer.MASK_STORE_NHWK) != 0)
            subflags |= AbstractDefined.MASK_DEFI_NHWK;
    }

    /**
     * <p>Promote predicate to static.</p>
     * <p>Implicit during clause consult or explicit by static directive.</p>
     *
     * @param pick  The predicate.
     * @param store The store.
     * @return The promotion result.
     */
    public static AbstractDelegate promoteStatic(Predicate pick,
                                                 Store store) {
        AbstractDelegate fun = pick.del;
        if (fun != null)
            return fun;
        AbstractDefined del;
        synchronized (pick) {
            fun = pick.del;
            if (fun != null)
                return fun;
            if ((pick.getBits() & Predicate.MASK_PRED_MULT) != 0) {
                del = new DefinedBlockingMulti(store.foyer.getBits());
            } else {
                del = new DefinedLockfree(store.foyer.getBits());
            }
            if ((pick.getBits() & Predicate.MASK_PRED_VIRT) != 0)
                del.subflags |= AbstractDelegate.MASK_DELE_VIRT;
            if ((pick.getBits() & Predicate.MASK_PRED_MULT) != 0)
                del.subflags |= AbstractDelegate.MASK_DELE_MULT;
            del.subflags |= AbstractDefined.MASK_DEFI_STAT;
            pick.del = del;
        }
        return del;
    }

    /**
     * <p>Promote predicate to dynamic.</p>
     * <p>Explicit by dynamic directive.</p>
     *
     * @param pick  The predicate.
     * @param store The store.
     * @return The promotion result.
     */
    public static AbstractDelegate promoteDynamic(Predicate pick,
                                                  Store store) {
        AbstractDelegate fun = pick.del;
        if (fun != null)
            return fun;
        AbstractDefined del;
        synchronized (pick) {
            fun = pick.del;
            if (fun != null)
                return fun;
            if ((pick.getBits() & Predicate.MASK_PRED_MULT) != 0) {
                del = new DefinedBlockingMulti(store.foyer.getBits());
            } else {
                del = new DefinedBlocking(store.foyer.getBits());
            }
            if ((pick.getBits() & Predicate.MASK_PRED_VIRT) != 0)
                del.subflags |= AbstractDelegate.MASK_DELE_VIRT;
            if ((pick.getBits() & Predicate.MASK_PRED_MULT) != 0)
                del.subflags |= AbstractDelegate.MASK_DELE_MULT;
            del.subflags |= AbstractDefined.MASK_DEFI_DYNA;
            pick.del = del;
        }
        return del;
    }

    /**
     * <p>Promote predicate to thread locale.</p>
     * <p>Explicit by thread_locale directive.</p>
     *
     * @param pick  The predicate.
     * @param store The store.
     * @return The promotion result.
     */
    public static AbstractDelegate promoteThreadLocal(Predicate pick,
                                                      Store store) {
        AbstractDelegate fun = pick.del;
        if (fun != null)
            return fun;
        AbstractDefined del;
        synchronized (pick) {
            fun = pick.del;
            if (fun != null)
                return fun;
            int s = store.foyer.acquireHole();
            del = new DefinedThreadLocal(s, store.foyer.getBits());
            if ((pick.getBits() & Predicate.MASK_PRED_VIRT) != 0)
                del.subflags |= AbstractDelegate.MASK_DELE_VIRT;
            if ((pick.getBits() & Predicate.MASK_PRED_MULT) != 0)
                del.subflags |= AbstractDelegate.MASK_DELE_MULT;
            del.subflags |= AbstractDefined.MASK_DEFI_THLC;
            pick.del = del;
        }
        return del;
    }

    /**
     * <p>Promote predicate to thread locale.</p>
     * <p>Explicit by thread_locale directive.</p>
     *
     * @param pick  The predicate.
     * @param store The store.
     * @return The promotion result.
     */
    public static AbstractDelegate promoteGroupLocal(Predicate pick,
                                                      Store store) {
        AbstractDelegate fun = pick.del;
        if (fun != null)
            return fun;
        AbstractDefined del;
        synchronized (pick) {
            fun = pick.del;
            if (fun != null)
                return fun;
            int s = store.foyer.acquireHole();
            del = new DefinedGroupLocal(s, store.foyer.getBits());
            if ((pick.getBits() & Predicate.MASK_PRED_VIRT) != 0)
                del.subflags |= AbstractDelegate.MASK_DELE_VIRT;
            if ((pick.getBits() & Predicate.MASK_PRED_MULT) != 0)
                del.subflags |= AbstractDelegate.MASK_DELE_MULT;
            del.subflags |= AbstractDefined.MASK_DEFI_GRLC;
            pick.del = del;
        }
        return del;
    }

    /*************************************************************/
    /* Execution Services                                        */
    /*************************************************************/

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        Object t = en.skel;
        Display d = en.display;
        Clause[] list = definedClauses(t, d, en);
        int at = 0;

        /* end of cursor */
        if (at == list.length)
            return false;

        AbstractBind mark = en.bind;
        Clause clause;
        DisplayClause dc = new DisplayClause();
        int lastalloc;
        /* search rope */
        for (; ; ) {
            clause = list[at++];
            if (dc.bind == null) {
                dc.bind = DisplayClause.newBindClause(clause.dispsize);
            } else {
                dc.bind = DisplayClause.setSizeClause(clause.dispsize, dc.bind);
            }
            dc.def = clause;
            lastalloc = (clause.intargs != null ?
                    AbstractDefined.unifyDefined(((SkelCompound) t).args, d,
                            ((SkelCompound) clause.head).args, dc,
                            clause.intargs, en) : 0);
            if (lastalloc != -1)
                break;

            /* end of cursor */
            if (at == list.length)
                return false;

            /* undo bindings */
            en.fault = null;
            en.releaseBind(mark);
            if (en.fault != null)
                throw en.fault;
        }
        DisplayClause u = en.contdisplay;
        dc.lastalloc = lastalloc;
        dc.number = en.number;
        dc.prune = ((subflags & MASK_DEFI_NOBR) != 0 ? u.prune : dc);
        dc.contskel = en.contskel;
        dc.contdisplay = u;

        if (at != list.length) {
            /* create choice point */
            en.choices = new ChoiceDefined(en.choices, at, list, dc, mark);
            en.number++;
            dc.flags |= DisplayClause.MASK_DPCL_MORE;
        }
        en.contskel = clause;
        en.contdisplay = dc;
        return en.getNext();
    }

    /**
     * <p>Unify the head of the clause with the given goal.</p>
     *
     * @param t1   The goal skeleton arguments.
     * @param ref  The goal display.
     * @param t2   The cause head skeleton arguments.
     * @param ref2 The cause head display.
     * @param arr  The unify instructions.
     * @param en   The engine.
     * @return True if the unification was successful, otherwise false.
     * @throws EngineException Shit happens.
     */
    static int unifyDefined(Object[] t1, Display ref,
                            Object[] t2, Display ref2,
                            int[] arr,
                            Engine en)
            throws EngineException {
        int k = 0;
        for (int i = 0; i < arr.length; i++) {
            int n = arr[i];
            if (n < 0) {
                if (n != Integer.MIN_VALUE)
                    if (!en.unifyTerm(t1[-n - 1], ref, t1[i], ref))
                        return -1;
            } else {
                if (k < n)
                    k = Engine.newBind(k, n, ref2.bind);
                if (!en.unifyTerm(t1[i], ref, t2[i], ref2))
                    return -1;
            }
        }
        return k;
    }

    /*************************************************************/
    /* Variation Points Predicate Defined                        */
    /*************************************************************/

    /**
     * <p>Retrieve the clause list.</p>
     *
     * @param en The engine.
     * @return The clauses list or null.
     */
    public abstract Clause[] listClauses(Engine en)
            throws EngineMessage;

    /**
     * <p>Retrieve a clause list for the given goal.</p>
     *
     * @param m  The goal skel.
     * @param d  The goal display.
     * @param en The engine.
     * @return The clauses, or null.
     */
    abstract Clause[] definedClauses(Object m, Display d, Engine en)
            throws EngineMessage;

    /**
     * <p>Retrieve the length of the clause list.</p>
     *
     * @param en The engine.
     * @return The length of the clause list.
     */
    public abstract int lengthClauses(Engine en);

    /**
     * <p>Add the clause to the predicate.</p>
     *
     * @param clause The clause.
     * @param flags  The flags.
     * @param en     The engine.
     * @throws EngineMessage Shit happens.
     */
    public abstract boolean assertClause(Clause clause, int flags,
                                         Engine en)
            throws EngineMessage;

    /**
     * <p>Remove the clause from the predicate.</p>
     *
     * @param clause The clause.
     * @param en     The engine.
     * @return True if clause was found and removed, otherwise false.
     */
    public abstract boolean retractClause(Clause clause, Engine en)
            throws EngineMessage;

    /**
     * <p>Inspect the index of a predicate.</p>
     *
     * @param wr The write.
     * @param en The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public abstract void inspectClauses(Writer wr, Engine en)
            throws EngineMessage, EngineException;

    /***********************************************************/
    /* Dynamic Database                                        */
    /***********************************************************/

    /**
     * <p>Enhance the knowledge base by a new clause.</p>
     * <p>The goal is passed via the engine skel and display.</p>
     * <p>UInderstood flags:</p>
     * <ul>
     * <li><b>OPT_ARGS_ASOP:</b> The goal has assert options.</li>
     * <li><b>MASK_OPER_DYNA:</b> Predicate should be dynamic.</li>
     * <li><b>MASK_OPER_THRE:</b> Predicate should be thread_local.</li>
     * <li><b>OPT_ACTI_BOTT:</b> The clause should be added at the end.</li>
     * </ul>
     *
     * @param flags The flags.
     * @param en    The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static void enhanceKnowledgebase(int flags, Engine en)
            throws EngineMessage, EngineException {
        Object[] temp = ((SkelCompound) en.skel).args;
        Display ref = en.display;
        EngineCopy ec = en.enginecopy;
        if (ec == null) {
            ec = new EngineCopy();
            en.enginecopy = ec;
        }
        ec.vars = null;
        ec.flags = 0;
        Object molec = ec.copyTermAndWrap(temp[0], ref, en);
        MapHashLink<String, SkelVar> vars;
        if ((flags & OPT_ARGS_ASOP) != 0) {
            MapHashLink<Object, NamedDistance> printmap = SpecialRef.decodeAssertOptions(temp[1], ref, en);
            vars = FileText.copyVars(ec.vars, printmap);
        } else {
            vars = null;
        }
        ec.vars = null;
        Clause clause = PreClause.determineCompiled(flags, molec, en);
        clause.vars = vars;
        clause.assertRef(flags, en);
    }

    /**
     * <p>Search the knowledge base.</p>
     * <p>The search goal is passed via the engine skel and display.</p>
     * <p>The following flags are recognized:</p>
     * <ul>
     * <li><b>OPT_ACTI_RETR:</b> Retract found clauses and one term argument.</li>
     * <li><b>MASK_OPER_CHWR:</b> Generate a write error instead of a read error.</li>
     * <li><b>MASK_OPER_DYNA:</b> Predicate should be accessible.</li>
     * <li><b>OPT_RSLT_CREF:</b> Return clause reference in last argument.</li>
     * <li><b>OPT_RSLT_FRME:</b> Return frame reference in last argument.</li>
     * </ul>
     *
     * @param flags The flags.
     * @param en    The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static boolean searchKnowledgebase(int flags, Engine en)
            throws EngineMessage, EngineException {
        Object[] temp = ((SkelCompound) en.skel).args;
        Display ref = en.display;
        /* detect head and body */
        SpecialQuali.colonToCallable(temp[0], ref, true, en);
        Object head = en.skel;
        Display refhead = en.display;

        /* check predicate existence and visibility */
        CachePredicate cp = StackElement.callableToPredicate(head, en);
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0) {
            EngineMessage.checkCallable(head, refhead);
            return false;
        }
        Predicate pick = cp.pick;
        /* check predicate modify/access */
        AbstractDelegate fun = pick.del;
        if ((flags & AbstractDefined.OPT_ACTI_WRIT) != 0) {
            switch (flags & OPT_CHCK_MASK) {
                case OPT_CHCK_DEFN:
                    AbstractDefined.checkDefinedWrite(fun, pick, en);
                    break;
                case OPT_CHCK_ASSE:
                    AbstractDefined.checkAssertableWrite(fun, pick, en);
                    break;
                default:
                    throw new IllegalArgumentException("illegal check");
            }
        } else {
            switch (flags & OPT_CHCK_MASK) {
                case OPT_CHCK_DEFN:
                    AbstractDefined.checkDefinedRead(fun, pick, en);
                    break;
                case OPT_CHCK_ASSE:
                    AbstractDefined.checkAssertableRead(fun, pick, en);
                    break;
                default:
                    throw new IllegalArgumentException("illegal check");
            }
        }
        /* find rope */
        return ((AbstractDefined) fun).searchFirst(flags, head, refhead, temp, ref, en);
    }

    /**
     * <p>Perform the search inside the delegate.</p></ü>
     *
     * @param flags   The flags.
     * @param head    The head skeleton.
     * @param refhead The head display.
     * @param temp    The arguments skeleton.
     * @param ref     The arguments display.
     * @param en      The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public boolean searchFirst(int flags,
                               Object head, Display refhead,
                               Object[] temp, Display ref,
                               Engine en)
            throws EngineException, EngineMessage {
        Clause[] list = definedClauses(head, refhead, en);
        int at = 0;

        /* end of cursor */
        if (at == list.length)
            return false;

        AbstractBind mark = en.bind;
        Clause clause;
        Display ref1 = new Display();
        /* search rope */
        for (; ; ) {
            clause = list[at++];
            if (ref1.bind == null) {
                ref1.bind = Display.newBind(clause.size);
            } else {
                ref1.bind = Display.setSize(clause.size, ref1.bind);
            }
            if (!(clause.head instanceof SkelCompound) ||
                    AbstractDefined.unifyArgs(((SkelCompound) head).args, refhead,
                            ((SkelCompound) clause.head).args, ref1, en)) {
                Object end = PreClause.intermediateToBody(clause.next, en.store);
                if (en.unifyTerm(temp[1], ref, end, ref1)) {
                    if ((flags & OPT_RSLT_FRME) != 0) {
                        Frame frame = new Frame(clause, ref1);
                        if (en.unifyTerm(temp[2], ref,
                                frame, Display.DISPLAY_CONST)) {
                            if ((flags & OPT_RSLT_CREF) != 0) {
                                if (en.unifyTerm(temp[3], ref,
                                        clause, Display.DISPLAY_CONST))
                                    break;
                            } else {
                                break;
                            }
                        }
                    } else if ((flags & OPT_RSLT_CREF) != 0) {
                        if (en.unifyTerm(temp[2], ref,
                                clause, Display.DISPLAY_CONST))
                            break;
                    } else {
                        break;
                    }
                }
            }

            /* end of cursor */
            if (at == list.length)
                return false;

            /* undo bindings */
            en.fault = null;
            en.releaseBind(mark);
            if (en.fault != null)
                throw en.fault;
        }
        if (clause.size != 0)
            BindCount.remTab(ref1.bind, en);

        if (at != list.length) {
            /* create choice point */
            en.choices = new ChoiceInspect(en.choices, at, list,
                    flags, (Goal) en.contskel, en.contdisplay,
                    ref1, mark);
            en.number++;
        }
        /* succeed */
        return en.getNext();
    }

    /**
     * <p>Unify the head args.</p>
     *
     * @param t1   The head skeleton arguments.
     * @param ref  The head display.
     * @param t2   The clause head skeleton arguments.
     * @param ref2 The clause display.
     * @return True if the unification succeeds, otherwise false.
     * @throws EngineException Shit happens.
     */
    static boolean unifyArgs(Object[] t1, Display ref,
                             Object[] t2, Display ref2,
                             Engine en)
            throws EngineException {
        for (int i = 0; i < t2.length; i++)
            if (!en.unifyTerm(t2[i], ref2, t1[i], ref))
                return false;
        return true;
    }

    /***********************************************************/
    /* Utilities                                               */
    /***********************************************************/

    /**
     * <p>Assure that the predicate is defined writeable.</p>
     *
     * @param fun  The delegate.
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void checkDefinedWrite(AbstractDelegate fun,
                                         Predicate pick, Engine en)
            throws EngineMessage {
        if (fun instanceof AbstractDefined)
            return;
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_MODIFY,
                EngineMessage.OP_PERMISSION_STATIC_PROCEDURE,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**
     * <p>Assure that the predicate is defined readable.</p>
     *
     * @param fun  The delegate.
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void checkDefinedRead(AbstractDelegate fun,
                                        Predicate pick, Engine en)
            throws EngineMessage {
        if (fun instanceof AbstractDefined)
            return;
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_ACCESS,
                EngineMessage.OP_PERMISSION_PRIVATE_PROCEDURE,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**
     * <p>Assure that the predicate is assertable.</p>
     *
     * @param fun  The delegate.
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void checkAssertableRead(AbstractDelegate fun,
                                           Predicate pick, Engine en)
            throws EngineMessage {
        if (fun != null && (fun.subflags & AbstractDefined.MASK_DEFI_ASSE) != 0)
            return;
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_ACCESS,
                EngineMessage.OP_PERMISSION_PRIVATE_PROCEDURE,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**
     * <p>Assure that the predicate is assertable.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void checkAssertableWrite(AbstractDelegate fun,
                                            Predicate pick, Engine en)
            throws EngineMessage {
        if (fun != null && (fun.subflags & AbstractDefined.MASK_DEFI_ASSE) != 0)
            return;
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_MODIFY,
                EngineMessage.OP_PERMISSION_STATIC_PROCEDURE,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /***************************************************************/
    /* Foreign Predicates                                          */
    /***************************************************************/

    /**
     * <p>Retrieve a name guess.</p>
     *
     * @return The name guess, or null.
     */
    public String getFun() {
        return null;
    }

    /**
     * <p>Retrieve an length guess.</p>
     *
     * @return The length guess, or -1.
     */
    public int getArity() {
        return -1;
    }

    /***************************************************************/
    /* Special Predicates                                          */
    /***************************************************************/

    /**
     * <p>Compute a hash code.</p>
     *
     * @return The hash code of this delegate.
     */
    public final int hashCode() {
        return subflags & MASK_DEFI_MASK;
    }

    /**
     * <p>Compare with another delegate.</p>
     *
     * @param o The other delegate.
     * @return True if this delegate equals the other delegate, otherwise false.
     */
    public final boolean equals(Object o) {
        if (!(o instanceof AbstractDefined))
            return false;
        int t1 = subflags & MASK_DEFI_MASK;
        int t2 = ((AbstractDefined) o).subflags & MASK_DEFI_MASK;
        if (t1 != t2)
            return false;
        return true;
    }

    /**
     * <p>Generate the spec of this delegate.</p>
     *
     * @param source The source.
     * @param en     The engine.
     * @return The spec.
     */
    public final Object toSpec(AbstractSource source, Engine en) {
        switch (subflags & MASK_DEFI_MASK) {
            case MASK_DEFI_STAT:
                return new SkelAtom(OP_STATIC);
            case MASK_DEFI_DYNA:
                return new SkelAtom(OP_DYNAMIC);
            case MASK_DEFI_THLC:
                return new SkelAtom(OP_THREAD_LOCAL);
            case MASK_DEFI_GRLC:
                return new SkelAtom(OP_GROUP_LOCAL);
            default:
                throw new IllegalArgumentException("illegal type");
        }

    }

}
