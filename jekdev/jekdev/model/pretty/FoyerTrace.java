package jekdev.model.pretty;

import jekdev.model.builtin.ClauseTrace;
import jekdev.model.builtin.DirectiveTrace;
import jekdev.model.builtin.SupervisorTrace;
import jekdev.reference.system.SpecialMode;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.*;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Operator;
import jekpro.model.rope.Resource;
import jekpro.tools.term.SkelAtom;

/**
 * <p>Shared by all store traces.</p>
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
public final class FoyerTrace extends Foyer {
    public final static String OP_SYS_IN = "sys_in";
    public final static String OP_SYS_OUT = "sys_out";
    public final static String OP_SYS_AT = "sys_at";

    public final static String OP_TRACE_GOAL = "trace_goal";

    public final static int MASK_FOYER_NLOG = 0x00010000;

    public SkelAtom ATOM_TRACE_GOAL = new SkelAtom(FoyerTrace.OP_TRACE_GOAL);

    public final SkelAtom ATOM_CALL = new SkelAtom(SpecialMode.OP_CALL);
    public final SkelAtom ATOM_FAIL = new SkelAtom(SpecialMode.OP_FAIL);
    public final SkelAtom ATOM_EXIT = new SkelAtom(SpecialMode.OP_EXIT);
    public final SkelAtom ATOM_REDO = new SkelAtom(SpecialMode.OP_REDO);
    public final SkelAtom ATOM_HEAD = new SkelAtom(SpecialMode.OP_HEAD);
    public final SkelAtom ATOM_CHOP = new SkelAtom(SpecialMode.OP_CHOP);

    public SkelAtom ATOM_SYS_IN;
    public SkelAtom ATOM_SYS_OUT;
    public SkelAtom ATOM_SYS_AT;

    private String monirunning = "";
    private String moniconfig = "";

    /**
     * <p>Retrieve the running monitor authority.</p>
     *
     * @return The running monitor authority.
     */
    public String getMonitorRunning() {
        return monirunning;
    }

    /**
     * <p>Set the running monitor authority.</p>
     *
     * @param au The running monitor authority.
     */
    public void setMonitorRunning(String au) {
        monirunning = au;
    }

    /**
     * <p>Retrieve the config monitor authority.</p>
     *
     * @return The config monitor authority.
     */
    public String getMonitorConfig() {
        return moniconfig;
    }

    /**
     * <p>Set the config monitor authority.</p>
     *
     * @param au The config monitor authority.
     */
    public void setMonitorConfig(String au) {
        moniconfig = au;
    }

    /**********************************************************/
    /* Foyer Livecycle                                        */
    /**********************************************************/

    /**
     * <p>Init the foyer.</p>
     *
     * @param en     The engine.
     * @param prompt The prompt flag.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void initFoyer(Engine en, boolean prompt)
            throws EngineMessage, EngineException {
        ATOM_SYS_IN = new SkelAtom(FoyerTrace.OP_SYS_IN, en.store.getRootSystem());
        ATOM_SYS_OUT = new SkelAtom(FoyerTrace.OP_SYS_OUT, en.store.getRootSystem());
        ATOM_SYS_AT = new SkelAtom(FoyerTrace.OP_SYS_AT, en.store.getRootSystem());

        super.initFoyer(en, prompt);
    }

    /**
     * <p>Fini the foyer.</p>
     *
     * @param store The store.
     * @throws EngineMessage Shit happens.
     */
    public void finiFoyer(Store store)
            throws EngineMessage, EngineException {
        super.finiFoyer(store);

        ATOM_SYS_IN = null;
        ATOM_SYS_OUT = null;
        ATOM_SYS_AT = null;
    }

    /*********************************************************/
    /* I/O Object Factories                                  */
    /*********************************************************/

    /**
     * <p>Factory for Prolog readers.</p>
     *
     * @return The Prolog reader.
     */
    public PrologReader createReader(int io) {
        switch (io) {
            case Foyer.IO_TERM:
                return new PrologReaderTrace();
            case Foyer.IO_ANNO:
                return new PrologReaderAnnoTrace();
            default:
                throw new IllegalArgumentException("illegal io");
        }
    }

    /**
     * <p>Factory for Prolog writers.</p>
     *
     * @return The Prolog writer.
     */
    public PrologWriter createWriter(int io) {
        switch (io) {
            case IO_TERM:
                return new PrologWriterTrace();
            case IO_ANNO:
                return new PrologWriterAnnoTrace();
            default:
                throw new IllegalArgumentException("illegal io");
        }
    }

    /*********************************************************/
    /* Store etc.. Object Factories                          */
    /*********************************************************/

    /**
     * <p>Factory for stores.</p>
     *
     * @param parent The parent store.
     * @return The store.
     */
    public Store createStore(Store parent) {
        return new StoreTrace(this, parent);
    }

    /**
     * <p>Factory for supervisors.</p>
     *
     * @return The supervisor.
     */
    public Supervisor createSupervisor() {
        return new SupervisorTrace(this);
    }

    /**
     * <p>Factory for resources by source key.</p>
     *
     * @param key The source key.
     * @return The resource.
     */
    public Resource createResource(String key) {
        return new ResourceTrace(key);
    }

    /*********************************************************/
    /* Atom etc.. Object Factories                           */
    /*********************************************************/

    /**
     * <p>Factory for atoms by name and source.</p>
     *
     * @param f The name.
     * @param s The source.
     * @param m The mask.
     */
    public SkelAtom createAtom(String f, AbstractSource s, int m) {
        if ((m & SkelAtom.MASK_ATOM_POSI) == 0)
            return super.createAtom(f, s, m);
        if ((m & SkelAtom.MASK_ATOM_ANNO) != 0)
            return new SkelAtomAnnoTrace(f, s);
        if ((m & SkelAtom.MASK_ATOM_QALI) != 0)
            return new SkelAtomQualiTrace(f, s);
        return new SkelAtomTrace(f, s);
    }

    /**
     * <p>Factor for directive.</p>
     *
     * @param copt The flags.
     * @return The directive.
     */
    public Directive createDirective(int copt) {
        return new DirectiveTrace(copt);
    }

    /**
     * <p>Factor for clause.</p>
     *
     * @param copt The flags.
     * @return The clause.
     */
    public Clause createClause(int copt) {
        return new ClauseTrace(copt);
    }

    /**
     * <p>Create a new locator.</p>
     *
     * @param src The source.
     * @return The new locator, or null.
     */
    public AbstractLocator createLocator(AbstractSource src) {
        return new LocatorTrace(src);
    }

    /**
     * <p>Factor for operators by type and key.</p>
     *
     * @param t The type.
     * @param k The key.
     * @return The operator.
     */
    public Operator createOperator(int t, String k) {
        return new OperatorTrace(t, k);
    }

}