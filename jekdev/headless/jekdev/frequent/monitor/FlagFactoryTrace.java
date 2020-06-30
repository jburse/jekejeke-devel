package jekdev.frequent.monitor;

import jekdev.model.pretty.FoyerTrace;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import matula.util.data.MapHash;

/**
 * <p>Toolkit trace flags.</p>
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
public final class FlagFactoryTrace extends AbstractFlag<Engine> {
    public final static MapHash<String, AbstractFlag<Engine>> DEFAULT
            = new MapHash<String, AbstractFlag<Engine>>();

    public final static String OP_SYS_MONITOR_LOGGING = "sys_monitor_logging";
    public final static String OP_SYS_MONITOR_RUNNING = "sys_monitor_running";
    public final static String OP_SYS_MONITOR_CONFIG = "sys_monitor_config";

    private static final int FLAG_SYS_MONITOR_LOGGING = 0;
    private static final int FLAG_SYS_MONITOR_RUNNING = 1;
    private static final int FLAG_SYS_MONITOR_CONFIG = 2;

    /**
     * <p>Create a flag.</p>
     *
     * @param i The id of the flag.
     */
    private FlagFactoryTrace(int i) {
        super(i);
    }

    static {
        DEFAULT.add(OP_SYS_MONITOR_LOGGING, new FlagFactoryTrace(FLAG_SYS_MONITOR_LOGGING));
        DEFAULT.add(OP_SYS_MONITOR_RUNNING, new FlagFactoryTrace(FLAG_SYS_MONITOR_RUNNING));
        DEFAULT.add(OP_SYS_MONITOR_CONFIG, new FlagFactoryTrace(FLAG_SYS_MONITOR_CONFIG));
    }

    /**
     * <p>Retrieve the value of this flag.</p>
     *
     * @param en The engine.
     * @return The value.
     */
    public Object getObjFlag(Engine obj, Engine en) {
        switch (id) {
            case FLAG_SYS_MONITOR_LOGGING:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() & FoyerTrace.MASK_FOYER_NLOG) == 0);
            case FLAG_SYS_MONITOR_RUNNING:
                return new SkelAtom(((FoyerTrace) en.store.foyer).getMonitorRunning());
            case FLAG_SYS_MONITOR_CONFIG:
                return new SkelAtom(((FoyerTrace) en.store.foyer).getMonitorConfig());
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
    public boolean setObjFlag(Engine obj, Object m, Display d, Engine en)
            throws EngineMessage {
        try {
            switch (id) {
                case FLAG_SYS_MONITOR_LOGGING:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.resetBit(FoyerTrace.MASK_FOYER_NLOG);
                    } else {
                        en.store.foyer.setBit(FoyerTrace.MASK_FOYER_NLOG);
                    }
                    return true;
                case FLAG_SYS_MONITOR_RUNNING:
                    String str = SpecialUniv.derefAndCastString(m, d);
                    ((FoyerTrace) en.store.foyer).setMonitorRunning(str);
                    return true;
                case FLAG_SYS_MONITOR_CONFIG:
                    str = SpecialUniv.derefAndCastString(m, d);
                    ((FoyerTrace) en.store.foyer).setMonitorConfig(str);
                    return true;
                default:
                    throw new IllegalArgumentException("illegal flag");
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

}