package jekdev.reference.wire;

import jekdev.model.pretty.FoyerTrace;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.array.FlagFactory;
import matula.util.data.MapHash;
import matula.util.wire.MapHashWithImport;

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
public final class FlagFactoryTrace extends AbstractFlag {
    public final static String OP_FLAG_SYS_MONITOR_CONFIG = "sys_monitor_config";
    public final static String OP_FLAG_SYS_MONITOR_RUNNING = "sys_monitor_running";
    public final static String OP_FLAG_SYS_MONITOR_LOGGING = "sys_monitor_logging";

    private static final int FLAG_SYS_MONITOR_CONFIG = 0;
    private static final int FLAG_SYS_MONITOR_RUNNING = 1;
    private static final int FLAG_SYS_MONITOR_LOGGING = 2;

    /**
     * <p>Create a flag.</p>
     *
     * @param i The id of the flag.
     */
    private FlagFactoryTrace(int i) {
        super(i);
    }

    /**
     * <p>Define the prolog flags.</p>
     *
     * @return The prolog flags.
     */
    static MapHash<String, AbstractFlag> defineFlags() {
        MapHash<String, AbstractFlag> prologflags = new MapHash<String, AbstractFlag>();
        prologflags.add(OP_FLAG_SYS_MONITOR_CONFIG, new FlagFactoryTrace(FLAG_SYS_MONITOR_CONFIG));
        prologflags.add(OP_FLAG_SYS_MONITOR_RUNNING, new FlagFactoryTrace(FLAG_SYS_MONITOR_RUNNING));
        prologflags.add(OP_FLAG_SYS_MONITOR_LOGGING, new FlagFactoryTrace(FLAG_SYS_MONITOR_LOGGING));
        return prologflags;
    }

    /**
     * <p>Retrieve the value of this flag.</p>
     *
     * @param en The engine.
     * @return The value.
     */
    public Object getFlag(Engine en) {
        switch (id) {
            case FLAG_SYS_MONITOR_CONFIG:
                return Integer.valueOf(((FoyerTrace) en.store.foyer).getMonitorConfig());
            case FLAG_SYS_MONITOR_RUNNING:
                return Integer.valueOf(((FoyerTrace) en.store.foyer).getMonitorRunning());
            case FLAG_SYS_MONITOR_LOGGING:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() & FoyerTrace.MASK_FOYER_NLOG) == 0);
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
    public boolean setFlag(Object m, Display d, Engine en)
            throws EngineMessage {
        try {
            switch (id) {
                case FLAG_SYS_MONITOR_CONFIG:
                    Number num = SpecialEval.derefAndCastInteger(m, d);
                    int n = SpecialEval.castIntValue(num);
                    ((FoyerTrace) en.store.foyer).setMonitorConfig(n);
                    return true;
                case FLAG_SYS_MONITOR_RUNNING:
                    num = SpecialEval.derefAndCastInteger(m, d);
                    n = SpecialEval.castIntValue(num);
                    ((FoyerTrace) en.store.foyer).setMonitorRunning(n);
                    return true;
                case FLAG_SYS_MONITOR_LOGGING:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.resetBit(FoyerTrace.MASK_FOYER_NLOG);
                    } else {
                        en.store.foyer.setBit(FoyerTrace.MASK_FOYER_NLOG);
                    }
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