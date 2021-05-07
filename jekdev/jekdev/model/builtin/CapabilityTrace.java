package jekdev.model.builtin;

import jekdev.reference.system.FlagTrace;
import jekpro.tools.call.Capability;

/**
 * <p>The development environment capability provides the system predicates
 * as defined in the language reference of the development environment. These
 * system predicates enhanced the system predicates of the language reference
 * of the runtime library.
 * </p>
 * <p>This capability is already predefined by a toolkit and it need not be
 * added to the knowledge base after its initialization. This capability
 * does need activation. It will not be useable without activation. This
 * capability provides the following status flags which can be accessed
 * via the methods setStatus() and getStatus() from the Interpreter class:</p>
 * <b>Table 1: Java Constant Prolog Flag Mapping</b>
 * <table>
 * <tr><th>Constant</th><th>Flag</th></tr>
 * <tr><td>STATUS_DEBUG</td><td>debug</td></tr>
 * <tr><td>STATUS_SYS_TRACE_MODE</td><td>sys_trace_mode</td></tr>
 * <tr><td>STATUS_SYS_CLOAK</td><td>sys_cloak</td></tr>
 * </table>
 *
 * @author Copyright 2011-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.8.9 (a fast and small prolog interpreter)
 */
public final class CapabilityTrace extends Capability {
    public final static String PROP_DEBUG = FlagTrace.OP_DEBUG;
    public final static String PROP_SYS_LEASH = FlagTrace.OP_SYS_LEASH;
    public final static String PROP_SYS_CLOAK = FlagTrace.OP_SYS_CLOAK;

    /**
     * <p>Create the capability trace.</p>
     */
    public CapabilityTrace() {
        super(BranchTrace.DEFAULT);
    }

}