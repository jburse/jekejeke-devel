package jekmin.model.builtin;

import jekpro.tools.call.Capability;

/**
 * <p>The minimal logic library capability.</p>
 *
 * @author Copyright 2011-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Minlog 0.1.0 (minimal logic capability)
 */
public final class CapabilityForward extends Capability {

    /**
     * <p>Create a new minimal logic capability.</p>
     */
    public CapabilityForward() {
        super(BranchForward.DEFAULT);
    }

}