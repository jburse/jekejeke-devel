package jekmin.model.builtin;

import matula.util.config.AbstractDescription;

/**
 * <p>Implementation of an abstract description.</p>
 *
 * @author Copyright 2011-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Minlog 0.1.0 (minimal logic capability)
 */
public final class DescriptionForward extends AbstractDescription {
    public final static DescriptionForward DEFAULT = new DescriptionForward();

    /**
     * <p>Create the branch.</p>
     */
    private DescriptionForward() {
        setMainRoot("jekmin/");
    }

}