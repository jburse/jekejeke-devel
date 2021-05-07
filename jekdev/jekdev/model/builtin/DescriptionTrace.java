package jekdev.model.builtin;

import matula.util.config.AbstractDescription;

/**
 * <p>Implementation of an abstract description.</p>
 *
 * @author Copyright 2010-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.2 (a fast and small prolog interpreter)
 */
public final class DescriptionTrace extends AbstractDescription {
    public final static DescriptionTrace DEFAULT = new DescriptionTrace();

    /**
     * <p>Create the branch.</p>
     */
    private DescriptionTrace() {
        setMainRoot("jekdev/");
    }

}