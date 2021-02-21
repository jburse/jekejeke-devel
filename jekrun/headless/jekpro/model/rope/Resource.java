package jekpro.model.rope;

import jekpro.tools.term.PositionKey;

/**
 * <p>A resource entry.</p>
 *
 * @author Copyright 2015-2018, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.9 (a fast and small prolog interpreter)
 */
public class Resource {
    private final String key;

    /**
     * <p>Create a reesource entry.</p>
     *
     * @param k The source key.
     */
    public Resource(String k) {
        key = k;
    }

    /**
     * <p>Retrieve the resource.</p>
     *
     * @return The source key.
     */
    public String getKey() {
        return key;
    }

    /**
     * <p>Retrieve the postion.</p>
     *
     * @return The postion, can be null.
     */
    public PositionKey getPosition() {
        return null;
    }

    /**
     * <p>Set the postion.</p>
     *
     * @param o The postion, can be null.
     */
    public void setPosition(PositionKey o) {
        /* */
    }

}
