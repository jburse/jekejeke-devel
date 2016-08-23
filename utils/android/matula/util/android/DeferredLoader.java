package matula.util.android;

import matula.util.data.ListArray;

/**
 * Deferred class loader.
 * <p/>
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 * <p/>
 * Rights & License
 * All industrial property rights regarding the information - copyright§
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
final class DeferredLoader {
    private final ListArray<String> paths = new ListArray<String>();
    private final ClassLoader parent;

    /**
     * <p>Create a paths class loader.</p>
     *
     * @param path The path.
     * @param p The parent.
     */
    DeferredLoader(String path, ClassLoader p) {
        paths.add(path);
        parent = p;
    }

    /**
     * <p>Retrieve the paths.</p>
     *
     * @return The paths.
     */
    ListArray<String> getPaths() {
        return paths;
    }

    /**
     * <p>Retrieve the parent.</p>
     *
     * @return The parent.
     */
    ClassLoader getParent() {
        return parent;
    }

    /**
     * <p>Extend the class loader.</p>
     *
     * @param path The path.
     */
    void addPath(String path) {
        paths.add(path);
    }

}

