package matula.util.wire;

import matula.util.format.AbstractDom;
import matula.util.format.AbstractWriter;

import java.io.IOException;

/**
 * <p>This class provides a JSON writer.</p>
 * </p>
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
public final class JsonWriter extends AbstractWriter {

    /****************************************************************/
    /* Store Methods                                                */
    /****************************************************************/

    /**
     * <p>Store the childeren.</p>
     *
     * @param nodes The nodes.
     * @throws IOException Shit happens.
     */
    public void storeNodes(AbstractDom[] nodes)
            throws IOException {
        /* t.b.d. */
    }

    /**
     * <p>Store this dom node.</p>
     * <p>Not synchronized, uses cursors.</p>
     *
     * @param node The dom node.
     * @throws IOException Shit happens.
     */
    public void storeNode(AbstractDom node)
            throws IOException {
        /* t.b.d. */
    }

    /****************************************************************/
    /* Text Wrapping                                                */
    /****************************************************************/

    /**
     * <p>Flush the dom writer.</p>
     *
     * @throws IOException Shit happens.
     */
    public void flush()
            throws IOException {
        /* t.b.d. */
    }

    /***************************************************************/
    /* Comment & Indent                                            */
    /***************************************************************/

    /**
     * <p>Write a first line comment.</p>
     *
     * @param comment The line comment.
     * @throws IOException IO error.
     */
    public void writeComment(String comment)
            throws IOException {
        /* t.b.d. */
    }

}