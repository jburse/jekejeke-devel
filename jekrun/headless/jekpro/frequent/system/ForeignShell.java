package jekpro.frequent.system;

import jekpro.tools.call.Interpreter;
import jekpro.tools.term.Lobby;
import jekpro.tools.term.TermCompound;

import java.util.Iterator;

/**
 * <p>The foreign predicates for the module system/shell.</p>
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
public final class ForeignShell {

    /*****************************************************************/
    /* Environment Variables                                         */
    /*****************************************************************/

    /**
     * <p>List the environment variable names.</p>
     *
     * @return The environment variable names.
     */
    public static Object sysListEnv(Interpreter inter) {
        Lobby lobby = inter.getKnowledgebase().getLobby();
        Iterator<String> iter = System.getenv().keySet().iterator();
        Object res = lobby.ATOM_NIL;
        while (iter.hasNext()) {
            res = new TermCompound(lobby.ATOM_CONS,
                    iter.next(), res);
        }
        return res;
    }

    /**
     * <p>Some testing.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) {
        Properties prop = System.getProperties();
        Enumeration<Object> keys = prop.keys();
        while (keys.hasMoreElements()) {
            Object key = keys.nextElement();
            Object value = prop.get(key);
            System.out.println(key + "\t" + value);
        }
    }
    */

}
