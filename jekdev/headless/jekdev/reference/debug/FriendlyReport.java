package jekdev.reference.debug;

import matula.util.data.AssocSorted;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>This class provides an intermediate code reporter.</p>
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
public final class FriendlyReport {
    AssocSorted<String, Integer> map;

    /**
     * <p>Increment report map by an operator.</p>
     *
     * @param op The operator.
     */
    void increment(String op) {
        int k = map.indexOf(op);
        if (k < 0) {
            map.add(-k - 1, op, Integer.valueOf(1));
        } else {
            Integer val = map.getValue(k);
            map.setValue(k, Integer.valueOf(val.intValue() + 1));
        }
    }

    /**
     * <p>Show a statistics map.</p>
     *
     * @param wr The writer.
     * @throws IOException IO Error.
     */
    void mapShow(Writer wr) throws IOException {
        for (int i = 0; i < map.size(); i++) {
            wr.write(map.getKey(i));
            wr.write('\t');
            wr.write(Integer.toString(map.getValue(i).intValue()));
            wr.write('\n');
            wr.flush();
        }
    }

}