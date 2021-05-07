package jekdev.reference.debug;

import matula.util.data.AssocSorted;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>This class provides a dump pair.</p>
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
final class DumpReport {
    private int count = 1;
    private long sum;

    /**
     * <p>Create a dump pair.</p>
     *
     * @param s The initial value.
     */
    DumpReport(int s) {
        sum = s;
    }

    /**
     * <p>Add to the dump pair a value.</p>
     *
     * @param s The value to add.
     */
    void add(int s) {
        count++;
        sum += s;
    }

    /**
     * <p>Retrieve the count.</p>
     *
     * @return The count.
     */
    int getCount() {
        return count;
    }

    /**
     * <p>Retrieve the sum.</p>
     *
     * @return The sum.
     */
    long getSum() {
        return sum;
    }

    /**
     * <p>Add to the averager by a key and a value.</p>
     *
     * @param map The map.
     * @param op  The key.
     * @param s   The value.
     */
    static void add(AssocSorted<String, DumpReport> map, String op, int s) {
        int k = map.indexOf(op);
        if (k < 0) {
            map.add(-k - 1, op, new DumpReport(s));
        } else {
            DumpReport val = map.getValue(k);
            val.add(s);
        }
    }

    /**
     * <p>Show the averager.</p>
     *
     * @param map The map.
     * @param wr  The writer.
     * @throws IOException IO Error.
     */
    static void show(AssocSorted<String, DumpReport> map, Writer wr) throws IOException {
        for (int i = 0; i < map.size(); i++) {
            wr.write(map.getKey(i));
            wr.write('\t');
            DumpReport val = map.getValue(i);
            wr.write(Integer.toString(val.getCount()));
            wr.write('\t');
            wr.write(Long.toString(val.getSum() * 100 / val.getCount()));
            wr.write("%\n");
            wr.flush();
        }
    }

    /**
     * <p>Check whether the averager is empty.</p>
     *
     * @param map The map.
     * @return True if the averager is empty, otherwise false.
     */
    static boolean empty(AssocSorted<String, DumpReport> map) {
        return (map.size() == 0);
    }

}