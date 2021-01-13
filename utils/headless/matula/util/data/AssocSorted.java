package matula.util.data;

import matula.util.regex.IgnoreCase;
import qa.norm.anon.bean.Sort;

import java.util.Comparator;

/**
 * <p>Refinement of the assoc array data type which supports inter-
 * section and union. Basically the ordmap datatype from Prolog
 * implemented in Java, except that we can do binary search.
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
public final class AssocSorted<K, V> extends AssocArray<K, V> {
    public static final AssocSorted<String, AssocSorted> FULL_SET
            = new AssocSorted<>(IgnoreCase.DEFAULT_TERTIARY);

    private final Comparator<K> comparator;

    /**
     * <p>Create a sorted asssoc.</p>
     *
     * @param c The comparator.
     */
    public AssocSorted(Comparator<K> c) {
        comparator = c;
    }

    /************************************************************/
    /* Binary Search                                            */
    /************************************************************/

    /**
     * <p>Returns the index of the first occurence.</p>
     *
     * @param o The object.
     * @return The index, or -1.
     */
    public int indexOf(Object o) {
        int low = 0;
        int high = size;
        while (low + 1 < high) {
            int mid = (low + high) / 2;
            int k = comparator.compare((K) o, getKey(mid));
            if (k < 0) {
                high = mid;
            } else if (k == 0) {
                return mid;
            } else {
                low = mid;
            }
        }
        int k = (low < size ? comparator.compare((K) o, getKey(low)) : -1);
        if (k < 0) {
            return -low - 1;
        } else if (k == 0) {
            return low;
        } else {
            return -(low + 1) - 1;
        }
    }

    /************************************************************/
    /* Merge Operations                                         */
    /************************************************************/

    /**
     * <p>Intersect this sorted assoc with another sorted assoc.</p>
     *
     * @param b The other sorted assoc.
     * @return The result sorted assoc.
     */
    public AssocSorted<K, V> intersect(AssocSorted<K, V> b) {
        AssocSorted<K, V> res = new AssocSorted<>(comparator);
        int i = 0;
        int j = 0;
        while (i < size && j < b.size) {
            int k = comparator.compare(getKey(i), b.getKey(j));
            if (k < 0) {
                i++;
            } else if (k == 0) {
                AssocSorted value = AssocSorted.intersect((AssocSorted) getValue(i), (AssocSorted) b.getValue(j));
                if (value == FULL_SET || value.size != 0)
                    res.add(getKey(i), (V) value);
                i++;
                j++;
            } else {
                j++;
            }
        }
        return res;
    }

    /**
     * <p>Compute intersection where null means the full domain.</p>
     *
     * @param a The first sorted array, can be null.
     * @param b The second sorted array, can be null.
     * @return The result, can be null.
     */
    public static AssocSorted<String, AssocSorted> intersect(
            AssocSorted<String, AssocSorted> a,
            AssocSorted<String, AssocSorted> b) {
        if (a == FULL_SET) {
            return b;
        } else if (b == FULL_SET) {
            return a;
        } else {
            return a.intersect(b);
        }
    }

    /**
     * <p>Union this sorted assoc with another sorted assoc.</p>
     *
     * @param b The other sorted list.
     * @return The result sorted list.
     */
    public AssocSorted<K, V> union(AssocSorted<K, V> b) {
        AssocSorted<K, V> res = new AssocSorted<>(comparator);
        int i = 0;
        int j = 0;
        while (i < size && j < b.size) {
            int k = comparator.compare(getKey(i), b.getKey(j));
            if (k < 0) {
                res.add(getKey(i), getValue(i));
                i++;
            } else if (k == 0) {
                AssocSorted value = AssocSorted.union((AssocSorted) getValue(i), (AssocSorted) b.getValue(j));
                res.add(getKey(i), (V) value);
                i++;
                j++;
            } else {
                res.add(b.getKey(j), b.getValue(j));
                j++;
            }
        }
        while (i < size) {
            res.add(getKey(i), getValue(i));
            i++;
        }
        while (j < b.size) {
            res.add(b.getKey(j), b.getValue(j));
            j++;
        }
        return res;
    }

    /**
     * <p>Compute union where null means the full domain.</p>
     *
     * @param a The first sorted array, can be null.
     * @param b The second sorted array, can be null.
     * @return The result, can be null.
     */
    public static AssocSorted<String, AssocSorted> union(
            AssocSorted<String, AssocSorted> a,
            AssocSorted<String, AssocSorted> b) {
        if (a == FULL_SET) {
            return FULL_SET;
        } else if (b == FULL_SET) {
            return FULL_SET;
        } else {
            return a.union(b);
        }
    }

}