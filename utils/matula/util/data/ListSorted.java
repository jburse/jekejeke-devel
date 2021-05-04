package matula.util.data;

import java.util.Comparator;

/**
 * <p>Refinement of the list array data type which supports inter-
 * section and union. Basically the ordset datatype from Prolog
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
public final class ListSorted<E> extends ListArray<E> {
    private final Comparator<E> comparator;

    /**
     * <p>Create a sorted list.</p>
     *
     * @param c The comparator.
     */
    public ListSorted(Comparator<E> c) {
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
            int k = comparator.compare((E) o, get(mid));
            if (k < 0) {
                high = mid;
            } else if (k == 0) {
                return mid;
            } else {
                low = mid;
            }
        }
        int k = (low < size ? comparator.compare((E) o, get(low)) : -1);
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
     * <p>Intersect this sorted list with another sorted list.</p>
     *
     * @param b The other sorted list.
     * @return The result sorted list.
     */
    public ListSorted<E> intersect(ListSorted<E> b) {
        ListSorted<E> res = new ListSorted<>(comparator);
        int i = 0;
        int j = 0;
        while (i < size && j < b.size) {
            int k = comparator.compare(get(i), b.get(j));
            if (k < 0) {
                i++;
            } else if (k == 0) {
                res.add(get(i));
                i++;
                j++;
            } else {
                j++;
            }
        }
        return res;
    }

    /**
     * <p>Union this sorted list with another sorted list.</p>
     *
     * @param b The other sorted list.
     * @return The result sorted list.
     */
    public ListSorted<E> union(ListSorted<E> b) {
        ListSorted<E> res = new ListSorted<>(comparator);
        int i = 0;
        int j = 0;
        while (i < size && j < b.size) {
            int k = comparator.compare(get(i), b.get(j));
            if (k < 0) {
                res.add(get(i));
                i++;
            } else if (k == 0) {
                res.add(get(i));
                i++;
                j++;
            } else {
                res.add(b.get(j));
                j++;
            }
        }
        while (i < size) {
            res.add(get(i));
            i++;
        }
        while (j < b.size) {
            res.add(b.get(j));
            j++;
        }
        return res;
    }

}
