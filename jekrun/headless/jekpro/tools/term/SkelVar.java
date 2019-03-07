package jekpro.tools.term;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.BindCount;
import jekpro.model.molec.BindSerno;
import jekpro.model.molec.Display;
import matula.util.wire.AbstractLivestock;

/**
 * <p>This class provides variable skeletons.</p>
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
public final class SkelVar extends AbstractSkel
        implements Comparable<SkelVar> {
    private static final int CACHE_SIZE = 8;
    private static final SkelVar[] cachevar = new SkelVar[CACHE_SIZE];
    private static final SkelVar[][] cachearray = new SkelVar[CACHE_SIZE][];

    /* initialize the caches */
    static {
        for (int i = 0; i < CACHE_SIZE; i++)
            cachevar[i] = new SkelVar(i);
        for (int i = 0; i < CACHE_SIZE; i++) {
            SkelVar[] temp = new SkelVar[i];
            if (i > 0)
                System.arraycopy(cachevar, 0, temp, 0, i);
            cachearray[i] = temp;
        }
    }

    public int id;

    /**
     * <p>Create a skel var.</p>
     *
     * @param i The index into the display.
     */
    public SkelVar(int i) {
        id = i;
    }

    /**
     * <p>Create a skel var, possibly cached.</p>
     *
     * @param i The index into the display.
     * @return The skel var.
     */
    public static SkelVar valueOf(int i) {
        if (i < CACHE_SIZE)
            return cachevar[i];
        return new SkelVar(i);
    }

    /**
     * <p>Create a skel var array, possibly cached.</p>
     *
     * @param i The length of the var array.
     * @return The skel var array.
     */
    public static SkelVar[] valueOfArray(int i) {
        if (i < CACHE_SIZE)
            return cachearray[i];
        SkelVar[] temp = new SkelVar[i];
        for (int j = 0; j < i; j++)
            temp[j] = SkelVar.valueOf(j);
        return temp;
    }

    /**
     * <p>Return the hash code of the id.</p>
     *
     * @return The hash code.
     */
    public int hashCode() {
        return id;
    }

    /**
     * <p>Compare this skel var to another skel var.</p>
     *
     * @param o The other skel var.
     * @return The comparison result.
     */
    public int compareTo(SkelVar o) {
        return id - o.id;
    }

    /**
     * <p>The hash code of a vaiable inside a display.</p>
     *
     * @param d The display.
     * @return The hash code.
     */
    public int hashCode(Display d) {
        return d.hashCode() * 31 + hashCode();
    }

    /**
     * <p>Retrieve the serial number of a variable.</p>
     *
     * @param ref The display.
     * @param en  The engine, or null.
     * @return The serial number.
     */
    public int getValue(Display ref, Engine en) {
        if (en != null) {
            BindCount bc = ref.bind[id];
            int i = bc.serno;
            if (i == -1)
                i = BindSerno.bindSerno(bc, en);
            return i;
        } else {
            return id;
        }
    }

    /**
     * <p>Retrieve the serial number of a variable.</p>
     *
     * @param ref The display.
     * @return The serial number.
     */
    public int getValue(Display ref) {
        BindCount bc = ref.bind[id];
        int i = bc.serno;
        if (i == -1) {
            Thread thread = Thread.currentThread();
            Supervisor visor = (Supervisor) AbstractLivestock.currentLivestock(thread);
            i = BindSerno.bindSerno(bc, visor.inuse);
        }
        return i;
    }

    /**
     * <p>Convert a serno to a string.</p>
     *
     * @param k     The serno.
     * @param under The underscore flag.
     * @return The string.
     */
    public static String sernoToString(int k, boolean under) {
        StringBuilder buf = new StringBuilder();
        if (under)
            buf.appendCodePoint('_');
        buf.appendCodePoint(k % 26 + 'A');
        if (k >= 26)
            buf.append(k / 26);
        return buf.toString();
    }

}
