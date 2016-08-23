package matula.util.data;

/**
 * <p>This class provides a hash scrambler. Currently we have only
 * implemented the murmur algorithm.</p>
 * <p>The murmur algorithm has good behaviour for float32 and float64,
 * and as well for some critical Prolog atoms such as '.' and '[]'.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
final class HashScrambler {

    /**
     * <p>The last phase of the murmur algorithm.</p>
     * <p>See also: https://sites.google.com/site/murmurhash/</p>
     *
     * @param x The hash code to scramble.
     * @return The scrambled hash code.
     */
    static int murmur(int x) {
        x ^= x >> 13;
        x *= 0x5bd1e995;
        return x ^ (x >> 15);
    }

    /*
    public static void main(String[] args) {
        float[] tabf = {1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f};
        for (int i = 0; i < tabf.length; i++) {
            float f = tabf[i];
            int h = Float.hashCode(f);
            h = murmur(h);
            System.out.println("f=" + f + ", murmur(hash(f))=" + h +
                    ", murmur(hash(f))%4=" + (h & 3));
        }
        double[] tabd = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0};
        for (int i = 0; i < tabd.length; i++) {
            double d = tabd[i];
            int h = Double.hashCode(d);
            h = murmur(h);
            System.out.println("d=" + d + ", murmur(hash(d))=" + h +
                    ", murmur(hash(d))%4=" + (h & 3));
        }
        String[] tabs = {".", "[]"};
        for (int i = 0; i < tabs.length; i++) {
            String s = tabs[i];
            int h = s.hashCode();
            h = murmur(h);
            System.out.println("s=" + s + ", murmur(hash(s))=" + h +
                    ", murmur(hash(s))%4=" + (h & 3));
        }
    }
    */

}
