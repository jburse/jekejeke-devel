package jekpro.model.pretty;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.Types;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>An object that is used to lookup predicates in the store</p>
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
public class StoreKey implements Comparable<StoreKey> {
    private final String fun;
    private final int arity;

    /**
     * <p>Create a store key from functor and length.</p>
     *
     * @param f The functor.
     * @param a The length.
     */
    public StoreKey(String f, int a) {
        if (f == null)
            throw new NullPointerException("functor missing");
        if (a < 0)
            throw new ArrayIndexOutOfBoundsException("negative length");
        fun = f;
        arity = a;
    }

    /**
     * <p>Check whether this store key equals to the other store key.</p>
     *
     * @param o The other store key.
     * @return True if store keys are equal, otherwise false.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof StoreKey))
            return false;
        StoreKey sk = (StoreKey) o;
        return (fun.equals(sk.fun) && arity == sk.arity);
    }

    /**
     * <p>Compute hash code of this store key.</p>
     *
     * @return The hash code.
     */
    public int hashCode() {
        return fun.hashCode() * 31 + arity;
    }

    /**
     * Compares this object with an object for order.
     *
     * @param o The other object.
     * @return < 0 if less than, 0 if equal, > 0 if greater than.
     */
    public int compareTo(StoreKey o) {
        if (o instanceof StoreKeyQuali)
            return -1;
        int res = fun.compareTo(o.fun);
        if (res != 0) return res;
        return arity - o.arity;
    }

    /**
     * <p>Retrieve the functor.</p>
     *
     * @return The functor.
     */
    public final String getFun() {
        return fun;
    }

    /**
     * <p>Retrieve the length.</p>
     *
     * @return The length.
     */
    public final int getArity() {
        return arity;
    }

    /**
     * <p>Convert property term to a store key. Will throw exception
     * when the compound is not well formed.</p>
     *
     * @param t  The skel of the compound.
     * @param d  The display of the compound.
     * @param en The engine.
     * @return The store key.
     * @throws EngineMessage Shit happens.
     */
    public static StoreKey propToStoreKey(Object t, Display d, Engine en)
            throws EngineMessage {
        int arity = StoreKey.derefAndCastIndicator(t, d, en);
        return new StoreKey(((SkelAtom) en.skel).fun, arity);
    }

    /**
     * <p>Convert a property term to an indicator. Will throw exception
     * when the compound is not well formed.</p>
     * <p>The following syntax is used.</p>
     * <pre>
     *     property --> name "/" integer.
     * </pre>
     * <p>The term is passed in skel and display.</p>
     * <p>The name is returned in skel.</p>
     *
     * @param t  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The length.
     * @throws EngineMessage The indicator is not wellformed.
     */
    public static int derefAndCastIndicator(Object t, Display d, Engine en)
            throws EngineMessage {
        try {
            en.skel = t;
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            if (t instanceof SkelCompound &&
                    ((SkelCompound) t).args.length == 2 &&
                    ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
                SkelCompound sc = (SkelCompound) t;
                Number num = SpecialEval.derefAndCastInteger(sc.args[1], d);
                SpecialEval.checkNotLessThanZero(num);
                int arity = SpecialEval.castIntValue(num);
                en.skel = SpecialUniv.derefAndCastStringWrapped(sc.args[0], d);
                return arity;
            } else {
                EngineMessage.checkInstantiated(t);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_PREDICATE_INDICATOR, t), d);
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

    /**
     * <p>Convert this store key to a compound.</p>
     *
     * @return The compound.
     */
    public Object storeKeyToSkel() {
        return new SkelCompound(new SkelAtom(Foyer.OP_SLASH),
                new SkelAtom(getFun()),
                Integer.valueOf(getArity()));
    }

}
