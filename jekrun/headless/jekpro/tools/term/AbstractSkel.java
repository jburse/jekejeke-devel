package jekpro.tools.term;

import jekpro.frequent.standard.EngineCopy;
import jekpro.frequent.standard.SpecialSort;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.PrologWriter;
import jekpro.reference.arithmetic.SpecialCompare;
import jekpro.reference.structure.SpecialLexical;

import java.io.StringWriter;

/**
 * <p>This is the base class for skeletons, except for numbers and references.</p>
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
public abstract class AbstractSkel {
    public final static Object VOID_OBJ = new Object();

    /**
     * <p>Return a string of a skeleton.</p>
     *
     * @return The string.
     */
    public String toString() {
        try {
            StringWriter sw = new StringWriter();
            PrologWriter.toString(this, Display.DISPLAY_CONST, sw, 0, null);
            return sw.toString();
        } catch (EngineMessage x) {
            throw new RuntimeException("shouldn't happen", x);
        } catch (EngineException x) {
            throw new RuntimeException("shouldn't happen", x);
        }
    }

    /**
     * <p>Copy a term into a new skeleton.</p>
     *
     * @param m  The term skeletion.
     * @param d  The term display.
     * @param en The engine.
     * @return The new skeleton.
     */
    public static Object copySkel(Object m, Display d, Engine en) {
        EngineCopy ec = en.enginecopy;
        if (ec == null) {
            ec = new EngineCopy();
            en.enginecopy = ec;
        }
        ec.vars = null;
        Object val = ec.copyTerm(m, d);
        ec.vars = null;
        return val;
    }

    /**
     * <p>Create a new display.</p>
     *
     * @param val The skeleton.
     * @return The display.
     */
    public static Display newDisplay(Object val) {
        int size = EngineCopy.displaySize(val);
        Display ref = (size != 0 ? new Display(Display.newLexical(size)) :
                Display.DISPLAY_CONST);
        if (size != 0)
            ref.flags |= Display.MASK_DPTM_MLTI;
        return ref;
    }

    /**********************************************************/
    /* Skel Comparison                                        */
    /**********************************************************/

    /**
     * <p>Compare two skeletons lexically.</p>
     * <p>Teil recursive solution.</p>
     * <p>Throws a runtime exception for uncomparable references.</p>
     *
     * @param alfa The skeleton of the first term.
     * @param beta The skeleton of the second term.
     * @return <0 alfa < beta, 0 alfa = beta, >0 alfa > beta
     * @see SpecialLexical#compareTerm
     */
    public static int compareSkel(Object alfa, Object beta)
            throws ArithmeticException {
        for (; ; ) {
            int i = SpecialLexical.cmpType(alfa);
            int k = i - SpecialLexical.cmpType(beta);
            if (k != 0) return k;
            switch (i) {
                case SpecialLexical.CMP_TYPE_VAR:
                    i = ((SkelVar) alfa).id;
                    k = ((SkelVar) beta).id;
                    return i - k;
                case SpecialLexical.CMP_TYPE_DECIMAL:
                    return SpecialLexical.compareDecimalLexical(alfa, beta);
                case SpecialLexical.CMP_TYPE_FLOAT:
                    return SpecialLexical.compareFloatLexical(alfa, beta);
                case SpecialLexical.CMP_TYPE_INTEGER:
                    return SpecialCompare.compareIntegerArithmetical(alfa, beta);
                case SpecialLexical.CMP_TYPE_REF:
                    if (alfa instanceof Comparable)
                        return ((Comparable) alfa).compareTo(beta);
                    throw new ArithmeticException(EngineMessage.OP_EVALUATION_ORDERED);
                case SpecialLexical.CMP_TYPE_ATOM:
                    return ((SkelAtom) alfa).compareTo(((SkelAtom) beta));
                case SpecialLexical.CMP_TYPE_COMPOUND:
                    Object[] t1 = ((SkelCompound) alfa).args;
                    Object[] t2 = ((SkelCompound) beta).args;
                    k = t1.length - t2.length;
                    if (k != 0) return k;
                    k = ((SkelCompound) alfa).sym.compareTo(((SkelCompound) beta).sym);
                    if (k != 0) return k;
                    i = 0;
                    for (; i < t1.length - 1; i++) {
                        k = compareSkel(t1[i], t2[i]);
                        if (k != 0) return k;
                    }
                    alfa = t1[i];
                    beta = t2[i];
                    break;
                default:
                    throw new IllegalArgumentException("unknown type");
            }
        }
    }

    /********************************************************************/
    /* Skel Hash                                                        */
    /********************************************************************/

    /**
     * <p>Compute the hash code.</p>
     * <p>Teil recursive solution.</p>
     *
     * @param t   The term.
     * @param res The preceding hash.
     * @return The hash value.
     * @see SpecialSort#hashTerm
     */
    public static int hashSkel(Object t, int res) {
        for (; ; ) {
            if (!(t instanceof SkelCompound))
                return res * 31 + t.hashCode();
            Object[] tc = ((SkelCompound) t).args;
            res = res * 31 + ((SkelCompound) t).sym.hashCode();
            int i = 0;
            for (; i < tc.length - 1; i++)
                res = hashSkel(tc[i], res);
            t = tc[i];
        }
    }

    /**********************************************************/
    /* Skel Equality                                          */
    /**********************************************************/

    /**
     * <p>Check two terms for lexical equivalence.</p>
     * <p>Teil recursive solution.</p>
     *
     * @param alfa The first term.
     * @param beta The second term.
     * @return True if they are lexically equal, otherwise false.
     * @see SpecialLexical#equalTerm
     */
    public static boolean equalSkel(Object alfa, Object beta) {
        for (; ; ) {
            boolean k = (alfa instanceof SkelCompound);
            if (k != (beta instanceof SkelCompound))
                return false;
            if (!k)
                return alfa.equals(beta);
            Object[] t1 = ((SkelCompound) alfa).args;
            Object[] t2 = ((SkelCompound) beta).args;
            if (t1.length != t2.length)
                return false;
            if (!((SkelCompound) alfa).sym.equals(((SkelCompound) beta).sym))
                return false;
            int i = 0;
            for (; i < t1.length - 1; i++) {
                if (!equalSkel(t1[i], t2[i]))
                    return false;
            }
            alfa = t1[i];
            beta = t2[i];
        }
    }

}
