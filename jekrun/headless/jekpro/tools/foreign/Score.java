package jekpro.tools.foreign;

import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.array.Types;

/**
 * <p>Provides scoring and test predicates of Java data types.
 * This is used for the sorting of overloaded predicates according
 * to their specificity and generating branching code during
 * the autoloading of a Java class.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
final class Score {

    /***********************************************************/
    /* Delegate Ordering                                       */
    /***********************************************************/

    private final static int MAX_PRIMITIVE = 6;
    private final static int MAX_DEPTH = 256 - MAX_PRIMITIVE;

    /**
     * <p>Calculate the inheritance depth of a class.</p>
     * <p>The result is in the range 1..MAX_DEPTH.</p>
     *
     * @param clazz The class.
     * @return The inheritance depth.
     */
    private static int inheritanceDepth(Class clazz) {
        Class superclazz = clazz.getSuperclass();
        int d = (superclazz != null ? inheritanceDepth(superclazz) : 0);

        Class[] interfaces = clazz.getInterfaces();
        for (int i = 0; i < interfaces.length; i++)
            d = Math.max(d, inheritanceDepth(interfaces[i]));
        if (d < MAX_DEPTH)
            return d + 1;
        throw new IllegalArgumentException("max depth");
    }

    /**
     * <p>Retrieve the specificity score.</p>
     *
     * @param typ   Java type.
     * @param clazz The class.
     * @return The specificity score.
     */
    static int getScore(int typ, Class clazz) {
        switch (typ) {
            case Types.TYPE_PRIMBOOL:
            case Types.TYPE_PRIMBYTE:
                return 0;
            case Types.TYPE_PRIMCHAR:
            case Types.TYPE_PRIMSHORT:
                return 1;
            case Types.TYPE_PRIMINT:
                return 2;
            case Types.TYPE_PRIMLONG:
                return 3;
            case Types.TYPE_PRIMFLOAT:
                return 4;
            case Types.TYPE_PRIMDOUBLE:
                return 5;
            case Types.TYPE_BOOL:
            case Types.TYPE_BYTE:
            case Types.TYPE_CHAR:
            case Types.TYPE_SHORT:
            case Types.TYPE_INTEGER:
            case Types.TYPE_LONG:
            case Types.TYPE_BIG_INTEGER:
            case Types.TYPE_FLOAT:
            case Types.TYPE_DOUBLE:
            case Types.TYPE_BIG_DECIMAL:
            case Types.TYPE_NUMBER:
            case Types.TYPE_STRING:
            case Types.TYPE_CHARSEQ:
            case Types.TYPE_REF:
            case Types.TYPE_OBJECT:
            case Types.TYPE_TERM:
                return MAX_PRIMITIVE + (MAX_DEPTH - inheritanceDepth(clazz));
            case Types.TYPE_INTERPRETER:
            case Types.TYPE_CALLOUT:
                return 0;
            default:
                throw new IllegalArgumentException("illegal type");
        }
    }

    /**
     * <p>Retrieve the test closure.</p>
     *
     * @param typ   The type.
     * @param clazz The class.
     * @return The test closure.
     */
    static Object getTest(int typ, Class clazz) {
        switch (typ) {
            case Types.TYPE_PRIMBOOL:
                return new SkelAtom("boolean");
            case Types.TYPE_PRIMBYTE:
                return new SkelAtom("integer8");
            case Types.TYPE_PRIMCHAR:
                return new SkelAtom("char16");
            case Types.TYPE_PRIMSHORT:
                return new SkelAtom("integer16");
            case Types.TYPE_PRIMINT:
                return new SkelAtom("integer32");
            case Types.TYPE_PRIMLONG:
                return new SkelAtom("integer64");
            case Types.TYPE_PRIMFLOAT:
                return new SkelAtom("integer64_or_float32");
            case Types.TYPE_PRIMDOUBLE:
                return new SkelAtom("integer64_or_float");

            case Types.TYPE_BOOL:
                return new SkelAtom("boolean");
            case Types.TYPE_BYTE:
                return new SkelAtom("integer8");
            case Types.TYPE_CHAR:
                return new SkelAtom("char16");
            case Types.TYPE_SHORT:
                return new SkelAtom("integer16_and_not_integer8");
            case Types.TYPE_INTEGER:
                return new SkelAtom("integer32_and_not_integer16");
            case Types.TYPE_LONG:
                return new SkelAtom("integer64_and_not_integer32");
            case Types.TYPE_BIG_INTEGER:
                return new SkelAtom("integer_and_not_integer64");
            case Types.TYPE_FLOAT:
                return new SkelAtom("float32");
            case Types.TYPE_DOUBLE:
                return new SkelAtom("float64");
            case Types.TYPE_BIG_DECIMAL:
                return new SkelAtom("decimal");
            case Types.TYPE_NUMBER:
                return new SkelAtom("number");

            case Types.TYPE_STRING:
                return new SkelAtom("atom");
            case Types.TYPE_CHARSEQ:
                return new SkelCompound(new SkelAtom("atom_or_instance_of"), clazz);
            case Types.TYPE_REF:
                return new SkelCompound(new SkelAtom("instance_of"), clazz);
            case Types.TYPE_OBJECT:
            case Types.TYPE_TERM:
                return null;
            default:
                throw new IllegalArgumentException("illegal type");
        }
    }

}
