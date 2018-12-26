package jekpro.tools.array;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.reference.reflect.SpecialForeign;
import jekpro.tools.term.SkelAtom;

import java.lang.reflect.Modifier;

/**
 * <p>Base class for the Java array class delegates.</p>
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
public abstract class AbstractLense extends AbstractDelegate {
    public final static int MASK_METH_FUNC = 0x00000100;

    final static int[] VOID_PARAS = new int[0];

    public int encodeobj;
    public int[] encodeparas;
    public int encoderet;

    /**
     * <p>Encode the signature of a foreign method.</p>
     *
     * @param en The engine.
     * @return True if the signature is ok, otherwise false.
     */
    public boolean encodeSignatureEval(Engine en) {
        if (!Modifier.isPublic(getModifiers())) {
            en.skel = EngineMessage.domainError(
                    AbstractFactory.OP_DOMAIN_FOREIGN_VISIBILITY,
                    new SkelAtom(Modifier.toString(getModifiers())));
            return false;
        }

        if (!Modifier.isStatic(getModifiers())) {
            subflags |= AbstractDelegate.MASK_DELE_VIRT;
            Class ret = getDeclaringClass();
            Integer encode = Types.typeeval.get(ret);
            if (encode == null ||
                    encode.intValue() == Types.TYPE_INTERPRETER) {
                en.skel = EngineMessage.domainError(
                        AbstractFactory.OP_DOMAIN_FOREIGN_RECEIVER,
                        SpecialForeign.classToName(ret));
                return false;
            } else {
                encodeobj = encode.intValue();
            }
        }

        Class ret = getReturnType();
        Integer encode = Types.typeeval.get(ret);
        if (encode == null ||
                encode.intValue() == Types.TYPE_INTERPRETER) {
            en.skel = EngineMessage.domainError(
                    AbstractFactory.OP_DOMAIN_FOREIGN_RETURN,
                    SpecialForeign.classToName(ret));
            return false;
        } else {
            encoderet = encode.intValue();
        }

        Class[] paras = getParameterTypes();
        encodeparas = (paras.length != 0 ? new int[paras.length] :
                VOID_PARAS);
        for (int i = 0; i < paras.length; i++) {
            ret = paras[i];
            encode = Types.typeeval.get(ret);
            if (encode == null) {
                en.skel = EngineMessage.domainError(
                        AbstractFactory.OP_DOMAIN_FOREIGN_PARAMETER,
                        SpecialForeign.classToName(ret));
                return false;
            } else {
                encodeparas[i] = encode.intValue();
            }
        }

        return true;
    }

    /**
     * <p>Encode the signature of a foreign method.</p>
     * <p>The culprit is returned in the engine skel.</p>
     *
     * @param en The engine.
     * @return True if the signature is ok, otherwise false.
     */
    public boolean encodeSignaturePred(Engine en) {
        if (!Modifier.isPublic(getModifiers())) {
            en.skel = EngineMessage.domainError(
                    AbstractFactory.OP_DOMAIN_FOREIGN_VISIBILITY,
                    new SkelAtom(Modifier.toString(getModifiers())));
            return false;
        }

        if (!Modifier.isStatic(getModifiers())) {
            subflags |= AbstractDelegate.MASK_DELE_VIRT;
            Class ret = getDeclaringClass();
            Integer encode = Types.typepred.get(ret);
            if (encode == null) {
                encodeobj = Types.TYPE_REF;
            } else if (encode.intValue() == Types.TYPE_INTERPRETER ||
                    encode.intValue() == Types.TYPE_CALLOUT) {
                en.skel = EngineMessage.domainError(
                        AbstractFactory.OP_DOMAIN_FOREIGN_RECEIVER,
                        SpecialForeign.classToName(ret));
                return false;
            } else {
                encodeobj = encode.intValue();
            }
        }

        Class ret = getReturnType();
        Integer encode = Types.typepred.get(ret);
        if (encode == null) {
            encoderet = Types.TYPE_REF;
        } else if (encode.intValue() == Types.TYPE_INTERPRETER ||
                encode.intValue() == Types.TYPE_CALLOUT) {
            en.skel = EngineMessage.domainError(
                    AbstractFactory.OP_DOMAIN_FOREIGN_RETURN,
                    SpecialForeign.classToName(ret));
            return false;
        } else {
            encoderet = encode.intValue();
        }

        Class[] paras = getParameterTypes();
        encodeparas = (paras.length != 0 ? new int[paras.length] : VOID_PARAS);
        for (int i = 0; i < paras.length; i++) {
            ret = paras[i];
            encode = Types.typepred.get(ret);
            if (encode == null) {
                encodeparas[i] = Types.TYPE_REF;
            } else if (encode.intValue() == Types.TYPE_VOID) {
                en.skel = EngineMessage.domainError(
                        AbstractFactory.OP_DOMAIN_FOREIGN_PARAMETER,
                        SpecialForeign.classToName(ret));
                return false;
            } else {
                encodeparas[i] = encode.intValue();
            }
        }

        if (Types.getRetFlag(encoderet))
            subflags |= MASK_METH_FUNC;

        return true;
    }

    /******************************************************************/
    /* Life-Cycle Services                                            */
    /******************************************************************/

    /**
     * <p>Shrink this predicate from the store for a source.</p>
     *
     * @param pick  The predicate.
     * @param scope The source.
     */
    public final void shrinkPredicate(Predicate pick, AbstractSource scope) {
        /* do noting */
    }

    /**
     * <p>Release this predicate from the store.</p>
     *
     * @param pick The predicate.
     */
    public final void releasePredicate(Predicate pick) {
        /* do noting */
    }

    /******************************************************************/
    /* Accessible Protocol                                            */
    /******************************************************************/

    /**
     * <p>Retrieve the modifier flags as an int.</p>
     *
     * @return The modifier flags as an int.
     */
    public abstract int getModifiers();

    /**
     * <p>Retrieve the return type as Java class.</p>
     *
     * @return The return type as Java class.
     */
    public abstract Class getReturnType();

    /**
     * <p>Retrieve the parameter types as Java classes.</p>
     *
     * @return The parameter types as Java classes.
     */
    public abstract Class[] getParameterTypes();

    /**
     * <p>Retrieve the declaring Java class.</p>
     *
     * @return The declaring Java class.
     */
    public abstract Class getDeclaringClass();

    /***************************************************************/
    /* Foreign Predicates                                          */
    /***************************************************************/

    /**
     * <p>Retrieve an length guess.</p>
     *
     * @return The length guess, or -1.
     */
    public int getArity() {
        int count = getParaCount();
        if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0)
            count++;
        if ((subflags & MASK_METH_FUNC) != 0)
            count++;
        return count;
    }

    /**
     * <p>Compute the length.</p>
     *
     * @return The length.
     */
    private int getParaCount() {
        int k = 0;
        for (int i = 0; i < encodeparas.length; i++) {
            switch (encodeparas[i]) {
                case Types.TYPE_STRING:
                case Types.TYPE_CHARSEQ:
                case Types.TYPE_PRIMBOOL:
                case Types.TYPE_BOOL:
                case Types.TYPE_PRIMBYTE:
                case Types.TYPE_BYTE:
                case Types.TYPE_PRIMCHAR:
                case Types.TYPE_CHAR:
                case Types.TYPE_PRIMSHORT:
                case Types.TYPE_SHORT:
                case Types.TYPE_PRIMINT:
                case Types.TYPE_INTEGER:
                case Types.TYPE_PRIMLONG:
                case Types.TYPE_LONG:
                case Types.TYPE_BIG_INTEGER:
                case Types.TYPE_PRIMFLOAT:
                case Types.TYPE_FLOAT:
                case Types.TYPE_PRIMDOUBLE:
                case Types.TYPE_DOUBLE:
                case Types.TYPE_BIG_DECIMAL:
                case Types.TYPE_NUMBER:
                case Types.TYPE_REF:
                case Types.TYPE_OBJECT:
                case Types.TYPE_TERM:
                case Types.TYPE_ATOMIC:
                    k++;
                    break;
                case Types.TYPE_INTERPRETER:
                case Types.TYPE_CALLOUT:
                    /* */
                    break;
                default:
                    throw new IllegalArgumentException("illegal parameter type");
            }
        }
        return k;
    }

}
