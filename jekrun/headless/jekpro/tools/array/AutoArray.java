package jekpro.tools.array;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.LoadForce;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.SpecialForeign;
import jekpro.tools.call.AbstractAuto;
import jekpro.tools.term.SkelAtom;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

import java.io.Reader;

/**
 * <p>Specialization of the synthetic source class for Java arrays.</p>
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
public final class AutoArray extends AbstractAuto {
    private MapHash<StoreKey, AbstractLense> meths;

    /**
     * <p>Create a source from path.</p>
     *
     * @param p The path.
     */
    public AutoArray(String p) {
        super(p);
    }

    /**************************************************************/
    /* Clear, Load & Check Module                                 */
    /**************************************************************/

    /**
     * <p>Consult a foreign module.</p>
     *
     * @param en  The interpreter.
     * @param rec The recursion flag.
     * @throws EngineMessage   FFI error.
     * @throws EngineException FFI error.
     */
    public void loadModule(Reader lr,
                           Engine en, boolean rec)
            throws EngineMessage, EngineException {
        super.loadModule(lr, en, rec);

        reexportSuperclass(en);
        reexportInterfaces(en);
        usemoduleArray(en);

        meths = new MapHash<StoreKey, AbstractLense>();
        collectArrays(en);

        defineMeths(en, rec);
    }

    /**
     * <p>Reexport the super class of a class.</p>
     *
     * @param en The interpreter.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private void usemoduleArray(Engine en)
            throws EngineException, EngineMessage {
        LoadOpts opts = new LoadOpts();
        opts.setFlags(opts.getFlags() | LoadOpts.MASK_LOAD_COND);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_AUTO);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_MODL);

        String key = "jekpro/frequent/basic/array.p";
        key = AbstractBranch.findPathLibrary(key, en);

        opts.makeLoad(this, key, en);
    }

    /*******************************************************************/
    /* Collect preds & Evaluables                                      */
    /*******************************************************************/

    /**
     * <p>Collect the arrays.</p>
     *
     * @param en The interpreter.
     * @throws EngineMessage FFI error.
     */
    private void collectArrays(Engine en)
            throws EngineMessage {
        if (createArray(getAuto(), en, AbstractFactory.ARRAY_NEW))
            addForeign((AbstractLense) en.skel);
        if (createArray(getAuto(), en, AbstractFactory.ARRAY_LENGTH))
            addForeign((AbstractLense) en.skel);
        if (createArray(getAuto(), en, AbstractFactory.ARRAY_GET_EVAL)) {
            addForeign((AbstractLense) en.skel);
        } else if (createArray(getAuto(), en, AbstractFactory.ARRAY_GET_PRED)) {
            addForeign((AbstractLense) en.skel);
        }
        if (createArray(getAuto(), en, AbstractFactory.ARRAY_SET))
            addForeign((AbstractLense) en.skel);
    }

    /**
     * <p>Add a foreign to the meths.</p>
     *
     * @param del The foreign.
     */
    public void addForeign(AbstractLense del) {
        StoreKey sk = new StoreKey(del.getFun(), del.getArity());
        if (meths.get(sk) != null)
            throw new IllegalArgumentException("indicator clash");
        meths.add(sk, del);
    }

    /*******************************************************************/
    /* Define preds & Evaluables                                  */
    /*******************************************************************/

    /**
     * <p>Define the predicates.</p>
     *
     * @param en  The interpreter.
     * @param rec The recursion flag.
     * @throws EngineMessage   FFI error.
     * @throws EngineException FFI error.
     */
    private void defineMeths(Engine en,
                             boolean rec)
            throws EngineException, EngineMessage {
        for (MapEntry<StoreKey, AbstractLense> entry = meths.getLastEntry();
             entry != null; entry = meths.predecessor(entry)) {
            StoreKey sk = entry.key;
            AbstractLense del = entry.value;
            SkelAtom sa = new SkelAtom(sk.getFun(), this);
            try {
                boolean virt = (del.subflags & AbstractDelegate.MASK_DELE_VIRT) != 0;
                Predicate pick = makePublic(sa, sk.getArity(), virt, en);
                Predicate over = makeOverride(sa, pick, en);
                if (over != null)
                    throw new IllegalArgumentException("indicator clash");
                Predicate.definePredicate(pick, del, en);
                Predicate.checkPredicateDecl(pick, sa, en);
            } catch (EngineException x) {
                if (SpecialLoad.systemConsultBreak(x, en, rec))
                    break;
            } catch (EngineMessage x) {
                EngineException y = new EngineException(x, EngineException.fetchStack(en));
                if (SpecialLoad.systemConsultBreak(y, en, rec))
                    break;
            }
        }
    }

    /*******************************************************************/
    /* Create Foreigns                                                 */
    /*******************************************************************/

    /**
     * <p>Create an array delegate.</p>
     * <p>Result or culprit is returned in the engine skel.</p>
     *
     * @param c  The class.
     * @param en The engine.
     * @param k  The desired delegate.
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public static boolean createArray(Class c, Engine en, int k) {
        if (!c.isArray()) {
            en.skel = EngineMessage.domainError(
                    AbstractFactory.OP_DOMAIN_FOREIGN_ARRAY,
                    SpecialForeign.classToName(c));
            return false;
        }
        AbstractLense del;
        switch (k) {
            case AbstractFactory.ARRAY_LENGTH:
                del = new LenseLength(c);
                if (!del.encodeSignatureEval(en))
                    return false;
                break;
            case AbstractFactory.ARRAY_GET_EVAL:
                del = new LenseMember(c);
                if (!del.encodeSignatureEval(en))
                    return false;
                break;
            case AbstractFactory.ARRAY_NEW:
                del = new LenseDimension(c);
                if (!del.encodeSignaturePred(en))
                    return false;
                break;
            case AbstractFactory.ARRAY_GET_PRED:
                del = new LenseElement(c);
                if (!del.encodeSignaturePred(en))
                    return false;
                break;
            case AbstractFactory.ARRAY_SET:
                del = new LenseUpdate(c);
                if (!del.encodeSignaturePred(en))
                    return false;
                break;
            default:
                throw new IllegalArgumentException("illegal delegate");
        }
        en.skel = del;
        return true;
    }

}
