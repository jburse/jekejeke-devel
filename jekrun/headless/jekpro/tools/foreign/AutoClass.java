package jekpro.tools.foreign;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.PrologReader;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Clause;
import jekpro.model.rope.LoadForce;
import jekpro.model.rope.LoadOpts;
import jekpro.model.rope.PreClause;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.SpecialForeign;
import jekpro.reference.runtime.EvaluableLogic;
import jekpro.reference.runtime.SpecialDynamic;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.Types;
import jekpro.tools.call.AbstractAuto;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.proxy.AbstractReflection;
import jekpro.tools.proxy.RuntimeWrap;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

import java.io.Reader;
import java.lang.reflect.*;
import java.util.Arrays;

/**
 * <p>Specialization of the synthetic source class for Java classes.</p>
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
public final class AutoClass extends AbstractAuto {
    private static final int SYNTHETIC = 0x00001000;

    private static final String OP_VARIANT = "_var";

    private MapHash<StoreKey, ListArray<AbstractMember>> meths;

    /**
     * <p>Create a source from path.</p>
     *
     * @param p The path.
     */
    public AutoClass(String p) {
        super(p);
    }

    /**
     * <p>Retrieve the preds.</p>
     *
     * @return The preds.
     */
    public MapHash<StoreKey, ListArray<AbstractMember>> getMeths() {
        return meths;
    }

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

        AutoClass superjava = reexportSuperclass(en);
        AutoClass[] interfacesjava = reexportInterfaces(en);
        usemoduleScore(en);

        meths = new MapHash<>();
        collectConstructors(en);
        collectMethods(en);
        collectFields(en);

        if (superjava != null)
            inheritMeths(superjava);
        for (int i = 0; i < interfacesjava.length; i++)
            inheritMeths(interfacesjava[i]);

        defineMeths(en, rec);
    }

    /**
     * <p>Reexport the super class of a class.</p>
     *
     * @param en The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private void usemoduleScore(Engine en)
            throws EngineException, EngineMessage {
        LoadOpts opts = new LoadOpts();
        opts.setFlags(opts.getFlags() | LoadOpts.MASK_LOAD_COND);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_AUTO);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_MODL);

        String key = "jekpro/frequent/basic/score.p";
        key = AbstractBranch.findPathLibrary(key, en);

        opts.makeLoad(this, key, en);
    }

    /*******************************************************************/
    /* Collect preds & Evaluables                                      */
    /*******************************************************************/

    /**
     * <p>Collect the constructors.</p>
     *
     * @param en The engine.
     */
    private void collectConstructors(Engine en) {
        Constructor[] constructors = getAuto().getDeclaredConstructors();
        for (int i = 0; i < constructors.length; i++) {
            Constructor constructor = constructors[i];
            if (!Modifier.isPublic(constructor.getModifiers()))
                continue;
            if ((constructor.getModifiers() & SYNTHETIC) != 0)
                continue;
            if (createConstructor(constructor, en))
                addForeignScore((AbstractMember) en.skel);
        }
    }

    /**
     * <p>Collect the methods.</p>
     *
     * @param en The engine.
     */
    private void collectMethods(Engine en) {
        Method[] methods = getAuto().getDeclaredMethods();
        for (int i = 0; i < methods.length; i++) {
            Method method = methods[i];
            if (!Modifier.isPublic(method.getModifiers()))
                continue;
            if ((method.getModifiers() & SYNTHETIC) != 0)
                continue;
            if (Modifier.isStatic(method.getModifiers())) {
                if (createMethod(method, en, AbstractReflection.INVOKE_VIRTUAL))
                    addForeignScore((AbstractMember) en.skel);
            } else {
                if (createMethod(method, en, AbstractReflection.INVOKE_SPECIAL))
                    addForeignScore((AbstractMember) en.skel);
            }
        }
    }

    /**
     * <p>Collect the fields.</p>
     *
     * @param en The engine.
     */
    private void collectFields(Engine en) {
        Field[] fields = getAuto().getDeclaredFields();
        for (int i = 0; i < fields.length; i++) {
            Field field = fields[i];
            if (!Modifier.isPublic(field.getModifiers()))
                continue;
            if ((field.getModifiers() & SYNTHETIC) != 0)
                continue;
            if (createField(field, en, AbstractReflection.FIELD_GET))
                addForeignScore((AbstractMember) en.skel);
            if (Modifier.isFinal(field.getModifiers()))
                continue;
            if (createField(field, en, AbstractReflection.FIELD_SET))
                addForeignScore((AbstractMember) en.skel);
        }
    }

    /**
     * <p>Add a foreign to the meths.</p>
     *
     * @param del The foreign.
     */
    public void addForeignScore(AbstractMember del) {
        StoreKey sk = new StoreKey(del.getFun(), del.getArity());
        ListArray<AbstractMember> dels = meths.get(sk);
        if (dels == null) {
            dels = new ListArray<>();
            meths.add(sk, dels);
        }
        dels.add(del);
    }

    /*******************************************************************/
    /* Inherit preds & Evaluables                                      */
    /*******************************************************************/

    /**
     * <p>Inherit the methods.</p>
     *
     * @param java The super Java source.
     */
    private void inheritMeths(AutoClass java) {
        MapHash<StoreKey, ListArray<AbstractMember>> preds2 = java.getMeths();
        for (MapEntry<StoreKey, ListArray<AbstractMember>> entry = preds2.getLastEntry();
             entry != null; entry = preds2.predecessor(entry)) {
            ListArray<AbstractMember> dels = entry.value;
            ListArray<AbstractMember> dels2 = meths.get(entry.key);
            if (dels2 == null) {
                meths.add(entry.key, dels);
            } else {
                for (int i = 0; i < dels.size(); i++) {
                    AbstractMember del = dels.get(i);
                    if (AutoClass.hasParameterTypes(dels2, del.getParameterTypes()))
                        continue;
                    dels2.add(del);
                }
            }
        }
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
    private void defineMeths(Engine en, boolean rec)
            throws EngineException, EngineMessage {
        for (MapEntry<StoreKey, ListArray<AbstractMember>> entry = meths.getLastEntry();
             entry != null; entry = meths.predecessor(entry)) {
            StoreKey sk = entry.key;
            if (!AutoClass.hasDeclaredClass(entry.value, getAuto()))
                continue;
            AbstractMember[] dels = AutoClass.sortByScore(entry.value);
            SkelAtom sa = new SkelAtom(sk.getFun(), this);
            try {
                boolean virt = false;
                for (int i = 0; i < dels.length; i++) {
                    AbstractMember del = dels[i];
                    virt |= (del.subflags & AbstractDelegate.MASK_DELE_VIRT) != 0;
                }
                Predicate pick = makePublic(sa, sk.getArity(), virt, en);
                Predicate over = makeOverride(sa, pick, en);
                if (dels.length == 1) {
                    AbstractMember del = dels[0];
                    if (del.isNumeric())
                        makeMeta(pick, sa, en);
                    Predicate.definePredicate(pick, del, en);
                    Predicate.checkPredicateBody(pick, sa, en);
                } else {
                    for (int i = 0; i < dels.length; i++) {
                        AbstractMember del = dels[i];
                        if (del.isNumeric()) {
                            makeMeta(pick, sa, en);
                            break;
                        }
                    }
                    Predicate.checkPredicateBody(pick, sa, en);
                    for (int i = 0; i < dels.length; i++) {
                        Object[] args = new Object[sk.getArity()];
                        for (int j = 0; j < args.length; j++)
                            args[j] = new SkelVar(j);
                        SkelCompound head = new SkelCompound(sa, args);
                        AbstractMember del = dels[i];
                        SkelCompound goal = makeGoal(del, over, args, i);
                        if (i != dels.length - 1) {
                            goal = new SkelCompound(new SkelAtom(PrologReader.OP_COMMA, this),
                                    new SkelAtom("!", this), goal);
                            Object[] closures = del.getTests(this);
                            for (int j = args.length - 1; j >= 0; j--) {
                                Object closure = closures[j];
                                if (closure == null)
                                    continue;
                                SkelCompound test = makeTest(closure, args[j]);
                                goal = new SkelCompound(new SkelAtom(PrologReader.OP_COMMA,
                                        this), test, goal);
                            }
                        }
                        Object molec = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), head, goal);
                        Clause clause = PreClause.determineCompiled(AbstractDefined.OPT_PERF_CNLT, molec, en);
                        clause.assertRef(AbstractDefined.OPT_PERF_CNLT, en);
                    }
                    for (int i = 0; i < dels.length; i++) {
                        AbstractMember del = dels[i];
                        if (!del.getDeclaringClass().equals(getAuto()))
                            continue;
                        sa = new SkelAtom(sk.getFun() + OP_VARIANT + i, this);
                        virt = (del.subflags & AbstractDelegate.MASK_DELE_VIRT) != 0;
                        pick = makePrivate(sa, sk.getArity(), virt, en);
                        if (del.isNumeric())
                            makeMeta(pick, sa, en);
                        Predicate.definePredicate(pick, del, en);
                        Predicate.checkPredicateBody(pick, sa, en);
                    }
                }
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

    /**
     * <p>Create a branching term.</p>
     *
     * @return The branching term.
     */
    private SkelCompound makeGoal(AbstractMember del, Predicate over,
                                  Object[] args, int i)
            throws EngineMessage {
        if (!del.getDeclaringClass().equals(getAuto())) {
            String mod = over.getSource().getFullName();
            Object t = SpecialDynamic.moduleToSlashSkel(mod, this);

            String fun = del.getFun();
            Object s = new SkelCompound(new SkelAtom(fun, this), args);

            SkelAtom sa2 = new SkelAtom(EvaluableLogic.OP_COLON, this);
            return new SkelCompound(sa2, t, s);
        } else {
            String fun = del.getFun() + OP_VARIANT + i;
            return new SkelCompound(new SkelAtom(fun, this), args);
        }
    }

    /**
     * <p>Create a test and change scope.</p>
     *
     * @param closure The closure.
     * @param arg     The argument.
     */
    private SkelCompound makeTest(Object closure, Object arg) {
        if (closure instanceof SkelAtom) {
            String fun = ((SkelAtom) closure).fun;
            return new SkelCompound(new SkelAtom(fun, this), arg);
        } else if (closure instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) closure;

            Object[] args = new Object[sc.args.length + 1];
            System.arraycopy(sc.args, 0, args, 0, sc.args.length);
            args[sc.args.length] = arg;

            String fun = sc.sym.fun;
            return new SkelCompound(new SkelAtom(fun, this), args);
        } else {
            throw new IllegalArgumentException("illegal closure");
        }
    }

    /**
     * <p>Make predicate or evaluable function private.</p>
     * <p>Will set the predicate also to automatic.</p>
     *
     * @param sa    The name and call-site.
     * @param arity The length.
     * @param virt  The static flag.
     * @param en    The interpreter.
     */
    private static Predicate makePrivate(SkelAtom sa, int arity,
                                         boolean virt,
                                         Engine en)
            throws EngineException, EngineMessage {
        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
        CachePredicate cp = CachePredicate.getPredicateDefined(sa,
                arity, en, CachePredicate.MASK_CACH_CRTE);
        Predicate pick = cp.pick;
        pick.setBit(Predicate.MASK_PRED_VSPR);
        pick.addDef(src, Predicate.MASK_TRCK_VSPR, en);
        pick.setBit(Predicate.MASK_PRED_AUTO);
        if (virt)
            pick.setBit(Predicate.MASK_PRED_VIRT);
        return pick;
    }

    /**
     * <p>Make meta declaration for predicate.</p>
     *
     * @param pick The predicate.
     * @param sa   The name.
     * @param en   The engine.
     */
    private static void makeMeta(Predicate pick, SkelAtom sa,
                                 Engine en)
            throws EngineException, EngineMessage {
        if (pick.getArity() == 1)
            return;
        Object[] args = new Object[pick.getArity()];
        for (int i = 0; i < pick.getArity(); i++) {
            if (i != pick.getArity() - 1) {
                args[i] = Integer.valueOf(1);
            } else {
                args[i] = new SkelAtom(Predicate.OP_QUESTION);
            }
        }
        pick.meta_predicate = new SkelCompound(sa, args);
        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
        pick.addDef(src, Predicate.MASK_TRCK_META, en);
    }

    /*******************************************************************/
    /* Auto Loader Heuristics                                          */
    /*******************************************************************/

    /**
     * <p>Check whether a member has the given parameter types.</p>
     *
     * @param dels  The list of members.
     * @param types The parameter types.
     * @return True if a member has the given parameter types, otherwise false.
     */
    private static boolean hasParameterTypes(ListArray<AbstractMember> dels,
                                             Class[] types) {
        for (int i = 0; i < dels.size(); i++) {
            if (Arrays.equals(dels.get(i).getParameterTypes(), types))
                return true;
        }
        return false;
    }

    /**
     * <p>Check whether a member has the given declared class.</p>
     *
     * @param dels  The list of members.
     * @param clazz The declared class.
     * @return True if a member has the given declared class, otherwise false.
     */
    private static boolean hasDeclaredClass(ListArray<AbstractMember> dels,
                                            Class clazz) {
        for (int i = 0; i < dels.size(); i++) {
            if (dels.get(i).getDeclaringClass().equals(clazz))
                return true;
        }
        return false;
    }

    /**
     * <p>Sort the foreigns.</p>
     *
     * @param dels The unsorted foreigns.
     * @return The sorted foreigns.
     */
    private static AbstractMember[] sortByScore(ListArray<AbstractMember> dels) {
        AbstractMember[] delarray = new AbstractMember[dels.size()];
        dels.toArray(delarray);
        Arrays.sort(delarray);
        return delarray;
    }

    /*******************************************************************/
    /* Create Foreigns                                                 */
    /*******************************************************************/

    /**
     * <p>Create a method delegate.</p>
     * <p>Result or culprit is returned in the engine skel.</p>
     *
     * @param m  The method.
     * @param en The engine.
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public static boolean createMethod(Method m, Engine en, int k) {
        AbstractMember del;
        if (!validateExceptionTypes(m.getExceptionTypes(), en))
            return false;
        switch (k) {
            case AbstractReflection.INVOKE_VIRTUAL:
                if (getNondet(m.getParameterTypes())) {
                    del = new MemberVirtualNondet(m);
                } else {
                    del = new MemberVirtualDet(m);
                }
                break;
            case AbstractReflection.INVOKE_SPECIAL:
                if (Modifier.isStatic(m.getModifiers())) {
                    en.skel = EngineMessage.domainError(
                            AbstractFactory.OP_DOMAIN_FOREIGN_ACCESS,
                            new SkelAtom(Modifier.toString(m.getModifiers())));
                    return false;
                }
                if (getNondet(m.getParameterTypes())) {
                    del = new MemberSpecialNondet(m);
                } else {
                    del = new MemberSpecialDet(m);
                }
                if (!del.encodeSpecial(en))
                    return false;
                break;
            default:
                throw new IllegalArgumentException("illegal delegate");

        }
        if (!del.encodeSignaturePred(en))
            return false;
        en.skel = del;
        return true;
    }

    /**
     * <p>Create a constructor delegate.</p>
     * <p>Result or culprit is returned in the engine skel.</p>
     *
     * @param c  The constructor.
     * @param en The engine.
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public static boolean createConstructor(Constructor c, Engine en) {
        if (!validateExceptionTypes(c.getExceptionTypes(), en))
            return false;
        AbstractMember del = new MemberConstructor(c);
        if (!del.encodeSignaturePred(en))
            return false;
        en.skel = del;
        return true;
    }

    /**
     * <p>Create a getter or setter delegate.</p>
     * <p>Result or culprit is returned in the engine skel.</p>
     *
     * @param f  The field.
     * @param en The engine.
     * @param k  The desired field delegate.
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public static boolean createField(Field f, Engine en, int k) {
        AbstractMember del;
        switch (k) {
            case AbstractReflection.FIELD_GET:
                del = new MemberFieldGet(f);
                if (!del.encodeSignaturePred(en))
                    return false;
                break;
            case AbstractReflection.FIELD_SET:
                if (Modifier.isFinal(f.getModifiers())) {
                    en.skel = EngineMessage.domainError(
                            AbstractFactory.OP_DOMAIN_FOREIGN_ACCESS,
                            new SkelAtom(Modifier.toString(f.getModifiers())));
                    return false;
                }
                del = new MemberFieldSet(f);
                if (!del.encodeSignaturePred(en))
                    return false;
                break;
            default:
                throw new IllegalArgumentException("illegal delegate");
        }
        en.skel = del;
        return true;
    }

    /**
     * <p>Validate the exception types.</p>
     * <p>Culprit is returned in engine skel.</p>
     *
     * @param exces The exception types.
     * @param en    The engine.
     * @return True if the exeception types are ok, otherwise false.
     */
    public static boolean validateExceptionTypes(Class[] exces, Engine en) {
        for (int i = 0; i < exces.length; i++) {
            Class ret = exces[i];
            if (InterpreterException.class == ret) {
            } else if (Types.validateThrowable(ret)) {
            } else {
                en.skel = EngineMessage.domainError(
                        AbstractFactory.OP_DOMAIN_FOREIGN_EXCEPTION,
                        SpecialForeign.classToName(ret));
                return false;
            }
        }
        return true;
    }

    /**
     * <p>Compute the declared non-determinism.</p>
     *
     * @param paras The formal parameters.
     * @return The declared non-determinism.
     */
    private static boolean getNondet(Class[] paras) {
        for (int i = 0; i < paras.length; i++) {
            if (paras[i] == CallOut.class)
                return true;
        }
        return false;
    }

    /***********************************************************/
    /* Invocation Helpers                                      */
    /***********************************************************/

    /**
     * <p>Invoke the method.</p>
     *
     * @param constructor The constructor.
     * @param args        The arguments array.
     * @return The invokcation result.
     * @throws EngineException FFI error.
     * @throws EngineMessage   FFI error.
     */
    public static Object invokeNew(Constructor constructor,
                                   Object... args)
            throws EngineException, EngineMessage {
        try {
            return constructor.newInstance(args);
        } catch (InvocationTargetException y) {
            Throwable x = y.getCause();
            if (x instanceof RuntimeWrap)
                x = x.getCause();
            if (x instanceof InterpreterException) {
                throw (EngineException) ((InterpreterException) x).getException();
            } else {
                throw Types.mapThrowable(x);
            }
        } catch (Exception x) {
            throw Types.mapException(x, constructor);
        } catch (Error x) {
            throw Types.mapError(x);
        }
    }

}
