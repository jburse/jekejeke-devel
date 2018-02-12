package jekpro.tools.foreign;

import jekpro.model.builtin.SpecialSpecial;
import jekpro.model.inter.*;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.DisplayClause;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.Intermediate;
import jekpro.model.rope.PreClause;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.array.Types;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.proxy.AbstractAuto;
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

        meths = new MapHash<StoreKey, ListArray<AbstractMember>>();
        collectConstructors(en);
        collectMethods(en);
        collectFields(en);

        if (superjava != null)
            inheritMeths(superjava);
        for (int i = 0; i < interfacesjava.length; i++)
            inheritMeths(interfacesjava[i]);

        defineMeths(en, rec);
    }

    /*******************************************************************/
    /* Collect preds & Evaluables                                      */
    /*******************************************************************/

    /**
     * <p>Collect the constructors.</p>
     *
     * @param en The interpreter.
     * @throws EngineMessage FFI error.
     */
    private void collectConstructors(Engine en)
            throws EngineMessage {
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
     * @param en The interpreter.
     * @throws EngineMessage FFI error.
     */
    private void collectMethods(Engine en)
            throws EngineMessage {
        Method[] methods = getAuto().getDeclaredMethods();
        for (int i = 0; i < methods.length; i++) {
            Method method = methods[i];
            if (!Modifier.isPublic(method.getModifiers()))
                continue;
            if ((method.getModifiers() & SYNTHETIC) != 0)
                continue;
            if (createMethod(method, en, false)) {
                addForeignScore((AbstractMember) en.skel);
            } else if (createMethod(method, en, true)) {
                addForeignScore((AbstractMember) en.skel);
            }
        }
    }

    /**
     * <p>Collect the fields.</p>
     *
     * @param en The interpreter.
     * @throws EngineMessage FFI error.
     */
    private void collectFields(Engine en)
            throws EngineMessage {
        Field[] fields = getAuto().getDeclaredFields();
        for (int i = 0; i < fields.length; i++) {
            Field field = fields[i];
            if (!Modifier.isPublic(field.getModifiers()))
                continue;
            if ((field.getModifiers() & SYNTHETIC) != 0)
                continue;
            if (createField(field, en, AbstractFactory.FIELD_GET_EVAL)) {
                addForeignScore((AbstractMember) en.skel);
            } else if (createField(field, en, AbstractFactory.FIELD_GET_PRED)) {
                addForeignScore((AbstractMember) en.skel);
            }
            if (Modifier.isFinal(field.getModifiers()))
                continue;
            if (createField(field, en, AbstractFactory.FIELD_SET))
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
            dels = new ListArray<AbstractMember>();
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
    private void defineMeths(Engine en,
                             boolean rec)
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
                Predicate over = makeOverride(pick, en);
                if (dels.length == 1) {
                    AbstractMember del = dels[0];
                    SpecialSpecial.definePredicate(pick, del);
                    Predicate.checkPredicateDecl(pick, sa, en);
                } else {
                    for (int i = 0; i < dels.length; i++) {
                        Object[] args = new Object[sk.getArity()];
                        for (int j = 0; j < args.length; j++)
                            args[j] = new SkelVar(j);
                        SkelCompound head = new SkelCompound(sa, args);
                        AbstractMember del = dels[i];
                        SkelCompound goal = makeGoal(del, over, args, i);
                        if (i != dels.length - 1) {
                            goal = new SkelCompound(new SkelAtom(","), new SkelAtom("!"), goal);
                            Object[] closures = del.getTests();
                            for (int j = args.length - 1; j >= 0; j--) {
                                Object closure = closures[j];
                                if (closure == null)
                                    continue;
                                SkelCompound test = makeTest(closure, args[j]);
                                goal = new SkelCompound(new SkelAtom(","), test, goal);
                            }
                        }
                        PreClause pre = new PreClause();
                        pre.molec = new SkelCompound(new SkelAtom(Store.OP_TURNSTILE), head, goal);
                        PreClause.consultClause(AbstractDefined.OPT_PROM_STAT |
                                AbstractDefined.OPT_CHCK_DEFN |
                                AbstractDefined.OPT_ACTI_BOTT, pre, en);
                    }
                    for (int i = 0; i < dels.length; i++) {
                        AbstractMember del = dels[i];
                        if (!del.getDeclaringClass().equals(getAuto()))
                            continue;
                        sa = new SkelAtom(sk.getFun() + OP_VARIANT + i, this);
                        virt = (del.subflags & AbstractDelegate.MASK_DELE_VIRT) != 0;
                        pick = makePrivate(sa, sk.getArity(), virt, en);
                        SpecialSpecial.definePredicate(pick, del);
                        Predicate.checkPredicateDecl(pick, sa, en);
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
     * <p>Create a branching goal.</p>
     *
     * @return The branching goal.
     */
    private SkelCompound makeGoal(AbstractMember del, Predicate over,
                                  Object[] args, int i) {
        SkelCompound goal;
        if (((del.subflags & AbstractDelegate.MASK_DELE_ARIT) != 0)) {
            Object[] args1 = new Object[args.length - 1];
            System.arraycopy(args, 0, args1, 0, args1.length);
            SkelCompound expr;
            if (!del.getDeclaringClass().equals(getAuto())) {
                String fun = over.getFun();
                expr = new SkelCompound(new SkelAtom(fun, this), args1);
            } else {
                String fun = del.getFun() + OP_VARIANT + i;
                expr = new SkelCompound(new SkelAtom(fun, this), args1);
            }
            goal = new SkelCompound(new SkelAtom("is"), args[args.length - 1], expr);
        } else {
            if (!del.getDeclaringClass().equals(getAuto())) {
                String fun = over.getFun();
                goal = new SkelCompound(new SkelAtom(fun, this), args);
            } else {
                String fun = del.getFun() + OP_VARIANT + i;
                goal = new SkelCompound(new SkelAtom(fun, this), args);
            }
        }
        return goal;
    }

    /**
     * <p>Create a test.</p>
     *
     * @param closure The closure.
     * @param arg     The argument.
     */
    private SkelCompound makeTest(Object closure, Object arg) {
        if (closure instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) closure;
            return new SkelCompound(sa, arg);
        } else if (closure instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) closure;
            Object[] args = new Object[sc.args.length + 1];
            System.arraycopy(sc.args, 0, args, 0, sc.args.length);
            args[sc.args.length] = arg;
            return new SkelCompound(sc.sym, args);
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
    private Predicate makePrivate(SkelAtom sa, int arity,
                                  boolean virt,
                                  Engine en)
            throws EngineException, EngineMessage {
        CachePredicate cp = CachePredicate.getPredicateDefined(sa,
                arity, en, true);
        Predicate pick = cp.pick;
        pick.setBit(Predicate.MASK_PRED_VSPR);
        Usage loc = pick.getUsage(this);
        if (loc != null)
            loc.setBit(Usage.MASK_USE_VSPR);
        pick.setBit(Predicate.MASK_PRED_AUTO);
        if (virt)
            pick.setBit(Predicate.MASK_PRED_VIRT);
        return pick;
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
     * @param k  The predicate flag.
     * @return True if creation of the delegate succeeded, otherwise false.
     * @throws EngineMessage FFI error.
     */
    public static boolean createMethod(Method m, Engine en, boolean k)
            throws EngineMessage {
        AbstractMember del;
        if (k) {
            if (!validateExceptionTypes(m.getExceptionTypes(), en))
                return false;
            if (getNondet(m.getParameterTypes())) {
                del = new MemberMethodNondet(m);
            } else {
                del = new MemberMethodDet(m);
            }
            if (!del.encodeSignaturePred(en))
                return false;
        } else {
            if (!validateExceptionTypes(m.getExceptionTypes(), en))
                return false;
            del = new MemberFunction(m);
            if (!del.encodeSignatureEval(en))
                return false;
        }
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
     * @throws EngineMessage FFI error.
     */
    public static boolean createConstructor(Constructor c, Engine en)
            throws EngineMessage {
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
     * @param k  The desired delegate.
     * @return True if creation of the delegate succeeded, otherwise false.
     * @throws EngineMessage FFI error.
     */
    public static boolean createField(Field f, Engine en, int k)
            throws EngineMessage {
        AbstractMember del;
        switch (k) {
            case AbstractFactory.FIELD_GET_PRED:
                del = new MemberFieldGet(f);
                if (!del.encodeSignaturePred(en))
                    return false;
                break;
            case AbstractFactory.FIELD_GET_EVAL:
                del = new MemberConstant(f);
                if (!del.encodeSignatureEval(en))
                    return false;
                break;
            case AbstractFactory.FIELD_SET:
                if (Modifier.isFinal(f.getModifiers())) {
                    en.skel = EngineMessage.domainError(
                            AbstractFactory.OP_DOMAIN_FOREIGN_ACCESS,
                            new SkelAtom(Modifier.toString(f.getModifiers())));
                    return false;
                }
                del = new MemberFieldSet(f);
                if (!del.encodeSignatureEval(en))
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
     * @throws EngineMessage FFI error.
     */
    public static boolean validateExceptionTypes(Class[] exces, Engine en)
            throws EngineMessage {
        for (int i = 0; i < exces.length; i++) {
            Class ret = exces[i];
            if (InterpreterException.class == ret) {
            } else if (Types.validateThrowable(ret)) {
            } else {
                en.skel = EngineMessage.domainError(
                        AbstractFactory.OP_DOMAIN_FOREIGN_EXCEPTION,
                        SpecialSpecial.classToName(ret, en.store.SOURCE_SYSTEM, en));
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
     * @param obj The receiver.
     * @return The invokcation result.
     * @throws EngineMessage FFI error.
     */
    public static Object invokeGetter(Field fld, Object obj)
            throws EngineMessage {
        try {
            return fld.get(obj);
        } catch (IllegalAccessException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_ACCESS,
                    AbstractFactory.OP_PERMISSION_FIELD,
                    new SkelAtom(fld.getName())));
        } catch (IllegalArgumentException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    AbstractFactory.OP_PERMISSION_APPLY,
                    AbstractFactory.OP_PERMISSION_GETTER,
                    new SkelAtom(fld.getName())));
        } catch (NullPointerException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    AbstractFactory.OP_PERMISSION_LOOKUP,
                    AbstractFactory.OP_PERMISSION_GETTER,
                    new SkelAtom(fld.getName())));
        } catch (NoClassDefFoundError x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_LINK,
                    EngineMessage.OP_PERMISSION_CLASS,
                    new SkelAtom(x.getMessage())));
        }
    }

    /**
     * <p>Invoke the method.</p>
     *
     * @param con  The constructor.
     * @param args The arguments array.
     * @param en   The engine.
     * @return The invokcation result.
     * @throws EngineException FFI error.
     * @throws EngineMessage   FFI error.
     */
    public static Object invokeNew(Constructor con, Object[] args, Engine en)
            throws EngineException, EngineMessage {
        try {
            return con.newInstance(args);
        } catch (InvocationTargetException y) {
            Throwable x = y.getCause();
            if (x instanceof RuntimeWrap)
                x = x.getCause();
            if (x instanceof InterpreterException) {
                throw (EngineException) ((InterpreterException) x).getException();
            } else {
                throw Types.mapThrowable(x);
            }
        } catch (IllegalAccessException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_ACCESS,
                    AbstractFactory.OP_PERMISSION_CONSTRUCTOR,
                    SpecialSpecial.constructorToCallable(
                            con.getParameterTypes(), en.store.SOURCE_SYSTEM, en)));
        } catch (IllegalArgumentException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    AbstractFactory.OP_PERMISSION_APPLY,
                    AbstractFactory.OP_PERMISSION_CONSTRUCTOR,
                    SpecialSpecial.constructorToCallable(
                            con.getParameterTypes(), en.store.SOURCE_SYSTEM, en)));
        } catch (InstantiationException e) {
            throw new EngineMessage(EngineMessage.permissionError(
                    AbstractFactory.OP_PERMISSION_NEW,
                    EngineMessage.OP_PERMISSION_CLASS,
                    SpecialSpecial.classToName(con.getDeclaringClass(), en.store.SOURCE_SYSTEM, en)));
        } catch (NoClassDefFoundError x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_LINK,
                    EngineMessage.OP_PERMISSION_CLASS,
                    new SkelAtom(x.getMessage())));
        }
    }

}