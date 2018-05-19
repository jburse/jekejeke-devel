package jekpro.reference.bootload;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.builtin.Branch;
import jekpro.model.builtin.PropertyIndicator;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.*;
import jekpro.model.rope.*;
import jekpro.reference.reflect.SpecialOper;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.reflect.SpecialSource;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.proxy.FactoryAPI;
import jekpro.tools.term.*;
import matula.comp.sharik.AbstractBundle;
import matula.comp.sharik.AbstractTracking;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.SetHash;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

/**
 * <p>Provides built-in predicates for the load theory.</p>
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
public final class SpecialLoad extends AbstractSpecial {
    private final static int SPECIAL_SYS_LOAD_FILE = 0;
    private final static int SPECIAL_SYS_DETACH_FILE = 1;
    private final static int SPECIAL_SYS_IMPORT_FILE = 2;
    private final static int SPECIAL_SYS_SHOW_PROVABLE_SOURCE = 3;
    private final static int SPECIAL_SYS_SHOW_SYNTAX_SOURCE = 4;
    private final static int SPECIAL_SYS_SHOW_BASE = 5;
    private final static int SPECIAL_SYS_CURRENT_PROVABLE = 6;
    private final static int SPECIAL_SYS_PROVABLE_PROPERTY_CHK = 7;
    private final static int SPECIAL_SYS_CURRENT_SYNTAX = 8;
    private final static int SPECIAL_SYS_SYNTAX_PROPERTY_CHK = 9;
    private final static int SPECIAL_SYS_REGISTER_FILE = 10;
    private final static int SPECIAL_SYS_MODULE_ACTION = 11;
    private final static int SPECIAL_SYS_PEEK_STACK = 12;

    public final static String OP_MODULE = "module";

    /**
     * <p>Create a load special.</p>
     *
     * @param i The id.
     */
    public SpecialLoad(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_SYS_LOAD_FILE:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                LoadOpts opts = new LoadOpts();
                opts.decodeLoadOpts(temp[1], ref, en);
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                SkelAtom sa = EngineMessage.castStringWrapped(en.skel, en.display);
                AbstractSource source = (sa.scope != null ? sa.scope : en.store.user);
                opts.makeLoad(source, sa.fun, en);
                return en.getNextRaw();
            case SPECIAL_SYS_DETACH_FILE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                opts = new LoadOpts();
                opts.decodeLoadOpts(temp[1], ref, en);
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                sa = EngineMessage.castStringWrapped(en.skel, en.display);
                opts.makeUnload(sa.scope, sa.fun, en);
                return en.getNextRaw();
            case SPECIAL_SYS_IMPORT_FILE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                opts = new LoadOpts();
                opts.decodeLoadOpts(temp[1], ref, en);
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                sa = EngineMessage.castStringWrapped(en.skel, en.display);
                source = (sa.scope != null ? sa.scope : en.store.user);
                SpecialLoad.performImport(source, sa.fun, en, opts);
                return en.getNextRaw();
            case SPECIAL_SYS_SHOW_PROVABLE_SOURCE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Predicate pick = indicatorToProvable(temp[0], ref, en);
                Predicate.checkExistentProvable(pick, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                sa = EngineMessage.castStringWrapped(en.skel, en.display);
                source = en.store.getSource(sa.fun);
                if (source == null)
                    return false;
                if (pick.getUsage(source) == null)
                    return false;
                Object obj = en.visor.curoutput;
                FactoryAPI.checkTextWrite(obj);
                Writer wr = (Writer) obj;
                PrologWriter pw = Foyer.createWriter(Foyer.IO_TERM);
                pw.setWriteOpts(new WriteOpts(en));
                pw.setEngineRaw(en);
                pw.setFlags(PrologWriter.FLAG_QUOT | PrologWriter.FLAG_MKDT);
                pw.setSpez(PrologWriter.SPEZ_META);
                pw.setOffset(-1);
                pw.setWriter(wr);
                SpecialLoad.listProvable(pw, pick, source, en);
                newLineFlush(wr);
                return en.getNextRaw();
            case SPECIAL_SYS_SHOW_SYNTAX_SOURCE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Operator oper = operToSyntax(temp[0], ref, en);
                Operator.checkExistentSyntax(oper, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                sa = EngineMessage.castStringWrapped(en.skel, en.display);
                source = en.store.getSource(sa.fun);
                if (source == null)
                    return false;
                if (oper.getScope() != source)
                    return false;
                obj = en.visor.curoutput;
                FactoryAPI.checkTextWrite(obj);
                wr = (Writer) obj;
                pw = Foyer.createWriter(Foyer.IO_TERM);
                pw.setWriteOpts(new WriteOpts(en));
                pw.setEngineRaw(en);
                pw.setFlags(PrologWriter.FLAG_QUOT | PrologWriter.FLAG_MKDT);
                pw.setSpez(PrologWriter.SPEZ_META);
                pw.setOffset(-1);
                pw.setWriter(wr);
                SpecialLoad.listSyntax(pw, oper, en);
                newLineFlush(wr);
                return en.getNextRaw();
            case SPECIAL_SYS_CURRENT_PROVABLE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!en.unifyTerm(temp[0], ref,
                        SpecialLoad.currentProvable(en), Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_PROVABLE_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = indicatorToProvable(temp[0], ref, en);
                if (pick == null)
                    return false;
                StoreKey prop = Predicate.propToStoreKey(temp[1], ref, en);
                SpecialPred.predicateToProperty(pick, prop, en);
                if (!en.unifyTerm(temp[2], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_CURRENT_SYNTAX:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!en.unifyTerm(temp[0], ref,
                        currentSyntax(en), Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_SYNTAX_PROPERTY_CHK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                oper = operToSyntax(temp[0], ref, en);
                if (oper == null)
                    return false;
                prop = Predicate.propToStoreKey(temp[1], ref, en);
                SpecialOper.operToProperty(prop, oper, en);
                if (!en.unifyTerm(temp[2], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SYS_SHOW_BASE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                sa = EngineMessage.castStringWrapped(en.skel, en.display);
                source = en.store.getSource(sa.fun);
                if (source == null)
                    return false;
                obj = en.visor.curoutput;
                FactoryAPI.checkTextWrite(obj);
                wr = (Writer) obj;
                pw = Foyer.createWriter(Foyer.IO_TERM);
                pw.setWriteOpts(new WriteOpts(en));
                pw.setEngineRaw(en);
                pw.setFlags(PrologWriter.FLAG_QUOT | PrologWriter.FLAG_MKDT);
                pw.setSpez(PrologWriter.SPEZ_META);
                pw.setOffset(-1);
                pw.setWriter(wr);
                SpecialLoad.listBase(pw, source, en);
                newLineFlush(wr);
                return en.getNextRaw();
            case SPECIAL_SYS_REGISTER_FILE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                sa = EngineMessage.castStringWrapped(en.skel, en.display);
                registerFile(sa.scope, sa.fun, sa.getPosition(), en.store);
                return en.getNextRaw();
            case SPECIAL_SYS_MODULE_ACTION:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                LoadForce opts2 = new LoadForce();
                opts2.decodeLoadForce(temp[1], ref, en);
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                sa = EngineMessage.castStringWrapped(en.skel, en.display);
                source = (sa.scope != null ? sa.scope : en.store.user);
                opts2.makeForce(source, sa.fun, en);
                return en.getNextRaw();
            case SPECIAL_SYS_PEEK_STACK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                AbstractSource src = en.visor.peekStack();
                if (src == null || !en.unifyTerm(temp[0], ref,
                        new SkelAtom(src.getPath()), Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>List a predicate.</p>
     * <p>System clauses are excluded.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param pw     The prolog writer.
     * @param pick   The predicate.
     * @param source The source.
     * @param en     The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void listProvable(PrologWriter pw,
                                     Predicate pick, AbstractSource source,
                                     Engine en)
            throws EngineMessage, EngineException {
        /* flesh out properties */
        ListArray<SkelAtom> modifiers = null;
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            AbstractProperty[] props = branch.listPredProp(pick);
            for (int k = 0; k < props.length; k++) {
                AbstractProperty prop = props[k];
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_HIDE) != 0)
                    continue;
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_NOCL) != 0 &&
                        !noClause(pick, en))
                    continue;
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_SUPR) != 0 &&
                        sameVisible(source, pick, en))
                    continue;
                Object[] vals = SpecialPred.getPropPred(pick, prop, en);
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_SLCF) != 0) {
                    vals = selectFirst(vals, new SkelAtom((source != null ? source.getPath() : "")));
                } else if ((prop.getFlags() & AbstractProperty.MASK_PROP_PRJF) != 0) {
                    vals = projectFirst(vals);
                } else if ((prop.getFlags() & AbstractProperty.MASK_PROP_DELE) != 0) {
                    vals = delegateSpec(vals, pick, source, en);
                }
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_MODI) != 0) {
                    for (int j = 0; j < vals.length; j++) {
                        if (modifiers == null)
                            modifiers = new ListArray<SkelAtom>();
                        modifiers.add((SkelAtom) AbstractTerm.getSkel(vals[j]));
                    }
                } else {
                    for (int j = 0; j < vals.length; j++) {
                        Object val = vals[j];
                        Object decl = prop.predDeclSkel(AbstractTerm.getSkel(val), pick,
                                source, en);
                        if (modifiers != null) {
                            decl = prependModifiers(modifiers, decl);
                            modifiers = null;
                        }
                        decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
                        decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
                        pw.unparseStatement(decl, Display.DISPLAY_CONST);
                        AbstractSource.flushWriter(pw.getWriter());
                    }
                }
            }
        }
        if (!(pick.del instanceof AbstractDefined))
            return;

        /* flesh out clauses */
        Clause[] list = ((AbstractDefined) pick.del).listClauses(en);
        for (int i = 0; i < list.length; i++) {
            Clause clause = list[i];
            SkelAtom sa = Frame.callableToName(clause.head);
            if (source != sa.scope)
                continue;
            if (modifiers != null) {
                Object decl = PropertyIndicator.storeKeyToColonSkel(
                        pick.getFun(), pick.getArity(), source, en);
                decl = prependModifiers(modifiers, decl);
                modifiers = null;
                decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
                decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
                pw.unparseStatement(decl, Display.DISPLAY_CONST);
                AbstractSource.flushWriter(pw.getWriter());
            }
            Object t = PreClause.intermediateToClause(clause.head, clause.next, en);
            EngineSkel ek = new EngineSkel();
            ek.vars = new SetHash<SkelVar>();
            ek.anon = new SetHash<SkelVar>();
            ek.singsOfSkel(t);
            Named[] vars = EngineSkel.numberVariablesSkel(ek.vars, ek.anon, clause.vars);
            pw.setFlags(PrologWriter.FLAG_QUOT | PrologWriter.FLAG_NEWL | PrologWriter.FLAG_MKDT);
//                pw.setFlags(PrologWriter.FLAG_QUOT | PrologWriter.FLAG_NEWL | PrologWriter.FLAG_NAVI
//                        | PrologWriter.FLAG_MKDT);
//                pw.setSource(source);
            AbstractSource.showClause(pw, t, vars, en);
            pw.setFlags(PrologWriter.FLAG_QUOT | PrologWriter.FLAG_MKDT);
        }
    }


    /**
     * <p>List the syntax operator.</p>
     */
    private static void listSyntax(PrologWriter pw,
                                   Operator oper,
                                   Engine en)
            throws EngineMessage, EngineException {
        /* flesh out properties */
        Object decl = oper.operatorToCompound(en.store);
        decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
        decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
        pw.unparseStatement(decl, Display.DISPLAY_CONST);
        AbstractSource.flushWriter(pw.getWriter());
    }

    /**
     * <p>List the source.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param pw  The Prolog writer.
     * @param src The source.
     * @param en  The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void listBase(PrologWriter pw,
                                 AbstractSource src, Engine en)
            throws EngineMessage, EngineException {
        /* show source comment */
        AbstractSource.showShortName(pw.getWriter(), src);

        if (src != null &&
                (src.getBits() & AbstractSource.MASK_SRC_VSNP) == 0 &&
                src.getFullName() == null) {
            Object decl = new SkelCompound(new SkelAtom(OP_MODULE),
                    new SkelAtom(Branch.OP_USER),
                    new SkelAtom(Foyer.OP_NIL));
            decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
            pw.unparseStatement(decl, Display.DISPLAY_CONST);
            AbstractSource.flushWriter(pw.getWriter());
        }

        if (src == null)
            return;

        /* flesh out properties */
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            AbstractProperty[] props = branch.listSrcProp();
            for (int k = 0; k < props.length; k++) {
                AbstractProperty prop = props[k];
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_HIDE) != 0)
                    continue;
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_MODL) != 0 &&
                        src.getFullName() == null)
                    continue;
                Object[] vals = SpecialSource.getPropSrc(prop, src, en);
                for (int j = 0; j < vals.length; j++) {
                    Object val = vals[j];
                    Object decl = prop.srcDeclSkel(AbstractTerm.getSkel(val), src);
                    decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
                    decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
                    pw.unparseStatement(decl, AbstractTerm.getDisplay(val));
                    AbstractSource.flushWriter(pw.getWriter());
                }
            }
        }
    }

    /************************************************************/
    /* Perform Import                                           */
    /************************************************************/

    /**
     * <p>Perform an import.</p>
     *
     * @param scope The scope.
     * @param key   The source key.
     * @param en    The engine.
     * @param opts  The consult options.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void performImport(AbstractSource scope, String key,
                                      Engine en, LoadOpts opts)
            throws EngineMessage, EngineException {
        scope = AbstractSource.derefParent(scope);
        AbstractSource source = scope.getStore().getSourceDefined(key, false);
        Reader reader;
        if (!Branch.OP_USER.equals(source.getPath())) {
            reader = source.openReader(false, opts, en);
            scope.loadModule(reader, en, true);
            scope.closeReader(reader);
        } else {
            Object obj = en.visor.dispinput;
            PrologReader.checkTextRead(obj);
            reader = (Reader) obj;
            scope.loadModule(reader, en, true);
        }
    }

    /****************************************************************************/
    /* Predicate Property Filters                                               */
    /****************************************************************************/

    /**
     * <p>Find all values that have as a first argument some other value.</p>
     *
     * @param vals The values.
     * @param val  The other value.
     * @return The found values.
     */
    private static Object[] selectFirst(Object[] vals, Object val) {
        ListArray<Object> res = null;
        for (int i = 0; i < vals.length; i++) {
            Object val2 = vals[i];
            SkelCompound sc = (SkelCompound) AbstractTerm.getSkel(val2);
            val2 = AbstractTerm.createMolec(sc.args[0], AbstractTerm.getDisplay(val2));
            if (!val2.equals(val))
                continue;
            if (res == null)
                res = new ListArray<Object>();
            if (sc.args.length == 1) {
                res.add(sc.sym);
            } else {
                Object[] newargs = new Object[sc.args.length - 1];
                System.arraycopy(sc.args, 1, newargs, 0, newargs.length);
                res.add(new TermCompound(new SkelCompound(sc.sym, newargs),
                        AbstractTerm.getDisplay(val2)));
            }
        }
        if (res == null)
            return AbstractBranch.FALSE_PROPERTY;
        Object[] newvals = new Object[res.size()];
        res.toArray(newvals);
        return newvals;
    }

    /**
     * <p>Check whether the predicate is defined and has no clauses.</p>
     *
     * @param pick The predicate.
     * @return True if the predicate has no clauses, otherwise false.
     */
    private static boolean noClause(Predicate pick, Engine en) {
        if (!(pick.del instanceof AbstractDefined))
            return false;
        AbstractDefined def = (AbstractDefined) pick.del;
        return (def.lengthClauses(en) == 0);
    }

    /**
     * <p>Check whether a source and a predicate have the same visibility.</p>
     *
     * @param src  The source.
     * @param pick The predicate.
     * @param en   The engine.
     * @return True if the source and the predicate have the same visibility, otherwise false.
     */
    private static boolean sameVisible(AbstractSource src, Predicate pick,
                                       Engine en)
            throws EngineMessage {
        Object[] vals = projectFirst(SpecialSource.getPropSrc(
                Branch.PROP2_SYS_SOURCE_VISIBLE, src, en));
        Object[] vals2 = projectFirst(SpecialPred.getPropPred(
                pick, Branch.PROP_VISIBLE, en));
        return sameValues2(vals, vals2);
    }

    /**
     * <p>Reduce the value list to its values.</p>
     *
     * @param vals The value list with property names.
     * @return The value list without property names.
     */
    private static Object[] projectFirst(Object[] vals) {
        if (vals.length == 0)
            return vals;
        Object[] newvals = new Object[vals.length];
        for (int i = 0; i < vals.length; i++) {
            Object val = vals[i];
            SkelCompound sc = (SkelCompound) AbstractTerm.getSkel(val);
            newvals[i] = AbstractTerm.createMolec(sc.args[0], AbstractTerm.getDisplay(val));
        }
        return newvals;
    }

    /**
     * <p>Convert the value list to its spec.</p>
     *
     * @param vals   The value list only buit_in.
     * @param pick   The predicate.
     * @param source The source.
     * @param en     The engine.
     * @return The value list with spec.
     */
    private static Object[] delegateSpec(Object[] vals, Predicate pick,
                                         AbstractSource source, Engine en)
            throws EngineMessage {
        if (vals.length == 0)
            return vals;
        Object[] newvals = new Object[vals.length];
        for (int i = 0; i < vals.length; i++) {
            Object val = pick.del.toSpec(source, en);
            newvals[i] = AbstractTerm.createMolec(val, Display.DISPLAY_CONST);
        }
        return newvals;
    }

    /**
     * <p>Prepend modifiers to a value.</p>
     *
     * @param modifiers The modifiers.
     * @param decl      The value.
     * @return The prependet value.
     */
    private static Object prependModifiers(ListArray<SkelAtom> modifiers, Object decl) {
        for (int i = modifiers.size() - 1; i >= 0; i--)
            decl = new SkelCompound(modifiers.get(i), decl);
        return decl;
    }

    /**
     * <p>Check whether the two value lists contain the same elements.</p>
     *
     * @param vals  The first value list.
     * @param vals2 The second value list.
     * @return True if both value lists contain the same elements, otherwise false.
     */
    private static boolean sameValues2(Object[] vals, Object[] vals2) {
        for (int i = 0; i < vals.length; i++)
            if (AbstractProperty.indexValue(vals2, vals[i]) == -1)
                return false;
        for (int i = 0; i < vals2.length; i++)
            if (AbstractProperty.indexValue(vals, vals2[i]) == -1)
                return false;
        return true;
    }

    /****************************************************************/
    /* Listing Members                                              */
    /****************************************************************/

    /**
     * <p>Create a prolog list with the directly accessible predicates.</p>
     *
     * @param en The engine.
     * @return The prolog list of the directly accessible predicates.
     */
    private static Object currentProvable(Engine en) {
        AbstractStore store = en.store;
        Object res = en.store.foyer.ATOM_NIL;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int j = sources.length - 1; j >= 0; j--) {
                AbstractSource base = sources[j].value;
                Predicate[] preds = base.snapshotRoutine();
                for (int i = preds.length - 1; i >= 0; i--) {
                    Predicate pick = preds[i];
                    Object val = indicatorToColonSkel(pick.getFun(), pick.getArity());
                    res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
                }
            }
            store = store.parent;
        }
        return res;
    }

    /**
     * <p>Create a prolog list with the directly accessible syntax operators.</p>
     *
     * @param en The engine.
     * @return The prolog list of the directly accessible syntax operators.
     */
    private static Object currentSyntax(Engine en) {
        AbstractStore store = en.store;
        Object res = en.store.foyer.ATOM_NIL;
        while (store != null) {
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int j = sources.length - 1; j >= 0; j--) {
                AbstractSource base = sources[j].value;
                MapEntry<String, Operator>[] opers = base.snapshotOper();
                for (int i = opers.length - 1; i >= 0; i--) {
                    Operator oper = opers[i].value;
                    Object val = operToColonSkel(oper.getType(), oper.getKey());
                    res = new SkelCompound(en.store.foyer.ATOM_CONS, val, res);
                }
            }
            store = store.parent;
        }
        return res;
    }

    /****************************************************************/
    /* Resource Handling                                            */
    /****************************************************************/

    /**
     * <p>Register a file.</p>
     *
     * @param scope The call-site.
     * @param key   The source key.
     * @param pos   The position.
     * @param store The store.
     */
    private static void registerFile(AbstractSource scope, String key,
                                     PositionKey pos, AbstractStore store) {
        AbstractSource src = (scope != null ? scope : store.user);
        Resource rsc = store.foyer.createResource(key);
        rsc.setPosition(pos);
        src.addResource(rsc);
    }

    /**
     * <p>Handle system exceptions in a consult loop.</p>
     * <p>Will do the following:</p>
     * <ul>
     * <li><b>system_error(user_abort):</b> Print chain rest, do not break.</li>
     * <li><b>system_error(user_exit):</b> Print chain rest, do break.</li>
     * <li><b>system_error(memory_threshold):</b> Print exception, do not break.</li>
     * <li><b>system_error(_):</b> Re throw exception.</li>
     * <li><b>_:</b> Print exception, do not break.</li>
     * </ul>
     *
     * @param ex  The exception.
     * @param en  The engine.
     * @param rec The recursion flag.
     * @return True if break, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static boolean systemConsultBreak(EngineException ex,
                                             Engine en, boolean rec)
            throws EngineMessage, EngineException {
        EngineMessage m;
        Object t;
        boolean res;
        if ((m = ex.exceptionType(EngineException.OP_ERROR)) != null &&
                (t = m.messageType(EngineMessage.OP_SYSTEM_ERROR)) != null) {
            if (t instanceof SkelAtom &&
                    ((SkelAtom) t).fun.equals(EngineMessage.OP_SYSTEM_USER_ABORT)) {
                EngineException rest = ex.causeChainRest();
                if (rest != null)
                    rest.printStackTrace(en);
                res = false;
            } else if (!rec && t instanceof SkelAtom &&
                    ((SkelAtom) t).fun.equals(EngineMessage.OP_SYSTEM_USER_EXIT)) {
                EngineException rest = ex.causeChainRest();
                if (rest != null)
                    rest.printStackTrace(en);
                res = true;
            } else {
                throw ex;
            }
        } else {
            ex.printStackTrace(en);
            res = false;
        }
        return res;
    }

    /**
     * <p>Output newline and flush.</p>
     *
     * @param wr The write.
     * @throws EngineMessage Shit happens.
     */
    public static void newLineFlush(Writer wr)
            throws EngineMessage {
        try {
            wr.write('\n');
            wr.flush();
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /*******************************************************************/
    /* Provable Direct Access                                          */
    /*******************************************************************/

    /**
     * <p>Get predicate by indicator.</p>
     *
     * @param t  The skel of the compound.
     * @param d  The display of the compound.
     * @param en The engine.
     * @return The predicate.
     * @throws EngineMessage Shit happens.
     */
    public static Predicate indicatorToProvable(Object t, Display d, Engine en)
            throws EngineMessage {
        Integer arity = SpecialLoad.colonToIndicator(t, d, en);
        SkelAtom sa = (SkelAtom) en.skel;
        AbstractSource base;
        if (!CacheFunctor.isQuali(sa.fun)) {
            StoreKey sk = new StoreKey(sa.fun, arity.intValue());
            return CachePredicate.getRoutineUser(sk, en.store);
        } else {
            String s = CacheFunctor.sepModule(sa.fun);
            base = AbstractSource.getModule(s, en.store);
            if (base == null)
                return null;
            StoreKey sk = new StoreKey(sa.fun, arity.intValue());
            return base.getRoutine(sk);
        }
    }

    /**
     * <p>Convert a colon indicator to a store key.</p>
     * <p>The converted name is returned in engine skel.</p>
     *
     * @param t  The slash skeleton.
     * @param d  The slash display.
     * @param en The engine.
     * @return The length.
     * @throws EngineMessage The indicator is not wellformed.
     */
    private static Integer colonToIndicator(Object t, Display d, Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(SpecialQuali.OP_COLON)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa2 = SpecialQuali.slashToPackageTest(temp.args[0], d, true, en);
            if (sa2 == null) {
                EngineMessage.checkInstantiated(temp.args[0]);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_MODULE, temp.args[0]), d);
            }
            Integer arity = colonToIndicator(temp.args[1], d, en);
            SkelAtom sa = (SkelAtom) en.skel;
            en.skel = CacheFunctor.getFunctor(sa, sa2.fun, temp.sym, en);
            return arity;
        } else if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
            SkelCompound sc = (SkelCompound) t;
            en.skel = sc.args[1];
            en.display = d;
            en.deref();
            EngineMessage.checkInstantiated(en.skel);
            Number num = EngineMessage.castInteger(en.skel, en.display);
            EngineMessage.checkNotLessThanZero(num);
            EngineMessage.castIntValue(num);
            en.skel = sc.args[0];
            en.display = d;
            en.deref();
            EngineMessage.checkInstantiated(en.skel);
            EngineMessage.castStringWrapped(en.skel, en.display);
            return (Integer) num;
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_PREDICATE_INDICATOR, t), d);
        }
    }

    /**
     * <p>Convert an store key to a colon.</p>
     *
     * @param fun   The name.
     * @param arity The length.
     * @return The colon
     */
    public static Object indicatorToColonSkel(String fun, int arity) {
        Object s;
        if (CacheFunctor.isQuali(fun)) {
            s = Clause.packageToSlashSkel(CacheFunctor.sepModule(fun));

            Object t = new SkelCompound(new SkelAtom(Foyer.OP_SLASH),
                    new SkelAtom(CacheFunctor.sepName(fun)),
                    Integer.valueOf(arity));
            s = new SkelCompound(new SkelAtom(SpecialQuali.OP_COLON), s, t);
        } else {
            s = new SkelCompound(new SkelAtom(Foyer.OP_SLASH),
                    new SkelAtom(fun),
                    Integer.valueOf(arity));
        }
        return s;
    }

    /*******************************************************************/
    /* Syntax Direct Access                                            */
    /*******************************************************************/

    /**
     * <p>Lookup an operator from a compound.</p>
     *
     * @param t  The compound skeleton.
     * @param d  The compound display.
     * @param en The engine copy.
     * @return The operator or null.
     * @throws EngineMessage Shit happends.
     */
    public static Operator operToSyntax(Object t, Display d, Engine en)
            throws EngineMessage {
        int type = colonToOper(t, d, en);
        String fun = ((SkelAtom) en.skel).fun;
        if (!CacheFunctor.isQuali(fun)) {
            return OperatorSearch.getOperUser(type, fun, en.store);
        } else {
            String s = CacheFunctor.sepModule(fun);
            AbstractSource base = AbstractSource.getModule(s, en.store);
            if (base == null)
                return null;
            return base.getOper(type, fun);
        }
    }

    /**
     * <p>Convert a colon oper to a type and key.</p>
     *
     * @param t  The slash skeleton.
     * @param d  The slash display.
     * @param en The engine.
     * @return The length.
     * @throws EngineMessage The indicator is not wellformed.
     */
    private static int colonToOper(Object t, Display d, Engine en)
            throws EngineMessage {
        int type = SpecialOper.opToType(t, d, en);
        en.skel = colonToAtom(en.skel, en.display, en);
        return type;
    }

    /**
     * <p>Convert a colon atom to a string.</p>
     *
     * @param t  The slash skeleton.
     * @param d  The slash display.
     * @param en The engine.
     * @return The string.
     * @throws EngineMessage The indicator is not wellformed.
     */
    private static SkelAtom colonToAtom(Object t, Display d, Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(SpecialQuali.OP_COLON)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa2 = SpecialQuali.slashToPackageTest(temp.args[0], d, true, en);
            if (sa2 == null) {
                EngineMessage.checkInstantiated(temp.args[0]);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_MODULE, temp.args[0]), d);
            }
            SkelAtom sa = colonToAtom(temp.args[1], d, en);
            return CacheFunctor.getFunctor(sa, sa2.fun, temp.sym, en);
        } else {
            EngineMessage.checkInstantiated(en.skel);
            return EngineMessage.castStringWrapped(en.skel, en.display);
        }
    }

    /**
     * <p>Convert a type and key to a colon oper.</p>
     *
     * @param type The type.
     * @param key  The key.
     * @return The compound.
     */
    public static Object operToColonSkel(int type, String key) {
        Object s = atomToColonSkel(key);

        return SpecialOper.typeToOpSkel(s, type);
    }

    /**
     * <p>Convert a string to colon atom.</p>
     *
     * @param fun The string.
     * @return The colon atom.
     */
    private static Object atomToColonSkel(String fun) {
        Object s;
        if (CacheFunctor.isQuali(fun)) {
            s = Clause.packageToSlashSkel(CacheFunctor.sepModule(fun));

            Object t = new SkelAtom(CacheFunctor.sepName(fun));
            s = new SkelCompound(new SkelAtom(SpecialQuali.OP_COLON), s, t);
        } else {
            s = new SkelAtom(fun);
        }
        return s;
    }

}
