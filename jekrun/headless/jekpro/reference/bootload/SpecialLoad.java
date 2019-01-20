package jekpro.reference.bootload;

import derek.util.protect.LicenseError;
import jekpro.frequent.standard.EngineCopy;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractInformation;
import jekpro.model.builtin.Branch;
import jekpro.model.builtin.InformationIndicator;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.*;
import jekpro.model.rope.*;
import jekpro.reference.reflect.SpecialOper;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.reflect.SpecialSource;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.reference.structure.EngineVars;
import jekpro.reference.structure.SpecialUniv;
import jekpro.reference.structure.SpecialVars;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.*;
import matula.comp.sharik.AbstractBundle;
import matula.comp.sharik.AbstractTracking;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
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
    private final static int SPECIAL_SYS_HAS_CLAUSE = 6;
    private final static int SPECIAL_SYS_SHORT_BASE = 7;
    private final static int SPECIAL_SYS_REGISTER_FILE = 8;

    public static final int MASK_SHOW_NANO = 0x00000001;
    public static final int MASK_SHOW_NRBD = 0x00000002;

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
                SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                AbstractSource source = (sa.scope != null ? sa.scope : en.store.user);
                opts.makeLoad(source, sa.fun, en);
                return en.getNextRaw();
            case SPECIAL_SYS_DETACH_FILE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                opts = new LoadOpts();
                opts.decodeLoadOpts(temp[1], ref, en);
                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                opts.makeUnload(source, sa.fun, en);
                return en.getNextRaw();
            case SPECIAL_SYS_IMPORT_FILE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                opts = new LoadOpts();
                opts.decodeLoadOpts(temp[1], ref, en);
                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                SpecialLoad.performImport(source, sa.fun, en, opts);
                return en.getNextRaw();
            case SPECIAL_SYS_SHOW_PROVABLE_SOURCE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                Predicate pick = SpecialPred.indicatorToProvable(temp[0], ref, en);
                if (pick == null)
                    return false;

                sa = SpecialUniv.derefAndCastStringWrapped(temp[1], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;
                if (pick.getDef(source) == null)
                    return false;

                Object obj = en.visor.curoutput;
                LoadOpts.checkTextWrite(obj);
                Writer wr = (Writer) obj;
                PrologWriter pw = Foyer.createWriter(Foyer.IO_TERM);
                pw.setWriteUtil(en.store);
                pw.setSource(en.store.user);
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
                Operator oper = SpecialOper.operToSyntax(temp[0], ref, en);
                if (oper == null)
                    return false;

                sa = SpecialUniv.derefAndCastStringWrapped(temp[1], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;
                if (oper.getScope() != source)
                    return false;
                obj = en.visor.curoutput;
                LoadOpts.checkTextWrite(obj);
                wr = (Writer) obj;
                pw = Foyer.createWriter(Foyer.IO_TERM);
                pw.setWriteUtil(en.store);
                pw.setSource(en.store.user);
                pw.setEngineRaw(en);
                pw.setFlags(PrologWriter.FLAG_QUOT | PrologWriter.FLAG_MKDT);
                pw.setSpez(PrologWriter.SPEZ_META);
                pw.setOffset(-1);
                pw.setWriter(wr);
                SpecialLoad.listSyntax(pw, oper, en);
                newLineFlush(wr);
                return en.getNextRaw();
            case SPECIAL_SYS_SHOW_BASE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;

                obj = en.visor.curoutput;
                LoadOpts.checkTextWrite(obj);
                wr = (Writer) obj;

                pw = Foyer.createWriter(Foyer.IO_TERM);
                pw.setWriteUtil(en.store);
                pw.setSource(en.store.user);
                pw.setEngineRaw(en);
                pw.setFlags(PrologWriter.FLAG_QUOT | PrologWriter.FLAG_MKDT);
                pw.setSpez(PrologWriter.SPEZ_META);
                pw.setOffset(-1);
                pw.setWriter(wr);
                SpecialLoad.listBase(pw, source, en);
                newLineFlush(wr);
                return en.getNextRaw();
            case SPECIAL_SYS_HAS_CLAUSE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                pick = SpecialPred.indicatorToProvable(temp[0], ref, en);
                if (pick == null)
                    return false;

                sa = SpecialUniv.derefAndCastStringWrapped(temp[1], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;
                if (pick.getDef(source) == null)
                    return false;

                if (!hasClause(pick, source, en))
                    return false;
                return en.getNextRaw();
            case SPECIAL_SYS_SHORT_BASE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;

                obj = en.visor.curoutput;
                LoadOpts.checkTextWrite(obj);
                wr = (Writer) obj;

                AbstractSource.showShortName(wr, source);
                newLineFlush(wr);
                return en.getNextRaw();
            case SPECIAL_SYS_REGISTER_FILE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                registerFile(sa.scope, sa.fun, sa.getPosition(), en.store);
                return en.getNextRaw();
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
            AbstractInformation[] props = branch.listPredProp(pick);
            for (int k = 0; k < props.length; k++) {
                AbstractInformation prop = props[k];
                if ((prop.getFlags() & AbstractInformation.MASK_PROP_HIDE) != 0)
                    continue;
                if ((prop.getFlags() & AbstractInformation.MASK_PROP_NOCL) != 0 &&
                        hasClause(pick, source, en))
                    continue;
                if ((prop.getFlags() & AbstractInformation.MASK_PROP_SUPR) != 0 &&
                        sameVisible(source, pick, en))
                    continue;
                Object[] vals = SpecialPred.getPropPred(pick, prop, en);
                if ((prop.getFlags() & AbstractInformation.MASK_PROP_SLCF) != 0) {
                    vals = selectFirst(vals, source.getPathAtom());
                } else if ((prop.getFlags() & AbstractInformation.MASK_PROP_PRJF) != 0) {
                    vals = projectFirst(vals);
                } else if ((prop.getFlags() & AbstractInformation.MASK_PROP_DELE) != 0) {
                    vals = delegateSpec(vals, pick, source, en);
                }
                if ((prop.getFlags() & AbstractInformation.MASK_PROP_MODI) != 0) {
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
                        SpecialLoad.flushWriter(pw.getWriter());
                    }
                }
            }
        }
        AbstractDelegate fun = pick.del;
        if (!(fun instanceof AbstractDefined))
            return;

        /* flesh out clauses */
        Clause[] list = ((AbstractDefined) fun).listClauses(en);
        for (int i = 0; i < list.length; i++) {
            Clause clause = list[i];
            SkelAtom sa = StackElement.callableToName(clause.head);
            if (source != sa.scope)
                continue;
            if (modifiers != null) {
                Object decl = InformationIndicator.storeKeyToColonSkel(
                        pick.getFun(), pick.getArity(), source, en);
                decl = prependModifiers(modifiers, decl);
                modifiers = null;
                decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
                decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
                pw.unparseStatement(decl, Display.DISPLAY_CONST);
                SpecialLoad.flushWriter(pw.getWriter());
            }
            Object t = PreClause.intermediateToClause(clause, en);
            pw.setFlags(PrologWriter.FLAG_QUOT | PrologWriter.FLAG_NEWL | PrologWriter.FLAG_MKDT);
            SpecialLoad.showClause(pw, t, clause.vars, en, 0);
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
        SpecialLoad.flushWriter(pw.getWriter());
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
                Branch.OP_USER.equals(src.getFullName())) {
            Object decl = new SkelCompound(new SkelAtom(OP_MODULE),
                    new SkelAtom(Branch.OP_USER),
                    new SkelAtom(Foyer.OP_NIL));
            decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
            pw.unparseStatement(decl, Display.DISPLAY_CONST);
            SpecialLoad.flushWriter(pw.getWriter());
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
            AbstractInformation[] props = branch.listSrcProp();
            for (int k = 0; k < props.length; k++) {
                AbstractInformation prop = props[k];
                if ((prop.getFlags() & AbstractInformation.MASK_PROP_HIDE) != 0)
                    continue;
                if ((prop.getFlags() & AbstractInformation.MASK_PROP_MODL) != 0 &&
                        Branch.OP_USER.equals(src.getFullName()))
                    continue;
                Object[] vals = SpecialSource.getPropSrc(prop, src, en);
                for (int j = 0; j < vals.length; j++) {
                    Object val = vals[j];
                    Object decl = prop.srcDeclSkel(AbstractTerm.getSkel(val), src, en);
                    decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
                    decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
                    pw.unparseStatement(decl, AbstractTerm.getDisplay(val));
                    SpecialLoad.flushWriter(pw.getWriter());
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
                res.add(AbstractTerm.createTerm(new SkelCompound(sc.sym, newargs),
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
     * <p>Check whether the predicate has some clauses.</p>
     *
     * @param pick   The predicate.
     * @param source The scope.
     * @param en     The engine.
     * @return True if the predicate has some clauses, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    private static boolean hasClause(Predicate pick, AbstractSource source,
                                     Engine en)
            throws EngineMessage {
        AbstractDelegate fun = pick.del;
        if (!(fun instanceof AbstractDefined))
            return false;
        Clause[] list = ((AbstractDefined) fun).listClauses(en);
        for (int i = 0; i < list.length; i++) {
            Clause clause = list[i];
            SkelAtom sa = StackElement.callableToName(clause.head);
            if (source != sa.scope)
                continue;
            return true;
        }
        return false;
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
            if (AbstractInformation.indexValue(vals2, vals[i]) == -1)
                return false;
        for (int i = 0; i < vals2.length; i++)
            if (AbstractInformation.indexValue(vals, vals2[i]) == -1)
                return false;
        return true;
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
                                     PositionKey pos, Store store) {
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
            } else if (!rec && t instanceof SkelAtom &&
                    ((SkelAtom) t).fun.equals(EngineMessage.OP_SYSTEM_READ_PROBLEM)) {
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

    /***************************************************************/
    /* Show Clause                                                 */
    /***************************************************************/

    /**
     * <p>List a clause.</p>
     *
     * @param pw    The prolog writer.
     * @param t     The term.
     * @param vars  The var hash
     * @param en    The engine.
     * @param flags The show flags.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public static Display showClause(PrologWriter pw, Object t,
                                     MapHashLink<String, SkelVar> vars,
                                     Engine en, int flags)
            throws EngineException, EngineMessage {
        if ((en.store.foyer.getBits() & Foyer.MASK_FOYER_CEXP) == 0 ||
                ((flags & MASK_SHOW_NRBD) != 0)) {
            int size = EngineCopy.displaySize(t);
            Display ref = (size != 0 ? new Display(Display.newBind(size)) : Display.DISPLAY_CONST);
            EngineVars ev = new EngineVars();
            ev.singsOf(t, ref);
            MapHashLink<Object, NamedDistance> print = SpecialVars.hashToMap(vars, ref, en);
            print = SpecialVars.numberVars(ev.vars, ev.anon, print, flags);
            pw.setPrintMap(print);
            t = new SkelCompound(new SkelAtom(Foyer.OP_CONS), t);
            pw.unparseStatement(t, ref);
            SpecialLoad.flushWriter(pw.getWriter());
            return ref;
        }
        AbstractBind mark = en.bind;
        int snap = en.number;
        int size = EngineCopy.displaySize(t);
        SkelVar res = SkelVar.valueOf(size);
        Display dc = new Display(Display.newBind(size + 1));
        t = new SkelCompound(new SkelAtom("rebuild_term"), t, res);
        t = new SkelCompound(new SkelAtom(SpecialQuali.OP_COLON, en.store.getRootSystem()),
                new SkelAtom("experiment/simp"), t);
        Intermediate r = en.contskel;
        DisplayClause u = en.contdisplay;
        try {
            Clause clause = en.store.foyer.CLAUSE_CALL;
            DisplayClause ref = new DisplayClause();
            ref.bind = DisplayClause.newBindClause(clause.dispsize);
            ref.addArgument(t, dc, en);
            ref.setEngine(en);
            en.contskel = clause.getNextRaw(en);
            en.contdisplay = ref;
            if (!en.runLoop(snap, true))
                throw new EngineMessage(
                        EngineMessage.syntaxError(EngineMessage.OP_SYNTAX_REBUILD_FAILED));
        } catch (EngineMessage x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = new EngineException(x, EngineException.fetchStack(en));
            en.releaseBind(mark);
            throw en.fault;
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.fault = x;
            en.releaseBind(mark);
            throw en.fault;
        }
        en.contskel = r;
        en.contdisplay = u;
        en.window = null;
        en.fault = null;
        en.cutChoices(snap);
        try {
            if (en.fault != null)
                throw en.fault;
            EngineVars ev = new EngineVars();
            ev.singsOf(res, dc);
            MapHashLink<Object, NamedDistance> print = SpecialVars.hashToMap(vars, dc, en);
            print = SpecialVars.numberVars(ev.vars, ev.anon, print, flags);
            pw.setPrintMap(print);
            t = new SkelCompound(new SkelAtom(Foyer.OP_CONS), res);
            pw.unparseStatement(t, dc);
            SpecialLoad.flushWriter(pw.getWriter());
        } catch (EngineMessage y) {
            en.fault = new EngineException(y, EngineException.fetchStack(en));
            en.releaseBind(mark);
            throw en.fault;
        } catch (EngineException x) {
            en.fault = x;
            en.releaseBind(mark);
            throw en.fault;
        }
        en.fault = null;
        en.releaseBind(mark);
        if (en.fault != null)
            throw en.fault;
        return dc;
    }

    /**
     * <p>Flush the writer.</p>
     *
     * @param wr The writer.
     * @throws EngineMessage Shit happens.
     */
    public static void flushWriter(Writer wr)
            throws EngineMessage {
        try {
            wr.flush();
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

}
