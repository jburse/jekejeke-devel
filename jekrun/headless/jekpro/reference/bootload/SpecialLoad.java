package jekpro.reference.bootload;

import derek.util.protect.LicenseError;
import jekpro.frequent.standard.EngineCopy;
import jekpro.model.builtin.*;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.*;
import jekpro.model.rope.*;
import jekpro.reference.reflect.*;
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
import matula.util.data.MapHash;
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
    public final static String OP_SET_PROLOG_FLAG = "set_prolog_flag";

    /**
     * <p>Create a load special.</p>
     *
     * @param i The id.
     */
    public SpecialLoad(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a term in a list of goals for the first time.</p>
     * <p>The term is passed via the skel and display of the engine.</p>
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
                return true;
            case SPECIAL_SYS_DETACH_FILE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                opts = new LoadOpts();
                opts.decodeLoadOpts(temp[1], ref, en);
                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                opts.makeUnload(source, sa.fun, en);
                return true;
            case SPECIAL_SYS_IMPORT_FILE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                opts = new LoadOpts();
                opts.decodeLoadOpts(temp[1], ref, en);
                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                SpecialLoad.performImport(source, sa.fun, en, opts);
                return true;
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
                PrologWriter pw = en.store.foyer.createWriter(Foyer.IO_TERM);
                pw.setSource(en.visor.peekStack());
                pw.setEngineRaw(en);
                pw.setFlags(pw.getFlags() | (PrologWriter.FLAG_QUOT | PrologWriter.FLAG_NEWL | PrologWriter.FLAG_MKDT));
                pw.setSpez(PrologWriter.SPEZ_META);
                pw.setOffset(-1);
                pw.setWriter(wr);
                SpecialLoad.listProvable(pw, pick, source, en);
                newLineFlush(wr);
                return true;
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
                pw = en.store.foyer.createWriter(Foyer.IO_TERM);
                pw.setSource(en.visor.peekStack());
                pw.setEngineRaw(en);
                pw.setFlags(pw.getFlags() | (PrologWriter.FLAG_QUOT | PrologWriter.FLAG_NEWL | PrologWriter.FLAG_MKDT));
                pw.setSpez(PrologWriter.SPEZ_META);
                pw.setOffset(-1);
                pw.setWriter(wr);
                SpecialLoad.listSyntax(pw, oper, source, en);
                newLineFlush(wr);
                return true;
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

                pw = en.store.foyer.createWriter(Foyer.IO_TERM);
                pw.setSource(en.visor.peekStack());
                pw.setEngineRaw(en);
                pw.setFlags(pw.getFlags() | (PrologWriter.FLAG_QUOT | PrologWriter.FLAG_NEWL | PrologWriter.FLAG_MKDT));
                pw.setSpez(PrologWriter.SPEZ_META);
                pw.setOffset(-1);
                pw.setWriter(wr);
                SpecialLoad.listBase(pw, source, en);
                newLineFlush(wr);
                return true;
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
                return true;
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
                return true;
            case SPECIAL_SYS_REGISTER_FILE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                registerFile(sa.scope, sa.fun, sa.getPosition(), en.store);
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>List a predicate.</p>
     * <p>System clauses are excluded.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param pw   The prolog writer.
     * @param pick The predicate.
     * @param src  The source, not null.
     * @param en   The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void listProvable(PrologWriter pw, Predicate pick,
                                     AbstractSource src, Engine en)
            throws EngineMessage, EngineException {
        if (pick.del == null)
            return;
        /* flesh out properties */
        ListArray<SkelAtom> modifiers = null;
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHashLink<StoreKey, AbstractProperty<Predicate>> props = branch.getPredProps();
            for (MapEntry<StoreKey, AbstractProperty<Predicate>> entry2 =
                 (props != null ? props.getFirstEntry() : null);
                 entry2 != null; entry2 = props.successor(entry2)) {
                AbstractProperty<Predicate> prop = entry2.value;
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_SHOW) == 0)
                    continue;
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_DEFL) != 0 &&
                        hasClause(pick, src, en))
                    continue;
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_SUPR) != 0 &&
                        sameVisiblePredicate(src, pick, en))
                    continue;
                Object[] vals = prop.getObjProps(pick, en);
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_SLCF) != 0) {
                    vals = selectFirst(vals, src.getPathAtom());
                } else if ((prop.getFlags() & AbstractProperty.MASK_PROP_DELE) != 0) {
                    vals = delegateSpec(vals, pick, src);
                }
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_MODI) != 0) {
                    for (int j = 0; j < vals.length; j++) {
                        Object val = vals[j];
                        if ((prop.getFlags() & AbstractProperty.MASK_PROP_PRJF) != 0)
                            val = firstArg(val);
                        if (modifiers == null)
                            modifiers = new ListArray<SkelAtom>();
                        modifiers.add((SkelAtom) AbstractTerm.getSkel(val));
                    }
                } else {
                    for (int j = 0; j < vals.length; j++) {
                        Object val = vals[j];
                        Object decl;
                        if ((prop.getFlags() & AbstractProperty.MASK_PROP_SETP) != 0) {
                            decl = SpecialModel.predDeclSkelSet(
                                    AbstractTerm.getSkel(val), pick, src);
                        } else if ((prop.getFlags() & AbstractProperty.MASK_PROP_META) != 0) {
                            decl = SpecialModel.predDeclSkelMeta(
                                    AbstractTerm.getSkel(val), pick, src);
                        } else {
                            if ((prop.getFlags() & AbstractProperty.MASK_PROP_PRJF) != 0)
                                val = firstArg(val);
                            decl = SpecialModel.predDeclSkelIndicator(
                                    AbstractTerm.getSkel(val), pick, src);
                        }
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
            SkelAtom sa = StackElement.callableToName(clause.term);
            if (src != sa.scope)
                continue;
            if (modifiers != null) {
                Object decl = SpecialModel.provableToColonSkel(pick, src);
                decl = prependModifiers(modifiers, decl);
                modifiers = null;
                decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
                decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
                pw.unparseStatement(decl, Display.DISPLAY_CONST);
                SpecialLoad.flushWriter(pw.getWriter());
            }
            Object t = PreClause.intermediateToClause(clause, en);
            pw.setSource(src);
            SpecialLoad.showClause(pw, t, clause.vars, en, 0);
            pw.setSource(en.visor.peekStack());
        }
    }

    /**
     * <p>Check whether a source and a predicate have the same visibility.</p>
     *
     * @param src  The source.
     * @param pick The predicate.
     * @param en   The engine.
     * @return True if the source and the predicate have the same visibility, otherwise false.
     */
    private static boolean sameVisiblePredicate(AbstractSource src, Predicate pick,
                                                Engine en)
            throws EngineMessage, EngineException {
        StoreKey sk = new StoreKey(PropertySource.OP_SYS_SOURCE_VISIBLE, 1);
        AbstractProperty<AbstractSource> prop = SpecialSource.findSrcProperty(sk, en);
        Object[] vals = prop.getObjProps(src, en);
        StoreKey sk2 = new StoreKey(PropertyPredicate.OP_VISIBLE, 1);
        AbstractProperty<Predicate> prop1 = SpecialPred.findPredProperty(sk2, en);
        Object[] vals2 = prop1.getObjProps(pick, en);
        return sameValues(vals, vals2);
    }

    /**
     * <p>List the syntax operator.</p>
     *
     * @param pw   The prolog writer.
     * @param oper The operator.
     * @param src  The source, not null.
     * @param en   The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void listSyntax(PrologWriter pw,
                                   Operator oper,
                                   AbstractSource src,
                                   Engine en)
            throws EngineMessage, EngineException {
        if (oper.getLevel() == 0)
            return;
        /* flesh out properties */
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHashLink<StoreKey, AbstractProperty<Operator>> props = branch.getOperProps();
            for (MapEntry<StoreKey, AbstractProperty<Operator>> entry2 =
                 (props != null ? props.getFirstEntry() : null);
                 entry2 != null; entry2 = props.successor(entry2)) {
                AbstractProperty<Operator> prop = entry2.value;
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_SHOW) == 0)
                    continue;
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_SUPR) != 0 &&
                        sameVisibleOper(src, oper, en))
                    continue;
                Object[] vals = prop.getObjProps(oper, en);
                for (int j = 0; j < vals.length; j++) {
                    Object val = vals[j];
                    Object decl;
                    if ((prop.getFlags() & AbstractProperty.MASK_PROP_SETP) != 0) {
                        decl = SpecialModel.operDeclSkelSet(
                                AbstractTerm.getSkel(val), oper, src);
                    } else if ((prop.getFlags() & AbstractProperty.MASK_PROP_META) != 0) {
                        decl = SpecialModel.operDeclSkelOp(
                                AbstractTerm.getSkel(val), oper, src);
                    } else {
                        if ((prop.getFlags() & AbstractProperty.MASK_PROP_PRJF) != 0)
                            val = firstArg(val);
                        decl = SpecialModel.operDeclSkelIndicator(
                                AbstractTerm.getSkel(val), oper, src);
                    }
                    decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
                    decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
                    pw.unparseStatement(decl, Display.DISPLAY_CONST);
                    SpecialLoad.flushWriter(pw.getWriter());
                }
            }
        }
    }

    /**
     * <p>Check whether a source and an operator have the same visibility.</p>
     *
     * @param src  The source.
     * @param oper The operator.
     * @param en   The engine.
     * @return True if the source and the operator have the same visibility, otherwise false.
     */
    private static boolean sameVisibleOper(AbstractSource src, Operator oper,
                                           Engine en)
            throws EngineMessage, EngineException {
        StoreKey sk = new StoreKey(PropertySource.OP_SYS_SOURCE_VISIBLE, 1);
        AbstractProperty<AbstractSource> prop = SpecialSource.findSrcProperty(sk, en);
        Object[] vals = prop.getObjProps(src, en);
        StoreKey sk2 = new StoreKey(PropertyPredicate.OP_VISIBLE, 1);
        AbstractProperty<Operator> prop1 = SpecialOper.findOperProperty(sk2, en);
        Object[] vals2 = prop1.getObjProps(oper, en);
        return sameValues(vals, vals2);
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
                (src.getBits() & AbstractSource.MASK_SRC_VISI) == 0 &&
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
            MapHash<StoreKey, AbstractProperty<AbstractSource>> props = branch.getSrcProps();
            for (MapEntry<StoreKey, AbstractProperty<AbstractSource>> entry2 =
                 (props != null ? props.getLastEntry() : null);
                 entry2 != null; entry2 = props.predecessor(entry2)) {
                AbstractProperty<AbstractSource> prop = entry2.value;
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_SHOW) == 0)
                    continue;
                if ((prop.getFlags() & AbstractProperty.MASK_PROP_DEFL) != 0 &&
                        Branch.OP_USER.equals(src.getFullName()))
                    continue;
                StoreKey sk = entry2.key;
                Object[] vals = prop.getObjProps(src, en);
                for (int j = 0; j < vals.length; j++) {
                    Object val = vals[j];
                    Object decl = srcDecl(sk, AbstractTerm.getSkel(val), src, en);
                    decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
                    decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
                    pw.unparseStatement(decl, AbstractTerm.getDisplay(val));
                    SpecialLoad.flushWriter(pw.getWriter());
                }
            }
        }

        if (src.utildouble != ReadOpts.UTIL_CODES) {
            Object val = ReadOpts.utilToAtom(src.utildouble);
            Object decl = new SkelCompound(new SkelAtom(OP_SET_PROLOG_FLAG),
                    new SkelAtom(Flag.OP_DOUBLE_QUOTES), val);
            decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
            pw.unparseStatement(decl, AbstractTerm.getDisplay(val));
            SpecialLoad.flushWriter(pw.getWriter());
        }
        if (src.utilback != ReadOpts.UTIL_ERROR) {
            Object val = ReadOpts.utilToAtom(src.utilback);
            Object decl = new SkelCompound(new SkelAtom(OP_SET_PROLOG_FLAG),
                    new SkelAtom(Flag.OP_BACK_QUOTES), val);
            decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
            pw.unparseStatement(decl, AbstractTerm.getDisplay(val));
            SpecialLoad.flushWriter(pw.getWriter());
        }
        if (src.utilsingle != ReadOpts.UTIL_ATOM) {
            Object val = ReadOpts.utilToAtom(src.utilsingle);
            Object decl = new SkelCompound(new SkelAtom(OP_SET_PROLOG_FLAG),
                    new SkelAtom(Flag.OP_SINGLE_QUOTES), val);
            decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
            pw.unparseStatement(decl, AbstractTerm.getDisplay(val));
            SpecialLoad.flushWriter(pw.getWriter());
        }
        if ((src.getBits() & AbstractSource.MASK_SRC_NSTY) != 0) {
            Object val = AbstractFlag.switchToAtom(false);
            Object decl = new SkelCompound(new SkelAtom(OP_SET_PROLOG_FLAG),
                    new SkelAtom(Flag.OP_STYLE_CHECK), val);
            decl = new SkelCompound(new SkelAtom(PreClause.OP_TURNSTILE), decl);
            decl = new SkelCompound(new SkelAtom(Foyer.OP_CONS), decl);
            pw.unparseStatement(decl, AbstractTerm.getDisplay(val));
            SpecialLoad.flushWriter(pw.getWriter());
        }
    }

    /**
     * <p>Determine the declaration term.</p>
     *
     * @param sk     The property name.
     * @param skel   The property value.
     * @param source The source.
     * @param en     The engine.
     * @return The declaration term.
     */
    private static Object srcDecl(StoreKey sk, Object skel,
                                  AbstractSource source, Engine en)
            throws EngineMessage {
        if (sk.getFun().equals(PropertySource.OP_SYS_LINK) && sk.getArity() == 2)
            return PropertySource.shortLink(skel, source, en);
        if (sk.getFun().equals(PropertySource.OP_SYS_SOURCE_NAME) && sk.getArity() == 1)
            return PropertySource.shortModule(skel, en);
        return skel;
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
            reader = source.openReader(false, opts);
            scope.loadModule(reader, en, true);
            scope.closeReader(reader);
        } else {
            Object obj = en.visor.curinput;
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
                res.add(AbstractTerm.createMolec(new SkelCompound(sc.sym, newargs),
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
            SkelAtom sa = StackElement.callableToName(clause.term);
            if (source != sa.scope)
                continue;
            return true;
        }
        return false;
    }


    /**
     * <p>Convert the value list to its spec.</p>
     *
     * @param vals   The value list only buit_in.
     * @param pick   The predicate.
     * @param source The source, not null.
     * @return The value list with spec.
     */
    private static Object[] delegateSpec(Object[] vals, Predicate pick,
                                         AbstractSource source)
            throws EngineMessage {
        if (vals.length == 0)
            return vals;
        Object[] newvals = new Object[vals.length];
        for (int i = 0; i < vals.length; i++) {
            Object val = pick.del.toSpec(source);
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
            Display ref = AbstractSkel.createDisplay(t);
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
        AbstractUndo mark = en.bind;
        int snap = en.number;
        int size = EngineCopy.displaySize(t);
        SkelVar res = SkelVar.valueOf(size);
        t = new SkelCompound(new SkelAtom("rebuild_term"), t, res);
        t = new SkelCompound(new SkelAtom(SpecialQuali.OP_COLON, en.store.getRootSystem()),
                new SkelAtom("experiment/simp"), t);
        Display dc = AbstractSkel.createDisplay(t);
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        try {
            Clause clause = en.store.foyer.CLAUSE_CALL;
            CallFrame ref = new CallFrame(clause.dispsize);
            ref.setClause(clause);
            ref.setArg(0, t, dc, en);
            ref.setEngine(en);
            en.contskel = clause;
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

    /*******************************************************************/
    /* Property Utilities                                              */
    /*******************************************************************/

    /**
     * <p>Check whether the two value lists contain the same elements.</p>
     *
     * @param vals  The first value list.
     * @param vals2 The second value list.
     * @return True if both value lists contain the same elements, otherwise false.
     */
    private static boolean sameValues(Object[] vals, Object[] vals2) {
        if (vals.length != vals2.length)
            return false;
        for (int i = 0; i < vals.length; i++) {
            Object val = firstArg(vals[i]);
            Object val2 = firstArg(vals2[i]);
            if (!val.equals(val2))
                return false;
        }
        return true;
    }

    /**
     * <p>Return the first arg of the value.</p>
     *
     * @param val The value.
     * @return The first arg.
     */
    private static Object firstArg(Object val) {
        SkelCompound sc = (SkelCompound) AbstractTerm.getSkel(val);
        return AbstractTerm.createMolec(sc.args[0], Display.DISPLAY_CONST);
    }

}
