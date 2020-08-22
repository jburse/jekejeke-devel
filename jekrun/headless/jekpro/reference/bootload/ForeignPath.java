package jekpro.reference.bootload;

import derek.util.protect.LicenseError;
import jekpro.frequent.stream.ForeignStream;
import jekpro.model.inter.Engine;
import jekpro.model.molec.CacheModule;
import jekpro.model.molec.CacheSubclass;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.*;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.reflect.PropertyStream;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.TermAtomic;
import jekpro.tools.term.TermCompound;
import matula.util.config.FileExtension;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;

import java.io.IOException;
import java.util.Enumeration;

/**
 * <p>The foreign predicates for the module path.</p>
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
public final class ForeignPath {
    /* prefix flags */
    public static final int MASK_PRFX_LIBR = 0x00000001;
    public static final int MASK_PRFX_FRGN = 0x00000002;

    /* suffix flags */
    public static final int MASK_SUFX_TEXT = 0x00000010;
    public static final int MASK_SUFX_BNRY = 0x00000020;
    public static final int MASK_SUFX_RSCS = 0x00000040;
    public static final int MASK_SUFX_BASE = MASK_SUFX_TEXT | MASK_SUFX_BNRY;
    public static final int MASK_SUFX_ALL = MASK_SUFX_BASE | MASK_SUFX_RSCS;

    /* failure flags */
    public static final int MASK_FAIL_READ = 0x00000100;
    public static final int MASK_FAIL_CHLD = 0x00000200;

    /* access flags */
    public static final int MASK_ACES_WRTE = 0x00001000;

    /* combined prefix, suffix and failure flags */
    public static final int MASK_MODL_LIBR = MASK_PRFX_LIBR |
            MASK_SUFX_TEXT;
    public static final int MASK_MODL_FRGN = MASK_PRFX_FRGN |
            MASK_SUFX_BNRY;
    public static final int MASK_MODL_RSCS = MASK_PRFX_LIBR |
            MASK_SUFX_RSCS;

    /* find prefix */
    public static final int MASK_MODL_AUTO = MASK_MODL_LIBR |
            MASK_MODL_FRGN | MASK_FAIL_CHLD;
    public static final int MASK_MODL_VERB = MASK_MODL_LIBR |
            MASK_FAIL_CHLD;

    /* find key */
    public static final int MASK_MODL_BASE = MASK_MODL_LIBR |
            MASK_MODL_FRGN;

    private static final String OP_SEARCH_PATH = "search_path";
    private static final String OP_SEARCH_PATH_ALL = "all";

    private static final String OP_FILE_TYPE = "file_type";
    private static final String OP_FILE_TYPE_TEXT = "text";
    private static final String OP_FILE_TYPE_BINARY = "binary";
    private static final String OP_FILE_TYPE_RESOURCE = "resource";
    private static final String OP_FILE_TYPE_BASE = "base";

    private static final String OP_FAILURE = "failure";
    private static final String OP_FAILURE_CHILD = "child";

    private static final String OP_ACCESS = "access";

    private static final String OP_MIME = "mime";

    private static final String OP_DATA = "data";
    private static final String OP_DATA_CLEAR = "clear";
    private static final String OP_DATA_ENCRYPT = "encrypt";

    private final static int USE_BINARY = 0;
    private final static int USE_TEXT = 1;
    private final static int USE_RESOURCE = 2;
    private final static int USE_PACKAGE = 3;

    private final static String OP_USE = "type";
    private final static String OP_USE_BINARY = "binary";
    private final static String OP_USE_TEXT = "text";
    private final static String OP_USE_PACKAGE = "package";
    private final static String OP_USE_RESOURCE = "resource";

    private static final int DATA_CLEAR = 0;
    private static final int DATA_ENCRYPT = 1;

    /*************************************************************/
    /* Search Write                                              */
    /*************************************************************/

    /**
     * <p>Find a write key according to the auto loader.</p>
     *
     * @param inter The interpreter.
     * @param path  The prefixed path.
     * @return The source key.
     * @throws IOException IO Error.
     */
    public static String sysFindWrite(Interpreter inter,
                                      String path)
            throws IOException {
        Store store = inter.getKnowledgebase().getStore();
        return LookupBase.findWrite(path, store);
    }

    /**
     * <p>Unfind a write key according to the auto loader.</p>
     *
     * @param inter The interpreter.
     * @param path  The prefixed path.
     * @return The source key.
     * @throws IOException IO Error.
     */
    public static String sysUnfindWrite(Interpreter inter,
                                        String path)
            throws IOException {
        Engine engine = inter.getEngine();
        String res = LookupBase.unfindWrite(path, engine);
        if (res != null)
            return res;
        return path;
    }

    /*************************************************************/
    /* Search Prefix                                             */
    /*************************************************************/

    /**
     * <p>Find a prefix according to the auto loader.</p>
     *
     * @param inter The interpreter.
     * @param path  The path.
     * @param key   The call-site.
     * @param mask   The search options flags.
     * @return The prefixed name, or null.
     * @throws InterpreterMessage Shit happens.
     * @throws IOException        IO Error.
     */
    public static String sysFindPrefix(Interpreter inter,
                                       String path, TermAtomic key,
                                       Integer mask)
            throws InterpreterMessage, IOException {
        Engine engine = inter.getEngine();
        AbstractSource scope;
        try {
            SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(key.getSkel(), key.getDisplay());
            if (!"".equals(sa.fun)) {
                scope = (sa.scope != null ? sa.scope : engine.store.user);
                scope = scope.getStore().getSource(sa.fun);
                AbstractSource.checkExistentSource(scope, sa);
            } else {
                scope = engine.store.user;
            }
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        return CacheModule.findPrefix(path, scope, mask.intValue());
    }

    /**
     * <p>Unfind a prefix according to the auto loader.</p>
     *
     * @param inter The interpreter.
     * @param path  The path.
     * @param key   The call-site.
     * @param mask   The search options flags.
     * @return The prefixed name, or null.
     * @throws InterpreterMessage Shit happens.
     * @throws IOException        IO Error.
     */
    public static Object sysUnfindPrefix(Interpreter inter,
                                         String path, TermAtomic key,
                                         Integer mask)
            throws InterpreterMessage, IOException {
        Engine engine = inter.getEngine();
        AbstractSource scope;
        try {
            SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(key.getSkel(), key.getDisplay());
            if (!"".equals(sa.fun)) {
                scope = (sa.scope != null ? sa.scope : engine.store.user);
                scope = scope.getStore().getSource(sa.fun);
                AbstractSource.checkExistentSource(scope, sa);
            } else {
                scope = engine.store.user;
            }
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        return CacheModule.unfindPrefix(path, scope, mask.intValue());
    }

    /*************************************************************/
    /* Search Key                                                */
    /*************************************************************/

    /**
     * <p>Find a read key according to the auto loader.</p>
     *
     * @param inter The interpreter.
     * @param path  The prefixed path.
     * @param key   The call-site.
     * @param mask   The search options flags.
     * @return The source key.
     * @throws InterpreterMessage Shit happens.
     */
    public static String sysFindKey(Interpreter inter,
                                    String path, TermAtomic key,
                                    Integer mask)
            throws InterpreterMessage, IOException {
        Engine engine = inter.getEngine();
        AbstractSource scope;
        try {
            SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(key.getSkel(), key.getDisplay());
            if (!"".equals(sa.fun)) {
                scope = (sa.scope != null ? sa.scope : engine.store.user);
                scope = scope.getStore().getSource(sa.fun);
                AbstractSource.checkExistentSource(scope, sa);
            } else {
                scope = engine.store.user;
            }
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        return CacheSubclass.findKey(path, scope, mask.intValue(), engine);
    }

    /**
     * <p>Unfind a key according to the auto loader.</p>
     *
     * @param inter The interpreter.
     * @param path  The prefixed path.
     * @param key   The call-site.
     * @param mask   The search options flags.
     * @return The source key.
     * @throws InterpreterMessage Shit happens.
     * @throws IOException        Shit happens.
     */
    public static Object sysUnfindKey(Interpreter inter,
                                      String path, TermAtomic key,
                                      Integer mask)
            throws InterpreterMessage, IOException {
        Engine engine = inter.getEngine();
        AbstractSource scope;
        try {
            SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(key.getSkel(), key.getDisplay());
            if (!"".equals(sa.fun)) {
                scope = (sa.scope != null ? sa.scope : engine.store.user);
                scope = scope.getStore().getSource(sa.fun);
                AbstractSource.checkExistentSource(scope, sa);
            } else {
                scope = engine.store.user;
            }
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        try {
            return CacheSubclass.unfindKey(path, scope, mask.intValue(), engine);
        } catch (LicenseError x) {
            throw new InterpreterMessage(
                    InterpreterMessage.licenseError(x.getMessage()));
        }
    }

    /****************************************************************/
    /* Find Options                                                 */
    /****************************************************************/

    /**
     * <p>Decode the find options.</p>
     *
     * @param opt The find options term.
     * @return The find options.
     */
    public static int sysSearchOptions(Object opt)
            throws InterpreterMessage {
        int mask = MASK_SUFX_ALL + MASK_FAIL_READ;
        while (opt instanceof TermCompound &&
                ((TermCompound) opt).getArity() == 2 &&
                ((TermCompound) opt).getFunctor().equals(
                        Knowledgebase.OP_CONS)) {
            Object temp = ((TermCompound) opt).getArg(0);
            if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_SEARCH_PATH)) {
                Object help = ((TermCompound) temp).getArg(0);
                String fun = InterpreterMessage.castString(help);
                if (fun.equals(ReadOpts.OP_TERMINATOR_NONE)) {
                    mask &= ~MASK_PRFX_LIBR;
                    mask &= ~MASK_PRFX_FRGN;
                } else if (fun.equals(LoadOpts.OP_PREFIX_LIBRARY)) {
                    mask |= MASK_PRFX_LIBR;
                    mask &= ~MASK_PRFX_FRGN;
                } else if (fun.equals(LoadOpts.OP_PREFIX_FOREIGN)) {
                    mask &= ~MASK_PRFX_LIBR;
                    mask |= MASK_PRFX_FRGN;
                } else if (fun.equals(OP_SEARCH_PATH_ALL)) {
                    mask |= MASK_PRFX_LIBR;
                    mask |= MASK_PRFX_FRGN;
                } else {
                    throw new InterpreterMessage(
                            InterpreterMessage.domainError("fix_option", help));
                }
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_FILE_TYPE)) {
                Object help = ((TermCompound) temp).getArg(0);
                String fun = InterpreterMessage.castString(help);
                if (fun.equals(ReadOpts.OP_TERMINATOR_NONE)) {
                    mask &= ~MASK_SUFX_TEXT;
                    mask &= ~MASK_SUFX_BNRY;
                    mask &= ~MASK_SUFX_RSCS;
                } else if (fun.equals(OP_FILE_TYPE_TEXT)) {
                    mask |= MASK_SUFX_TEXT;
                    mask &= ~MASK_SUFX_BNRY;
                    mask &= ~MASK_SUFX_RSCS;
                } else if (fun.equals(OP_FILE_TYPE_BINARY)) {
                    mask &= ~MASK_SUFX_TEXT;
                    mask |= MASK_SUFX_BNRY;
                    mask &= ~MASK_SUFX_RSCS;
                } else if (fun.equals(OP_FILE_TYPE_RESOURCE)) {
                    mask &= ~MASK_SUFX_TEXT;
                    mask &= ~MASK_SUFX_BNRY;
                    mask |= MASK_SUFX_RSCS;
                } else if (fun.equals(OP_FILE_TYPE_BASE)) {
                    mask |= MASK_SUFX_TEXT;
                    mask |= MASK_SUFX_BNRY;
                    mask &= ~MASK_SUFX_RSCS;
                } else if (fun.equals(OP_SEARCH_PATH_ALL)) {
                    mask |= MASK_SUFX_TEXT;
                    mask |= MASK_SUFX_BNRY;
                    mask |= MASK_SUFX_RSCS;
                } else {
                    throw new InterpreterMessage(
                            InterpreterMessage.domainError("fix_option", help));
                }
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_FAILURE)) {
                Object help = ((TermCompound) temp).getArg(0);
                String fun = InterpreterMessage.castString(help);
                if (fun.equals(ReadOpts.OP_TERMINATOR_NONE)) {
                    mask &= ~MASK_FAIL_READ;
                    mask &= ~MASK_FAIL_CHLD;
                } else if (fun.equals(PropertyStream.OP_MODE_READ)) {
                    mask |= MASK_FAIL_READ;
                    mask &= ~MASK_FAIL_CHLD;
                } else if (fun.equals(OP_FAILURE_CHILD)) {
                    mask &= ~MASK_FAIL_READ;
                    mask |= MASK_FAIL_CHLD;
                } else if (fun.equals(OP_SEARCH_PATH_ALL)) {
                    mask |= MASK_FAIL_READ;
                    mask |= MASK_FAIL_CHLD;
                } else {
                    throw new InterpreterMessage(InterpreterMessage.domainError(
                            "fix_option", help));
                }
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_ACCESS)) {
                Object help = ((TermCompound) temp).getArg(0);
                String fun = InterpreterMessage.castString(help);
                if (fun.equals(PropertyStream.OP_MODE_READ)) {
                    mask &= ~MASK_ACES_WRTE;
                } else if (fun.equals(PropertyStream.OP_MODE_WRITE)) {
                    mask |= MASK_ACES_WRTE;
                } else {
                    throw new InterpreterMessage(InterpreterMessage.domainError(
                            "fix_option", help));
                }
            } else {
                InterpreterMessage.checkInstantiated(temp);
                throw new InterpreterMessage(InterpreterMessage.domainError(
                        "fix_option", temp));
            }
            opt = ((TermCompound) opt).getArg(1);
        }
        if (opt.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            InterpreterMessage.checkInstantiated(opt);
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    InterpreterMessage.OP_TYPE_LIST, opt));
        }
        return mask;
    }

    /**
     * <p>Check if the search options is read.</p>
     *
     * @param mask The search options flags.
     * @return True if the search options is read, otherwise false.
     */
    public static boolean sysSearchRead(Integer mask) {
        return (mask.intValue() & MASK_ACES_WRTE) == 0;
    }

    /********************************************************/
    /* Class Path Modification & Access                     */
    /********************************************************/

    /**
     * <p>Add a class path.</p>
     *
     * @param inter The interpreter.
     * @param path  The class path.
     * @throws InterpreterMessage Validation error.
     */
    public static void sysAddClassdPath(Interpreter inter, String path)
            throws InterpreterMessage {
        inter.getKnowledgebase().addClassPath(path);
    }

    /**
     * <p>Retrieve the class paths along the class paths.</p>
     *
     * @param inter The interpreter.
     * @return The list of class paths.
     */
    public static Object sysGetClassPaths(Interpreter inter)
            throws InterpreterMessage {
        Knowledgebase know = inter.getKnowledgebase();
        Object end = know.getTermNil();
        while (know != null) {
            String[] paths = know.getClassPaths();
            for (int i = paths.length - 1; i >= 0; i--)
                end = new TermCompound(know.getTermCons(), paths[i], end);
            know = know.getParent();
        }
        return end;
    }

    /**
     * <p>Add a file extension.</p>
     *
     * @param inter The interpreter.
     * @param e     The file extension.
     * @param opt   The type and mime options.
     * @throws InterpreterMessage Shit happens.
     */
    public static void sysAddFileExtenstion(Interpreter inter,
                                            String e, Object opt)
            throws InterpreterMessage {
        FileExtension fe = decodeFileExtension(opt);
        inter.getKnowledgebase().addFileExtension(e, fe);
    }

    /**
     * <p>Decode type and mime options.</p>
     *
     * @param opt The type and mime options.
     * @return The type and mime options.
     * @throws InterpreterMessage Shit happens.
     */
    private static FileExtension decodeFileExtension(Object opt)
            throws InterpreterMessage {
        int type = FileExtension.MASK_USES_TEXT;
        String mime = null;
        while (opt instanceof TermCompound &&
                ((TermCompound) opt).getArity() == 2 &&
                ((TermCompound) opt).getFunctor().equals(Knowledgebase.OP_CONS)) {
            Object temp = ((TermCompound) opt).getArg(0);
            if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_USE)) {
                Object help = ((TermCompound) temp).getArg(0);
                type &= ~FileExtension.MASK_PCKG_LOAD;
                type &= ~FileExtension.MASK_USES_SUFX;
                switch (ForeignPath.atomToUse(help)) {
                    case USE_BINARY:
                        type |= FileExtension.MASK_USES_BNRY;
                        break;
                    case USE_TEXT:
                        type |= FileExtension.MASK_USES_TEXT;
                        break;
                    case USE_RESOURCE:
                        type |= FileExtension.MASK_USES_RSCS;
                        break;
                    case USE_PACKAGE:
                        type |= FileExtension.MASK_PCKG_LOAD;
                        break;
                    default:
                        throw new InterpreterMessage(InterpreterMessage.domainError(
                                InterpreterMessage.OP_DOMAIN_FLAG_VALUE, help));
                }
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_MIME)) {
                Object help = ((TermCompound) temp).getArg(0);
                String str = InterpreterMessage.castString(help);
                mime = (!"".equals(str) ? str : null);
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_DATA)) {
                Object help = ((TermCompound) temp).getArg(0);
                switch (atomToData(help)) {
                    case DATA_CLEAR:
                        type &= ~FileExtension.MASK_DATA_ECRY;
                        break;
                    case DATA_ENCRYPT:
                        type |= FileExtension.MASK_DATA_ECRY;
                        break;
                    default:
                        throw new InterpreterMessage(InterpreterMessage.domainError(
                                InterpreterMessage.OP_DOMAIN_FLAG_VALUE, help));
                }
            } else {
                InterpreterMessage.checkInstantiated(temp);
                throw new InterpreterMessage(InterpreterMessage.domainError(
                        ForeignStream.OP_OPEN_OPTION, temp));
            }
            opt = ((TermCompound) opt).getArg(1);
        }
        if (opt.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            InterpreterMessage.checkInstantiated(opt);
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    InterpreterMessage.OP_TYPE_LIST, opt));
        }
        if (mime != null) {
            return new FileExtension(type, mime);
        } else {
            return new FileExtension(type);
        }
    }

    /**
     * <p>Remove a file extension.</p>
     *
     * @param inter The interpreter.
     * @param e     The file extension.
     */
    public static void sysRemoveFileExtenstion(Interpreter inter,
                                               String e) {
        inter.getKnowledgebase().removeFileExtension(e);
    }

    /**
     * <p>Retrieve the file extensions along the knowledge bases.</p>
     *
     * @param co    The call-out.
     * @param inter The interpreter.
     * @return The list of class paths.
     */
    public static Object sysGetFileExtenstions(CallOut co, Interpreter inter) {
        Enumeration<Object> at;
        if (co.getFirst()) {
            ListArray<Object> help = listFileExtensions(inter);
            at = help.elements();
            if (!at.hasMoreElements())
                return null;
            co.setData(at);
        } else {
            at = (Enumeration<Object>) co.getData();
        }
        Object val = at.nextElement();
        co.setRetry(at.hasMoreElements());
        return val;
    }

    /**
     * <p>Collect the file extensions.</p>
     *
     * @param inter The interpreter.
     * @return The file extensions.
     */
    private static ListArray<Object> listFileExtensions(Interpreter inter) {
        Knowledgebase know = inter.getKnowledgebase();
        ListArray<Object> res = new ListArray<Object>();
        do {
            MapEntry<String, FileExtension>[] exts = know.getFileExtensions();
            for (int i = exts.length - 1; i >= 0; i--) {
                MapEntry<String, FileExtension> ext = exts[i];
                Object val = encodeFileExtension(inter, ext.value);
                val = new TermCompound(know.getTermSub(), ext.key, val);
                res.add(val);
            }
            know = know.getParent();
        } while (know != null);
        return res;
    }

    /**
     * <p>Encode type and mime options.</p>
     *
     * @param inter The interpreter.
     * @param fe    The type and mime options.
     * @return The type and mime options.
     */
    private static Object encodeFileExtension(Interpreter inter, FileExtension fe) {
        Knowledgebase know = inter.getKnowledgebase();
        Object end = know.getTermNil();
        if (fe.getMimeType() != null) {
            Object val = new TermCompound(OP_MIME, fe.getMimeType());
            end = new TermCompound(know.getTermCons(), val, end);
        }
        if ((fe.getType() & FileExtension.MASK_USES_BNRY) != 0) {
            Object val = new TermCompound(OP_USE, OP_USE_BINARY);
            end = new TermCompound(know.getTermCons(), val, end);
        } else if ((fe.getType() & FileExtension.MASK_USES_RSCS) != 0) {
            Object val = new TermCompound(OP_USE, OP_USE_RESOURCE);
            end = new TermCompound(know.getTermCons(), val, end);
        } else if ((fe.getType() & FileExtension.MASK_PCKG_LOAD) != 0) {
            Object val = new TermCompound(OP_USE, OP_USE_PACKAGE);
            end = new TermCompound(know.getTermCons(), val, end);
        }
        if ((fe.getType() & FileExtension.MASK_DATA_ECRY) != 0) {
            Object val = new TermCompound(OP_DATA, OP_DATA_ENCRYPT);
            end = new TermCompound(know.getTermCons(), val, end);
        }
        return end;
    }

    /**
     * <p>Convert an atom to a type. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param t The type term.
     * @return The type value.
     * @throws InterpreterMessage Validation error.
     */
    public static int atomToData(Object t) throws InterpreterMessage {
        String val = InterpreterMessage.castString(t);
        if (val.equals(OP_DATA_CLEAR)) {
            return DATA_CLEAR;
        } else if (val.equals(OP_DATA_ENCRYPT)) {
            return DATA_ENCRYPT;
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    InterpreterMessage.OP_DOMAIN_FLAG_VALUE, t));
        }
    }


    /**
     * <p>Convert an atom to a type. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param t The type term.
     * @return The type value.
     * @throws InterpreterMessage Validation error.
     */
    public static int atomToUse(Object t) throws InterpreterMessage {
        String val = InterpreterMessage.castString(t);
        if (val.equals(OP_USE_BINARY)) {
            return USE_BINARY;
        } else if (val.equals(OP_USE_TEXT)) {
            return USE_TEXT;
        } else if (val.equals(OP_USE_RESOURCE)) {
            return USE_RESOURCE;
        } else if (val.equals(OP_USE_PACKAGE)) {
            return USE_PACKAGE;
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    InterpreterMessage.OP_DOMAIN_FLAG_VALUE, t));
        }
    }

}
