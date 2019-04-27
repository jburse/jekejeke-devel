package jekpro.reference.bootload;

import jekpro.frequent.stream.ForeignStream;
import jekpro.model.inter.Engine;
import jekpro.model.molec.CacheModule;
import jekpro.model.molec.CacheSubclass;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.LookupBase;
import jekpro.model.pretty.Store;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.*;
import matula.util.config.FileExtension;
import matula.util.data.MapEntry;

import java.io.IOException;

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
    /* prefix relationship flags */
    public static final int MASK_PRFX_LIBR = 0x00000001;
    public static final int MASK_PRFX_FRGN = 0x00000002;

    /* suffix relationship flags */
    public static final int MASK_SUFX_TEXT = 0x00000010;
    public static final int MASK_SUFX_BNRY = 0x00000020;
    public static final int MASK_SUFX_RSCS = 0x00000040;

    /* failure relationship flags */
    public static final int MASK_FAIL_READ = 0x00000100;
    public static final int MASK_FAIL_CHLD = 0x00000200;

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

    private static final String OP_PACKAGE = "package";
    private static final String OP_PACKAGE_NONE = "none";
    private static final String OP_PACKAGE_BOTH = "both";

    private static final String OP_FILE_EXTENSION = "file_extension";
    private static final String OP_FILE_EXTENSION_FILE = "file";
    private static final String OP_FILE_EXTENSION_RESOURCE = "resource";

    private static final String OP_FAILURE = "failure";
    private static final String OP_FAILURE_READ = "read";
    private static final String OP_FAILURE_CHILD = "child";

    private static final String OP_MIME = "mime";

    private static final String OP_DATA = "data";
    private static final String OP_DATA_PLAIN = "plain";
    private static final String OP_DATA_ENCRYPT = "encrypt";

    private static final int DATA_PLAIN = 0;
    private static final int DATA_ENCRYPT = 1;

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
        Store store = (Store) inter.getKnowledgebase().getStore();
        return LookupBase.findWrite(path, store);
    }

    /**
     * <p>Find a prefix according to the auto loader.</p>
     *
     * @param inter The interpreter.
     * @param path  The path.
     * @param key   The call-site.
     * @param opt   The options list.
     * @return The prefixed name, or null.
     * @throws InterpreterMessage Shit happens.
     * @throws IOException        IO Error.
     */
    public static String sysFindPrefix(Interpreter inter,
                                       String path, TermAtomic key,
                                       Object opt)
            throws InterpreterMessage, IOException {
        int mask = decodeFindOptions(opt);
        Engine engine = (Engine) inter.getEngine();
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
        return CacheModule.findPrefix(path, scope, mask);
    }

    /**
     * <p>Unfind a prefix according to the auto loader.</p>
     *
     * @param inter The interpreter.
     * @param path  The path.
     * @param key   The call-site.
     * @param opt   The options list.
     * @return The prefixed name, or null.
     * @throws InterpreterMessage Shit happens.
     * @throws IOException        IO Error.
     */
    public static Object sysUnfindPrefix(Interpreter inter,
                                         String path, TermAtomic key,
                                         Object opt)
            throws InterpreterMessage, IOException {
        int mask = decodeFindOptions(opt);
        Object res;
        Engine engine = (Engine) inter.getEngine();
        try {
            SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(key.getSkel(), key.getDisplay());
            AbstractSource scope;
            if (!"".equals(sa.fun)) {
                scope = (sa.scope != null ? sa.scope : engine.store.user);
                scope = scope.getStore().getSource(sa.fun);
                AbstractSource.checkExistentSource(scope, sa);
            } else {
                scope = engine.store.user;
            }
            res = CacheModule.unfindPrefix(path, scope, mask);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        return res;
    }

    /**
     * <p>Find a read key according to the auto loader.</p>
     *
     * @param inter The interpreter.
     * @param path  The prefixed path.
     * @param key   The call-site.
     * @param opt   The options list.
     * @return The source key.
     * @throws InterpreterMessage Shit happens.
     */
    public static String sysFindKey(Interpreter inter,
                                    String path, TermAtomic key,
                                    Object opt)
            throws InterpreterMessage, IOException {
        int mask = decodeFindOptions(opt);
        Engine engine = (Engine) inter.getEngine();
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
        return CacheSubclass.findKey(path, scope, mask, engine);
    }

    /**
     * <p>Unfind a key according to the auto loader.</p>
     *
     * @param inter The interpreter.
     * @param path  The prefixed path.
     * @param key   The call-site.
     * @param opt   The options list.
     * @return The source key.
     * @throws InterpreterMessage Shit happens.
     */
    public static Object sysUnfindKey(Interpreter inter,
                                      String path, TermAtomic key,
                                      Object opt)
            throws InterpreterMessage, IOException {
        int mask = decodeFindOptions(opt);
        Object res;
        Engine engine = (Engine) inter.getEngine();
        try {
            AbstractSource scope;
            SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(key.getSkel(), key.getDisplay());
            if (!"".equals(sa.fun)) {
                scope = (sa.scope != null ? sa.scope : engine.store.user);
                scope = scope.getStore().getSource(sa.fun);
                AbstractSource.checkExistentSource(scope, sa);
            } else {
                scope = engine.store.user;
            }
            res = CacheSubclass.unfindKey(path, scope, mask, engine);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        return res;
    }

    /**
     * <p>Decode the find options.</p>
     *
     * @param opt The find options term.
     * @return The find options.
     */
    public static int decodeFindOptions(Object opt)
            throws InterpreterMessage {
        int mask = 0;
        while (opt instanceof TermCompound &&
                ((TermCompound) opt).getArity() == 2 &&
                ((TermCompound) opt).getFunctor().equals(
                        Knowledgebase.OP_CONS)) {
            Object temp = ((TermCompound) opt).getArg(0);
            if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_PACKAGE)) {
                Object help = ((TermCompound) temp).getArg(0);
                String fun = InterpreterMessage.castString(help);
                if (fun.equals(OP_PACKAGE_NONE)) {
                    mask &= ~MASK_PRFX_LIBR;
                    mask &= ~MASK_PRFX_FRGN;
                } else if (fun.equals(LoadOpts.OP_PREFIX_LIBRARY)) {
                    mask |= MASK_PRFX_LIBR;
                    mask &= ~MASK_PRFX_FRGN;
                } else if (fun.equals(LoadOpts.OP_PREFIX_FOREIGN)) {
                    mask &= ~MASK_PRFX_LIBR;
                    mask |= MASK_PRFX_FRGN;
                } else if (fun.equals(OP_PACKAGE_BOTH)) {
                    mask |= MASK_PRFX_LIBR;
                    mask |= MASK_PRFX_FRGN;
                } else {
                    throw new InterpreterMessage(
                            InterpreterMessage.domainError("fix_option", help));
                }
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_FILE_EXTENSION)) {
                Object help = ((TermCompound) temp).getArg(0);
                String fun = InterpreterMessage.castString(help);
                if (fun.equals("none")) {
                    mask &= ~MASK_SUFX_TEXT;
                    mask &= ~MASK_SUFX_BNRY;
                    mask &= ~MASK_SUFX_RSCS;
                } else if (fun.equals(OP_FILE_EXTENSION_FILE)) {
                    mask |= MASK_SUFX_TEXT;
                    mask |= MASK_SUFX_BNRY;
                    mask &= ~MASK_SUFX_RSCS;
                } else if (fun.equals(OP_FILE_EXTENSION_RESOURCE)) {
                    mask &= ~MASK_SUFX_TEXT;
                    mask &= ~MASK_SUFX_BNRY;
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
                if (fun.equals("none")) {
                    mask &= ~MASK_FAIL_READ;
                    mask &= ~MASK_FAIL_CHLD;
                } else if (fun.equals(OP_FAILURE_READ)) {
                    mask |= MASK_FAIL_READ;
                    mask &= ~MASK_FAIL_CHLD;
                } else if (fun.equals(OP_FAILURE_CHILD)) {
                    mask &= ~MASK_FAIL_READ;
                    mask |= MASK_FAIL_CHLD;
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
        Lobby lobby = inter.getKnowledgebase().getLobby();
        Knowledgebase know = inter.getKnowledgebase();
        Object end = lobby.ATOM_NIL;
        while (know != null) {
            String[] paths = know.getClassPaths();
            for (int i = paths.length - 1; i >= 0; i--)
                end = new TermCompound(lobby.ATOM_CONS, paths[i], end);
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
        int type = 0;
        String mime = null;
        while (opt instanceof TermCompound &&
                ((TermCompound) opt).getArity() == 2 &&
                ((TermCompound) opt).getFunctor().equals(Knowledgebase.OP_CONS)) {
            Object temp = ((TermCompound) opt).getArg(0);
            if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(ForeignStream.OP_TYPE)) {
                Object help = ((TermCompound) temp).getArg(0);
                switch (ForeignStream.atomToType(help)) {
                    case ForeignStream.TYPE_NONE:
                        type &= ~FileExtension.MASK_USES_BNRY;
                        type &= ~FileExtension.MASK_USES_TEXT;
                        type &= ~FileExtension.MASK_USES_RSCS;
                        break;
                    case ForeignStream.TYPE_BINARY:
                        type |= FileExtension.MASK_USES_BNRY;
                        type &= ~FileExtension.MASK_USES_TEXT;
                        type &= ~FileExtension.MASK_USES_RSCS;
                        break;
                    case ForeignStream.TYPE_TEXT:
                        type &= ~FileExtension.MASK_USES_BNRY;
                        type |= FileExtension.MASK_USES_TEXT;
                        type &= ~FileExtension.MASK_USES_RSCS;
                        break;
                    case ForeignStream.TYPE_RESOURCE:
                        type &= ~FileExtension.MASK_USES_BNRY;
                        type &= ~FileExtension.MASK_USES_TEXT;
                        type |= FileExtension.MASK_USES_RSCS;
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
                    ((TermCompound) temp).getFunctor().equals(OP_MIME)) {
                Object help = ((TermCompound) temp).getArg(0);
                switch (atomToData(help)) {
                    case DATA_PLAIN:
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
        return new FileExtension(type, mime);
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
     * @param inter The interpreter.
     * @return The list of class paths.
     */
    public static Object sysGetFileExtenstions(Interpreter inter) {
        Lobby lobby = inter.getKnowledgebase().getLobby();
        Knowledgebase know = inter.getKnowledgebase();
        Object end = lobby.ATOM_NIL;
        do {
            MapEntry<String, FileExtension>[] exts = know.getFileExtensions();
            for (int i = exts.length - 1; i >= 0; i--) {
                MapEntry<String, FileExtension> ext = exts[i];
                Object val = encodeFileExtension(inter, ext.value);
                val = new TermCompound(lobby.ATOM_SUB, ext.key, val);
                end = new TermCompound(lobby.ATOM_CONS, val, end);
            }
            know = know.getParent();
        } while (know != null);
        return end;
    }

    /**
     * <p>Encode type and mime options.</p>
     *
     * @param inter The interpreter.
     * @param fe    The type and mime options.
     * @return The type and mime options.
     */
    private static Object encodeFileExtension(Interpreter inter, FileExtension fe) {
        Lobby lobby = inter.getKnowledgebase().getLobby();
        Object end = lobby.ATOM_NIL;
        if (fe.getMimeType() != null) {
            Object val = new TermCompound(OP_MIME, fe.getMimeType());
            end = new TermCompound(lobby.ATOM_CONS, val, end);
        }
        if ((fe.getType() & FileExtension.MASK_USES_SUFX) != 0) {
            Object val;
            if ((fe.getType() & FileExtension.MASK_USES_BNRY) != 0) {
                val = new TermCompound(ForeignStream.OP_TYPE, ForeignStream.OP_TYPE_BINARY);
            } else if ((fe.getType() & FileExtension.MASK_USES_TEXT) != 0) {
                val = new TermCompound(ForeignStream.OP_TYPE, ForeignStream.OP_TYPE_TEXT);
            } else if ((fe.getType() & FileExtension.MASK_USES_RSCS) != 0) {
                val = new TermCompound(ForeignStream.OP_TYPE, ForeignStream.OP_TYPE_RESOURCE);
            } else {
                throw new IllegalArgumentException("unkown uses");
            }
            end = new TermCompound(lobby.ATOM_CONS, val, end);
        }
        if ((fe.getType() & FileExtension.MASK_DATA_ECRY) != 0) {
            Object val = new TermCompound(OP_DATA, OP_DATA_ENCRYPT);
            end = new TermCompound(lobby.ATOM_CONS, val, end);
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
        if (val.equals(OP_DATA_PLAIN)) {
            return DATA_PLAIN;
        } else if (val.equals(OP_DATA_ENCRYPT)) {
            return DATA_ENCRYPT;
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    InterpreterMessage.OP_DOMAIN_FLAG_VALUE, t));
        }
    }

}
