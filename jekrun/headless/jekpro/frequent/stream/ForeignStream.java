package jekpro.frequent.stream;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.reflect.PropertyStream;
import jekpro.tools.array.PropertyStreamAPI;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;
import matula.comp.sharik.AbstractTracking;
import matula.util.config.AbstractBundle;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.system.*;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.net.Socket;

/**
 * The foreign predicates for the module stream.
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
public final class ForeignStream {
    public final static int MODE_READ = 0;
    public final static int MODE_WRITE = 1;
    public final static int MODE_APPEND = 2;

    public final static int TYPE_BINARY = 0;
    public final static int TYPE_TEXT = 1;

    private final static String OP_IF_MODIFIED_SINCE = "if_modified_since";
    private final static String OP_IF_NONE_MATCH = "if_none_match";
    public final static String OP_USE_CACHES = "use_caches";
    public final static String OP_NEWLINE = "newline";

    /* error terms */
    public final static String OP_PERMISSION_OPEN = "open";
    public final static String OP_OPEN_OPTION = "open_option";

    /****************************************************************/
    /* Stream Control                                               */
    /****************************************************************/

    /**
     * <p>Open the stream from the given socket, mode and options.</p>
     *
     * @param inter The call-in.
     * @param sock  The socket.
     * @param mode  The mode.
     * @param opt   The options.
     * @return The created stream, or null if not modified.
     * @throws IOException        IO error.
     * @throws ClassCastException Validation error.
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysDuplex(Interpreter inter, Socket sock,
                                   String mode, Object opt)
            throws ClassCastException, InterpreterMessage, IOException {
        try {
            int modecode = atomToMode(mode);
            OpenDuplex options = decodeOpenDuplex(modecode, opt);
            switch (modecode) {
                case MODE_READ:
                    Knowledgebase know = inter.getKnowledgebase();
                    return options.openRead((Store) know.getStore(), sock);
                case MODE_WRITE:
                    return options.openWrite(sock);
                default:
                    throw new IllegalArgumentException("illegal mode");
            }
        } catch (IllegalArgumentException x) {
            throw new InterpreterMessage(InterpreterMessage.permissionError(
                    ForeignStream.OP_PERMISSION_OPEN, EngineMessage.OP_PERMISSION_SOURCE_SINK,
                    new TermCompound(PropertyStream.OP_REPOSITION,
                            Foyer.OP_TRUE)));
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(
                    x.getError()));
        }
    }

    /**
     * <p>Open the stream with the given address, mode and options.</p>
     *
     * @param inter The call-in.
     * @param adr   The address.
     * @param mode  The mode.
     * @param opt   The options.
     * @return The created stream, or null if not modified.
     * @throws IOException        IO error.
     * @throws ClassCastException Validation error.
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysOpen(Interpreter inter, String adr,
                                 String mode, Object opt)
            throws ClassCastException, InterpreterMessage, IOException {
        try {
            int modecode = atomToMode(mode);
            OpenOpts options = decodeOpenOpts(modecode, opt);
            switch (modecode) {
                case MODE_READ:
                    Knowledgebase know = inter.getKnowledgebase();
                    return options.openRead((Store) know.getStore(), adr);
                case MODE_WRITE:
                    return options.openWrite(adr);
                case MODE_APPEND:
                    return options.openAppend(adr);
                default:
                    throw new IllegalArgumentException("illegal mode");
            }
        } catch (IllegalArgumentException x) {
            throw new InterpreterMessage(InterpreterMessage.permissionError(
                    ForeignStream.OP_PERMISSION_OPEN, EngineMessage.OP_PERMISSION_SOURCE_SINK,
                    new TermCompound(PropertyStream.OP_REPOSITION,
                            Foyer.OP_TRUE)));
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(
                    x.getError()));
        }
    }


    /**
     * <p>Convert an atom to a mode. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param fun The atom.
     * @return The mode.
     * @throws InterpreterMessage Validation error.
     */
    private static int atomToMode(String fun) throws InterpreterMessage {
        if (fun.equals(PropertyStream.OP_MODE_READ)) {
            return MODE_READ;
        } else if (fun.equals(PropertyStream.OP_MODE_WRITE)) {
            return MODE_WRITE;
        } else if (fun.equals(PropertyStream.OP_MODE_APPEND)) {
            return MODE_APPEND;
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    EngineMessage.OP_DOMAIN_IO_MODE, fun));
        }
    }

    /**
     * <p>Helper to retrieve the raf of a stream.</p>
     *
     * @param str The stream.
     * @return The raf or null.
     */
    public static RandomAccessFile getRaf(Object str) {
        RandomAccessFile raf;
        if (str instanceof ConnectionOutput) {
            raf = ((ConnectionOutput) str).getRaf();
        } else if (str instanceof ConnectionWriter) {
            OutputStream cout = ((ConnectionWriter) str).getUncoded();
            if (cout instanceof ConnectionOutput) {
                raf = ((ConnectionOutput) cout).getRaf();
            } else {
                raf = null;
            }
        } else if (str instanceof ConnectionInput) {
            raf = ((ConnectionInput) str).getRaf();
        } else if (str instanceof ConnectionReader) {
            InputStream cin = ((ConnectionReader) str).getUncoded();
            if (cin instanceof ConnectionInput) {
                raf = ((ConnectionInput) cin).getRaf();
            } else {
                raf = null;
            }
        } else {
            raf = null;
        }
        return raf;
    }

    /****************************************************************/
    /* Stream Properties                                            */
    /****************************************************************/

    /**
     * <p>Retrieve all the properties of a stream.</p>
     *
     * @param inter The interpreter.
     * @param obj   The stream.
     * @return The properties.
     * @throws InterpreterException Validation Error.
     * @throws InterpreterMessage   Validation Error.
     */
    public static Object sysStreamProperty(Interpreter inter,
                                           Object obj)
            throws InterpreterException, InterpreterMessage {
        Engine en = (Engine) inter.getEngine();
        try {
            streamToProperties(obj, en);
            return AbstractTerm.createTerm(en.skel, en.display);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Retrieve some properties of a knowledge base.</p>
     *
     * @param inter The interpreter.
     * @param obj   The stream.
     * @param key   The property name and arity.
     * @return The properties.
     * @throws InterpreterException Validation Error.
     * @throws InterpreterMessage   Validation Error.
     */
    public static Object sysStreamPropertyChk(Interpreter inter,
                                              Object obj, Object key)
            throws InterpreterException, InterpreterMessage {
        Engine en = (Engine) inter.getEngine();
        try {
            StoreKey sk = StoreKey.propToStoreKey(AbstractTerm.getSkel(key),
                    AbstractTerm.getDisplay(key), en);
            streamToProperty(obj, sk, en);
            return AbstractTerm.createTerm(en.skel, en.display);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Set some properties of a stream.</p>
     *
     * @param inter The interpreter.
     * @param obj   The stream.
     * @param val   The property functor and arguments.
     * @throws InterpreterMessage Validation Error.
     */
    public static void sysSetStreamProperty(Interpreter inter,
                                            Object obj, Object val)
            throws InterpreterMessage {
        Engine en = (Engine) inter.getEngine();
        try {
            setStreamProp(obj, AbstractTerm.getSkel(val),
                    AbstractTerm.getDisplay(val), en);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Reset some properties of a knowledge base.</p>
     *
     * @param inter The interpreter.
     * @param obj   The stream.
     * @param val   The property functor and arguments.
     * @throws InterpreterMessage Validation Error.
     */
    public static void sysResetStreamProperty(Interpreter inter,
                                              Object obj, Object val)
            throws InterpreterMessage {
        Engine en = (Engine) inter.getEngine();
        try {
            resetStreamProp(obj, AbstractTerm.getSkel(val),
                    AbstractTerm.getDisplay(val), en);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /****************************************************************/
    /* Open Options                                                 */
    /****************************************************************/

    /**
     * <p>Decode the duplex options.</p>
     *
     * @param mode The mode.
     * @param opt  The open options term.
     * @return The open options.
     * @throws ClassCastException Validation error.
     * @throws InterpreterMessage Validation error.
     */
    public static OpenDuplex decodeOpenDuplex(int mode, Object opt)
            throws ClassCastException, InterpreterMessage {
        OpenDuplex res = new OpenDuplex();
        while (opt instanceof TermCompound &&
                ((TermCompound) opt).getArity() == 2 &&
                ((TermCompound) opt).getFunctor().equals(Knowledgebase.OP_CONS)) {
            Object temp = ((TermCompound) opt).getArg(0);
            if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(PropertyStreamAPI.OP_ENCODING)) {
                Object help = ((TermCompound) temp).getArg(0);
                String fun = InterpreterMessage.castString(help);
                res.setEncoding(fun);
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(PropertyStream.OP_TYPE)) {
                Object help = ((TermCompound) temp).getArg(0);
                switch (atomToType(help)) {
                    case TYPE_BINARY:
                        res.setFlags(res.getFlags() | OpenDuplex.MASK_OPEN_BINR);
                        break;
                    case TYPE_TEXT:
                        res.setFlags(res.getFlags() & ~OpenDuplex.MASK_OPEN_BINR);
                        break;
                    default:
                        throw new InterpreterMessage(InterpreterMessage.domainError(
                                InterpreterMessage.OP_DOMAIN_FLAG_VALUE, help));
                }
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(PropertyStream.OP_BUFFER)) {
                Object help = ((TermCompound) temp).getArg(0);
                Number num = InterpreterMessage.castInteger(help);
                SpecialEval.checkNotLessThanZero(num);
                int size = SpecialEval.castIntValue(num);
                res.setBuffer(size);
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_NEWLINE)) {
                switch (mode) {
                    case MODE_READ:
                        throw new InterpreterMessage(InterpreterMessage.permissionError(
                                OP_PERMISSION_OPEN, EngineMessage.OP_PERMISSION_SOURCE_SINK, temp));
                    case MODE_WRITE:
                        Object help = ((TermCompound) temp).getArg(0);
                        String newline = InterpreterMessage.castString(help);
                        res.setNewLine(newline);
                        break;
                    default:
                        throw new IllegalArgumentException("illegal mode");
                }
            } else {
                InterpreterMessage.checkInstantiated(temp);
                throw new InterpreterMessage(InterpreterMessage.domainError(
                        OP_OPEN_OPTION, temp));
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
        return res;
    }

    /**
     * <p>Decode the open options.</p>
     *
     * @param mode The mode.
     * @param opt  The open options term.
     * @return The open options.
     * @throws ClassCastException Validation error.
     * @throws InterpreterMessage Validation error.
     */
    private static OpenOpts decodeOpenOpts(int mode, Object opt)
            throws ClassCastException, InterpreterMessage {
        OpenOpts res = new OpenOpts();
        while (opt instanceof TermCompound &&
                ((TermCompound) opt).getArity() == 2 &&
                ((TermCompound) opt).getFunctor().equals(Knowledgebase.OP_CONS)) {
            Object temp = ((TermCompound) opt).getArg(0);
            if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(PropertyStreamAPI.OP_BOM)) {
                switch (mode) {
                    case MODE_READ:
                        Object help = ((TermCompound) temp).getArg(0);
                        if (atomToBool(help)) {
                            res.setFlags(res.getFlags() & ~OpenOpts.MASK_OPEN_NOBR);
                        } else {
                            res.setFlags(res.getFlags() | OpenOpts.MASK_OPEN_NOBR);
                        }
                        break;
                    case MODE_WRITE:
                        help = ((TermCompound) temp).getArg(0);
                        if (atomToBool(help)) {
                            res.setFlags(res.getFlags() | OpenOpts.MASK_OPEN_BOMW);
                        } else {
                            res.setFlags(res.getFlags() & ~OpenOpts.MASK_OPEN_BOMW);
                        }
                        break;
                    case MODE_APPEND:
                        throw new InterpreterMessage(InterpreterMessage.permissionError(
                                OP_PERMISSION_OPEN, EngineMessage.OP_PERMISSION_SOURCE_SINK, temp));
                    default:
                        throw new IllegalArgumentException("illegal mode");
                }
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(PropertyStreamAPI.OP_ENCODING)) {
                Object help = ((TermCompound) temp).getArg(0);
                String fun = InterpreterMessage.castString(help);
                res.setEncoding(fun);
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_IF_MODIFIED_SINCE)) {
                switch (mode) {
                    case MODE_READ:
                        Object help = ((TermCompound) temp).getArg(0);
                        Number num = InterpreterMessage.castInteger(help);
                        long time = SpecialEval.castLongValue(num);
                        res.setIfModifiedSince(time);
                        break;
                    case MODE_WRITE:
                    case MODE_APPEND:
                        throw new InterpreterMessage(InterpreterMessage.permissionError(
                                OP_PERMISSION_OPEN, EngineMessage.OP_PERMISSION_SOURCE_SINK, temp));
                    default:
                        throw new IllegalArgumentException("illegal mode");
                }
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_IF_NONE_MATCH)) {
                switch (mode) {
                    case MODE_READ:
                        Object help = ((TermCompound) temp).getArg(0);
                        String etag = InterpreterMessage.castString(help);
                        res.setIfNoneMatch(etag);
                        break;
                    case MODE_WRITE:
                    case MODE_APPEND:
                        throw new InterpreterMessage(InterpreterMessage.permissionError(
                                OP_PERMISSION_OPEN, EngineMessage.OP_PERMISSION_SOURCE_SINK, temp));
                    default:
                        throw new IllegalArgumentException("illegal mode");
                }

            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_USE_CACHES)) {
                switch (mode) {
                    case MODE_READ:
                        Object help = ((TermCompound) temp).getArg(0);
                        if (atomToBool(help)) {
                            res.setFlags(res.getFlags() | OpenOpts.MASK_OPEN_CACH);
                        } else {
                            res.setFlags(res.getFlags() & ~OpenOpts.MASK_OPEN_CACH);
                        }
                        break;
                    case MODE_WRITE:
                    case MODE_APPEND:
                        throw new InterpreterMessage(InterpreterMessage.permissionError(
                                OP_PERMISSION_OPEN, EngineMessage.OP_PERMISSION_SOURCE_SINK, temp));
                    default:
                        throw new IllegalArgumentException("illegal mode");
                }
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(PropertyStream.OP_TYPE)) {
                Object help = ((TermCompound) temp).getArg(0);
                switch (atomToType(help)) {
                    case TYPE_BINARY:
                        res.setFlags(res.getFlags() | OpenDuplex.MASK_OPEN_BINR);
                        break;
                    case TYPE_TEXT:
                        res.setFlags(res.getFlags() & ~OpenDuplex.MASK_OPEN_BINR);
                        break;
                    default:
                        throw new InterpreterMessage(InterpreterMessage.domainError(
                                InterpreterMessage.OP_DOMAIN_FLAG_VALUE, help));
                }
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(PropertyStream.OP_REPOSITION)) {
                Object help = ((TermCompound) temp).getArg(0);
                if (atomToBool(help)) {
                    res.setFlags(res.getFlags() | OpenOpts.MASK_OPEN_RPOS);
                } else {
                    res.setFlags(res.getFlags() & ~OpenOpts.MASK_OPEN_RPOS);
                }
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(PropertyStream.OP_BUFFER)) {
                Object help = ((TermCompound) temp).getArg(0);
                Number num = InterpreterMessage.castInteger(help);
                SpecialEval.checkNotLessThanZero(num);
                int size = SpecialEval.castIntValue(num);
                res.setBuffer(size);
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_NEWLINE)) {
                switch (mode) {
                    case MODE_READ:
                        throw new InterpreterMessage(InterpreterMessage.permissionError(
                                OP_PERMISSION_OPEN, EngineMessage.OP_PERMISSION_SOURCE_SINK, temp));
                    case MODE_WRITE:
                    case MODE_APPEND:
                        Object help = ((TermCompound) temp).getArg(0);
                        String newline = InterpreterMessage.castString(help);
                        res.setNewLine(newline);
                        break;
                    default:
                        throw new IllegalArgumentException("illegal mode");
                }
            } else {
                InterpreterMessage.checkInstantiated(temp);
                throw new InterpreterMessage(InterpreterMessage.domainError(
                        OP_OPEN_OPTION, temp));
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
        return res;
    }

    /**
     * <p>Convert an atom to a type. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param t The type term.
     * @return The type value.
     * @throws InterpreterMessage Validation error.
     */
    public static int atomToType(Object t) throws InterpreterMessage {
        String val = InterpreterMessage.castString(t);
        if (val.equals(PropertyStream.OP_TYPE_BINARY)) {
            return TYPE_BINARY;
        } else if (val.equals(PropertyStream.OP_TYPE_TEXT)) {
            return TYPE_TEXT;
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    InterpreterMessage.OP_DOMAIN_FLAG_VALUE, t));
        }
    }

    /**
     * <p>Convert an atom to a bool. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param t The bool term.
     * @return The bool value.
     * @throws InterpreterMessage Validation error.
     */
    public static boolean atomToBool(Object t) throws
            InterpreterMessage {
        String val = InterpreterMessage.castString(t);
        if (val.equals(Foyer.OP_TRUE)) {
            return true;
        } else if (val.equals(AbstractFlag.OP_FALSE)) {
            return false;
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    InterpreterMessage.OP_DOMAIN_FLAG_VALUE, t));
        }
    }

    /***************************************************************/
    /* High-Level Stream Access/Modification                       */
    /***************************************************************/

    /**
     * <p>Create a prolog list for the properties of the given stream.</p>
     * <p>Result is returned in skeleton and display.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param obj The stream.
     * @param en  The engine.
     * @throws EngineMessage   Validation Error.
     * @throws EngineException Validation Error.
     */
    private static void streamToProperties(Object obj, Engine en)
            throws EngineMessage, EngineException {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        for (int i = snapshot.length - 1; i >= 0; i--) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            ListArray<MapHash<StoreKey, AbstractProperty<Object>>> props = branch.getStreamProps();
            for (int j = 0; j < props.size(); j++)
                streamPropToProperties(obj, props.get(j), en);
        }
    }

    /**
     * <p>Create a prolog list of the properties of the given stream and properties.</p>
     *
     * @param obj   The stream.
     * @param props The properties.
     * @param en    The engine.
     * @throws EngineMessage   Validation Error.
     * @throws EngineException Validation Error.
     */
    private static void streamPropToProperties(Object obj,
                                               MapHash<StoreKey, AbstractProperty<Object>> props,
                                               Engine en)
            throws EngineMessage, EngineException {
        for (MapEntry<StoreKey, AbstractProperty<Object>> entry2 =
             (props != null ? props.getLastEntry() : null);
             entry2 != null; entry2 = props.predecessor(entry2)) {
            AbstractProperty<Object> prop = entry2.value;
            Object t = en.skel;
            Display d = en.display;
            Object[] vals = prop.getObjProps(obj, en);
            en.skel = t;
            en.display = d;
            AbstractProperty.consArray(vals, en);
        }
    }

    /**
     * <p>Create a prolog list for the property of the given stream.</p>
     * <p>Result is returned in skeleton and display.</p>
     *
     * @param obj The stream.
     * @param sk  The property.
     * @param en  The engine.
     * @throws EngineMessage   Validation Error.
     * @throws EngineException Validation Error.
     */
    private static void streamToProperty(Object obj, StoreKey sk,
                                         Engine en)
            throws EngineMessage, EngineException {
        AbstractProperty<Object> prop = ForeignStream.findStreamProperty(sk, en);
        Object[] vals = prop.getObjProps(obj, en);
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        AbstractProperty.consArray(vals, en);
    }

    /**
     * <p>Set a stream property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param obj The stream.
     * @param m   The value skeleton.
     * @param d   The value display.
     * @param en  The engine.
     * @throws EngineMessage Validation Error.
     */
    private static void setStreamProp(Object obj, Object m, Display d,
                                      Engine en)
            throws EngineMessage {
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty<Object> prop = ForeignStream.findStreamProperty(sk, en);
        if (!prop.setObjProp(obj, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    StoreKey.storeKeyToSkel(sk)));
    }

    /**
     * <p>Reset a stream property.</p>
     * <p>Throws a domain error for undefined flags.</p>
     *
     * @param obj The stream.
     * @param m   The value skeleton.
     * @param d   The value display.
     * @param en  The engine.
     * @throws EngineMessage Validation Error.
     */
    private static void resetStreamProp(Object obj, Object m, Display d,
                                        Engine en)
            throws EngineMessage {
        StoreKey sk = StackElement.callableToStoreKey(m);
        AbstractProperty<Object> prop = ForeignStream.findStreamProperty(sk, en);
        if (!prop.resetObjProp(obj, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_PROPERTY,
                    StoreKey.storeKeyToSkel(sk)));
    }

    /**
     * <p>Retrieve a stream property.</p>
     * <p>Throws a domain error for undefined stream properties.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param sk The property name and arity.
     * @param en The engine.
     * @return The property.
     * @throws EngineMessage Validation Error.
     */
    private static AbstractProperty<Object> findStreamProperty(StoreKey sk,
                                                               Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            ListArray<MapHash<StoreKey, AbstractProperty<Object>>> props = branch.getStreamProps();
            for (int j = 0; j < props.size(); j++) {
                AbstractProperty<Object> prop = props.get(j).get(sk);
                if (prop != null)
                    return prop;
            }
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_PROPERTY,
                StoreKey.storeKeyToSkel(sk)));
    }

}
