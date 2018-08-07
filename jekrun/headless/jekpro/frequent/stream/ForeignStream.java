package jekpro.frequent.stream;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermAtomic;
import jekpro.tools.term.TermCompound;
import matula.util.regex.ScannerError;
import matula.util.system.*;

import java.io.*;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignStream {
    public final static int MODE_READ = 0;
    public final static int MODE_WRITE = 1;
    public final static int MODE_APPEND = 2;

    /* mode values */
    public final static String OP_READ = "read";
    public final static String OP_WRITE = "write";
    public final static String OP_APPEND = "append";

    /* type values */
    private final static String OP_BINARY = "binary";
    private final static String OP_TEXT = "text";

    /* open options */
    public final static String OP_BOM = "bom";
    public final static String OP_ENCODING = "encoding";
    public final static String OP_BUFFER = "buffer";
    private final static String OP_IF_MODIFIED_SINCE = "if_modified_since";
    private final static String OP_IF_NONE_MATCH = "if_none_match";
    public final static String OP_USE_CACHES = "use_caches";
    public final static String OP_TYPE = "type";
    public final static String OP_REPOSITION = "reposition";
    public final static String OP_NEWLINE = "newline";

    /* stream properties */
    public final static String OP_OUTPUT = "output";
    public final static String OP_INPUT = "input";
    public final static String OP_MODE = "mode";
    public final static String OP_FILE_NAME = "file_name";
    public final static String OP_LAST_MODIFIED = "last_modified";
    public final static String OP_VERSION_TAG = "version_tag";
    public final static String OP_EXPIRATION = "expiration";

    /* error terms */
    public final static String OP_PERMISSION_OPEN = "open";
    public final static String OP_OPEN_OPTION = "open_option";

    /****************************************************************/
    /* Stream Control                                               */
    /****************************************************************/

    /**
     * <p>Open the stream with the given address, mode and options.</p>
     *
     * @param inter The call-in.
     * @param adr   The address.
     * @param mode  The mode.
     * @param opt   The options.
     * @return The created stream, or null if not modified.
     * @throws IOException        IO error.
     * @throws InterpreterMessage Validation error.
     */
    public static Object sysOpen(Interpreter inter, String adr,
                                 String mode, Object opt)
            throws InterpreterMessage, IOException {
        try {
            int modecode = atomToMode(mode);
            OpenOpts options = decodeOpenOpts(modecode, opt);
            switch (modecode) {
                case MODE_READ:
                    return options.openRead(inter.getKnowledgebase(), adr);
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
                    new TermCompound(ForeignStream.OP_REPOSITION,
                            Foyer.OP_TRUE)));
        } catch (LicenseError x) {
            throw new InterpreterMessage(InterpreterMessage.licenseError(
                    x.getError()));
        } catch (ScannerError x) {
            throw new InterpreterMessage(InterpreterMessage.syntaxError(
                    x.getError()));
        }
    }

    /**
     * <p>Close the stream with the given option.</p>
     *
     * @param str The stream.
     * @param opt The options.
     * @throws InterpreterMessage Validation error.
     * @throws IOException        IO error.
     * @throws InterpreterMessage Validation error.
     */
    public static void sysClose(Object str, Object opt)
            throws InterpreterMessage, IOException {
        if (opt.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            InterpreterMessage.checkInstantiated(opt);
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    InterpreterMessage.OP_TYPE_LIST, opt));
        }
        if (str instanceof Reader) {
            ((Reader) str).close();
        } else if (str instanceof Writer) {
            ((Writer) str).close();
        } else if (str instanceof InputStream) {
            ((InputStream) str).close();
        } else if (str instanceof OutputStream) {
            ((OutputStream) str).close();
        } else {
            throw new InterpreterMessage(
                    InterpreterMessage.domainError("stream", str));
        }
    }

    /**
     * <p>Return the list of stream properties.</p>
     *
     * @param str Thew stream.
     * @return The properties of the stream.
     * @throws InterpreterMessage Validation error.
     * @throws IOException        IO error.
     */
    public static Object sysStreamProperties(Object str)
            throws InterpreterMessage, IOException {
        Object res = Knowledgebase.OP_NIL;
        if (str instanceof Reader) {
            res = new TermCompound(Knowledgebase.OP_CONS,
                    new TermCompound(OP_TYPE, OP_TEXT), res);
            res = new TermCompound(Knowledgebase.OP_CONS,
                    OP_INPUT, res);
            if (str instanceof ConnectionReader)
                res = sysReaderProperties((ConnectionReader) str, res);
        } else if (str instanceof Writer) {
            res = new TermCompound(Knowledgebase.OP_CONS,
                    new TermCompound(OP_TYPE, OP_TEXT), res);
            res = new TermCompound(Knowledgebase.OP_CONS,
                    OP_OUTPUT, res);
            if (str instanceof ConnectionWriter)
                res = sysWriterProperties((ConnectionWriter) str, res);
        } else if (str instanceof InputStream) {
            res = new TermCompound(Knowledgebase.OP_CONS,
                    new TermCompound(OP_TYPE, OP_BINARY), res);
            res = new TermCompound(Knowledgebase.OP_CONS,
                    OP_INPUT, res);
            if (str instanceof ConnectionInput)
                res = sysInputProperties((ConnectionInput) str, res);
        } else if (str instanceof OutputStream) {
            res = new TermCompound(Knowledgebase.OP_CONS,
                    new TermCompound(OP_TYPE, OP_BINARY), res);
            res = new TermCompound(Knowledgebase.OP_CONS,
                    OP_OUTPUT, res);
            if (str instanceof ConnectionOutput)
                res = sysOutputProperties((ConnectionOutput) str, res);
        } else {
            throw new InterpreterMessage(
                    InterpreterMessage.domainError("stream", str));
        }
        RandomAccessFile raf = ForeignStream.getRaf(str);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_REPOSITION,
                        (raf != null ? Foyer.OP_TRUE :
                                AbstractFlag.OP_FALSE)), res);
        if (raf != null) {
            res = new TermCompound(Knowledgebase.OP_CONS,
                    new TermCompound("position",
                            TermAtomic.normBigInteger(raf.getFilePointer())), res);
            res = new TermCompound(Knowledgebase.OP_CONS,
                    new TermCompound("length",
                            TermAtomic.normBigInteger(raf.length())), res);
        }
        return res;
    }


    /**
     * <p>Reposition the stream.</p>
     *
     * @param str  The stream.
     * @param fpos The position.
     * @throws InterpreterMessage Validation or permission error.
     */
    public static void sysSetStreamPosition(Object str, long fpos)
            throws InterpreterMessage, IOException {
        RandomAccessFile raf = ForeignStream.getRaf(str);
        if (raf == null)
            throw new InterpreterMessage(InterpreterMessage.permissionError(
                    "reposition", "stream", str));
        raf.seek(fpos);
    }

    /**
     * <p>Truncate the stream.</p>
     *
     * @param str  The stream.
     * @param fpos The position.
     * @throws InterpreterMessage Validation or permission error.
     */
    public static void sysSetStreamLength(Object str, long fpos)
            throws InterpreterMessage, IOException {
        RandomAccessFile raf = ForeignStream.getRaf(str);
        if (raf == null)
            throw new InterpreterMessage(InterpreterMessage.permissionError(
                    "reposition", "stream", str));
        raf.setLength(fpos);
    }

    /**
     * <p>Helper to retrieve the raf of a stream.</p>
     *
     * @param str The stream.
     * @return The raf or null.
     */
    private static RandomAccessFile getRaf(Object str) {
        RandomAccessFile raf;
        if (str instanceof ConnectionOutput) {
            raf = ((ConnectionOutput) str).getRaf();
        } else if (str instanceof ConnectionWriter) {
            raf = ((ConnectionWriter) str).getRaf();
        } else if (str instanceof ConnectionInput) {
            raf = ((ConnectionInput) str).getRaf();
        } else if (str instanceof ConnectionReader) {
            raf = ((ConnectionReader) str).getRaf();
        } else {
            raf = null;
        }
        return raf;
    }

    /****************************************************************/
    /* Stream Properties                                            */
    /****************************************************************/

    /**
     * <p>Retrieve the properties of an input stream.</p>
     *
     * @param res The properties before.
     * @return The properties after.
     */
    public static Object sysInputProperties(ConnectionInput in, Object res) {
        long lastmodified = in.getLastModified();
        String etag = in.getETag();
        long expiration = in.getExpiration();
        String path = in.getPath();
        int buffer = in.getBuffer();
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_LAST_MODIFIED,
                        TermAtomic.normBigInteger(lastmodified)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_VERSION_TAG,
                        etag), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_EXPIRATION,
                        TermAtomic.normBigInteger(expiration)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_BUFFER,
                        Integer.valueOf(buffer)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_MODE,
                        OP_READ), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_FILE_NAME,
                        (path != null ? path : "")), res);
        return res;
    }

    /**
     * <p>Retrieve the properties of an output stream.</p>
     *
     * @param out The connection output.
     * @param res The properties before.
     * @return The properties after.
     */
    public static Object sysOutputProperties(ConnectionOutput out,
                                             Object res) {
        String path = out.getPath();
        boolean append = out.getAppend();
        int buffer = out.getBuffer();
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_BUFFER,
                        Integer.valueOf(buffer)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_MODE,
                        (append ? OP_APPEND : OP_WRITE)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_FILE_NAME,
                        (path != null ? path : "")), res);
        return res;
    }

    /**
     * <p>Retrieve the properties of this connection reader.</p>
     *
     * @param read The connection reader.
     * @param res  The properties before.
     * @return The properties after.
     */
    public static Object sysReaderProperties(ConnectionReader read,
                                             Object res) {
        boolean bom = read.getBom();
        String encoding = read.getEncoding();
        long lastmodified = read.getLastModified();
        String etag = read.getETag();
        long expiration = read.getExpiration();
        String path = read.getPath();
        int lineno = read.getLineNumber();
        int buffer = read.getBuffer();
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_ENCODING,
                        encoding), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_BOM, (bom ? Foyer.OP_TRUE :
                        AbstractFlag.OP_FALSE)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_LAST_MODIFIED,
                        TermAtomic.normBigInteger(lastmodified)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_VERSION_TAG,
                        etag), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_EXPIRATION,
                        TermAtomic.normBigInteger(expiration)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound("line_no",
                        Integer.valueOf(lineno)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_BUFFER,
                        Integer.valueOf(buffer)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_MODE,
                        OP_READ), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_FILE_NAME,
                        (path != null ? path : "")), res);
        return res;
    }

    /**
     * <p>Retrieve the properties of a writer.</p>
     *
     * @param write The connection writer.
     * @param res   The properties before.
     * @return The properties after.
     */
    public static Object sysWriterProperties(ConnectionWriter write,
                                             Object res) {
        boolean bom = write.getBom();
        String encoding = write.getEncoding();
        String path = write.getPath();
        boolean append = write.getAppend();
        int buffer = write.getBuffer();
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_ENCODING,
                        encoding), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_BOM,
                        (bom ? Foyer.OP_TRUE :
                                AbstractFlag.OP_FALSE)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_BUFFER,
                        Integer.valueOf(buffer)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_MODE,
                        (append ? OP_APPEND : OP_WRITE)), res);
        res = new TermCompound(Knowledgebase.OP_CONS,
                new TermCompound(OP_FILE_NAME,
                        (path != null ? path : "")), res);
        return res;
    }

    /****************************************************************/
    /* Open Options                                                 */
    /****************************************************************/

    /**
     * <p>Convert an atom to a mode. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param fun The atom.
     * @return The mode.
     * @throws InterpreterMessage Validation error.
     */
    public static int atomToMode(String fun) throws InterpreterMessage {
        if (fun.equals(OP_READ)) {
            return MODE_READ;
        } else if (fun.equals(OP_WRITE)) {
            return MODE_WRITE;
        } else if (fun.equals(OP_APPEND)) {
            return MODE_APPEND;
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    EngineMessage.OP_DOMAIN_IO_MODE, fun));
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
    public static boolean atomToType(Object t) throws InterpreterMessage {
        String val = InterpreterMessage.castString(t);
        if (val.equals(OP_BINARY)) {
            return true;
        } else if (val.equals(OP_TEXT)) {
            return false;
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
    public static boolean atomToBool(Object t) throws InterpreterMessage {
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

    /**
     * <p>Decode the open options.</p>
     *
     * @param mode The mode.
     * @param opt  The open options term.
     * @return The open options.
     * @throws InterpreterMessage Validation error.
     */
    public static OpenOpts decodeOpenOpts(int mode, Object opt)
            throws InterpreterMessage {
        try {
            OpenOpts res = new OpenOpts();
            while (opt instanceof TermCompound &&
                    ((TermCompound) opt).getArity() == 2 &&
                    ((TermCompound) opt).getFunctor().equals(Knowledgebase.OP_CONS)) {
                Object temp = ((TermCompound) opt).getArg(0);
                if (temp instanceof TermCompound &&
                        ((TermCompound) temp).getArity() == 1 &&
                        ((TermCompound) temp).getFunctor().equals(OP_BOM)) {
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
                        ((TermCompound) temp).getFunctor().equals(OP_ENCODING)) {
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
                        ((TermCompound) temp).getFunctor().equals(OP_TYPE)) {
                    Object help = ((TermCompound) temp).getArg(0);
                    if (atomToType(help)) {
                        res.setFlags(res.getFlags() | OpenOpts.MASK_OPEN_BINR);
                    } else {
                        res.setFlags(res.getFlags() & ~OpenOpts.MASK_OPEN_BINR);
                    }
                } else if (temp instanceof TermCompound &&
                        ((TermCompound) temp).getArity() == 1 &&
                        ((TermCompound) temp).getFunctor().equals(OP_REPOSITION)) {
                    Object help = ((TermCompound) temp).getArg(0);
                    if (atomToBool(help)) {
                        res.setFlags(res.getFlags() | OpenOpts.MASK_OPEN_RPOS);
                    } else {
                        res.setFlags(res.getFlags() & ~OpenOpts.MASK_OPEN_RPOS);
                    }
                } else if (temp instanceof TermCompound &&
                        ((TermCompound) temp).getArity() == 1 &&
                        ((TermCompound) temp).getFunctor().equals(OP_BUFFER)) {
                    Object help = ((TermCompound) temp).getArg(0);
                    Number num = InterpreterMessage.castInteger(help);
                    InterpreterMessage.checkNotLessThanZero(num);
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
        } catch (ClassCastException x) {
            throw new InterpreterMessage(
                    InterpreterMessage.representationError(x.getMessage()));
        }
    }

}
