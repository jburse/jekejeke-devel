package jekpro.reference.reflect;

import jekpro.frequent.stream.ForeignStream;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;
import matula.util.data.MapHash;
import matula.util.system.ConnectionInput;
import matula.util.system.ConnectionOutput;
import matula.util.system.ConnectionReader;
import matula.util.system.ConnectionWriter;

import java.io.*;

/**
 * <p>This class provides stream properties.</p>
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
 * Only to be distributed with programs that add sgnificant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class PropertyStream extends AbstractProperty<Object> {
    public static final MapHash<StoreKey, AbstractProperty<Object>> DEFAULT
            = new MapHash<StoreKey, AbstractProperty<Object>>();

    public final static String OP_TYPE = "type";
    public final static String OP_OUTPUT = "output";
    public final static String OP_INPUT = "input";
    public final static String OP_REPOSITION = "reposition";
    public final static String OP_POSITION = "position";
    public final static String OP_LENGTH = "length";
    public final static String OP_BUFFER = "buffer";
    public final static String OP_MODE = "mode";
    public final static String OP_FILE_NAME = "file_name";

    public final static int PROP_TYPE = 0;
    public final static int PROP_OUTPUT = 1;
    public final static int PROP_INPUT = 2;
    public final static int PROP_REPOSITION = 3;
    public final static int PROP_POSITION = 4;
    public final static int PROP_LENGTH = 5;
    public final static int PROP_BUFFER = 6;
    public final static int PROP_MODE = 7;
    public final static int PROP_FILE_NAME = 8;

    static {
        DEFAULT.add(new StoreKey(OP_TYPE, 1), new PropertyStream(PROP_TYPE));
        DEFAULT.add(new StoreKey(OP_OUTPUT, 0), new PropertyStream(PROP_OUTPUT));
        DEFAULT.add(new StoreKey(OP_INPUT, 0), new PropertyStream(PROP_INPUT));
        DEFAULT.add(new StoreKey(OP_REPOSITION, 1), new PropertyStream(PROP_REPOSITION));
        DEFAULT.add(new StoreKey(OP_POSITION, 1), new PropertyStream(PROP_POSITION));
        DEFAULT.add(new StoreKey(OP_LENGTH, 1), new PropertyStream(PROP_LENGTH));
        DEFAULT.add(new StoreKey(OP_BUFFER, 1), new PropertyStream(PROP_BUFFER));
        DEFAULT.add(new StoreKey(OP_MODE, 1), new PropertyStream(PROP_MODE));
        DEFAULT.add(new StoreKey(OP_FILE_NAME, 1), new PropertyStream(PROP_FILE_NAME));
    }

    public final static String OP_TYPE_BINARY = "binary";
    public final static String OP_TYPE_TEXT = "text";

    public final static String OP_MODE_READ = "read";
    public final static String OP_MODE_WRITE = "write";
    public final static String OP_MODE_APPEND = "append";

    /**
     * <p>Create a stream property.</p>
     *
     * @param i The id of the stream property.
     */
    private PropertyStream(int i) {
        super(i);
    }

    /**
     * <p>Retrieve all the stream properties.</p>
     *
     * @param obj The stream.
     * @param en  The engine.
     * @return The stream properties.
     */
    public Object[] getObjProps(Object obj, Engine en)
            throws EngineException, EngineMessage {
        switch (id) {
            case PROP_TYPE:
                if (obj instanceof Reader || obj instanceof Writer) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_TYPE), new SkelAtom(OP_TYPE_TEXT)),
                            Display.DISPLAY_CONST)};
                } else if (obj instanceof InputStream || obj instanceof OutputStream) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_TYPE), new SkelAtom(OP_TYPE_BINARY)),
                            Display.DISPLAY_CONST)};
                } else {
                    throw new EngineMessage(
                            EngineMessage.domainError("stream", obj));
                }
            case PROP_OUTPUT:
                if (obj instanceof Writer || obj instanceof OutputStream) {
                    return new Object[]{new SkelAtom(OP_OUTPUT)};
                } else if (obj instanceof Reader || obj instanceof InputStream) {
                    return AbstractBranch.FALSE_PROPERTY;
                } else {
                    throw new EngineMessage(
                            EngineMessage.domainError("stream", obj));
                }
            case PROP_INPUT:
                if (obj instanceof Reader || obj instanceof InputStream) {
                    return new Object[]{new SkelAtom(OP_INPUT)};
                } else if (obj instanceof Writer || obj instanceof OutputStream) {
                    return AbstractBranch.FALSE_PROPERTY;
                } else {
                    throw new EngineMessage(
                            EngineMessage.domainError("stream", obj));
                }
            case PROP_REPOSITION:
                RandomAccessFile raf = ForeignStream.getRaf(obj);
                if (raf != null) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_REPOSITION),
                                    new SkelAtom(Foyer.OP_TRUE)),
                            Display.DISPLAY_CONST)};
                } else {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_REPOSITION),
                                    new SkelAtom(AbstractFlag.OP_FALSE)),
                            Display.DISPLAY_CONST)};
                }
            case PROP_POSITION:
                raf = ForeignStream.getRaf(obj);
                if (raf != null) {
                    long val;
                    try {
                        val = raf.getFilePointer();
                    } catch (IOException x) {
                        throw EngineMessage.mapIOException(x);
                    }
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_POSITION),
                                    TermAtomic.normBigInteger(val)),
                            Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_LENGTH:
                raf = ForeignStream.getRaf(obj);
                if (raf != null) {
                    long val;
                    try {
                        val = raf.length();
                    } catch (IOException x) {
                        throw EngineMessage.mapIOException(x);
                    }
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_LENGTH),
                                    TermAtomic.normBigInteger(val)),
                            Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_BUFFER:
                if (obj instanceof ConnectionReader) {
                    obj = ((ConnectionReader)obj).getUncoded();
                } else if (obj instanceof ConnectionWriter) {
                    obj = ((ConnectionWriter)obj).getUncoded();
                }
                if (obj instanceof ConnectionInput) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_BUFFER),
                                    Integer.valueOf(((ConnectionInput) obj).getBuffer())),
                            Display.DISPLAY_CONST)};
                } else if (obj instanceof ConnectionOutput) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_BUFFER),
                                    Integer.valueOf(((ConnectionOutput) obj).getBuffer())),
                            Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_MODE:
                if (obj instanceof ConnectionReader) {
                    obj = ((ConnectionReader)obj).getUncoded();
                } else if (obj instanceof ConnectionWriter) {
                    obj = ((ConnectionWriter)obj).getUncoded();
                }
                if (obj instanceof ConnectionInput) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_MODE),
                                    new SkelAtom(OP_MODE_READ)),
                            Display.DISPLAY_CONST)};
                } else if (obj instanceof ConnectionOutput) {
                    String val = ((ConnectionOutput) obj).getAppend() ? OP_MODE_APPEND : OP_MODE_WRITE;
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_MODE),
                                    new SkelAtom(val)),
                            Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_FILE_NAME:
                if (obj instanceof ConnectionReader) {
                    obj = ((ConnectionReader)obj).getUncoded();
                } else if (obj instanceof ConnectionWriter) {
                    obj = ((ConnectionWriter)obj).getUncoded();
                }
                if (obj instanceof ConnectionInput) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_FILE_NAME),
                                    new SkelAtom(((ConnectionInput) obj).getPath())),
                            Display.DISPLAY_CONST)};
                } else if (obj instanceof ConnectionOutput) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_FILE_NAME),
                                    new SkelAtom(((ConnectionOutput) obj).getPath())),
                            Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Set a stream property.</p>
     *
     * @param obj The stream.
     * @param m   The property skeleton.
     * @param d   The property display.
     * @param en  The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Validation Error.
     */
    public boolean setObjProp(Object obj, Object m,
                              Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_TYPE:
            case PROP_OUTPUT:
            case PROP_INPUT:
            case PROP_REPOSITION:
                /* can't modify */
                return false;
            case PROP_POSITION:
                Number num = SpecialEval.derefAndCastInteger(m, d);
                long val = SpecialEval.castLongValue(num);
                RandomAccessFile raf = ForeignStream.getRaf(obj);
                if (raf == null)
                    throw new EngineMessage(EngineMessage.permissionError(
                            "reposition", "stream", obj));
                try {
                    raf.seek(val);
                } catch (IOException x) {
                    throw EngineMessage.mapIOException(x);
                }
                return true;
            case PROP_LENGTH:
                num = SpecialEval.derefAndCastInteger(m, d);
                val = SpecialEval.castLongValue(num);
                raf = ForeignStream.getRaf(obj);
                if (raf == null)
                    throw new EngineMessage(EngineMessage.permissionError(
                            "reposition", "stream", obj));
                try {
                    raf.setLength(val);
                } catch (IOException x) {
                    throw EngineMessage.mapIOException(x);
                }
                return true;
            case PROP_BUFFER:
            case PROP_MODE:
            case PROP_FILE_NAME:
                /* can't modify */
                return false;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /**
     * <p>Reset a stream property.</p>
     *
     * @param obj The stream.
     * @param m   The property skeleton.
     * @param d   The property display.
     * @param en  The engine.
     * @return True if property could be set, otherwise false.
     * @throws EngineMessage Validation Error.
     */
    public boolean resetObjProp(Object obj, Object m,
                                Display d, Engine en)
            throws EngineMessage {
        switch (id) {
            case PROP_TYPE:
            case PROP_OUTPUT:
            case PROP_INPUT:
            case PROP_REPOSITION:
                /* can't modify */
                return false;
            case PROP_POSITION:
                RandomAccessFile raf = ForeignStream.getRaf(obj);
                if (raf == null)
                    throw new EngineMessage(EngineMessage.permissionError(
                            "reposition", "stream", obj));
                try {
                    raf.seek(0);
                } catch (IOException x) {
                    throw EngineMessage.mapIOException(x);
                }
                return true;
            case PROP_LENGTH:
                raf = ForeignStream.getRaf(obj);
                if (raf == null)
                    throw new EngineMessage(EngineMessage.permissionError(
                            "reposition", "stream", obj));
                try {
                    raf.setLength(0);
                } catch (IOException x) {
                    throw EngineMessage.mapIOException(x);
                }
                return true;
            case PROP_BUFFER:
            case PROP_MODE:
            case PROP_FILE_NAME:
                /* can't modify */
                return false;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

}
