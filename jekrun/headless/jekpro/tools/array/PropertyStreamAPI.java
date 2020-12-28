package jekpro.tools.array;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.ReadOpts;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.reflect.PropertySource;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;
import matula.util.data.MapHash;
import matula.util.system.ConnectionInput;
import matula.util.system.ConnectionReader;
import matula.util.system.ConnectionWriter;

import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

/**
 * <p>This class provides additional stream properties.</p>
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
public class PropertyStreamAPI extends AbstractProperty<Object> {
    public static final MapHash<StoreKey, AbstractProperty<Object>> DEFAULT
            = new MapHash<>();

    public final static String OP_ENCODING = "encoding";
    public final static String OP_MIME_TYPE = "mime_type";
    public final static String OP_BOM = "bom";

    private final static int PROP_ENCODING = 0;
    private final static int PROP_MIME_TYPE = 1;
    private final static int PROP_LAST_MODIFIED = 2;
    private final static int PROP_VERSION_TAG = 3;
    private final static int PROP_BOM = 4;
    private final static int PROP_LINE_NO = 5;
    private final static int PROP_EXPIRATION = 6;
    private final static int PROP_DATE = 7;
    private final static int PROP_MAX_AGE = 8;

    static {
        DEFAULT.add(new StoreKey(OP_ENCODING, 1), new PropertyStreamAPI(PROP_ENCODING));
        DEFAULT.add(new StoreKey(OP_MIME_TYPE, 1), new PropertyStreamAPI(PROP_MIME_TYPE));
        DEFAULT.add(new StoreKey(PropertySource.OP_LAST_MODIFIED, 1), new PropertyStreamAPI(PROP_LAST_MODIFIED));
        DEFAULT.add(new StoreKey(PropertySource.OP_VERSION_TAG, 1), new PropertyStreamAPI(PROP_VERSION_TAG));
        DEFAULT.add(new StoreKey(OP_BOM, 1), new PropertyStreamAPI(PROP_BOM));
        DEFAULT.add(new StoreKey(ReadOpts.OP_LINE_NO, 1), new PropertyStreamAPI(PROP_LINE_NO));
        DEFAULT.add(new StoreKey(PropertySource.OP_EXPIRATION, 1), new PropertyStreamAPI(PROP_EXPIRATION));
        DEFAULT.add(new StoreKey(PropertySource.OP_DATE, 1), new PropertyStreamAPI(PROP_DATE));
        DEFAULT.add(new StoreKey(PropertySource.OP_MAX_AGE, 1), new PropertyStreamAPI(PROP_MAX_AGE));
    }

    /**
     * <p>Create an additional stream property.</p>
     *
     * @param i The id of the additional stream property.
     */
    private PropertyStreamAPI(int i) {
        super(i);
    }

    /**
     * <p>Retrieve all the stream properties.</p>
     *
     * @param obj The stream.
     * @param en  The engine.
     * @return The stream properties.
     */
    public Object[] getObjProps(Object obj, Engine en) {
        switch (id) {
            case PROP_ENCODING:
                if (obj instanceof ConnectionReader) {
                    obj = ((ConnectionReader) obj).getUnbuf();
                } else if (obj instanceof ConnectionWriter) {
                    obj = ((ConnectionWriter) obj).getUnbuf();
                }
                if (obj instanceof InputStreamReader) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_ENCODING),
                                    new SkelAtom(((InputStreamReader) obj).getEncoding())),
                            Display.DISPLAY_CONST)};
                } else if (obj instanceof OutputStreamWriter) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_ENCODING),
                                    new SkelAtom(((OutputStreamWriter) obj).getEncoding())),
                            Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_MIME_TYPE:
                if (obj instanceof ConnectionReader) {
                    obj = ((ConnectionReader) obj).getUncoded();
                }
                if (obj instanceof ConnectionInput) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_MIME_TYPE),
                                    new SkelAtom(((ConnectionInput) obj).getMimeType())),
                            Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_LAST_MODIFIED:
                if (obj instanceof ConnectionReader) {
                    obj = ((ConnectionReader) obj).getUncoded();
                }
                if (obj instanceof ConnectionInput) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(PropertySource.OP_LAST_MODIFIED),
                                    TermAtomic.normBigInteger(((ConnectionInput) obj).getLastModified())),
                            Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_VERSION_TAG:
                if (obj instanceof ConnectionReader) {
                    obj = ((ConnectionReader) obj).getUncoded();
                }
                if (obj instanceof ConnectionInput) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(PropertySource.OP_VERSION_TAG),
                                    new SkelAtom(((ConnectionInput) obj).getETag())),
                            Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_BOM:
                if (obj instanceof ConnectionReader) {
                    String val = ((ConnectionReader) obj).getBom() ? Foyer.OP_TRUE : AbstractFlag.OP_FALSE;
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_BOM),
                                    new SkelAtom(val)),
                            Display.DISPLAY_CONST)};
                } else if (obj instanceof ConnectionWriter) {
                    String val = ((ConnectionWriter) obj).getBom() ? Foyer.OP_TRUE : AbstractFlag.OP_FALSE;
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(OP_BOM),
                                    new SkelAtom(val)),
                            Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_LINE_NO:
                if (obj instanceof ConnectionReader) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(ReadOpts.OP_LINE_NO),
                                    Integer.valueOf(((ConnectionReader) obj).getLineNumber())),
                            Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_EXPIRATION:
                if (obj instanceof ConnectionReader) {
                    obj = ((ConnectionReader) obj).getUncoded();
                }
                if (obj instanceof ConnectionInput) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(PropertySource.OP_EXPIRATION),
                                    TermAtomic.normBigInteger(((ConnectionInput) obj).getExpiration())),
                            Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_DATE:
                if (obj instanceof ConnectionReader) {
                    obj = ((ConnectionReader) obj).getUncoded();
                }
                if (obj instanceof ConnectionInput) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(PropertySource.OP_DATE),
                                    TermAtomic.normBigInteger(((ConnectionInput) obj).getDate())),
                            Display.DISPLAY_CONST)};
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
            case PROP_MAX_AGE:
                if (obj instanceof ConnectionReader) {
                    obj = ((ConnectionReader) obj).getUncoded();
                }
                if (obj instanceof ConnectionInput) {
                    return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                    new SkelAtom(PropertySource.OP_MAX_AGE),
                                    Integer.valueOf(((ConnectionInput) obj).getMaxAge())),
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
            case PROP_ENCODING:
            case PROP_MIME_TYPE:
            case PROP_LAST_MODIFIED:
            case PROP_VERSION_TAG:
            case PROP_BOM:
            case PROP_LINE_NO:
            case PROP_EXPIRATION:
            case PROP_DATE:
            case PROP_MAX_AGE:
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
            case PROP_ENCODING:
            case PROP_MIME_TYPE:
            case PROP_LAST_MODIFIED:
            case PROP_VERSION_TAG:
            case PROP_BOM:
            case PROP_LINE_NO:
            case PROP_EXPIRATION:
            case PROP_DATE:
            case PROP_MAX_AGE:
                /* can't modify */
                return false;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

}
