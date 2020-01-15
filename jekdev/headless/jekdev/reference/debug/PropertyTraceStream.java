package jekdev.reference.debug;

import jekdev.reference.system.ProtocolReader;
import jekdev.reference.system.ProtocolWriter;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractProperty;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.MapHash;
import matula.util.system.ConnectionReader;
import matula.util.system.ConnectionWriter;

import java.io.Writer;

/**
 * <p>This class provides trace stream properties.</p>
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
public final class PropertyTraceStream extends AbstractProperty<Object> {
    public static final MapHash<StoreKey, AbstractProperty<Object>> DEFAULT
            = new MapHash<StoreKey, AbstractProperty<Object>>();

    public final static String OP_SYS_PROTOCOL = "sys_protocol";

    private final static int PROP_SYS_PROTOCOL = 0;

    static {
        DEFAULT.add(new StoreKey(OP_SYS_PROTOCOL, 1), new PropertyTraceStream(PROP_SYS_PROTOCOL));
    }

    /**
     * <p>Create a trace stream property.</p>
     *
     * @param i The id of the trace stream property.
     */
    private PropertyTraceStream(int i) {
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
            case PROP_SYS_PROTOCOL:
                if (obj instanceof ConnectionReader) {
                    obj = ((ConnectionReader) obj).getUnbuf();
                } else if (obj instanceof ConnectionWriter) {
                    obj = ((ConnectionWriter) obj).getUnbuf();
                }
                Writer wr;
                if (obj instanceof ProtocolReader) {
                    wr = ((ProtocolReader) obj).getProtocol();
                } else if (obj instanceof ProtocolWriter) {
                    wr = ((ProtocolWriter) obj).getProtocol();
                } else {
                    return AbstractBranch.FALSE_PROPERTY;
                }
                return new Object[]{AbstractTerm.createMolec(new SkelCompound(
                                new SkelAtom(OP_SYS_PROTOCOL),
                                (wr != null ? wr : new SkelAtom(Knowledgebase.OP_NULL))),
                        Display.DISPLAY_CONST)};
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
            case PROP_SYS_PROTOCOL:
                Writer wr = derefAndCastProtocol(m, d, en);
                if (obj instanceof ConnectionReader) {
                    obj = ((ConnectionReader) obj).getUnbuf();
                } else if (obj instanceof ConnectionWriter) {
                    obj = ((ConnectionWriter) obj).getUnbuf();
                }
                if (obj instanceof ProtocolReader) {
                    ((ProtocolReader) obj).setProtocol(wr);
                } else if (obj instanceof ProtocolWriter) {
                    ((ProtocolWriter) obj).setProtocol(wr);
                } else {
                    throw new EngineMessage(
                            EngineMessage.permissionError("protocol", "stream", obj));
                }
                return true;
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
            case PROP_SYS_PROTOCOL:
                if (obj instanceof ConnectionReader) {
                    obj = ((ConnectionReader) obj).getUnbuf();
                } else if (obj instanceof ConnectionWriter) {
                    obj = ((ConnectionWriter) obj).getUnbuf();
                }
                if (obj instanceof ProtocolReader) {
                    ((ProtocolReader) obj).setProtocol(null);
                } else if (obj instanceof ProtocolWriter) {
                    ((ProtocolWriter) obj).setProtocol(null);
                } else {
                    throw new EngineMessage(
                            EngineMessage.permissionError("protocol", "stream", obj));
                }
                return true;
            default:
                throw new IllegalArgumentException("illegal prop");
        }
    }

    /****************************************************************/
    /* Deref Utility                                                */
    /****************************************************************/

    /**
     * <p>Deref and cast to stream protocol.</p>
     *
     * @param m  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The stream protocol.
     * @throws EngineMessage Validation Error.
     */
    private static Writer derefAndCastProtocol(Object m, Display d, Engine en)
            throws EngineMessage, ClassCastException {
        en.skel = m;
        en.display = d;
        en.deref();
        m = en.skel;
        d = en.display;
        if (m instanceof SkelCompound &&
                ((SkelCompound) m).args.length == 1 &&
                ((SkelCompound) m).sym.fun.equals(OP_SYS_PROTOCOL)) {
            m = ((SkelCompound) m).args[0];
            m = SpecialUniv.derefAndCastRefOrNull(m, d);
            if (m == null || m instanceof Writer) {
                return (Writer) m;
            } else {
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_FLAG_VALUE, m));
            }
        } else {
            EngineMessage.checkInstantiated(m);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }

}
