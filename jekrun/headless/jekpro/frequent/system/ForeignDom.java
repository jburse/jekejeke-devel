package jekpro.frequent.system;

import jekpro.tools.call.CallOut;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermAtomic;
import jekpro.tools.term.TermCompound;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.format.DomElement;
import matula.util.format.DomNode;
import matula.util.format.DomText;
import matula.util.regex.ScannerError;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

/**
 * <p>The foreign predicates for the module system/xml.</p>
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
public final class ForeignDom {
    /* mask values */
    private final static String OP_LIST = "list";
    private final static String OP_TEXT = "text";

    /* control values */
    private final static String OP_NONE = "none";
    private final static String OP_EMPTY = "empty";
    private final static String OP_ANY = "any";

    /* dom options */
    private final static String OP_ROOT = "root";
    private final static String OP_TYPE = "type";

    /* error terms */
    private final static String OP_DOM_OPTION = "dom_option";

    /**
     * <p>Check whether the dom node is a dom element.</p>
     *
     * @param dn The node.
     * @return True if the node is a dom element.
     */
    public static boolean sysNodeIsElem(DomNode dn) {
        return (dn instanceof DomElement);
    }

    /**
     * <p>Check whether the dom node is a dom text.</p>
     *
     * @param dn The node.
     * @return True if the node is a dom text.
     */
    public static boolean sysNodeIsText(DomNode dn) {
        return (dn instanceof DomText);
    }

    /**
     * <p>Non-determinstic predicate for the attribute names.</p>
     *
     * @param co The call out.
     * @param dh The dom hashtable.
     * @return The attribute names.
     */
    public static String sysElemAttr(CallOut co, DomElement dh) {
        DomCursor<String> dc;
        if (co.getFirst()) {
            dc = new DomCursor<String>(dh.snapshotAttrs());
            co.setData(dc);
        } else {
            dc = (DomCursor<String>) co.getData();
        }
        if (dc.hasMoreElements()) {
            co.setRetry(true);
            return dc.nextElement();
        }
        return null;
    }

    /**
     * <p>Non-determinstic predicate for the children.</p>
     *
     * @param co The call out.
     * @param dh The dom hashtable.
     * @return The attribute names.
     */
    public static DomNode sysElemChild(CallOut co, DomElement dh) {
        DomCursor<DomNode> dc;
        if (co.getFirst()) {
            dc = new DomCursor<DomNode>(dh.snapshotChildren());
            co.setData(dc);
        } else {
            dc = (DomCursor<DomNode>) co.getData();
        }
        if (dc.hasMoreElements()) {
            co.setRetry(true);
            return dc.nextElement();
        }
        return null;
    }

    /**
     * <p>Retrieve a dom element attribute as a Prolog term.</p>
     *
     * @param dh  The dom element.
     * @param key The attribute name.
     * @return The Prolog term or null.
     */
    public static Object sysGetElemAttr(DomElement dh, String key) {
        Object val = dh.getAttrObj(key);
        if (!(val instanceof String)) {
            return TermAtomic.normBigInteger(((Long) val).longValue());
        } else {
            return val;
        }
    }

    /**
     * <p>Set a dom element attribute as a Prolog term.</p></op>
     *
     * @param dh  The dom element.
     * @param key The attribute name.
     * @param val The Prolog term.
     * @throws InterpreterMessage Validation error.
     */
    public static void sysSetElemAttr(DomElement dh, String key, Object val)
            throws InterpreterMessage {
        if (!(val instanceof String)) {
            Number num = InterpreterMessage.castNumber(val);
            long x = InterpreterMessage.castLongValue(num);
            dh.setAttrLong(key, x);
        } else {
            dh.setAttr(key, (String) val);
        }
    }

    /**
     * <p>Load a dom node.</p>
     *
     * @param dn     The dom node.
     * @param reader The reader.
     * @param opts   The options.
     * @throws InterpreterMessage Validation error.
     * @throws ScannerError       Syntax error.
     * @throws IOException        IO error.
     */
    public static void sysNodeLoad(DomNode dn, Reader reader, Object opts)
            throws InterpreterMessage, IOException, ScannerError {
        DomOpts res = decodeDomOpts(opts);
        dn.load(reader, res.getMask(), res.getControl());
    }

    /**
     * <p>Store a dom node.</p>
     *
     * @param dn      The dom node.
     * @param writer  The writer.
     * @param comment The comment.
     * @param opts    The options.
     * @throws InterpreterMessage Validation error.
     * @throws IOException        IO error.
     */
    public static void sysNodeStore(DomNode dn, Writer writer,
                                    String comment, Object opts)
            throws InterpreterMessage, IOException {
        DomOpts res = decodeDomOpts(opts);
        dn.store(writer, comment, res.getMask(), res.getControl());
    }

    /**
     * <p>Convert an atom to a mask. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param t The mask term.
     * @return The mask value.
     * @throws InterpreterMessage Validation error.
     */
    public static int atomToMask(Object t) throws InterpreterMessage {
        InterpreterMessage.checkInstantiated(t);
        String val = InterpreterMessage.castString(t);
        if (val.equals(OP_LIST)) {
            return DomNode.MASK_LIST;
        } else if (val.equals(OP_TEXT)) {
            return DomNode.MASK_TEXT;
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
    public static int atomToType(Object t) throws InterpreterMessage {
        InterpreterMessage.checkInstantiated(t);
        String val = InterpreterMessage.castString(t);
        if (val.equals(OP_NONE)) {
            return DomNode.TYPE_NONE;
        } else if (val.equals(OP_EMPTY)) {
            return DomNode.TYPE_EMPTY;
        } else if (val.equals(OP_ANY)) {
            return DomNode.TYPE_ANY;
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    InterpreterMessage.OP_DOMAIN_FLAG_VALUE, t));
        }
    }

    /**
     * <p>Decode the dom options.</p>
     *
     * @param opt The dom options term.
     * @return The dom options.
     * @throws InterpreterMessage Validation error.
     */
    public static DomOpts decodeDomOpts(Object opt)
            throws InterpreterMessage {
        DomOpts res = new DomOpts();
        while (opt instanceof TermCompound &&
                ((TermCompound) opt).getArity() == 2 &&
                ((TermCompound) opt).getFunctor().equals(Knowledgebase.OP_CONS)) {
            Object temp = ((TermCompound) opt).getArg(0);
            if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_ROOT)) {
                Object help = ((TermCompound) temp).getArg(0);
                int mask = atomToMask(help);
                res.setMask(mask);
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 2 &&
                    ((TermCompound) temp).getFunctor().equals(OP_TYPE)) {
                Object help = ((TermCompound) temp).getArg(0);
                InterpreterMessage.checkInstantiated(help);
                String key = InterpreterMessage.castString(help);
                help = ((TermCompound) temp).getArg(1);
                Integer type = Integer.valueOf(atomToType(help));
                MapHash<String, Integer> control = res.getControl();
                if (control == null) {
                    control = new MapHash<String, Integer>();
                    res.setControl(control);
                }
                MapEntry<String, Integer> entry = control.getEntry(key);
                if (entry != null) {
                    entry.value = type;
                } else {
                    control.add(key, type);
                }
            } else {
                InterpreterMessage.checkInstantiated(temp);
                throw new InterpreterMessage(
                        InterpreterMessage.domainError(OP_DOM_OPTION, temp));
            }
            opt = ((TermCompound) opt).getArg(1);
        }
        if (opt.equals(Knowledgebase.OP_NIL)) {
            /* */
        } else {
            InterpreterMessage.checkInstantiated(opt);
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    InterpreterMessage.OP_TYPE_LIST, opt));
        }
        return res;
    }

}
