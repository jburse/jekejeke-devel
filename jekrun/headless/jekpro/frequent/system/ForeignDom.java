package jekpro.frequent.system;

import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.TermAtomic;
import matula.util.format.DomElement;
import matula.util.format.DomNode;
import matula.util.format.DomText;
import matula.util.regex.ScannerError;
import matula.util.system.OpenOpts;

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
     * @param inter   The interpreter.
     * @param callout Marker for frame.
     * @param dn     The dom node.
     * @param reader The reader.
     * @param opts   The DOM options.
     * @throws InterpreterMessage   Validation error.
     * @throws IOException          IO error.
     * @throws InterpreterException Syntax error.
     */
    public static void sysNodeLoad(Interpreter inter, CallOut callout,
                                   DomNode dn, Reader reader,
                                   Object opts)
            throws InterpreterMessage, IOException, InterpreterException {
        try {
            DomOpts res = DomOpts.decodeDomOpts(opts);
            dn.load(reader, res.getMask(), res.getControl());
        } catch (ScannerError y) {
            String line = ScannerError.linePosition(OpenOpts.getLine(reader), y.getPos());
            InterpreterMessage x = new InterpreterMessage(
                    InterpreterMessage.syntaxError(y.getError()));
            throw new InterpreterException(x,
                    InterpreterException.fetchPos(
                            InterpreterException.fetchStack(inter), line, inter));
        }
    }

    /**
     * <p>Store a dom node.</p>
     *
     * @param dn      The dom node.
     * @param writer  The writer.
     * @param comment The comment.
     * @param opts    The DOM options.
     * @throws InterpreterMessage Validation error.
     * @throws IOException        IO error.
     */
    public static void sysNodeStore(DomNode dn, Writer writer,
                                    String comment, Object opts)
            throws InterpreterMessage, IOException {
        DomOpts res = DomOpts.decodeDomOpts(opts);
        dn.store(writer, comment, res.getMask(), res.getControl());
    }

}
