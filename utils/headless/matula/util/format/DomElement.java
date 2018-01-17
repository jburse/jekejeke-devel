package matula.util.format;

import matula.util.data.AssocArray;
import matula.util.data.ListArray;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignXml;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.text.ParseException;

/**
 * <p>This class provides a dom element. The API is simplicistic in
 * that a DOM node might appear in multiple DOM elements. This
 * happends when the same DOM node is added twice. As a result we
 * might have update anomalise. Thereofore this is not recommended
 * and the API programmer has to avoid the situation.</p>
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
public final class DomElement extends AbstractDom {
    private static final String DOM_MISSING_ELEM = "dom_missing_elem";
    private static final String DOM_ILLEGAL_VALUE = "dom_illegal_value";
    private static final String DOM_CLOSED_EMPTY = "dom_closed_empty";
    private static final String DOM_MISSING_END = "dom_missing_end";
    private static final String DOM_UNEXPECTED_ATTR = "dom_unexpected_attr";
    private static final String DOM_MISMATCHED_END = "dom_mismatched_end";

    private static final String[] VOID_ATTRS = new String[0];
    private static final AbstractDom[] VOID_CHILDREN = new AbstractDom[0];

    private String name = XmlMachine.VALUE_EMPTY;
    private AssocArray<String, Object> kvs;
    private String[] cachekvs;
    private ListArray<AbstractDom> children;
    private AbstractDom[] cachechildren;

    /**
     * <p>Retrieve the name.</p>
     *
     * @return The name.
     */
    public String getName() {
        return name;
    }

    /**
     * <p>Set the name.</p>
     *
     * @param n The name.
     */
    public void setName(String n) {
        if (n == null)
            throw new NullPointerException("name missing");
        name = n;
    }

    /**
     * <p>Check the name.</p>
     *
     * @param n The name.
     * @return True if the name matches, otherwise false.
     */
    public boolean isName(String n) {
        return name.equalsIgnoreCase(n);
    }

    /**
     * <p>Load a dom element.</p>
     * <p>Not synchronized, uses cut-over.</p>
     *
     * @param dr The dom reader.
     * @throws IOException  IO error..
     * @throws ScannerError Syntax error.
     */
    void loadNode(DomReader dr) throws IOException, ScannerError {
        switch (dr.getRes()) {
            case XmlMachine.RES_TEXT:
                throw new ScannerError(DOM_MISSING_ELEM, OpenOpts.getOffset(dr.getReader()));
            case XmlMachine.RES_TAG:
                if (dr.getType().length() > 0 &&
                        dr.getType().charAt(0) == XmlMachine.CHAR_SLASH)
                    throw new ScannerError(DOM_MISSING_ELEM, OpenOpts.getOffset(dr.getReader()));
                boolean closed = checkClosed(dr);
                String type = dr.getType();
                AssocArray<String, Object> newkvs = null;
                for (int i = 0; i < dr.getAttrCount(); i++) {
                    String valstr = dr.getValueAt(i);
                    Object val;
                    if (XmlMachine.isQuoted(valstr) || "".equals(valstr)) {
                        val = ForeignXml.sysTextUnescape(XmlMachine.stripValue(valstr));
                    } else if (XmlMachine.isNumber(valstr)) {
                        try {
                            val = AbstractDom.parseNumber(valstr);
                        } catch (ParseException x) {
                            throw new ScannerError(DOM_ILLEGAL_VALUE, OpenOpts.getOffset(dr.getReader()));
                        } catch (NumberFormatException x) {
                            throw new ScannerError(DOM_ILLEGAL_VALUE, OpenOpts.getOffset(dr.getReader()));
                        }
                    } else {
                        throw new ScannerError(DOM_ILLEGAL_VALUE, OpenOpts.getOffset(dr.getReader()));
                    }
                    if (newkvs == null)
                        newkvs = new AssocArray<String, Object>();
                    newkvs.add(dr.getAttr(i), val);
                }
                boolean empty = DomWriter.checkEmpty(dr.getControl(), type);
                ListArray<AbstractDom> newchildren;
                if (!closed && !empty) {
                    if ((dr.getMask() & AbstractDom.MASK_TEXT) == 0 &&
                            DomWriter.checkAny(dr.getControl(), type)) {
                        int backmask = dr.getMask();
                        try {
                            dr.setMask(dr.getMask() | AbstractDom.MASK_TEXT);
                            newchildren = DomElement.loadNodes(dr);
                            dr.setMask(backmask);
                        } catch (IOException x) {
                            dr.setMask(backmask);
                            throw x;
                        } catch (ScannerError x) {
                            dr.setMask(backmask);
                            throw x;
                        }
                    } else {
                        newchildren = DomElement.loadNodes(dr);
                    }
                    checkEnd(type, dr);
                } else {
                    if (closed && empty)
                        throw new ScannerError(DOM_CLOSED_EMPTY, -1);
                    newchildren = null;
                    dr.nextTagOrText();
                }
                name = type;
                setKeyValuesFast(newkvs);
                setChildrenFast(newchildren);
                break;
            case XmlMachine.RES_EOF:
                throw new ScannerError(DOM_MISSING_ELEM, OpenOpts.getOffset(dr.getReader()));
            default:
                throw new IllegalArgumentException("illegal res");
        }
    }

    /**
     * <p>Set the key-value pairs fast.</p>
     *
     * @param as The key-value pairs.
     */
    void setKeyValuesFast(AssocArray<String, Object> as) {
        synchronized (this) {
            kvs = as;
            cachekvs = null;
        }
    }

    /**
     * <p>Set the children fast.</p>
     *
     * @param cs The children or null.
     */
    void setChildrenFast(ListArray<AbstractDom> cs) {
        if (cs != null) {
            for (int i = 0; i < cs.size(); i++) {
                AbstractDom node = cs.get(i);
                node.parent = this;
            }
        }
        synchronized (this) {
            children = cs;
            cachechildren = null;
        }
    }

    /**
     * <p>Check whether the actual tag is closed.</p>
     * <p>As a side effect the tag is validated.</p>
     * <p>As a side effect the tag is made open.</p
     *
     * @param dr The dom reader.
     * @return True of the tag is closed, otherwise false.
     */
    private static boolean checkClosed(DomReader dr) {
        int n = dr.getAttrCount();
        if (n != 0 &&
                "".equals(dr.getValueAt(n - 1)) &&
                dr.getAttr(n - 1).length() == 1 &&
                dr.getAttr(n - 1).charAt(0) == XmlMachine.CHAR_SLASH) {
            dr.removeAttrValue(n - 1);
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Check whether the actual tag is an end tag.</p>
     * <p>As a side effect a correct tag is consumed.</p>
     *
     * @param type The type.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private static void checkEnd(String type, DomReader dr)
            throws ScannerError, IOException {
        switch (dr.getRes()) {
            case XmlMachine.RES_TEXT:
                throw new ScannerError(DOM_MISSING_END, OpenOpts.getOffset(dr.getReader()));
            case XmlMachine.RES_TAG:
                if (dr.getType().length() == 0 ||
                        dr.getType().charAt(0) != XmlMachine.CHAR_SLASH)
                    throw new ScannerError(DOM_MISSING_END, OpenOpts.getOffset(dr.getReader()));
                if (dr.getAttrCount() != 0)
                    throw new ScannerError(DOM_UNEXPECTED_ATTR, OpenOpts.getOffset(dr.getReader()));
                String temp = dr.getType();
                temp = temp.substring(1);
                if (!type.equals(temp))
                    throw new ScannerError(DOM_MISMATCHED_END, OpenOpts.getOffset(dr.getReader()));
                dr.nextTagOrText();
                break;
            case XmlMachine.RES_EOF:
                throw new ScannerError(DOM_MISSING_END, OpenOpts.getOffset(dr.getReader()));
            default:
                throw new IllegalArgumentException("illegal res");
        }
    }

    /**
     * <p>Store this dom element.</p>
     * <p>Not synchronized, uses cursors.</p>
     *
     * @param dw The dom writer.
     * @throws IOException Shit happens.
     */
    void storeNode(DomWriter dw) throws IOException {
        if ((dw.getMask() & AbstractDom.MASK_TEXT) == 0 &&
                DomWriter.checkAny(dw.getControl(), name)) {
            int backmask = dw.getMask();
            try {
                dw.setMask(dw.getMask() | AbstractDom.MASK_TEXT);
                storeNodes2(dw);
                dw.setMask(backmask);
            } catch (IOException x) {
                dw.setMask(backmask);
                throw x;
            }
        } else {
            storeNodes2(dw);
        }
    }

    /**
     * <p>Store the childeren.</p>
     *
     * @param dw The dom writer.
     * @throws IOException Shit happens.
     */
    private void storeNodes2(DomWriter dw)
            throws IOException {
        AbstractDom[] nodes = snapshotNodes();
        if (nodes.length == 0 &&
                DomWriter.checkEmpty(dw.getControl(), name)) {
            dw.copyStart(this);
        } else if (nodes.length != 0 ||
                (dw.getMask() & MASK_TEXT) != 0) {
            dw.copyStart(this);
            if ((dw.getMask() & MASK_TEXT) != 0) {
                storeNodes(dw, nodes);
            } else {
                dw.write("\n");
                dw.incIndent();
                storeNodes(dw, nodes);
                dw.decIndent();
                dw.writeIndent();
            }
            dw.copyEnd(this);
        } else {
            dw.copyEmpty(this);
        }
    }

    /*****************************************************/
    /* Children Store/Load                               */
    /*****************************************************/

    /**
     * <p>Load the children.</p>
     *
     * @param dr The dom reader.
     * @return The children.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    static ListArray<AbstractDom> loadNodes(DomReader dr)
            throws IOException, ScannerError {
        dr.nextTagOrText();
        ListArray<AbstractDom> res = null;
        for (; ; ) {
            switch (dr.getRes()) {
                case XmlMachine.RES_TEXT:
                    DomText dt = new DomText();
                    dt.loadNode(dr);
                    if (res == null)
                        res = new ListArray<AbstractDom>();
                    res.add(dt);
                    break;
                case XmlMachine.RES_TAG:
                    if (dr.getType().length() > 0 &&
                            dr.getType().charAt(0) == XmlMachine.CHAR_SLASH)
                        return res;
                    DomElement dh = new DomElement();
                    dh.loadNode(dr);
                    if (res == null)
                        res = new ListArray<AbstractDom>();
                    res.add(dh);
                    break;
                case XmlMachine.RES_EOF:
                    return res;
                default:
                    throw new IllegalArgumentException("illegal res");
            }
        }
    }

    /**
     * <p>Store the childeren.</p>
     *
     * @param dw    The dom writer.
     * @param nodes The nodes.
     * @throws IOException Shit happens.
     */
    public static void storeNodes(DomWriter dw, AbstractDom[] nodes)
            throws IOException {
        for (int i = 0; i < nodes.length; i++) {
            AbstractDom node = nodes[i];
            if ((dw.getMask() & MASK_TEXT) != 0) {
                node.storeNode(dw);
            } else {
                dw.writeIndent();
                node.storeNode(dw);
                dw.write("\n");
            }
        }
    }

    /*****************************************************/
    /* Synchronized API Attributes                       */
    /*****************************************************/

    /**
     * <p>Retrieve a value for a key.</p>
     *
     * @param key The key.
     * @return The String value.
     */
    public String getAttr(String key) {
        return (String) getAttrObj(key);
    }

    /**
     * <p>Retrieve a value for a key.</p>
     *
     * @param key The key.
     * @return The long value.
     */
    public long getAttrLong(String key) {
        return ((Long) getAttrObj(key)).longValue();
    }

    /**
     * <p>Retrieve a value for a key.</p>
     *
     * @param key The key.
     * @return The value.
     */
    public Object getAttrObj(String key) {
        if (key == null)
            throw new NullPointerException("key missing");
        synchronized (this) {
            int k = (kvs != null ? XmlMachine.indexAttr(kvs, key) : -1);
            return (k >= 0 ? kvs.getValue(k) : null);
        }
    }

    /**
     * <p>Create a snapshot of the attribute names.</p>
     *
     * @return The snapshot.
     */
    public String[] snapshotAttrs() {
        String[] names = cachekvs;
        if (names != null)
            return names;
        synchronized (this) {
            names = cachekvs;
            if (names != null)
                return names;
            if (kvs != null) {
                names = new String[kvs.size()];
                for (int i = 0; i < kvs.size(); i++)
                    names[i] = kvs.getKey(i);
            } else {
                names = VOID_ATTRS;
            }
            cachekvs = names;
        }
        return names;
    }

    /**
     * <p>Add the key to the map.</p>
     * <p>Entry is create at the bottom.</p>
     *
     * @param key The key.
     * @param val The String value.
     */
    public void setAttr(String key, String val) {
        setAttrObj(key, val);
    }

    /**
     * <p>Add the key to the map.</p>
     * <p>Entry is create at the bottom.</p>
     *
     * @param key The key.
     * @param val The long value.
     */
    public void setAttrLong(String key, long val) {
        setAttrObj(key, Long.valueOf(val));
    }

    /**
     * <p>Add the key to the map.</p>
     * <p>Entry is create at the bottom.</p>
     *
     * @param key The key.
     * @param val The value.
     */
    public void setAttrObj(String key, Object val) {
        if (key == null)
            throw new NullPointerException("key missing");
        if (val == null)
            throw new NullPointerException("value missing");
        synchronized (this) {
            int k = (kvs != null ? XmlMachine.indexAttr(kvs, key) : -1);
            if (k >= 0) {
                kvs.setValue(k, val);
            } else {
                if (kvs == null)
                    kvs = new AssocArray<String, Object>();
                kvs.add(key, val);
                cachekvs = null;
            }
        }
    }

    /**
     * <p>Remove the key from the map.</p>
     *
     * @param key The key.
     */
    public void resetAttr(String key) {
        if (key == null)
            throw new NullPointerException("key missing");
        synchronized (this) {
            int k = (kvs != null ? XmlMachine.indexAttr(kvs, key) : -1);
            if (k >= 0) {
                kvs.removeEntry(k);
                if (kvs.size() == 0) {
                    kvs = null;
                } else {
                    kvs.resize();
                }
                cachekvs = null;
            }
        }
    }

    /*****************************************************/
    /* Synchronized API Nodes                            */
    /*****************************************************/

    /**
     * <p>Retrieve a DOM nodes snapshot.</p>
     *
     * @return The DOM nodes snapshot.
     */
    public AbstractDom[] snapshotNodes() {
        AbstractDom[] nodes = cachechildren;
        if (nodes != null)
            return nodes;
        synchronized (this) {
            nodes = cachechildren;
            if (nodes != null)
                return nodes;
            if (children != null) {
                nodes = new AbstractDom[children.size()];
                children.toArray(nodes);
            } else {
                nodes = VOID_CHILDREN;
            }
            cachechildren = nodes;
        }
        return nodes;
    }

    /**
     * <p>Add a DOM node.</p>
     *
     * @param node The DOM node.
     */
    public void addNode(AbstractDom node) {
        addNode(-1, node);
    }

    /**
     * <p>Add a DOM node at some index.</p>
     *
     * @param i    The index, negative index counts from last.
     * @param node The DOM node.
     */
    public void addNode(int i, AbstractDom node) {
        if (node == null)
            throw new NullPointerException("node missing");
        synchronized (this) {
            if (children == null)
                children = new ListArray<AbstractDom>();
            if (i < 0)
                i += children.size() + 1;
            if (i < 0)
                i = 0;
            if (i > children.size())
                i = children.size();
            children.add(i, node);
            node.parent = this;
            cachechildren = null;
        }
    }

    /**
     * <p>Remove a DOM node.</p>
     *
     * @param node The DOM node.
     */
    public void removeNode(AbstractDom node) {
        if (node == null)
            throw new NullPointerException("node missing");
        synchronized (this) {
            int k = (children != null ? children.indexOf(node) : -1);
            if (k >= 0) {
                children.remove(k);
                if (children.size() == 0)
                    children = null;
                cachechildren = null;
            }
        }
    }

    /*****************************************************/
    /* Synchronized API Children                         */
    /*****************************************************/

    /**
     * <p>Retrieve a child by name,</p>
     *
     * @param key The child name,
     * @return The DOM element, or null.
     */
    public DomElement getChild(String key) {
        if (key == null)
            throw new NullPointerException("key missing");
        synchronized (this) {
            int k = (children != null ? indexChild(children, key) : -1);
            return (k >= 0 ? (DomElement) children.get(k) : null);
        }
    }

    /**
     * <p>Set a child by name</p>
     *
     * @param key  The child name
     * @param node The DOM element.
     */
    public void setChild(String key, DomElement node) {
        if (key == null)
            throw new NullPointerException("key missing");
        if (node == null)
            throw new NullPointerException("node missing");
        synchronized (this) {
            int k = (children != null ? indexChild(children, key) : -1);
            if (k >= 0) {
                children.set(k, node);
            } else {
                if (children == null)
                    children = new ListArray<AbstractDom>();
                children.add(node);
            }
            node.parent = this;
            cachechildren = null;
        }
    }

    /**
     * <p>Remove a child by name.</p>
     *
     * @param key The name.
     */
    public void resetChild(String key) {
        synchronized (this) {
            int k = (children != null ? indexChild(children, key) : -1);
            if (k >= 0) {
                children.remove(k);
                if (children.size() == 0)
                    children = null;
                cachechildren = null;
            }
        }
    }

    /**
     * <p>Find the index of a child.</p>
     * <p>The case of the child is ignored.</p>
     *
     * @param n The child name.
     * @return The DOM element index, or -1.
     */
    private static int indexChild(ListArray<AbstractDom> l, String n) {
        int m = l.size();
        for (int i = 0; i < m; i++) {
            AbstractDom node = l.get(i);
            if (node instanceof DomElement &&
                    ((DomElement) node).isName(n))
                return i;
        }
        return -1;
    }

    /*****************************************************/
    /* Synchronized API Index                            */
    /*****************************************************/

    /**
     * <p>Retrieve a DOM node index.</p>
     *
     * @param node The DOM node.
     * @return The DOM node index, or -1.
     */
    public int getNodeIndex(AbstractDom node) {
        synchronized (this) {
            return (children != null ? children.indexOf(node) : -1);
        }
    }

    /**
     * <p>Retrieve the child at some index.</p>
     *
     * @param i The index, negative index counts from last.
     * @return The child, or null.
     */
    public AbstractDom getNode(int i) {
        synchronized (this) {
            if (children == null)
                return null;
            if (i < 0)
                i += children.size();
            if (i < 0)
                return null;
            if (i >= children.size())
                return null;
            return children.get(i);
        }
    }

    /**
     * <p>Retrieve the DOM nodes count.</p>
     *
     * @return The DOM nodes count.
     */
    public int getNodesCount() {
        synchronized (this) {
            return (children != null ? children.size() : 0);
        }
    }

    /***************************************************************/
    /* Object Protocol                                             */
    /***************************************************************/

    /**
     * <p>Create a deep copy.</p>
     *
     * @return The deep copy.
     */
    public Object clone() {
        DomElement de = (DomElement) super.clone();
        AssocArray<String, Object> newkvs;
        synchronized (this) {
            newkvs = (kvs != null ? (AssocArray<String, Object>) kvs.clone() : null);
        }
        ListArray<AbstractDom> newchildren = null;
        AbstractDom[] nodes = snapshotNodes();
        for (int i = 0; i < nodes.length; i++) {
            AbstractDom node = nodes[i];
            if (newchildren == null)
                newchildren = new ListArray<AbstractDom>();
            node = (AbstractDom) node.clone();
            node.parent = de;
            newchildren.add(node);
        }
        de.kvs = newkvs;
        de.children = newchildren;
        return de;
    }

    /**
     * Reset to initial default state.
     */
    void reinitialize() {
        super.reinitialize();
        kvs = null;
        cachekvs = null;
        children = null;
        cachechildren = null;
    }

    /**
     * <p>Some test cases.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) throws IOException, ScannerError {
        PrintWriter pw = new PrintWriter(System.out);
        String str = "<>Some <b>text</b></>";
        pw.println("str=" + str);

        StringReader sr = new StringReader(str);
        DomElement de = new DomElement();
        de.load(sr, AbstractDom.MASK_TEXT);
        pw.print("de=");
        de.store(pw, null, AbstractDom.MASK_TEXT);
        pw.println();

        DomElement de2 = (DomElement) de.clone();
        pw.print("de2=");
        de2.store(pw, null, AbstractDom.MASK_TEXT);
        pw.println();

        pw.println();

        str = "<>Some <b></b> text</>";
        pw.println("str=" + str);

        sr = new StringReader(str);
        de = new DomElement();
        de.load(sr, AbstractDom.MASK_TEXT);
        pw.print("de=");
        de.store(pw, null, AbstractDom.MASK_TEXT);
        pw.println();
    }
    */

}
