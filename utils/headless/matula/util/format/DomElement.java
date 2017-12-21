package matula.util.format;

import matula.util.data.AssocArray;
import matula.util.data.ListArray;
import matula.util.data.MapHash;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignXml;
import matula.util.system.OpenOpts;

import java.io.IOException;

/**
 * <p>This class provides a dom element.</p>
 * </p>
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
public final class DomElement extends DomNode {
    private static final String DOM_MISSING_ELEM = "dom_missing_elem";
    private static final String DOM_ILLEGAL_VALUE = "dom_illegal_value";
    private static final String DOM_CLOSED_EMPTY = "dom_closed_empty";
    private static final String DOM_MISSING_END = "dom_missing_end";
    private static final String DOM_UNEXPECTED_ATTR = "dom_unexpected_attr";
    private static final String DOM_MISMATCHED_END = "dom_mismatched_end";

    private String name = XmlMachine.VALUE_EMPTY;
    private AssocArray<String, Object> kvs = new AssocArray<String, Object>();
    private ListArray<DomNode> children = new ListArray<DomNode>();

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
                AssocArray<String, Object> newkvs = new AssocArray<String, Object>();
                for (int i = 0; i < dr.getAttrCount(); i++) {
                    String valstr = dr.getValueAt(i);
                    Object val;
                    if (isNumber(valstr)) {
                        try {
                            val = Long.parseLong(valstr);
                        } catch (NumberFormatException x) {
                            throw new ScannerError(DOM_ILLEGAL_VALUE, OpenOpts.getOffset(dr.getReader()));
                        }
                    } else {
                        val = ForeignXml.sysTextUnescape(XmlMachine.stripValue(valstr));
                    }
                    newkvs.add(dr.getAttr(i), val);
                }
                name = type;
                synchronized (this) {
                    kvs = newkvs;
                }
                boolean empty = checkEmpty(dr.getControl(), type);
                ListArray<DomNode> cs;
                if (!closed && !empty) {
                    if ((dr.getMask() & DomNode.MASK_TEXT) == 0 && checkAny(dr.getControl(), type)) {
                        int mask = dr.getMask();
                        dr.setMask(dr.getMask() | DomNode.MASK_TEXT);
                        dr.nextTagOrText();
                        cs = loadChildren(dr);
                        dr.setMask(mask);
                    } else {
                        dr.nextTagOrText();
                        cs = loadChildren(dr);
                    }
                    checkEnd(type, dr);
                } else {
                    if (closed && empty)
                        throw new ScannerError(DOM_CLOSED_EMPTY, -1);
                    cs = new ListArray<DomNode>();
                    dr.nextTagOrText();
                }
                setChildren(cs);
                break;
            case XmlMachine.RES_EOF:
                throw new ScannerError(DOM_MISSING_ELEM, OpenOpts.getOffset(dr.getReader()));
            default:
                throw new IllegalArgumentException("illegal res");
        }
    }

    /**
     * <p>Check whether the value starts as a number.</p>
     *
     * @param v The value.
     * @return True if the value starts as a number, otherwise false.
     */
    private static boolean isNumber(String v) {
        return (v.length() > 0 && (Character.isDigit(v.codePointAt(0)) ||
                (v.charAt(0) == '-' && v.length() > 1 &&
                        Character.isDigit(v.codePointAt(1)))));
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
        if (sizeChildren() != 0) {
            dw.copyStart(this);
            if ((dw.getMask() & DomNode.MASK_TEXT) == 0 &&
                    checkAny(dw.getControl(), name)) {
                int mask = dw.getMask();
                dw.setMask(dw.getMask() | DomNode.MASK_TEXT);
                storeChildren2(dw);
                dw.setMask(mask);
            } else {
                storeChildren2(dw);
            }
            dw.copyEnd(this);
        } else {
            if (!checkEmpty(dw.getControl(), name)) {
                dw.copyEmpty(this);
            } else {
                dw.copyStart(this);
            }
        }
    }

    /**
     * <p>Store the childeren.</p>
     *
     * @param dw The dom writer.
     * @throws IOException Shit happens.
     */
    private void storeChildren2(DomWriter dw) throws IOException {
        if ((dw.getMask() & MASK_TEXT) != 0) {
            storeChildren(dw);
        } else {
            dw.write("\n");
            dw.incIndent();
            storeChildren(dw);
            dw.decIndent();
            dw.writeIndent();
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
    static ListArray<DomNode> loadChildren(DomReader dr)
            throws IOException, ScannerError {
        ListArray<DomNode> res = new ListArray<DomNode>();
        for (; ; ) {
            switch (dr.getRes()) {
                case XmlMachine.RES_TEXT:
                    DomText dt = new DomText();
                    dt.loadNode(dr);
                    res.add(dt);
                    break;
                case XmlMachine.RES_TAG:
                    if (dr.getType().length() > 0 &&
                            dr.getType().charAt(0) == XmlMachine.CHAR_SLASH)
                        return res;
                    DomElement dh = new DomElement();
                    dh.loadNode(dr);
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
     * <p>Set the elements.</p>
     * <p>
     * <p>Not synchronized, uses cut-over.</p>
     */
    void setChildren(ListArray<DomNode> cs) {
        for (int i = 0; i < cs.size(); i++) {
            DomNode node = cs.get(i);
            node.parent = this;
        }
        synchronized (this) {
            children = cs;
        }
    }

    /**
     * <p>Store the childeren.</p>
     *
     * @param dw The dom writer.
     * @throws IOException Shit happens.
     */
    void storeChildren(DomWriter dw)
            throws IOException {
        DomNode[] nodes = snapshotChildren();
        for (int i = 0; i < nodes.length; i++) {
            DomNode node = nodes[i];
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
    /* Tag Control                                       */
    /*****************************************************/

    /**
     * <p>Check whether the tag type is empty.</p>
     *
     * @param control The tag control.
     * @param type    The tag type.
     * @return True if the tag type is empty, otherwise false.
     */
    public static boolean checkEmpty(MapHash<String, Integer> control,
                                     String type) {
        if (control == null)
            return false;
        Integer val = control.get(type);
        if (val == null) {
            return false;
        } else if (val.intValue() == DomNode.TYPE_EMPTY) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Check whether the tag type is any.</p>
     *
     * @param control The tag control.
     * @param type    The tag type.
     * @return True if the tag type is any, otherwise false.
     */
    public static boolean checkAny(MapHash<String, Integer> control,
                                   String type) {
        if (control == null)
            return false;
        Integer val = control.get(type);
        if (val == null) {
            return false;
        } else if (val.intValue() == DomNode.TYPE_ANY) {
            return true;
        } else {
            return false;
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
            int k = XmlMachine.indexAttr(kvs, key);
            if (k != -1) {
                return kvs.getValue(k);
            } else {
                return null;
            }
        }
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
            int k = XmlMachine.indexAttr(kvs, key);
            if (k != -1) {
                kvs.setValue(k, val);
            } else {
                kvs.add(key, val);
            }
        }
    }

    /**
     * <p>Remove the key from the map.</p>
     *
     * @param key The key.
     */
    public void removeAttr(String key) {
        if (key == null)
            throw new NullPointerException("key missing");
        synchronized (this) {
            int k = XmlMachine.indexAttr(kvs, key);
            if (k != -1) {
                kvs.removeEntry(k);
                kvs.resize();
            }
        }
    }

    /**
     * <p>Create a snapshot of the attribute names.</p>
     *
     * @return The snapshot.
     */
    public String[] snapshotAttrs() {
        String[] names;
        synchronized (this) {
            names = new String[kvs.size()];
            for (int i = 0; i < kvs.size(); i++)
                names[i] = kvs.getKey(i);
        }
        return names;
    }

    /*****************************************************/
    /* Synchronized API Children                         */
    /*****************************************************/

    /**
     * <p>Retrieve the number of elements.</p>
     *
     * @return The number of elements.
     */
    public int sizeChildren() {
        return children.size();
    }

    /**
     * <p>Add a child.</p>
     *
     * @param dh The child.
     * @return True if add succeeded, otherwise false.
     * @throws InterruptedException Transaction was interrupted.
     */
    public boolean addChild(DomNode dh) throws InterruptedException {
        if (dh == null)
            throw new NullPointerException("child missing");
        dh.beginReparent();
        try {
            if (dh.parent != null)
                return false;
            synchronized (this) {
                children.add(dh);
            }
            dh.parent = this;
        } finally {
            dh.endReparent();
        }
        return true;
    }

    /**
     * <p>Remoe a child.</p>
     *
     * @param dh The child.
     * @throws InterruptedException Transaction was interrupted.
     */
    public boolean removeChild(DomNode dh) throws InterruptedException {
        if (dh == null)
            throw new NullPointerException("child missing");
        dh.beginReparent();
        try {
            if (dh.parent != this)
                return false;
            synchronized (this) {
                children.remove(dh);
            }
            dh.parent = null;
        } finally {
            dh.endReparent();
        }
        return true;
    }

    /**
     * <p>Retrieve a children snapshot.</p>
     *
     * @return The children snapshot.
     */
    public DomNode[] snapshotChildren() {
        DomNode[] nodes;
        synchronized (this) {
            nodes = new DomNode[children.size()];
            children.toArray(nodes);
        }
        return nodes;
    }

    /**
     * <p>Retrieve the child at some index.</p>
     *
     * @param i The index, negative index counts from last.
     * @return The child, or null.
     */
    public DomNode getChildAt(int i) {
        DomNode node;
        synchronized (this) {
            if (i < 0)
                i += children.size();
            if (i < 0)
                return null;
            if (i >= children.size())
                return null;
            node = children.get(i);
        }
        return node;
    }

    /**
     * <p>Retrieve the child index.</p>
     *
     * @param dh The child.
     * @return The index.
     */
    public int getChildIndex(DomNode dh) {
        int res;
        synchronized (this) {
            res = children.indexOf(dh);
        }
        return res;
    }

    /**
     * <p>Add a child at some index.</p>
     *
     * @param i  The index, negative index counts from last.
     * @param dh The child.
     * @return True if add succeeded, otherwise false.
     * @throws InterruptedException Transaction was interrupted.
     */
    public boolean addChild(int i, DomNode dh) throws InterruptedException {
        if (dh == null)
            throw new NullPointerException("child missing");
        dh.beginReparent();
        try {
            if (dh.parent != null)
                return false;
            synchronized (this) {
                if (i < 0)
                    i += children.size() + 1;
                if (i < 0)
                    return false;
                if (i > children.size())
                    return false;
                children.add(i, dh);
            }
            dh.parent = this;
        } finally {
            dh.endReparent();
        }
        return true;
    }

}
