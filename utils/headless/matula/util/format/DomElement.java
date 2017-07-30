package matula.util.format;

import matula.util.data.ListArray;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignXml;

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
    private String name = XmlMachine.VALUE_EMPTY;
    private ListArray<String> attr = new ListArray<String>();
    private ListArray<String> value = new ListArray<String>();
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
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    void load(DomReader dr) throws IOException, ScannerError {
        switch (dr.getRes()) {
            case XmlMachine.RES_TEXT:
                throw new ScannerError(DomReader.DOM_START_MISSING);
            case XmlMachine.RES_TAG:
                if (dr.getType().startsWith(DomReader.STRING_SLASH))
                    throw new ScannerError(DomReader.DOM_START_MISSING);
                boolean closed = checkClosed(dr);
                String type = dr.getType();
                ListArray<String> as = new ListArray<String>();
                ListArray<String> vs = new ListArray<String>();
                for (int i = 0; i < dr.getAttrCount(); i++) {
                    if (XmlMachine.indexAttr(as, dr.getAttr(i)) != -1)
                        throw new ScannerError(DomReader.DOM_DUPLICATE_ATTRIBUTE);
                    String val = dr.getValueAt(i);
                    val = ForeignXml.sysTextUnescape(XmlMachine.stripValue(val));
                    as.add(dr.getAttr(i));
                    vs.add(val);
                }
                name = type;
                synchronized (this) {
                    attr = as;
                    value = vs;
                }
                dr.nextTagOrText();
                if (!closed) {
                    loadChildren(dr);
                    checkEnd(type, dr);
                } else {
                    clearChildren();
                }
                break;
            case XmlMachine.RES_EOF:
                throw new ScannerError(DomReader.DOM_START_MISSING);
            default:
                throw new IllegalArgumentException("illegal res");
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
                dr.getAttr(n - 1).equals(DomReader.STRING_SLASH)) {
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
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private static void checkEnd(String type, DomReader dr)
            throws ScannerError, IOException {
        switch (dr.getRes()) {
            case XmlMachine.RES_TEXT:
                throw new ScannerError(DomReader.DOM_END_MISSING);
            case XmlMachine.RES_TAG:
                if (!dr.getType().startsWith(DomReader.STRING_SLASH))
                    throw new ScannerError(DomReader.DOM_END_MISSING);
                if (dr.getAttrCount() != 0)
                    throw new ScannerError(DomReader.DOM_ATTRIBUTES_UNEXPECTED);
                String temp = dr.getType();
                temp = temp.substring(1);
                if (!type.equals(temp))
                    throw new ScannerError(DomReader.DOM_TYPE_MISMATCH);
                dr.nextTagOrText();
                break;
            case XmlMachine.RES_EOF:
                throw new ScannerError(DomReader.DOM_END_MISSING);
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
    void store(DomWriter dw) throws IOException {
        dw.writer.write("<");
        dw.writer.write(name);

        String[] names = snapshotAttrs();
        for (int i = 0; i < names.length; i++) {
            String name = names[i];
            String val = getAttr(name);
            if (val == null)
                continue;
            dw.writer.write(" ");
            dw.writer.write(name);
            if (!"".equals(val)) {
                dw.writer.write("=\"");
                dw.writer.write(ForeignXml.sysTextEscape(val));
                dw.writer.write("\"");
            }
        }

        if (sizeChildren() == 0) {
            dw.writer.write("/>");
        } else {
            if ((dw.ret & MASK_TEXT) != 0) {
                dw.writer.write(">");
                storeChildren(dw);
                dw.writer.write("</");
                dw.writer.write(name);
                dw.writer.write(">");
            } else {
                dw.writer.write(">\n");
                dw.incIndent();
                storeChildren(dw);
                dw.decIndent();
                dw.writeIndent();
                dw.writer.write("</");
                dw.writer.write(name);
                dw.writer.write(">");
            }
        }
    }

    /*****************************************************/
    /* Children Store/Load                               */
    /*****************************************************/

    /**
     * <p>Load a dom element.</p>
     * <p>Not synchronized, uses cut-over.</p>
     *
     * @param dr The dom reader.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    void loadChildren(DomReader dr) throws IOException, ScannerError {
        ListArray<DomNode> cs = loadChildren2(dr);
        for (int i = 0; i < cs.size(); i++) {
            DomNode node = cs.get(i);
            node.parent = this;
        }
        synchronized (this) {
            children = cs;
        }
    }

    /**
     * <p>Load the children.</p>
     *
     * @param dr The dom reader.
     * @return The children.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private static ListArray<DomNode> loadChildren2(DomReader dr)
            throws IOException, ScannerError {
        ListArray<DomNode> res = new ListArray<DomNode>();
        for (; ; ) {
            switch (dr.getRes()) {
                case XmlMachine.RES_TEXT:
                    DomText dt = new DomText();
                    dt.load(dr);
                    res.add(dt);
                    break;
                case XmlMachine.RES_TAG:
                    if (dr.getType().startsWith(DomReader.STRING_SLASH))
                        return res;
                    DomElement dh = new DomElement();
                    dh.load(dr);
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
     * <p>Clear the elements.</p>
     *
     * <p>Not synchronized, uses cut-over.</p>
     */
    void clearChildren() {
        ListArray<DomNode> cs = new ListArray<DomNode>();
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
            if ((dw.ret & MASK_TEXT) != 0) {
                node.store(dw);
            } else {
                dw.writeIndent();
                node.store(dw);
                dw.writer.write("\n");
            }
        }
    }

    /**
     * <p>Retrieve the number of elements.</p>
     *
     * @return The number of elements.
     */
    int sizeChildren() {
        return children.size();
    }

    /*****************************************************/
    /* Synchronized API Attributes                       */
    /*****************************************************/

    /**
     * <p>Retrieve a value for a key.</p>
     *
     * @param key The key.
     * @return The value.
     */
    public String getAttr(String key) {
        if (key == null)
            throw new NullPointerException("key missing");
        synchronized (this) {
            int k = XmlMachine.indexAttr(attr, key);
            if (k != -1) {
                return value.get(k);
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
     * @param val The value.
     */
    public void setAttr(String key, String val) {
        if (key == null)
            throw new NullPointerException("key missing");
        if (val == null)
            throw new NullPointerException("value missing");
        synchronized (this) {
            int k = XmlMachine.indexAttr(attr, key);
            if (k != -1) {
                value.set(k, val);
            } else {
                attr.add(key);
                value.add(val);
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
            int k = XmlMachine.indexAttr(attr, key);
            if (k != -1) {
                attr.remove(k);
                value.remove(k);
            }
        }
    }

    /**
     * <p>Create a snapshot of the attribute names.</p>
     *
     * @return The snapshot.
     */
    public String[] snapshotAttrs() {
        String[] res;
        synchronized (this) {
            res = new String[attr.size()];
            attr.toArray(res);
        }
        return res;
    }

    /*****************************************************/
    /* Synchronized API Children                         */
    /*****************************************************/

    /**
     * <p>Add a child.</p>
     *
     * @param dh The child.
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
        DomNode[] res;
        synchronized (this) {
            res = new DomNode[children.size()];
            children.toArray(res);
        }
        return res;
    }

}
