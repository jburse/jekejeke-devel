package matula.util.format;

import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;
import matula.util.data.SetEntry;
import matula.util.data.SetHashLink;
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
    private String name = "";
    private MapHashLink<String, String> attributes = new MapHashLink<String, String>();
    private SetHashLink<DomNode> children = new SetHashLink<DomNode>();
    private DomNode[] cached;

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
     * @param t The name.
     */
    public void setName(String t) {
        if (t == null)
            throw new NullPointerException("name missing");
        name = t;
    }

    /**
     * <p>Reintialize this dom element.</p>
     */
    void reinitialize() {
        name = "";
        attributes.clear();
        children.clear();
        cached = null;
    }

    /**
     * <p>Load a dom element.</p>
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
                name = dr.getType();
                for (int i = 0; i < dr.getAttrCount(); i++) {
                    if (attributes.get(dr.getAttr(i)) != null)
                        throw new ScannerError(DomReader.DOM_DUPLICATE_ATTRIBUTE);
                    String temp = dr.getValueAt(i);
                    temp = ForeignXml.sysTextUnescape(XmlMachine.stripValue(temp));
                    attributes.put(dr.getAttr(i), temp);
                }
                dr.nextTagOrText();
                if (!closed) {
                    loadChildren(dr);
                    checkEnd(dr);
                }
                break;
            case XmlMachine.RES_EOF:
                throw new ScannerError(DomReader.DOM_START_MISSING);
            default:
                throw new IllegalArgumentException("illegal res");
        }
    }


    /**
     * <p>Load the children.</p>
     *
     * @param dr The dom reader.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private void loadChildren(DomReader dr) throws IOException, ScannerError {
        for (; ; ) {
            switch (dr.getRes()) {
                case XmlMachine.RES_TEXT:
                    DomText dt = new DomText();
                    dt.load(dr);
                    dt.parent = this;
                    children.putKey(dt);
                    break;
                case XmlMachine.RES_TAG:
                    if (dr.getType().startsWith(DomReader.STRING_SLASH))
                        return;
                    DomElement dh = new DomElement();
                    dh.load(dr);
                    dh.parent = this;
                    children.putKey(dh);
                    break;
                case XmlMachine.RES_EOF:
                    return;
                default:
                    throw new IllegalArgumentException("illegal res");
            }
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
    private boolean checkClosed(DomReader dr) throws ScannerError {
        int n = dr.getAttrCount();
        if (n != 0 &&
                "".equals(dr.getValueAt(n - 1)) &&
                dr.getAttr(n - 1).equals(DomReader.STRING_SLASH)) {
            String temp = dr.getAttr(n - 1);
            temp = temp.substring(0, temp.length() - 1);
            if (!"".equals(temp)) {
                dr.setAttr(n - 1, temp);
            } else {
                dr.removeAttrValue(n - 1);
            }
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Check whether the actual tag is an end tag.</p>
     * <p>As a side effect a correct tag is consumed.</p>
     *
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private void checkEnd(DomReader dr) throws ScannerError, IOException {
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
                if (!name.equals(temp))
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
     *
     * @param dw The dom writer.
     * @throws IOException Shit happens.
     */
    void store(DomWriter dw) throws IOException {
        dw.writer.write("<");
        dw.writer.write(name);
        for (MapEntry<String, String> entry = attributes.getFirstEntry();
             entry != null; entry = attributes.successor(entry)) {
            dw.writer.write(" ");
            dw.writer.write(entry.key);
            if (!"".equals(entry.value)) {
                dw.writer.write("=\"");
                dw.writer.write(ForeignXml.sysTextEscape(entry.value));
                dw.writer.write("\"");
            }
        }
        if (children.size() == 0) {
            dw.writer.write("/>");
        } else {
            if ((dw.ret & DomReader.MASK_TEXT) != 0) {
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


    /**
     * <p>Store the childeren.</p>
     *
     * @param dw The dom writer.
     * @throws IOException Shit happens.
     */
    private void storeChildren(DomWriter dw) throws IOException {
        for (SetEntry<DomNode> entry = children.getFirstEntry();
             entry != null; entry = children.successor(entry)) {
            if ((dw.ret & DomReader.MASK_TEXT) != 0) {
                entry.key.store(dw);
            } else {
                dw.writeIndent();
                entry.key.store(dw);
                dw.writer.write("\n");
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
     * @return The value.
     */
    public String getAttr(String key) {
        if (key == null)
            throw new NullPointerException("key missing");
        synchronized (this) {
            return attributes.get(key);
        }
    }

    /**
     * <p>Add the key to the map.</p>
     * <p>Entry is create at the bottom.</p>
     *
     * @param key   The key.
     * @param value The value.
     */
    public void setAttr(String key, String value) {
        if (key == null)
            throw new NullPointerException("key missing");
        if (value == null)
            throw new NullPointerException("value missing");
        synchronized (this) {
            MapEntry<String, String> entry = attributes.getEntry(key);
            if (entry != null) {
                entry.value = value;
            } else {
                attributes.put(key, value);
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
            if (attributes.get(key) != null)
                attributes.remove(key);
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
            res = new String[attributes.size()];
            attributes.toArrayKeys(res);
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
     */
    public boolean addChild(DomNode dh) throws InterruptedException {
        if (dh == null)
            throw new NullPointerException("child missing");
        dh.beginReparent();
        try {
            if (dh.parent != null)
                return false;
            synchronized (this) {
                children.putKey(dh);
                cached = null;
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
                cached = null;
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
        DomNode[] res = cached;
        if (res != null)
            return res;
        synchronized (this) {
            res = cached;
            if (res != null)
                return res;
            res = new DomNode[children.size()];
            children.toArray(res);
            cached = res;
        }
        return res;
    }

}
