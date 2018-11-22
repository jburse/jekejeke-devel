package matula.util.format;

import matula.util.data.ListArray;

/**
 * <p>This class provides a dom element.</p>
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
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class DomElement extends AbstractDom {
    private static final AbstractDom[] VOID_NODES = new AbstractDom[0];

    private String name;
    private ListArray<AbstractDom> attrs;
    private AbstractDom[] cacheattrs;
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
        return (name != null ? name.equalsIgnoreCase(n) : null == n);
    }

    /**
     * <p>Set the key-value pairs fast.</p>
     *
     * @param as The key-value pairs.
     */
    public void setAttrsFast(ListArray<AbstractDom> as) {
        if (as != null) {
            for (int i = 0; i < as.size(); i++) {
                AbstractDom node = as.get(i);
                node.setParent(this);
            }
        }
        synchronized (this) {
            attrs = as;
            cacheattrs = null;
        }
    }

    /**
     * <p>Set the children fast.</p>
     *
     * @param cs The children or null.
     */
    public void setChildrenFast(ListArray<AbstractDom> cs) {
        if (cs != null) {
            for (int i = 0; i < cs.size(); i++) {
                AbstractDom node = cs.get(i);
                node.setParent(this);
            }
        }
        synchronized (this) {
            children = cs;
            cachechildren = null;
        }
    }

    /*****************************************************/
    /* Synchronized API Attributes                       */
    /*****************************************************/

    /**
     * <p>Retrieve the string value for a key.</p>
     *
     * @param key The key name.
     * @return The string value.
     */
    public String getAttr(String key) {
        DomText dt = (DomText) getAttrDom(key);
        return (dt != null ? dt.getData() : null);
    }

    /**
     * <p>Retrieve the long value for a key.</p>
     *
     * @param key The key name.
     * @return The long value.
     */
    public long getAttrLong(String key) {
        DomText dt = (DomText) getAttrDom(key);
        return (dt != null ? dt.getDataLong() : null);
    }

    /**
     * <p>Retrieve a value for a key.</p>
     *
     * @param key The key name.
     * @return The value.
     */
    public Object getAttrObj(String key) {
        DomText dt = (DomText) getAttrDom(key);
        return (dt != null ? dt.getDataObj() : null);
    }

    /**
     * <p>Retrieve an element for a key.</p
     *
     * @param key The key name.
     * @return The element.
     */
    public AbstractDom getAttrDom(String key) {
        if (key == null)
            throw new NullPointerException("key missing");
        synchronized (this) {
            int k = (attrs != null ? indexAttr(attrs, key) : -1);
            return (k >= 0 ? attrs.get(k) : null);
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
        AbstractDom node = getAttrDom(key);
        DomText dt;
        if (!(node instanceof DomText)) {
            dt = new DomText();
            dt.setKey(key);
            setAttrDom(key, dt);
        } else {
            dt = (DomText) node;
        }
        dt.setData(val);
    }

    /**
     * <p>Add the key to the map.</p>
     * <p>Entry is create at the bottom.</p>
     *
     * @param key The key.
     * @param val The long value.
     */
    public void setAttrLong(String key, long val) {
        AbstractDom node = getAttrDom(key);
        DomText dt;
        if (!(node instanceof DomText)) {
            dt = new DomText();
            dt.setKey(key);
            setAttrDom(key, dt);
        } else {
            dt = (DomText) node;
        }
        dt.setDataLong(val);
    }

    /**
     * <p>Set a value for a key.</p>
     * <p>Entry is create at the bottom.</p>
     *
     * @param key The key.
     * @param val The value.
     */
    public void setAttrObj(String key, Object val) {
        AbstractDom node = getAttrDom(key);
        DomText dt;
        if (!(node instanceof DomText)) {
            dt = new DomText();
            dt.setKey(key);
            setAttrDom(key, dt);
        } else {
            dt = (DomText) node;
        }
        dt.setDataObj(val);
    }

    /**
     * <p>Set an element for a key.</p>
     * <p>Entry is create at the bottom.</p>
     *
     * @param key  The key.
     * @param node The element.
     */
    public void setAttrDom(String key, AbstractDom node) {
        if (key == null)
            throw new NullPointerException("key missing");
        if (node == null)
            throw new NullPointerException("node missing");
        synchronized (this) {
            int k = (attrs != null ? indexAttr(attrs, key) : -1);
            if (k >= 0) {
                attrs.set(k, node);
            } else {
                if (attrs == null)
                    attrs = new ListArray<AbstractDom>();
                attrs.add(node);
            }
            node.setParent(this);
            cacheattrs = null;
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
            int k = (attrs != null ? indexAttr(attrs, key) : -1);
            if (k >= 0) {
                attrs.removeEntry(k);
                if (attrs.size() == 0) {
                    attrs = null;
                } else {
                    attrs.resize();
                }
                cacheattrs = null;
            }
        }
    }

    /**
     * <p>Find the index of an attribute.</p>
     * <p>The case of the attribute is ignored.</p>
     *
     * @param n The attribute name.
     * @return The DOM text index, or -1.
     */
    private static int indexAttr(ListArray<AbstractDom> l, String n) {
        int m = l.size();
        for (int i = 0; i < m; i++) {
            AbstractDom node = l.get(i);
            if (node.isKey(n))
                return i;
        }
        return -1;
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
            node.setParent(this);
            cachechildren = null;
        }
    }

    /**
     * <p>Remove a child by name.</p>
     *
     * @param key The name.
     */
    public void resetChild(String key) {
        if (key == null)
            throw new NullPointerException("key missing");
        synchronized (this) {
            int k = (children != null ? indexChild(children, key) : -1);
            if (k >= 0) {
                children.removeEntry(k);
                if (children.size() == 0) {
                    children = null;
                } else {
                    children.resize();
                }
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
    /* Synchronized API Attrs I                          */
    /*****************************************************/

    /**
     * <p>Retrieve a attributes snapshot.</p>
     *
     * @return The attributes snapshot.
     */
    public AbstractDom[] snapshotAttrs() {
        AbstractDom[] nodes = cacheattrs;
        if (nodes != null)
            return nodes;
        synchronized (this) {
            nodes = cacheattrs;
            if (nodes != null)
                return nodes;
            if (attrs != null) {
                nodes = new AbstractDom[attrs.size()];
                attrs.toArray(nodes);
            } else {
                nodes = VOID_NODES;
            }
            cacheattrs = nodes;
        }
        return nodes;
    }

    /**
     * <p>Add an attribute node.</p>
     *
     * @param node The attribute node.
     */
    public void addAttr(AbstractDom node) {
        addAttr(-1, node);
    }

    /**
     * <p>Add an attribute node at some index.</p>
     *
     * @param i    The index, negative index counts from last.
     * @param node The child node.
     */
    public void addAttr(int i, AbstractDom node) {
        if (node == null)
            throw new NullPointerException("attribute missing");
        synchronized (this) {
            if (attrs == null)
                attrs = new ListArray<AbstractDom>();
            if (i < 0)
                i += attrs.size() + 1;
            if (i < 0)
                i = 0;
            if (i > attrs.size())
                i = attrs.size();
            attrs.add(i, node);
            node.setParent(this);
            cacheattrs = null;
        }
    }

    /**
     * <p>Remove an attribute node.</p>
     *
     * @param node The attribute node.
     */
    public int removeAttr(AbstractDom node) {
        if (node == null)
            throw new NullPointerException("attribute missing");
        int k;
        synchronized (this) {
            k = (attrs != null ? attrs.indexOf(node) : -1);
            if (k >= 0) {
                attrs.remove(k);
                if (attrs.size() == 0)
                    attrs = null;
                cacheattrs = null;
            }
        }
        return k;
    }

    /*****************************************************/
    /* Synchronized API Children I                       */
    /*****************************************************/

    /**
     * <p>Retrieve a children snapshot.</p>
     *
     * @return The children snapshot.
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
                nodes = VOID_NODES;
            }
            cachechildren = nodes;
        }
        return nodes;
    }

    /**
     * <p>Add a child node.</p>
     *
     * @param node The child node.
     */
    public void addNode(AbstractDom node) {
        addNode(-1, node);
    }

    /**
     * <p>Add a child node at some index.</p>
     *
     * @param i    The index, negative index counts from last.
     * @param node The child node.
     */
    public void addNode(int i, AbstractDom node) {
        if (node == null)
            throw new NullPointerException("child missing");
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
            node.setParent(this);
            cachechildren = null;
        }
    }

    /**
     * <p>Remove a child node.</p>
     *
     * @param node The child node.
     */
    public int removeNode(AbstractDom node) {
        if (node == null)
            throw new NullPointerException("child missing");
        int k;
        synchronized (this) {
            k = (children != null ? children.indexOf(node) : -1);
            if (k >= 0) {
                children.remove(k);
                if (children.size() == 0)
                    children = null;
                cachechildren = null;
            }
        }
        return k;
    }

    /*****************************************************/
    /* Synchronized API Attrs II                         */
    /*****************************************************/

    /**
     * <p>Retrieve an attribute index.</p>
     *
     * @param node The attribute.
     * @return The attribute index, or -1.
     */
    public int getAttrIndex(AbstractDom node) {
        synchronized (this) {
            return (attrs != null ? attrs.indexOf(node) : -1);
        }
    }

    /**
     * <p>Retrieve the attribute at some index.</p>
     *
     * @param i The index, negative index counts from last.
     * @return The attribute, or null.
     */
    public AbstractDom getAttr(int i) {
        synchronized (this) {
            if (attrs == null)
                return null;
            if (i < 0)
                i += attrs.size();
            if (i < 0)
                return null;
            if (i >= attrs.size())
                return null;
            return attrs.get(i);
        }
    }

    /**
     * <p>Retrieve the children count.</p>
     *
     * @return The children count.
     */
    public int getAttrsCount() {
        synchronized (this) {
            return (attrs != null ? attrs.size() : 0);
        }
    }

    /*****************************************************/
    /* Synchronized API Children II                      */
    /*****************************************************/

    /**
     * <p>Retrieve a child index.</p>
     *
     * @param node The child.
     * @return The child index, or -1.
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
     * <p>Retrieve the children count.</p>
     *
     * @return The children count.
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

        /* clone the attributes */
        ListArray<AbstractDom> newattrs = null;
        AbstractDom[] attrs = snapshotAttrs();
        for (int i = 0; i < attrs.length; i++) {
            AbstractDom attr = attrs[i];
            if (newattrs == null)
                newattrs = new ListArray<AbstractDom>();
            attr = (AbstractDom) attr.clone();
            attr.setParent(de);
            newattrs.add(attr);
        }
        de.attrs = newattrs;

        /* clone the children */
        ListArray<AbstractDom> newchildren = null;
        AbstractDom[] nodes = snapshotNodes();
        for (int i = 0; i < nodes.length; i++) {
            AbstractDom node = nodes[i];
            if (newchildren == null)
                newchildren = new ListArray<AbstractDom>();
            node = (AbstractDom) node.clone();
            node.setParent(de);
            newchildren.add(node);
        }
        de.children = newchildren;

        return de;
    }

    /**
     * Reset to initial default state.
     */
    void reinitialize() {
        super.reinitialize();
        attrs = null;
        cacheattrs = null;
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
