package matula.util.transform;

import matula.util.data.AssocArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;
import matula.util.format.AbstractDom;
import matula.util.format.DomElement;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignUri;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * <p>This class provides an xml schema digester.</p>
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
public final class XSDSchema {
    public static final int MODE_DRY = 0x00000001;

    private static final String NAME_IMPORT = "import";
    private static final String ATTR_IMPORT_BEAN = "bean";
    private static final String NAME_PARENT = "parent";
    private static final String ATTR_PARENT_NAME = "name";
    private static final String ATTR_PARENT_OCCURS = "occurs";

    public static final String SCHEMA_DUPLICATE_DECL = "schema_duplicate_decl";
    public static final String SCHEMA_UNDECLARED_ELEM = "schema_undeclared_elem";
    public static final String SCHEMA_UNDECLARED_ATTR = "schema_undeclared_attr";
    public static final String SCHEMA_DUPLICATE_IMPORT = "schema_duplicate_import";

    private final MapHashLink<String, XSDDecl> decls = new MapHashLink<String, XSDDecl>();
    private final MapHashLink<String, XSDSchema> imports = new MapHashLink<String, XSDSchema>();
    private int flags;
    private XSDResolver resolver;

    private static XSDSchema meta = new XSDSchema();

    static {
        try {
            InputStream in = XSDSchema.class.getResourceAsStream("schema.xsd");

            DomElement schema = new DomElement();
            BufferedReader reader = new BufferedReader(
                    new InputStreamReader(in, ForeignUri.ENCODING_UTF8));
            schema.load(reader, AbstractDom.MASK_LIST);
            reader.close();

            meta.traverseElements(schema, 0);
            meta.fixupConstraints();
        } catch (ScannerError x) {
            throw new RuntimeException("meta failed", x);
        } catch (ValidationError x) {
            throw new RuntimeException("meta failed", x);
        } catch (IOException x) {
            throw new RuntimeException("meta failed", x);
        }
    }

    /**
     * <p>Create an XSDSchema.</p>
     */
    public XSDSchema() {
        XSDDeclElem xe = new XSDDeclElem();
        decls.add("", xe);
    }

    /**
     * <p>Set the flags.</p>
     *
     * @param f The flags.
     */
    public void setFlags(int f) {
        flags = f;
    }

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public int getFlags() {
        return flags;
    }

    /**
     * <p>Set the resolver.</p>
     *
     * @param r The resolver.
     */
    public void setResolver(XSDResolver r) {
        resolver = r;
    }

    /**
     * <p>Retrieve the resolver.</p>
     *
     * @return The resolver.
     */
    public XSDResolver getResolver() {
        return resolver;
    }

    /**
     * <p>Check the schema and digest the elements of the XSD schema.</p>
     *
     * @param de   The schema dom element.
     * @param mode The mode flags.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check errror.
     */
    public void digestElements(DomElement de, int mode)
            throws ValidationError, IOException, ScannerError {
        XMLCheck xc = new XMLCheck();
        xc.setMask(AbstractDom.MASK_LIST);
        xc.setSchema(meta);
        xc.check(de);
        traverseElements(de, mode);
        fixupConstraints();
    }

    /**
     * <p>Check the schema and digest the elements of the XSD schema.</p>
     *
     * @param de The schema dom element.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check errror.
     */
    public void digestElements(DomElement de)
            throws ValidationError, IOException, ScannerError {
        digestElements(de, 0);
    }

    /**********************************************************/
    /* First Pass                                             */
    /**********************************************************/

    /**
     * <p>Digest the elements of the XSD schema.</p>
     *
     * @param de   The schema dom element.
     * @param mode The mode flags.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check errror.
     */
    private void traverseElements(DomElement de, int mode)
            throws ValidationError, IOException, ScannerError {
        AbstractDom[] nodes = de.snapshotNodes();
        for (int i = 0; i < nodes.length; i++) {
            DomElement e = (DomElement) nodes[i];
            if (e.isName(XSDDeclElem.NAME_ELEMENT)) {
                String name = e.getAttr(XSDDeclElem.ATTR_ELEMENT_NAME);
                XSDDeclElem xe = XSDDeclElem.traverseElement(e);
                putDecl(name, xe);
                traverseAttributes(e, xe, mode);
            } else if (e.isName(NAME_IMPORT)) {
                String bean = e.getAttr(ATTR_IMPORT_BEAN);
                XSDSchema schema;
                if ((mode & MODE_DRY) != 0) {
                    schema = new XSDSchema();
                } else {
                    Class<?> _class = XSLSheet.findClass(bean);
                    schema = resolver.resolveSchema(_class);
                }
                putImport(bean, schema);
            } else {
                throw new IllegalArgumentException("illegal node");
            }
        }
    }

    /**
     * <p>Digest the attributes of the XSD schema.</p>
     *
     * @param de   The schema dom element.
     * @param xe   The element declaration.
     * @param mode The mode flags.
     * @throws ValidationError Check error.
     */
    private void traverseAttributes(DomElement de, XSDDeclElem xe, int mode)
            throws ValidationError {
        String name = de.getAttr(XSDDeclElem.ATTR_ELEMENT_NAME);
        AbstractDom[] nodes = de.snapshotNodes();
        for (int i = 0; i < nodes.length; i++) {
            DomElement e = (DomElement) nodes[i];
            if (e.isName(XSDDeclAttr.NAME_ATTRIBUTE)) {
                String attr = e.getAttr(XSDDeclAttr.ATTR_ATTRIBUTE_NAME);
                XSDDeclAttr xa = XSDDeclAttr.traverseAttribute(e);
                putDecl(name + "." + attr, xa);
                if (!xa.getOptional())
                    xe.addMandatory(attr);
            } else if (e.isName(NAME_PARENT)) {
                String par = e.getAttr(ATTR_PARENT_NAME);
                int occurs;
                if ((mode & MODE_DRY) != 0) {
                    occurs = XSDDeclElem.OCCURS_NONDET;
                } else {
                    occurs = XSDDeclElem.checkOccurs(e, e.getAttr(ATTR_PARENT_OCCURS));
                }
                xe.putParent(par, occurs);
            } else {
                throw new IllegalArgumentException("illegal node");
            }
        }
    }

    /*****************************************************/
    /* Second Pass                                       */
    /*****************************************************/

    /**
     * <p>Fixup the constraints.</p>
     *
     * @throws ValidationError Check error.
     */
    private void fixupConstraints()
            throws ValidationError {
        String[] decls = snapshotDecls();
        for (int i = 0; i < decls.length; i++) {
            String name = decls[i];
            int k = name.indexOf('.');
            if (k != -1)
                continue;
            XSDDeclElem decl = getDeclElem(name);
            AssocArray<String, Integer> parent = decl.getParent();
            if (parent == null)
                continue;
            for (int j = 0; j < parent.size(); j++) {
                int occurs = parent.getValue(j).intValue();
                if (occurs == XSDDeclElem.OCCURS_NONDET)
                    continue;
                String par = parent.getKey(j);
                XSDDeclElem decl2 = getDeclElem(par);
                decl2.addConstraint(name, occurs);
            }
        }
    }

    /*****************************************************/
    /* Declaration Access/Modification                   */
    /*****************************************************/

    /**
     * <p>Set a XSD schema declaration.</p>
     * <p>Name should be passed already lower case.</p>
     *
     * @param name The name.
     * @param xd   The XSD schema declaration.
     * @throws ValidationError Check error.
     */
    public void putDecl(String name, XSDDecl xd)
            throws ValidationError {
        if (decls.get(name) != null)
            throw new ValidationError(SCHEMA_DUPLICATE_DECL, name);
        decls.add(name, xd);
    }

    /**
     * <p>Retrieve a XSD schema declaration.</p>
     *
     * @param name The name of an element or attribute.
     * @return The XSD schema declearation.
     */
    public XSDDecl getDecl(String name) {
        for (MapEntry<String, XSDSchema> entry = imports.getFirstEntry();
             entry != null; entry = imports.successor(entry)) {
            XSDDecl res = entry.value.getDecl(name);
            if (res != null)
                return res;
        }
        return decls.get(name);
    }

    /**
     * <p>Retrieve the XSD schema declarations.</p>
     *
     * @return The XSD schema declaration.
     */
    public String[] snapshotDecls() {
        String[] res = new String[decls.size()];
        int k = 0;
        for (MapEntry<String, XSDDecl> entry = decls.getFirstEntry();
             entry != null; entry = decls.successor(entry))
            res[k++] = entry.key;
        return res;
    }

    /*****************************************************/
    /* Checked Access/Modification                       */
    /*****************************************************/

    /**
     * <p>Retrieve an element declaration.</p>
     *
     * @param name The element name.
     * @return The element declaration.
     * @throws ValidationError Check error.
     */
    XSDDeclElem getDeclElem(String name)
            throws ValidationError {
        XSDDecl decl = getDecl(name);
        if (decl == null || !(decl instanceof XSDDeclElem))
            throw new ValidationError(SCHEMA_UNDECLARED_ELEM, name);
        return (XSDDeclElem) decl;
    }

    /**
     * <p>Retrieve an attribute declaration.</p>
     *
     * @param name The element name.
     * @param attr The attribute name.
     * @return The element declaration.
     * @throws ValidationError Check error.
     */
    XSDDeclAttr getDeclAttr(String name, String attr)
            throws ValidationError {
        String key = name + "." + attr;
        XSDDecl decl = getDecl(name + "." + attr);
        if (decl == null || !(decl instanceof XSDDeclAttr))
            throw new ValidationError(SCHEMA_UNDECLARED_ATTR, key);
        return (XSDDeclAttr) decl;
    }

    /*****************************************************/
    /* Imports Access/Modification                       */
    /*****************************************************/

    /**
     * <p>Set a XSD schema declaration.</p>
     *
     * @param name The name.
     * @throws ValidationError Check error.
     */
    public void putImport(String name, XSDSchema schema)
            throws ValidationError {
        if (imports.get(name) != null)
            throw new ValidationError(SCHEMA_DUPLICATE_IMPORT, name);
        imports.add(name, schema);
    }

    /**
     * <p>Retrieve the XSD schema imports.</p>
     *
     * @return The XSD schema imports.
     */
    public String[] snapshotImports() {
        String[] res = new String[imports.size()];
        int k = 0;
        for (MapEntry<String, XSDSchema> entry = imports.getFirstEntry();
             entry != null; entry = imports.successor(entry))
            res[k++] = entry.key;
        return res;
    }

    /*****************************************************/
    /* Bean Resolver                                     */
    /*****************************************************/

    /**
     * <p>Some test cases.</p
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) {
        String[] decls = XSLSheetCheck.meta.snapshotDecls();
        for (int i = 0; i < decls.length; i++) {
            String attr = decls[i];
            if ("".equals(attr))
                continue;
            System.out.print(attr);
            XSDDecl decl = XSLSheetCheck.meta.getDecl(attr);
            if (decl instanceof XSDDeclElem) {
                int complex = ((XSDDeclElem) decl).getComplex();
                if (complex == XSDDeclElem.COMPLEX_EMPTY) {
                    System.out.print(" complex=\"empty\"");
                } else if (complex == XSDDeclElem.COMPLEX_ANY) {
                    System.out.print(" complex=\"any\"");
                }
                System.out.println();
                AssocArray<String, Integer> parent = ((XSDDeclElem) decl).getParent();
                if (parent == null)
                    continue;
                for (int j = 0; j < parent.size(); j++) {
                    System.out.print(attr + "." + j);
                    System.out.print(" name=\"" + parent.getKey(j) + "\"");
                    int occurs = parent.getValue(j).intValue();
                    if (occurs == XSDDeclElem.OCCURS_DET) {
                        System.out.print(" occurs=\"" + XSDDeclElem.OP_DET + "\"");
                    } else if (occurs == XSDDeclElem.OCCURS_SEMIDET) {
                        System.out.print(" occurs=\"" + XSDDeclElem.OP_SEMIDET + "\"");
                    } else if (occurs == XSDDeclElem.OCCURS_MULTI) {
                        System.out.print(" occurs=\"" + XSDDeclElem.OP_MULTI + "\"");
                    }
                    System.out.println();
                }
            } else {
                boolean optional = ((XSDDeclAttr) decl).getOptional();
                if (optional)
                    System.out.print(" use=\"optional\"");
                int type = ((XSDDeclAttr) decl).getType();
                if (type == XSDDeclAttr.TYPE_STRING) {
                    System.out.print(" type=\"" + XSDDeclAttr.OP_STRING + "\"");
                } else if (type == XSDDeclAttr.TYPE_INTEGER) {
                    System.out.print(" type=\"" + XSDDeclAttr.OP_INTEGER + "\"");
                }
                System.out.println();
            }
        }
    }
    */

}