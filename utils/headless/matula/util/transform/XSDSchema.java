package matula.util.transform;

import matula.util.data.ListArray;
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
 * <p>This class provides an xml schema.</p>
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
    public static XSDSchema meta = new XSDSchema();

    public static final String SCHEMA_DUPLICATE_DECL = "schema_duplicate_decl";

    private final MapHashLink<String, XSDDecl> decls = new MapHashLink<String, XSDDecl>();

    static {
        try {
            InputStream in = XSDSchema.class.getResourceAsStream("schema.xsd");

            DomElement schema = new DomElement();
            BufferedReader reader = new BufferedReader(
                    new InputStreamReader(in, ForeignUri.ENCODING_UTF8));
            schema.load(reader, AbstractDom.MASK_LIST);
            reader.close();

            meta.traverseElements(schema);
        } catch (ScannerError x) {
            throw new RuntimeException("meta failed", x);
        } catch (ValidationError x) {
            throw new RuntimeException("meta failed", x);
        } catch (IOException x) {
            throw new RuntimeException("meta failed", x);
        }
    }

    /**
     * <p>Check the schema and digest the elements of the XSD schema.</p>
     *
     * @param de The schema dom element.
     * @throws ValidationError Check errror.
     */
    public void digestElements(DomElement de) throws ValidationError {
        XMLCheck xc = new XMLCheck();
        xc.setMask(AbstractDom.MASK_LIST);
        xc.setSchema(meta);
        xc.check(de);
        traverseElements(de);
    }

    /**
     * <p>Digest the elements of the XSD schema.</p>
     *
     * @param de The schema dom element.
     * @throws ValidationError Check error.
     */
    private void traverseElements(DomElement de) throws ValidationError {
        AbstractDom[] nodes = de.snapshotNodes();
        for (int i = 0; i < nodes.length; i++) {
            DomElement e = (DomElement) nodes[i];
            String name = e.getAttr(XSDDeclElem.ATTR_ELEMENT_NAME);
            XSDDeclElem xe = XSDDeclElem.traverseElement(e);
            putDecl(name, xe);
            ListArray<String> mandatory = traverseAttributes(e);
            xe.setMandatory(mandatory);
        }
    }

    /**
     * <p>Digest the attributes of the XSD schema.</p>
     *
     * @param de      The schema dom element.
     * @return The mandatory attributes.
     * @throws ValidationError Check error.
     */
    private ListArray<String> traverseAttributes(DomElement de)
            throws ValidationError {
        String name = de.getAttr(XSDDeclElem.ATTR_ELEMENT_NAME);
        ListArray<String> mandatory = new ListArray<String>();
        AbstractDom[] nodes = de.snapshotNodes();
        for (int i = 0; i < nodes.length; i++) {
            DomElement e = (DomElement) nodes[i];
            String attr = e.getAttr(XSDDeclAttr.ATTR_ATTRIBUTE_NAME);
            XSDDeclAttr xa = XSDDeclAttr.traverseAttribute(e);
            putDecl(name + "." + attr, xa);
            if (!xa.getOptional())
                mandatory.add(attr);
        }
        return mandatory;
    }

    /**
     * <p>Set a XSD schema declaration.</p>
     *
     * @param name The name.
     * @param xd   The XSD schema decalaration.
     * @throws ValidationError Check error.
     */
    public void putDecl(String name, XSDDecl xd) throws ValidationError {
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
        return decls.get(name);
    }

    /**
     * <p>Retrieve the XSD schema declarations.</p>
     *
     * @return The XSD schema declarations.
     */
    public String[] snapshotDecls() {
        String[] res = new String[decls.size()];
        int k = 0;
        for (MapEntry<String, XSDDecl> entry = decls.getFirstEntry();
             entry != null; entry = decls.successor(entry))
            res[k++] = entry.key;
        return res;
    }

    /**
     * <p>Some test cases.</p
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) {
        String[] decls = meta.snapshotDecls();
        for (int i = 0; i < decls.length; i++) {
            String name = decls[i];
            System.out.print(name);
            XSDDecl decl = meta.getDecl(name);
            if (decl instanceof XSDDeclElem) {
                String parent = ((XSDDeclElem) decl).getParent();
                if (parent != null)
                    System.out.print(" parent='" + parent + "'");
            } else {
                boolean optional = ((XSDDeclAttr) decl).getOptional();
                if (optional)
                    System.out.print(" use='optional'");
                int type = ((XSDDeclAttr) decl).getType();
                if (type == XSDDeclAttr.TYPE_STRING) {
                    System.out.print(" type='string'");
                } else if (type == XSDDeclAttr.TYPE_INTEGER) {
                    System.out.print(" type='integer'");
                }
            }
            System.out.println();
        }
    }
    */

}