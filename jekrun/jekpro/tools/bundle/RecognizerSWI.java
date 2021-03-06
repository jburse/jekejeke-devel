package jekpro.tools.bundle;

import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.PrologReader;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.config.BaseBundle;
import matula.util.config.AbstractDescription;
import matula.util.regex.ScannerError;
import matula.util.wire.AbstractRecognizer;
import matula.util.wire.FileExtension;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.Properties;

/**
 * <p>The recogizer for SWI-Packages.</p>
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
public final class RecognizerSWI extends AbstractRecognizer {
    public final static RecognizerSWI DEFAULT = new RecognizerSWI();

    public final static String OP_TITLE = "title";
    public final static String OP_VERSION = "version";

    public final static String OP_HOME = "home";
    public final static String OP_AUTHOR = "author";

    public final static String OP_DATE = "date";
    public final static String OP_ICON = "icon";

    /**
     * <p>Create a default recognizer.</p>
     */
    private RecognizerSWI() {
        addFileExtension(".plx", new FileExtension(FileExtension.MASK_USES_TEXT
                | FileExtension.MASK_DATA_ECRY, "text/prolog"));
        addFileExtension(".pl", new FileExtension(FileExtension.MASK_USES_TEXT, "text/prolog"));
    }


    /**
     * <p>Load binary properties.</p>
     *
     * @param prop  The properties.
     * @param in    The reader.
     * @param param The param or null.
     */
    public void loadBinary(Properties prop, InputStream in,
                           Object param) {
        throw new IllegalArgumentException("not supported");
    }

    /**
     * <p>Load text properties.</p>
     *
     * @param prop   The properties.
     * @param reader The reader.
     * @param param  The param or null.
     * @throws IOException  Problem reading.
     * @throws ScannerError Problem reading.
     */
    public void loadText(Properties prop, Reader reader,
                         Object param)
            throws IOException, ScannerError {
        if (param == null) {
            loadTextPack(prop, reader);
        } else {
            loadTextIcon(prop, reader, param);
        }
    }

    /**
     * <p>Load text properties of a package slip.</p>
     *
     * @param prop   The properties.
     * @param reader The reader.
     * @throws IOException  Problem reading.
     * @throws ScannerError Problem reading.
     */
    private static void loadTextPack(Properties prop, Reader reader)
            throws IOException, ScannerError {
        PrologReader rd = new PrologReader();
        rd.getScanner().setReader(reader);
        for (; ; ) {
            try {
                Object val = rd.parseHeadStatement();
                if (val instanceof SkelAtom &&
                        ((SkelAtom) val).fun.equals(AbstractSource.OP_END_OF_FILE))
                    break;
                loadTextPackSkel(prop, val);
            } catch (EngineMessage x) {
                throw new RuntimeException("shouldn't happen", x);
            } catch (EngineException x) {
                throw new RuntimeException("shouldn't happen", x);
            }
        }
    }

    /**
     * <p>Populate the properties with a SWI-Package attribute.</p>
     *
     * @param prop The properties.
     * @param obj  The attribute.
     */
    private static void loadTextPackSkel(Properties prop, Object obj) {
        if (obj instanceof SkelCompound &&
                ((SkelCompound) obj).args.length == 1 &&
                ((SkelCompound) obj).sym.fun.equals(OP_TITLE)) {
            obj = ((SkelCompound) obj).args[0];
            if (obj instanceof SkelAtom)
                prop.put(AbstractDescription.PROP_CAPA_PRODUCT, ((SkelAtom) obj).fun);
        } else if (obj instanceof SkelCompound &&
                ((SkelCompound) obj).args.length == 1 &&
                ((SkelCompound) obj).sym.fun.equals(OP_VERSION)) {
            obj = ((SkelCompound) obj).args[0];
            if (obj instanceof SkelAtom)
                prop.put(AbstractDescription.PROP_CAPA_RELEASE, ((SkelAtom) obj).fun);
        } else if (obj instanceof SkelCompound &&
                ((SkelCompound) obj).args.length == 1 &&
                ((SkelCompound) obj).sym.fun.equals(OP_HOME)) {
            obj = ((SkelCompound) obj).args[0];
            if (obj instanceof SkelAtom)
                prop.put(BaseBundle.PROP_LICENSE_INFO, ((SkelAtom) obj).fun);
        } else if (obj instanceof SkelCompound &&
                ((SkelCompound) obj).args.length == 2 &&
                ((SkelCompound) obj).sym.fun.equals(OP_AUTHOR)) {
            obj = ((SkelCompound) obj).args[0];
            if (obj instanceof SkelAtom)
                addText(prop, BaseBundle.PROP_PRODUCT_COMPANY, ((SkelAtom) obj).fun);
        }
    }

    /**
     * <p>Add an attribute value to the properties.</p>
     *
     * @param prop  The properties.
     * @param key   The key.
     * @param value The value.
     */
    private static void addText(Properties prop, String key, String value) {
        String old = prop.getProperty(key);
        if (old == null) {
            prop.put(key, value);
        } else {
            prop.put(key, old + ", " + value);
        }
    }

    /**
     * <p>Load text properties of an icon slip.</p>
     *
     * @param prop   The properties.
     * @param reader The reader.
     * @param param  The param.
     * @throws IOException  Problem reading.
     * @throws ScannerError Problem reading.
     */
    private static void loadTextIcon(Properties prop, Reader reader,
                                     Object param)
            throws IOException, ScannerError {
        PrologReader rd = new PrologReader();
        rd.getScanner().setReader(reader);
        for (; ; ) {
            try {
                Object val = rd.parseHeadStatement();
                if (val instanceof SkelAtom &&
                        ((SkelAtom) val).fun.equals(AbstractSource.OP_END_OF_FILE))
                    break;
                loadTextIconSkel(prop, val, param);
            } catch (EngineMessage x) {
                throw new RuntimeException("shouldn't happen", x);
            } catch (EngineException x) {
                throw new RuntimeException("shouldn't happen", x);
            }
        }
    }

    /**
     * <p>Populate the properties with a SWI-Package attribute.</p>
     *
     * @param prop  The properties.
     * @param obj   The attribute.
     * @param param The param.
     */
    private static void loadTextIconSkel(Properties prop, Object obj, Object param) {
        if (obj instanceof SkelCompound &&
                ((SkelCompound) obj).args.length == 1 &&
                ((SkelCompound) obj).sym.fun.equals(OP_DATE)) {
            obj = ((SkelCompound) obj).args[0];
            if (obj instanceof SkelAtom)
                prop.put(AbstractDescription.PROP_CAPA_DATE, ((SkelAtom) obj).fun);
        } else if (obj instanceof SkelCompound &&
                ((SkelCompound) obj).args.length == 1 &&
                ((SkelCompound) obj).sym.fun.equals(OP_ICON)) {
            obj = ((SkelCompound) obj).args[0];
            if (obj instanceof SkelAtom)
                prop.put(BaseBundle.PROP_CAPA_ICON, param + ((SkelAtom) obj).fun);
        }
    }

    /*****************************************************************/
    /* Class Loader                                                  */
    /*****************************************************************/

    /**
     * <p>Retrieve the loader.</p>
     *
     * @return The loader.
     */
    public ClassLoader getLoader() {
        return null;
    }

    /**
     * <p>Retrieve the parent.</p>
     *
     * @return The parent.
     */
    public AbstractRecognizer getParent() {
        return null;
    }

}