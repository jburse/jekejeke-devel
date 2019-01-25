package jekpro.model.pretty;

import derek.util.protect.LicenseError;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.foreign.LookupResource;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.regex.ScannerError;
import matula.util.system.AbstractRecognizer;
import matula.util.system.ConnectionReader;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.Reader;

/**
 * <p>Specialization of the source class for file based sources.</p>
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
public abstract class AbstractFile extends AbstractSource {
    public static final int MASK_SRC_FBOM = 0x00000100;

    public final static String OP_DSLH = "../";
    public final static String OP_DSLH2 = "..";
    public final static String OP_DSLH3 = "/..";

    private long lastmodified;
    private String etag = "";
    private long expiration;
    private String encoding;
    private int buffer;

    /**
     * <p>Retrieve the last modified date.</p>
     *
     * @return The last modified date, or 0.
     */
    public long getLastModified() {
        return lastmodified;
    }

    /**
     * <p>Set the last modified date.</p>
     *
     * @param l The last modified date.
     */
    public void setLastModified(long l) {
        lastmodified = l;
    }

    /**
     * <p>Retrieve the ETag,</p>
     *
     * @return The ETag, or "":
     */
    public String getETag() {
        return etag;
    }

    /**
     * <p>Set the ETag.</p>
     *
     * @param e The ETag.
     */
    public void setETag(String e) {
        etag = e;
    }

    /**
     * <p>Retrieve the expiration.</p>
     *
     * @return The expiration.
     */
    public long getExpiration() {
        return expiration;
    }

    /**
     * <p>Set the expiration.</p>
     *
     * @param e The expiration.
     */
    public void setExpiration(long e) {
        expiration = e;
    }

    /**
     * <p>Retrieve the encoding.</p>
     *
     * @return The encoding.
     */
    public String getEncoding() {
        return encoding;
    }

    /**
     * <p>Set the encoding.</p>
     *
     * @param e The encoding.
     */
    public void setEncoding(String e) {
        encoding = e;
    }

    /**
     * <p>Retrieve the buffer size.</p>
     *
     * @return The buffer size.
     */
    public int getBuffer() {
        return buffer;
    }

    /**
     * <p>Set the buffer size.</p>
     *
     * @param b The buffer size.
     */
    public void setBuffer(int b) {
        buffer = b;
    }

    /**
     * <p>Create a source file from path.</p>
     *
     * @param p The path.
     */
    AbstractFile(String p) {
        super(p);
    }

    /***************************************************************/
    /* Open & Close Reader                                         */
    /***************************************************************/

    /**
     * <p>Open a read stream.</p>
     *
     * @param if_modified The if modified flag.
     * @param lopts       The options.
     * @param en          The engine.
     * @return The reader or null.
     * @throws EngineMessage Shit happens.
     */
    public Reader openReader(boolean if_modified,
                             LoadOpts lopts, Engine en)
            throws EngineMessage {
        if ((getBits() & AbstractSource.MASK_SRC_PREL) != 0) {
            if (if_modified)
                return null;
            if_modified = true;
        }

        if ((lopts.getFlags() & LoadOpts.MASK_LOAD_CACH) != 0) {
            long expiration = getExpiration();
            if (expiration != 0 &&
                    System.currentTimeMillis() < expiration)
                return null;
        }

        OpenOpts fopts = new OpenOpts();
        if ((lopts.getFlags() & LoadOpts.MASK_LOAD_NOBO) != 0)
            fopts.setFlags(fopts.getFlags() | OpenOpts.MASK_OPEN_NOBR);
        if ((lopts.getFlags() & LoadOpts.MASK_LOAD_CACH) != 0)
            fopts.setFlags(fopts.getFlags() | OpenOpts.MASK_OPEN_CACH);
        fopts.setEncoding(lopts.getEncoding());
        fopts.setBuffer(lopts.getBuffer());
        if (if_modified) {
            fopts.setIfModifiedSince(getLastModified());
            fopts.setIfNoneMatch(getETag());
        }
        Reader reader;
        try {
            reader = (Reader) fopts.openRead((AbstractRecognizer) en.store.proxy, getPath());
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        } catch (LicenseError x) {
            throw new EngineMessage(EngineMessage.licenseError(x.getError()));
        } catch (ScannerError x) {
            throw new EngineMessage(EngineMessage.syntaxError(x.getMessage()));
        }
        return reader;
    }

    /**
     * <p>Move the attributes.</p>
     *
     * @param reader The reader.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public void closeReader(Reader reader)
            throws EngineMessage, EngineException {
        super.closeReader(reader);

        if (!(reader instanceof ConnectionReader))
            return;

        ConnectionReader cr = (ConnectionReader) reader;
        setEncoding(cr.getEncoding());
        setLastModified(cr.getLastModified());
        setETag(cr.getETag());
        setExpiration(cr.getExpiration());
        if (cr.getBom()) {
            setBit(AbstractFile.MASK_SRC_FBOM);
        } else {
            resetBit(AbstractFile.MASK_SRC_FBOM);
        }
        setBuffer(cr.getBuffer());
    }

    /**************************************************************/
    /* Source Keys                                                */
    /**************************************************************/

    /**
     * <p>Create a file source.</p>
     *
     * @param key   The source key.
     * @param rsc   The rscs flag.
     * @param store The store.
     * @return The file source.
     */
    public static AbstractFile createSourceFile(String key, boolean rsc,
                                                Store store) {
        AbstractFile src;
        if (rsc) {
            src = new FileResource(key);
        } else {
            src = new FileText(key);
        }
        src.setBranch(LookupResource.AbsoluteURIstoRoots(key, store));
        return src;
    }

    /**************************************************************/
    /* OS Paths                                                   */
    /**************************************************************/

    /**
     * @param file The file flag.
     * @param en   The engine.
     * @return The string.
     * @throws EngineMessage Shit happens.
     */
    public static String slashToOsString(Object t, Display d,
                                         boolean file, Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        if (file && t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SET)) {
            SkelCompound sc = (SkelCompound) t;
            String name = slashToOsString(sc.args[0], d, file, en);
            return CachePackage.composeArray(name);
        } else if (file && t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(OP_DSLH)) {
            SkelCompound sc = (SkelCompound) t;
            String fun = SpecialUniv.derefAndCastString(sc.args[0], d);
            return OP_DSLH + fun;
        } else if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
            SkelCompound sc = (SkelCompound) t;
            String pack = slashToOsString(sc.args[0], d, file, en);
            String fun = SpecialUniv.derefAndCastString(sc.args[1], d);
            return CacheModule.composeOs(pack, fun);
        } else if (t instanceof SkelAtom) {
            return ((SkelAtom) t).fun;
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_ATOM, t), d);
        }
    }

    /**
     * @param file  The file flag.
     * @param fun   The string.
     * @param scope The call-site, not null.
     * @return The package expression.
     */
    public static Object osToSlashSkel(String fun,
                                       boolean file,
                                       AbstractSource scope) {
        String dir;
        if (file & CachePackage.isArray(fun)) {
            return new SkelCompound(new SkelAtom(Foyer.OP_SET, scope),
                    osToSlashSkel(CachePackage.sepComp(fun), file, null));
        } else if (CacheModule.isOs(fun) &&
                !OP_DSLH2.equals(dir = CacheModule.sepDirectory(fun)) &&
                !dir.endsWith(OP_DSLH3)) {
            return new SkelCompound(new SkelAtom(Foyer.OP_SLASH, scope),
                    osToSlashSkel(dir, file, null),
                    new SkelAtom(CacheModule.sepFile(fun)));
        } else if (file && fun.startsWith(OP_DSLH)) {
            dir = fun.substring(OP_DSLH.length());
            return new SkelCompound(new SkelAtom(OP_DSLH, scope),
                    osToSlashSkel(dir, file, null));
        } else {
            return new SkelAtom(fun, scope);
        }
    }

    /**
     * <p>Some tests.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) {
        String fun = "../../abc/def[]";
        Object res=osToSlashSkel(fun, true, null);
        System.out.println("res="+res);
    }
    */

}
