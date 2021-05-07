package jekdev.model.pretty;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.PrologWriter;
import jekpro.reference.reflect.SpecialPred;
import matula.util.data.MapEntry;
import matula.util.system.ForeignFile;
import matula.util.system.ForeignUri;
import matula.util.system.ForeignXml;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

/**
 * <p>This class extends the Prolog writer.</p>
 *
 * @author Copyright 2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.5 (a fast and small prolog interpreter)
 */
public final class PrologWriterTrace extends PrologWriter {
    private final static String PREFIX_FOREIGN = "foreign:";
    private final static String EXTENSION_HTML = ".html";

    /************************************************************/
    /* Navigation Code                                          */
    /************************************************************/

    /**
     * <p>Linkify a string and append it</p>
     *
     * @param t  The string.
     * @param cp The predicate.
     * @throws IOException   IO error.
     * @throws EngineMessage Shit happens.
     */
    protected void appendLink(String t, CachePredicate cp)
            throws IOException, EngineMessage, EngineException {
        if ((spez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkBegin(wr, getSource(), cp, engine);
        append(t);
        if ((spez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            writeLinkEnd(wr);
    }

    /**
     * <p>Write the navigation link begin comment.</p>
     *
     * @param wr  The writer.
     * @param src The source.
     * @param cp  The predicate.
     * @param en  The engine.
     * @throws IOException IO Error.
     */
    static void writeLinkBegin(Writer wr, AbstractSource src,
                               CachePredicate cp, Engine en)
            throws IOException, EngineMessage, EngineException {
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0) {
            StringBuilder buf = new StringBuilder();
            buf.append("%<a href=\"");
            buf.append("__error__");
            buf.append("\">\n");
            wr.write(buf.toString());
            return;
        }
        Predicate pick = cp.pick;
        AbstractSource pos = PrologWriterTrace.getLocation(src, pick);
        if (pos == null) {
            StringBuilder buf = new StringBuilder();
            buf.append("%<a href=\"");
            buf.append("__error__");
            buf.append("\">\n");
            wr.write(buf.toString());
            return;
        }
        StringBuilder buf;
        String uri = ForeignUri.sysUriMake(PrologWriterTrace.getNavigation(
                src, pos), "", PrologWriterTrace.getHash(pick, pos, en));
        buf = new StringBuilder();
        buf.append("%<a href=\"");
        buf.append(ForeignXml.sysTextEscape(ForeignUri.sysUriEncode(uri)));
        buf.append("\">\n");
        wr.write(buf.toString());
    }

    /**
     * <p>Write the navigation link end comment.</p>
     *
     * @param wr The writer.
     * @throws IOException IO Error.
     */
    static void writeLinkEnd(Writer wr)
            throws IOException {
        wr.write("%</a>\n");
    }

    /************************************************************/
    /* Navigation Helper I                                      */
    /************************************************************/

    /**
     * <p>Retrieve the predicate home.</p>
     *
     * @param src  The default.
     * @param pick The predicate.
     * @return The predicate home or null.
     */
    private static AbstractSource getLocation(AbstractSource src, Predicate pick) {
        if (src == null || pick.getDef(src) == null) {
            MapEntry<AbstractSource, Integer>[] snapshot = pick.snapshotDefs();
            src = (snapshot.length > 0 ? snapshot[0].key : null);
        }
        return (src != null ? getHomeFile(src) : null);
    }

    /**
     * <p>Retrieve the navigation to the predicate location.</p>
     *
     * @param src The source.
     * @param pos The predicate location.
     * @return The navigation.
     */
    private static String getNavigation(AbstractSource src, AbstractSource pos) {
        AbstractSource home = getHomeFile(src);
        String orig = getHTMLPath(home.getPath());
        String module = getHTMLPath(pos.getPath());
        return ForeignFile.sysPathRelative(orig, module);
    }

    /**
     * <p>Retrieve the hash to the predicate location.</p>
     *
     * @param pick The predicate.
     * @param src  The source.
     * @param en   The engine.
     * @return The hash.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static String getHash(Predicate pick, AbstractSource src, Engine en)
            throws EngineMessage, EngineException {
        Object sc = SpecialPred.provableToColonSkel(pick, src);
        StringWriter wr = new StringWriter();
        PrologWriter.toString(sc, Display.DISPLAY_CONST, wr, PrologWriter.FLAG_QUOT, en);
        return wr.toString();
    }

    /**
     * <p>Retrieve the home file for a given source.</p>
     *
     * @param src The source.
     * @return The home file.
     */
    private static AbstractSource getHomeFile(AbstractSource src) {
        MapEntry<AbstractSource, Integer>[] deps = src.snapshotDeps();
        for (int i = 0; i < deps.length; i++) {
            MapEntry<AbstractSource, Integer> dep = deps[i];
            if ((dep.value.intValue() & AbstractSource.MASK_IMPT_HOFL) != 0)
                return dep.key;
        }
        return src;
    }

    /**
     * <p>Retrieve the HTML path.</p></op>
     *
     * @param path The path.
     * @return The HTML path.
     */
    private static String getHTMLPath(String path) {
        StringBuilder buf = new StringBuilder();
        if (ForeignUri.sysUriIsRelative(path))
            buf.append(PREFIX_FOREIGN);
        buf.append(ForeignFile.sysNameBase(path));
        buf.append(EXTENSION_HTML);
        return buf.toString();
    }

}