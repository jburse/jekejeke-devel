package jekdev.model.pretty;

import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.PrologWriterAnno;

import java.io.IOException;

/**
 * <p>This class extends the Prolog writer anno.</p>
 *
 * @author Copyright 2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.3.6 (a fast and small prolog interpreter)
 */
public final class PrologWriterAnnoTrace extends PrologWriterAnno {

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
            PrologWriterTrace.writeLinkBegin(wr, getSource(), cp, engine);
        append(t);
        if ((spez & SPEZ_META) != 0 && (flags & FLAG_NAVI) != 0)
            PrologWriterTrace.writeLinkEnd(wr);
    }

}