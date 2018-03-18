package matula.util.transform;

import matula.util.data.MapHash;
import matula.util.regex.ScannerError;

import java.io.IOException;

/**
 * <p>This class provides an xml schema resolver.</p>
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
public final class XSDResolver {
    private MapHash<Class<?>, XSDSchema> resolved = new MapHash<Class<?>, XSDSchema>();

    /**
     * <p>Resolve a bean to a schema.</p>
     *
     * @param _class The class of the bean.
     * @return The schema.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check errror.
     */
    public XSDSchema resolveSchema(Class<?> _class)
            throws ValidationError, IOException, ScannerError {
        XSDSchema schema = resolved.get(_class);
        if (schema == null) {
            InterfacePath pu = XSLSheet.newBean(_class);

            pu.setFlags(pu.getFlags() | InterfacePath.FLAG_SCHM);
            pu.list();
            boolean f = pu.next();
            pu.close();
            if (!f)
                throw new IllegalArgumentException("schema missing");

            int flags = 0;
            if ((pu.getFlags() & InterfacePath.FLAG_STYL) != 0)
                flags |= InterfacePath.FLAG_STYL;
            if ((pu.getFlags() & InterfacePath.FLAG_DIRE) != 0)
                flags |= InterfacePath.FLAG_DIRE;

            schema = new XSDSchema();
            schema.setFlags(flags);
            schema.setResolver(this);
            schema.digestElements(pu.getFound());

            resolved.add(_class, schema);
        }
        return schema;
    }

}