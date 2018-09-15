package matula.util.transform;

import matula.util.data.MapHash;
import matula.util.regex.ScannerError;
import matula.util.system.AbstractRuntime;

import java.io.IOException;
import java.text.ParseException;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class XSDResolver {
    public static final String BEAN_MISSING_CLASS = "bean_missing_class";
    public static final String BEAN_MISMATCHED_BEAN = "bean_mismatched_bean";
    public static final String BEAN_ILLEGAL_ACCESS = "bean_illegal_access";
    public static final String BEAN_INST_EXCEPTION = "bean_inst_exception";

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
            throws ValidationError, IOException, ScannerError, ParseException {
        XSDSchema schema = resolved.get(_class);
        if (schema == null) {
            InterfacePath pu = newPath(_class);

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
            schema.setName(AbstractRuntime.classToString(_class));
            schema.digestElements(pu.getFound());

            resolved.add(_class, schema);
        }
        return schema;
    }

    /*************************************************************/
    /* Bean Loader                                               */
    /*************************************************************/

    /**
     * <p>Find class of a bean.</p>
     *
     * @param bean The bean name.
     * @return The class of the bean.
     * @throws ValidationError Check error.
     */
    static Class<?> findClass(String bean)
            throws ValidationError {
        ClassLoader loader = XSDSchema.class.getClassLoader();
        Class<?> _class = AbstractRuntime.stringToClass(bean, loader);
        if (_class == null)
            throw new ValidationError(BEAN_MISSING_CLASS, bean);
        return _class;
    }

    /**
     * <p>Create an instance of a bean.</p>
     *
     * @param _class The class of the bean.
     * @return The instance of the bean.
     * @throws ValidationError Check error.
     */
    static InterfacePath newPath(Class<?> _class)
            throws ValidationError {
        try {
            Object obj = _class.newInstance();
            if (!(obj instanceof InterfacePath))
                throw new ValidationError(BEAN_MISMATCHED_BEAN,
                        AbstractRuntime.classToString(_class));
            return (InterfacePath) obj;
        } catch (IllegalAccessException x) {
            throw new ValidationError(BEAN_ILLEGAL_ACCESS,
                    AbstractRuntime.classToString(_class));
        } catch (InstantiationException x) {
            throw new ValidationError(BEAN_INST_EXCEPTION,
                    AbstractRuntime.classToString(_class));
        }
    }

    /**
     * <p>Create an instance of a bean.</p>
     *
     * @param _class The class of the bean.
     * @return The instance of the bean.
     * @throws ValidationError Check error.
     */
    static InterfaceFunc newFunc(Class<?> _class)
            throws ValidationError {
        try {
            Object obj = _class.newInstance();
            if (!(obj instanceof InterfaceFunc))
                throw new ValidationError(BEAN_MISMATCHED_BEAN,
                        AbstractRuntime.classToString(_class));
            return (InterfaceFunc) obj;
        } catch (IllegalAccessException x) {
            throw new ValidationError(BEAN_ILLEGAL_ACCESS,
                    AbstractRuntime.classToString(_class));
        } catch (InstantiationException x) {
            throw new ValidationError(BEAN_INST_EXCEPTION,
                    AbstractRuntime.classToString(_class));
        }
    }

}