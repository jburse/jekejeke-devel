package matula.util.system;

import derek.util.protect.LicenseError;
import matula.util.data.MapHash;

import java.lang.reflect.Array;

/**
 * <p>An abstract runtime such as Hotspot or Dalvik.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public abstract class AbstractRuntime {
    public final static String JAVA_ARRAY = "[]";
    private final static MapHash<String, Class> primitive = new MapHash<String, Class>();

    /* initialize the primitives */
    static {
        primitive.put(Character.TYPE.getName(), Character.TYPE);
        primitive.put(Byte.TYPE.getName(), Byte.TYPE);
        primitive.put(Short.TYPE.getName(), Short.TYPE);
        primitive.put(Integer.TYPE.getName(), Integer.TYPE);
        primitive.put(Long.TYPE.getName(), Long.TYPE);
        primitive.put(Float.TYPE.getName(), Float.TYPE);
        primitive.put(Double.TYPE.getName(), Double.TYPE);
        primitive.put(Boolean.TYPE.getName(), Boolean.TYPE);
        primitive.put(Void.TYPE.getName(), Void.TYPE);
    }

    /**
     * <p>Extend a class loader by a given path.</p>
     *
     * @param loader The old class loader.
     * @param path   The path.
     * @param data   The client data.
     * @return The new class loader.
     * @throws LicenseError License problem.
     */
    public abstract Object addPath(Object loader, String path, Object data)
            throws LicenseError;

    /**
     * <p>Commit the extension of a class loader.</p>
     *
     * @param loader The old class loader.
     * @return The new class loader.
     */
    public abstract Object commitPaths(Object loader);

    /**
     * <p>Convert a string to a class.</p>
     *
     * @param name The class name.
     * @param cl   The class loader.
     * @return The class, or null.
     */
    public static Class stringToClass(String name, ClassLoader cl) {
        if (name.endsWith(JAVA_ARRAY)) {
            name = name.substring(0, name.length() - JAVA_ARRAY.length());
            int count = 1;
            while (name.endsWith(JAVA_ARRAY)) {
                name = name.substring(0, name.length() - JAVA_ARRAY.length());
                count++;
            }
            Class clazz = stringToClassNonArray(name, cl);
            while (count > 0 && clazz != null) {
                Object o;
                try {
                    o = Array.newInstance(clazz, 0);
                } catch (IllegalArgumentException x) {
                    return null;
                }
                clazz = o.getClass();
                count--;
            }
            return clazz;
        } else {
            return stringToClassNonArray(name, cl);
        }
    }

    /**
     * <p>Convert a non-array string to a class.</p>
     *
     * @param name The class name.
     * @param cl   The class loader.
     * @return The class, or null.
     */
    private static Class stringToClassNonArray(String name, ClassLoader cl) {
        Class clazz = primitive.get(name);
        if (clazz != null)
            return clazz;
        try {
            clazz = cl.loadClass(name);
        } catch (ClassNotFoundException x) {
            return null;
        }
        return clazz;
    }

    /**
     * <p>Convert a class to a string.</p>
     *
     * @param clazz The class.
     * @return The string.
     */
    public static String classToString(Class clazz) {
        if (clazz.isArray()) {
            String res = JAVA_ARRAY;
            clazz = clazz.getComponentType();
            while (clazz.isArray()) {
                res = JAVA_ARRAY + res;
                clazz = clazz.getComponentType();
            }
            return clazz.getName() + res;
        } else {
            return clazz.getName();
        }
    }

}
