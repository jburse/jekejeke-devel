package matula.util.config;

import derek.util.protect.LicenseError;
import matula.util.data.ListArray;
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public abstract class AbstractRuntime {
    public final static String JAVA_ARRAY = "[]";
    private final static MapHash<String, Class> primitive = new MapHash<String, Class>();

    public final static String ASPECT_SWING = "swing";
    public final static String ASPECT_ANDROID = "android";

    private String aspect;

    /* initialize the primitives */
    static {
        primitive.add(Character.TYPE.getName(), Character.TYPE);
        primitive.add(Byte.TYPE.getName(), Byte.TYPE);
        primitive.add(Short.TYPE.getName(), Short.TYPE);
        primitive.add(Integer.TYPE.getName(), Integer.TYPE);
        primitive.add(Long.TYPE.getName(), Long.TYPE);
        primitive.add(Float.TYPE.getName(), Float.TYPE);
        primitive.add(Double.TYPE.getName(), Double.TYPE);
        primitive.add(Boolean.TYPE.getName(), Boolean.TYPE);
        primitive.add(Void.TYPE.getName(), Void.TYPE);
    }


    /**
     * <p>Retrieve the aspect.</p>
     *
     * @return The aspect.
     */
    public String getAspect() {
        return aspect;
    }

    /**
     * <p>Set the aspect.</p>
     *
     * @param a The aspect.
     */
    public void setAspect(String a) {
        aspect = a;
    }

    /*******************************************************************/
    /* Class Paths                                                     */
    /*******************************************************************/

    /**
     * <p>Extend a class loader by a given path.</p>
     *
     * @param parent The parent.
     * @param adr    The URL.
     * @param data   The client data.
     * @return The new class loader.
     * @throws LicenseError License problem.
     */
    public abstract ClassLoader addURL(ClassLoader parent, String adr,
                                       ClassLoader stop, Object data)
            throws LicenseError;

    /**
     * <p>Retrieve the paths.</p>
     *
     * @param loader The chain start.
     * @param stop   The chain stop.
     * @param data   The client data.
     * @return The paths.
     * @throws LicenseError License problem.
     */
    public abstract ListArray<String> getURLs(ClassLoader loader,
                                              ClassLoader stop, Object data)
            throws LicenseError;

    /********************************************************************/
    /* Class Loaders                                                    */
    /********************************************************************/

    /**
     * <p>Check whether a class loader is in some chain.</p>
     *
     * @param loader The chain start.
     * @param stop   The chain stop.
     * @param what   The class loader or null.
     * @return True if the class loader is in the chain, otherwise false.
     */
    public static boolean inChain(ClassLoader loader, ClassLoader stop,
                                  ClassLoader what) {
        if (what == null)
            return (stop == null);
        while (stop != loader) {
            if (loader == what)
                return true;
            loader = loader.getParent();
        }
        return false;
    }

    /********************************************************************/
    /* Class Names                                                      */
    /********************************************************************/

    /**
     * <p>Convert a string to a class.</p>
     *
     * @param name The class name.
     * @param cl   The class loader.
     * @return The class, or null.
     */
    public static Class stringToClass(String name, ClassLoader cl) {
        int k = name.length();
        if (name.startsWith(JAVA_ARRAY, k - JAVA_ARRAY.length())) {
            k -= JAVA_ARRAY.length();
            int count = 1;
            while (name.startsWith(JAVA_ARRAY, k - JAVA_ARRAY.length())) {
                k -= JAVA_ARRAY.length();
                count++;
            }
            Class clazz = stringToClassNonArray(name.substring(0, k), cl);
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
            int count = 1;
            clazz = clazz.getComponentType();
            while (clazz.isArray()) {
                count++;
                clazz = clazz.getComponentType();
            }
            StringBuilder buf = new StringBuilder();
            buf.append(clazz.getName());
            while (count > 0) {
                buf.append(JAVA_ARRAY);
                count--;
            }
            return buf.toString();
        } else {
            return clazz.getName();
        }
    }

}
