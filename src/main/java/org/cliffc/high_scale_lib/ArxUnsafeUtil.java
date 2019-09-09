package org.cliffc.high_scale_lib;

import sun.misc.Unsafe;

import java.lang.reflect.Field;

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/12
 * Time: 4:23 PM
 * Created by nonvirtualthunk
 */
public class ArxUnsafeUtil {
    public static final Unsafe unsafe = getUnsafe();
    public static Unsafe getUnsafe () { return getUnsafeIntern(); }


    private static Unsafe getUnsafeIntern() {
        if (ArxUnsafeUtil.class.getClassLoader() == null) {
            return Unsafe.getUnsafe();
        } else {
            try {
                Field fld = Unsafe.class.getDeclaredField("theUnsafe");
                fld.setAccessible(true);
                return (Unsafe)fld.get(ArxUnsafeUtil.class);
            } catch (Exception var1) {
                throw new RuntimeException("Could not obtain access to sun.misc.Unsafe", var1);
            }
        }
    }

}
