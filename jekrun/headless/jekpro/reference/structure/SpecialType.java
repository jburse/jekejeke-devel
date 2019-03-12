package jekpro.reference.structure;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * <p>Provides built-in predicates for type checking.</p>
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
public final class SpecialType extends AbstractSpecial {
    private final static int SPECIAL_INTEGER = 0;
    private final static int SPECIAL_FLOAT = 1;
    private final static int SPECIAL_ATOM = 2;
    private final static int SPECIAL_COMPOUND = 3;
    private final static int SPECIAL_REFERENCE = 4;
    private final static int SPECIAL_DECIMAL = 5;
    private final static int SPECIAL_NUMBER = 6;
    private final static int SPECIAL_CALLABLE = 7;
    private final static int SPECIAL_ATOMIC = 8;
    private final static int SPECIAL_FLOAT32 = 9;
    private final static int SPECIAL_FLOAT64 = 10;

    /**
     * <p>Create a tyoe special.</p>
     *
     * @param i The id.
     */
    public SpecialType(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     */
    public final boolean moniFirst(Engine en) {
        switch (id) {
            case SPECIAL_INTEGER:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof Integer) && !(en.skel instanceof BigInteger))
                    return false;
                return true;
            case SPECIAL_FLOAT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof Float) && !(en.skel instanceof Double))
                    return false;
                return true;
            case SPECIAL_ATOM:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof SkelAtom))
                    return false;
                return true;
            case SPECIAL_COMPOUND:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof SkelCompound))
                    return false;
                return true;
            case SPECIAL_REFERENCE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (en.skel instanceof AbstractSkel || en.skel instanceof Number)
                    return false;
                return true;
            case SPECIAL_DECIMAL:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof Long) && !(en.skel instanceof BigDecimal))
                    return false;
                return true;
            case SPECIAL_NUMBER:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof Number))
                    return false;
                return true;
            case SPECIAL_CALLABLE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof SkelAtom) && !(en.skel instanceof SkelCompound))
                    return false;
                return true;
            case SPECIAL_ATOMIC:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (en.skel instanceof SkelVar || en.skel instanceof SkelCompound)
                    return false;
                return true;
            case SPECIAL_FLOAT32:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof Float))
                    return false;
                return true;
            case SPECIAL_FLOAT64:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!(en.skel instanceof Double))
                    return false;
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }
}
