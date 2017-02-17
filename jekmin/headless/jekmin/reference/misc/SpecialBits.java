package jekmin.reference.misc;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Special;
import jekpro.model.molec.*;
import jekpro.model.rope.Goal;
import jekpro.reference.arithmetic.SpecialCompare;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

/**
 * <p>Provides additional bitwise predicates.</p>
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
public final class SpecialBits extends Special {
    private final static int SPECIAL_SYS_TEST_BIT = 0;

    /**
     * <p>Create a additional bitwise operations special.</p>
     *
     * @param i The id.
     */
    public SpecialBits(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param r  The continuation skeleton.
     * @param u  The continuation display.
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean findFirst(Goal r, DisplayClause u,
                                   Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_SYS_TEST_BIT:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                Number alfa = EngineMessage.castInteger(en.skel, en.display);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                Number beta = EngineMessage.castInteger(en.skel, en.display);
                if (!sysTestBit(alfa, beta))
                    return false;
                return r.getNextRaw(u, en);
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }


    /********************************************************************/
    /* Additional Binary Number Built-in:                               */
    /*      sys_test_bit/3: sysTestBit()                                */
    /********************************************************************/

    /**
     * <p>Test a bit.</p>
     *
     * @param a The first operand.
     * @param b The second operand.
     * @return The result.
     * @throws EngineMessage Shit happens.
     */
    private static boolean sysTestBit(Number a, Number b) throws EngineMessage {
        EngineMessage.checkNotLessThanZero(a);
        int k = EngineMessage.castIntValue(a);
        if (b instanceof Integer) {
            if (k <= 31) {
                return (b.intValue() & (1 << k)) != 0;
            } else {
                return b.intValue() < 0;
            }
        } else {
            return ((BigInteger) b).testBit(k);
        }
    }

}