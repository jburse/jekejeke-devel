package jekmin.frequent.decimal;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;

/**
 * <p>Provides some constants.</p>
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
public class SpecialHelper extends AbstractSpecial {
    private final static int EVALUABLE_LOG2 = 0;
    private final static int EVALUABLE_LOG10 = 1;

    public static final Double DOUBLE_LOG2 = Double.valueOf(Math.log(2));
    public static final Double DOUBLE_LOG10 = Double.valueOf(Math.log(10));

    /**
     * <p>Create a constant evaluable.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialHelper(int i) {
        super(i);
    }

    /**
     * <p>Arithmetically evaluate an evaluable.</p>
     * <p>The evaluable is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the contskel and contdisplay of the engine.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     *
     * @param en The engine.
     */
    public final void moniEvaluate(Engine en) {
        switch (id) {
            case EVALUABLE_LOG2:
                en.skel = DOUBLE_LOG2;
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_LOG10:
                en.skel = DOUBLE_LOG10;
                en.display = Display.DISPLAY_CONST;
                return;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

}
