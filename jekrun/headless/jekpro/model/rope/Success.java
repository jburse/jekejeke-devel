package jekpro.model.rope;

import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.CallFrame;
import jekpro.model.molec.Display;

/**
 * <p>The class provides a success node.</p>
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
public final class Success extends Intermediate {
    public final static Success DEFAULT = new Success();

    /**
     * <p>Resolve the current term.</p>
     *
     * @param en The engine.
     * @return True if success, otherwise false.
     */
    public final boolean resolveNext(Engine en) {
        CallFrame u = en.contdisplay;
        if ((((u.flags & Directive.MASK_DIRE_MORE) != 0) ?
                u.number + 1 : u.number) >= en.number) {
            if ((u.flags & Directive.MASK_DIRE_LTGC) == 0) {
                Display d = u.disp;
                if (d.bind.length > 0)
                    d.remTab(en);
                u.flags |= Directive.MASK_DIRE_LTGC;
            }
        }

        if ((u.flags & AbstractDefined.MASK_DEFI_STOP) != 0) {
            en.contskel = null;
            en.contdisplay = null;
        } else {
            en.contskel = u.contskel;
            en.contdisplay = u.contdisplay;
        }
        return true;
    }

}