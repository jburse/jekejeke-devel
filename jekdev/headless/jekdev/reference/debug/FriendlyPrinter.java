package jekdev.reference.debug;

import jekdev.model.builtin.ClauseTrace;
import jekdev.model.builtin.DirectiveTrace;
import jekdev.model.builtin.GoalTrace;
import jekpro.model.pretty.PrologWriter;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Intermediate;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>This class provides an intermediate code printer.</p>
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
final class FriendlyPrinter {
    int flags;
    int count;
    PrologWriter pw;
    int level;

    /**
     * <p>Get the first literal of a directive.</p>
     *
     * @param dire The directive.
     * @return The first literal.
     */
    Intermediate nextDirective(Directive dire) {
        if ((flags & SpecialFriendly.MASK_FRIEND_DEBUG) != 0 &&
                dire instanceof DirectiveTrace)
            return ((DirectiveTrace) dire).nexttrace;
        if ((flags & SpecialFriendly.MASK_FRIEND_DEBUG) != 0 &&
                dire instanceof ClauseTrace)
            return ((ClauseTrace) dire).nexttrace;
        return dire.next;
    }

    /**
     * <p>Get the last literal of a directive.</p>
     *
     * @param dire The directive.
     * @return The first literal.
     */
    Intermediate lastDirective(Directive dire) {
        if ((flags & SpecialFriendly.MASK_FRIEND_DEBUG) != 0 &&
                dire instanceof DirectiveTrace)
            return ((DirectiveTrace) dire).lasttrace;
        if ((flags & SpecialFriendly.MASK_FRIEND_DEBUG) != 0 &&
                dire instanceof ClauseTrace)
            return ((ClauseTrace) dire).lasttrace;
        return dire.last;
    }

    /**
     * <p>Get the next literal of a goal.</p>
     *
     * @param goal The goal.
     * @return The next literal.
     */
    Intermediate nextGoal(Intermediate goal) {
        if ((flags & SpecialFriendly.MASK_FRIEND_DEBUG) != 0 &&
                goal instanceof GoalTrace)
            return ((GoalTrace) goal).nexttrace;
        return goal.next;
    }

    /**
     * <p>Write a line number.</p>
     *
     * @throws IOException IO Error.
     */
    void friendlyCount()
            throws IOException {
        Writer wr = pw.getWriter();
        String str = Integer.toString(count);
        for (int i = str.length(); i < 3; i++)
            wr.write(" ");
        wr.write(str);
        wr.write(" ");
        for (int i = 0; i < level; i++)
            wr.write("   ");
        count++;
    }

}