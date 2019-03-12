package jekpro.tools.term;

import jekpro.model.inter.Engine;
import jekpro.model.molec.BindLexical;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.tools.call.Interpreter;

/**
 * <p>This represents a variable term. New variables can be created via
 * the method createVars(). This will create multiple variables that
 * share the same display. These variables will have ascending
 * local indexes.
 * </p>
 * <p>The serial number can be retrieved via getValue(). If the
 * interpreter is null then the local index is returned. If the
 * interpreter is non-null then if necessary a new serial number is
 * created and this serial number is returned.
 * </p>
 * <p>A variable can be dereferenced by the method deref(). Dereferencing a
 * un-instantiated variable yields the variable itself. Dereferencing a
 * variable that is bound to a term yields this term or the dereferencing
 * of this term when the term was a variable again. The method variant
 * derefWrapped() returns a wrapped result.
 * </p>
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
public final class TermVar extends AbstractTerm {
    final SkelVar skel;
    final Display display;

    /**
     * <p>Constructor for internal use only.</p>
     *
     * @param s The var skel.
     * @param d The var display.
     */
    TermVar(SkelVar s, Display d) {
        skel = s;
        display = d;
    }

    /**
     * <p>Create a new variables.</p>
     */
    public TermVar() {
        skel = SkelVar.valueOf(0);
        display = new Display(BindLexical.newLexical(1));
        display.flags |= Display.MASK_DPTM_MLTI;
    }

    /************************************************************/
    /* Variation Points                                         */
    /************************************************************/

    /**
     * <p>Retrieve the skeleton.</p>
     *
     * @return The skeleton.
     */
    public Object getSkel() {
        return skel;
    }

    /**
     * <p>Retrieve the display.</p>
     *
     * @return The display.
     */
    public Display getDisplay() {
        return display;
    }

    /**
     * <p>Compute the hash.</p>
     *
     * @return The hash value.
     */
    public int hashCode() {
        return skel.hashCode(display);
    }

    /**
     * <p>Check the identity to another molec var.</p>
     *
     * @param o The other object.
     * @return True if the other object is a molec and identical, otherwise false.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof TermVar))
            return false;
        TermVar m = (TermVar) o;
        return (skel == m.skel && display == m.display);
    }

    /**
     * <p>Retrieve the serial number of a variable.</p>
     *
     * @param inter The call-in.
     * @return The serial number.
     */
    public int getValue(Interpreter inter) {
        Engine en = (inter != null ? (Engine) inter.getEngine() : null);
        return skel.getValue(display, en);
    }

    /**
     * <p>Fully dereference this variable.</p>
     *
     * @return The dereferenced term.
     */
    public Object deref() {
        Object t = skel;
        Display d = display;
        BindUniv b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        if (t != skel || d != display) {
            return AbstractTerm.createTerm(t, d);
        } else {
            return this;
        }
    }

    /**
     * <p>Fully dereference this variable.</p>
     *
     * @return The dereferenced term.
     */
    public AbstractTerm derefWrapped() {
        Object t = skel;
        Display d = display;
        BindUniv b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        if (t != skel || d != display) {
            return AbstractTerm.createTermWrapped(t, d);
        } else {
            return this;
        }
    }

}
