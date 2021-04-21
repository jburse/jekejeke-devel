package jekpro.tools.call;

/**
 * <p>This class represents a call-out. Call-out objects cannot be directly
 * constructed by the application programmer. A call-out object is created
 * by the interpreter and passed to a foreign predicate if it has such a
 * parameter.
 * </p>
 * <p>Foreign predicates can be programmatically registered via the method
 * defineForeign() of the Interpreter class in the programming interface or
 * by invoking the built-in foreign/3. The accepted formal parameters and
 * their interpretation are listed in the language reference documentation.
 * A formal parameter of type Interpreter is needed for foreign predicate
 * that change variable bindings. A formal parameter of type CallOut is
 * needed for non-deterministic foreign predicates.
 * </p>
 * <p>The API of the CallOut allows fine control of choice points. Via the
 * method getAnon() the foreign predicate can query whether it was called
 * for the first time. Via the method setRetry() the foreign predicate can
 * indicate that it desires a choice point. The methods setData() and getData()
 * can be used to access client data in the choice point.
 * </p>
 * <p>In case a choice point was desired the foreign predicate can additionally
 * indicate via the method setCutter() whether it desires to be notified when
 * the choice point is cleaned up. Choice points are cleaned up by the interpreter
 * when a cut occurs in the continuation, when an exception occurs in the
 * continuation or when the search is externally terminated. Via the method
 * getCleanup() the foreign predicate can query whether it is called for
 * a clean-up.
 * </p>
 * <p>During a clean-up the foreign predicate can query the current exception
 * via the method getException(). If needed it can then aggregate_alld further
 * exceptions and store the new current exception via the
 * method setException().
 * </p>
 * <p>By default the interpreter does barrier handling for non-deterministic foreign
 * predicates. This includes invoking markFrame() prior to invoking a foreign
 * predicate and then when the predicate succeeds swapBarrier (). This also
 * includes invoking swapBarrier() and releaseFrame() upon redo of the foreign
 * predicate. Finally this also includes invoking swapBarrier() during a clean-up
 * for foreign predicates with a cutter. The method setSpecial() allows switching
 * off this default barrier handling.</p>
 * <p>
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
public final class CallOut {
    public final static int MASK_CALL_FRST = 0x00000001;
    public final static int MASK_CALL_CLEN = 0x00000002;

    public final static int MASK_CALL_RTRY = 0x00000010;
    public final static int MASK_CALL_CTTR = 0x00000020;
    public final static int MASK_CALL_SPCI = 0x00000040;

    private Object data;
    public int flags;
    private InterpreterException exception;

    /**
     * <p>Indicates whether the foreign predicate was invoked for the first time.</p>
     *
     * @return The first flag.
     */
    public boolean getFirst() {
        return ((flags & MASK_CALL_FRST) != 0);
    }

    /**
     * <p>Indicates whether the foreign predicate was invoked for cleanup.</p>
     *
     * @return The cleanup flag.
     */
    public boolean getCleanup() {
        return ((flags & MASK_CALL_CLEN) != 0);
    }

    /**
     * <p>Indicates whether the foreign predicate desires a choice point.</p>
     *
     * @param r The retry flag.
     */
    public void setRetry(boolean r) {
        if (r) {
            flags |= MASK_CALL_RTRY;
        } else {
            flags &= ~MASK_CALL_RTRY;
        }
    }

    /**
     * <p>Indicates whether the foreign predicate desires a cleanup.</p>
     *
     * @param c The cutter flag.
     */
    public void setCutter(boolean c) {
        if (c) {
            flags |= MASK_CALL_CTTR;
        } else {
            flags &= ~MASK_CALL_CTTR;
        }
    }

    /**
     * <p>Indicates whether the foreign predicate desires custom barriers.</p>
     *
     * @param s The special flag.
     */
    public void setSpecial(boolean s) {
        if (s) {
            flags |= MASK_CALL_SPCI;
        } else {
            flags &= ~MASK_CALL_SPCI;
        }
    }

    /**
     * <p>Set the client data.</p>
     *
     * @param o The client data.
     */
    public void setData(Object o) {
        data = o;
    }

    /**
     * <p>Retrieve the client data.</p>
     *
     * @return The client data.
     */
    public Object getData() {
        return data;
    }

    /**
     * <p>Set the aggregate_alld exception.</p>
     *
     * @param e The aggregate_alld exception.
     */
    public void setException(InterpreterException e) {
        exception = e;
    }

    /**
     * <p>Retrieve the aggregate_alld exception.</p>
     *
     * @return The aggregate_alld exception.
     */
    public InterpreterException getException() {
        return exception;
    }

}

