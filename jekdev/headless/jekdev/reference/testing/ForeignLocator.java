package jekdev.reference.testing;

import jekdev.model.pretty.LocatorTrace;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractLocator;
import jekpro.model.pretty.AbstractSource;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.call.ArrayEnumeration;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.TermAtomic;

/**
 * <p>This class provides locator built-ins.</p>
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
 * Only to be distributed with programs that add sgnificant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignLocator {

    /**
     * <p>Retrieve the first location.</p>
     *
     * @param inter  The interpreter.
     * @param co     The call-out.
     * @param atomic The atomic.
     * @param orig   The origin.
     * @param lineno The line number.
     * @return The predicate indicator.
     * @throws InterpreterMessage Shit happens
     */
    public static Object sysFirstLocation(Interpreter inter, CallOut co,
                                    TermAtomic atomic,
                                    String orig, int lineno)
            throws InterpreterMessage {
        try {
            Engine engine = (Engine) inter.getEngine();
            ArrayEnumeration<Predicate> dc;
            if (co.getFirst()) {
                AbstractSource scope = derefAndCastSource(atomic, engine);
                AbstractLocator locator = scope.locator;
                if (locator == null)
                    return null;
                PositionKey pos = new PositionKey(orig, lineno);
                Predicate[] preds = ((LocatorTrace) locator).allFirstPredicates(pos);
                if (preds.length == 0)
                    return null;
                dc = new ArrayEnumeration<Predicate>(preds);
                co.setData(dc);
            } else {
                dc = (ArrayEnumeration<Predicate>) co.getData();
            }
            if (!dc.hasMoreElements())
                return null;
            Predicate pick = dc.nextElement();
            co.setRetry(dc.hasMoreElements());
            Object skel = SpecialQuali.indicatorToColonSkel(pick.getFun(),
                    pick.getSource().getStore().user,
                    pick.getArity(), engine);
            return skel;
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Retrieve the first location.</p>
     *
     * @param inter  The interpreter.
     * @param co     The call-out.
     * @param atomic The atomic.
     * @param orig   The origin.
     * @param lineno The line number.
     * @return The predicate indicator.
     * @throws InterpreterMessage Shit happens
     */
    public static Object sysLocation(Interpreter inter, CallOut co,
                               TermAtomic atomic,
                               String orig, int lineno)
            throws InterpreterMessage {
        try {
            Engine engine = (Engine) inter.getEngine();
            ArrayEnumeration<Predicate> dc;
            if (co.getFirst()) {
                AbstractSource scope = derefAndCastSource(atomic, engine);
                AbstractLocator locator = scope.locator;
                if (locator == null)
                    return null;
                PositionKey pos = new PositionKey(orig, lineno);
                Predicate[] preds = ((LocatorTrace) locator).allPredicates(pos);
                if (preds.length == 0)
                    return null;
                dc = new ArrayEnumeration<Predicate>(preds);
                co.setData(dc);
            } else {
                dc = (ArrayEnumeration<Predicate>) co.getData();
            }
            if (!dc.hasMoreElements())
                return null;
            Predicate pick = dc.nextElement();
            co.setRetry(dc.hasMoreElements());
            Object skel = SpecialQuali.indicatorToColonSkel(pick.getFun(),
                    pick.getSource().getStore().user,
                    pick.getArity(), engine);
            return skel;
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /****************************************************************/
    /* Deref Utility                                                */
    /****************************************************************/

    /**
     * <p>Deref and cast a source.</p>
     *
     * @param atomic The atomic.
     * @param en     The engine.
     * @return The source.
     */
    public static AbstractSource derefAndCastSource(TermAtomic atomic, Engine en)
            throws EngineMessage {
        SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(atomic.getSkel(),
                atomic.getDisplay());
        AbstractSource source = (sa.scope != null ? sa.scope : en.store.user);
        source = source.getStore().getSource(sa.fun);
        AbstractSource.checkExistentSource(source, sa);
        return source;
    }

}