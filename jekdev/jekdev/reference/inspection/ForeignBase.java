package jekdev.reference.inspection;

import jekdev.model.pretty.LocatorTrace;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractLocator;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Operator;
import jekpro.reference.reflect.SpecialOper;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.*;
import matula.util.regex.ArrayEnumeration;

/**
 * <p>This class provides built-ins for the module base.</p>
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
public final class ForeignBase {

    /**
     * <p>Retrieve the first location.</p>
     *
     * @param inter  The interpreter.
     * @param co     The call-out.
     * @param source The source.
     * @param orig   The origin.
     * @param lineno The line number.
     * @return The predicate indicator.
     * @throws InterpreterMessage Shit happens
     */
    public static Object sysFirstLocation(Interpreter inter, CallOut co,
                                          TermAtomic source,
                                          String orig, int lineno)
            throws InterpreterMessage {
        try {
            Engine engine = inter.getEngine();
            ArrayEnumeration<Predicate> dc;
            if (co.getFirst()) {
                AbstractSource src = derefAndCastSource(source, engine);
                AbstractLocator locator = src.locator;
                if (locator == null)
                    return null;
                PositionKey pos = new PositionKey(orig, lineno);
                Predicate[] preds = ((LocatorTrace) locator).allFirstPredicates(pos);
                if (preds.length == 0)
                    return null;
                dc = new ArrayEnumeration<>(preds);
                co.setData(dc);
            } else {
                dc = (ArrayEnumeration<Predicate>) co.getData();
            }
            if (!dc.hasMoreElements())
                return null;
            Predicate pick = dc.nextElement();
            co.setRetry(dc.hasMoreElements());
            AbstractSource src = pick.getSource();
            Object t = SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                    src.getFullName(), src.getStore().user);
            return AbstractTerm.createMolec(t, Display.DISPLAY_CONST);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Retrieve the first location.</p>
     *
     * @param inter  The interpreter.
     * @param co     The call-out.
     * @param source The source.
     * @param orig   The origin.
     * @param lineno The line number.
     * @return The predicate indicator.
     * @throws InterpreterMessage Shit happens
     */
    public static Object sysLocation(Interpreter inter, CallOut co,
                                     TermAtomic source,
                                     String orig, int lineno)
            throws InterpreterMessage {
        try {
            Engine engine = inter.getEngine();
            ArrayEnumeration<Predicate> dc;
            if (co.getFirst()) {
                AbstractSource src = derefAndCastSource(source, engine);
                AbstractLocator locator = src.locator;
                if (locator == null)
                    return null;
                PositionKey pos = new PositionKey(orig, lineno);
                Predicate[] preds = ((LocatorTrace) locator).allPredicates(pos);
                if (preds.length == 0)
                    return null;
                dc = new ArrayEnumeration<>(preds);
                co.setData(dc);
            } else {
                dc = (ArrayEnumeration<Predicate>) co.getData();
            }
            if (!dc.hasMoreElements())
                return null;
            Predicate pick = dc.nextElement();
            co.setRetry(dc.hasMoreElements());
            AbstractSource src = pick.getSource();
            Object t = SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                    src.getFullName(), src.getStore().user);
            return AbstractTerm.createMolec(t, Display.DISPLAY_CONST);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Determine the provable hash.</p>
     *
     * @param inter    The interpreter.
     * @param provable The provable indicator.
     * @param source   The source.
     * @return The provable hash.
     * @throws InterpreterMessage   Shit happens
     * @throws InterpreterException Shit happens
     */
    public static Object sysProvableHash(Interpreter inter,
                                         Object provable, TermAtomic source)
            throws InterpreterMessage, InterpreterException {
        try {
            Engine engine = inter.getEngine();
            Predicate pick = SpecialPred.indicatorToPredicateDefined(AbstractTerm.getSkel(provable),
                    AbstractTerm.getDisplay(provable), engine, CachePredicate.MASK_CACH_UCHK);
            if (pick == null)
                return null;
            AbstractSource src = derefAndCastSource(source, engine);
            Object t = SpecialPred.provableToColonSkel(pick, src);
            return AbstractTerm.createMolec(t, Display.DISPLAY_CONST);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Determine the syntax hash.</p>
     *
     * @param inter  The interpreter.
     * @param syntax The syntax indicator.
     * @param source The source.
     * @return The provable hash.
     * @throws InterpreterMessage   Shit happens
     * @throws InterpreterException Shit happens
     */
    public static Object sysSyntaxHash(Interpreter inter,
                                       Object syntax, TermAtomic source)
            throws InterpreterMessage, InterpreterException {
        try {
            Engine engine = inter.getEngine();
            Operator oper = SpecialOper.operToOperatorDefined(AbstractTerm.getSkel(syntax),
                    AbstractTerm.getDisplay(syntax), engine, CachePredicate.MASK_CACH_UCHK);
            if (oper == null)
                return null;
            AbstractSource src = derefAndCastSource(source, engine);
            Object t = SpecialOper.syntaxToColonSkel(oper, src);
            t = new SkelCompound(SpecialOper.typeToOp(oper.getType()), t);
            return AbstractTerm.createMolec(t, Display.DISPLAY_CONST);
        } catch (EngineException x) {
            throw new InterpreterException(x);
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
     * @param source The source.
     * @param en     The engine.
     * @return The source.
     * @throws EngineMessage Shit happens.
     */
    private static AbstractSource derefAndCastSource(TermAtomic source, Engine en)
            throws EngineMessage {
        SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(source.getSkel(), source.getDisplay());
        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
        src = src.getStore().getSource(sa.fun);
        AbstractSource.checkExistentSource(src, sa);
        return src;
    }

}