package jekpro.reference.structure;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.tools.call.CallIn;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.proxy.RuntimeWrap;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.TermAtomic;
import jekpro.tools.term.TermCompound;
import jekpro.tools.term.TermVar;
import matula.util.wire.AbstractLivestock;

/**
 * <p>Comparator that is a Prolog callback.</p>
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
public final class LexicalCallback extends AbstractLexical {
    private Object comparator;

    /**
     * <p>Set the comparator.</p>
     *
     * @param c The comparator.
     */
    public void setComparator(Object c) {
        comparator = c;
    }

    /**
     * <p>Retrieve the comparator.</p>
     *
     * @return The comparator.
     */
    public Object getComparator() {
        return comparator;
    }

    /**
     * <p>Compare two terms lexically.</p>
     * <p>As a side effect will dynamically allocate display serial numbers.</p>
     *
     * @param alfa The skeleton of the first term.
     * @param d1   The display of the first term.
     * @param beta The skeleton of the second term.
     * @param d2   The display of the second term.
     * @return <0 alfa < beta, 0 alfa = beta, >0 alfa > beta
     * @throws ArithmeticException Incomparable reference.
     */
    public int compareTerm(Object alfa, Display d1,
                           Object beta, Display d2)
            throws ArithmeticException {
        Object first = AbstractTerm.createMolec(alfa, d1);
        Object second = AbstractTerm.createMolec(beta, d2);
        try {
            return compareMolec(first, second, engine);
        } catch (InterpreterException x) {
            throw new RuntimeWrap(x);
        } catch (InterpreterMessage x) {
            throw new RuntimeWrap(x);
        }
    }

    /**
     * <p>Compare two terms lexically.</p>
     * <p>As a side effect will dynamically allocate display serial numbers.</p>
     *
     * @param first  The first molec.
     * @param second The second molec.
     * @param en     The engine.
     * @throws ArithmeticException  Incomparable reference.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    private int compareMolec(Object first,
                             Object second, Engine en)
            throws ArithmeticException, InterpreterMessage, InterpreterException {
        try {
            Interpreter inter = (Interpreter) en.proxy;
            TermAtomic call = new TermAtomic(en.store.foyer.ATOM_CALL);
            Object help = new TermVar();
            TermCompound goal = new TermCompound(inter, call, comparator, help, first, second);
            CallIn callin = inter.iterator(goal);
            callin.next();
            help = AbstractTerm.copyMolec(inter, help);
            callin.close();
            return atomComp(AbstractTerm.getSkel(help), AbstractTerm.getDisplay(help));
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /**
     * <p>Compute the comparison integer fromm an atom.</p>
     *
     * @param m The atom skeleton.
     * @param d The atom display.
     * @return The integer.
     * @throws EngineMessage Shit happens.
     */
    private static int atomComp(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(Foyer.OP_LESS)) {
            return -1;
        } else if (fun.equals(Foyer.OP_EQUAL)) {
            return 0;
        } else if (fun.equals(Foyer.OP_GREATER)) {
            return 1;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    "atom_comp", m), d);
        }
    }

    /**
     * <p>Compare two skeletons lexically.</p>
     *
     * @param alfa The skeleton of the first term.
     * @param beta The skeleton of the second term.
     * @return <0 alfa < beta, 0 alfa = beta, >0 alfa > beta
     * @throws ArithmeticException Incomparable reference.
     */
    public int compareTermSkel(Object alfa, Object beta)
            throws ArithmeticException {
        Thread thread = Thread.currentThread();
        Supervisor visor = (Supervisor) AbstractLivestock.currentLivestock(thread);
        if (visor == null)
            throw new ArithmeticException(EngineMessage.OP_EVALUATION_ORDERED);
        Engine en = visor.inuse;
        if (en == null)
            throw new ArithmeticException(EngineMessage.OP_EVALUATION_ORDERED);
        int size = Math.max(SupervisorCopy.displaySize(alfa),
                SupervisorCopy.displaySize(beta));
        Display ref = Display.valueOf(size);
        Object first = AbstractTerm.createMolec(alfa, ref);
        Object second = AbstractTerm.createMolec(beta, ref);
        try {
            return compareMolec(first, second, en);
        } catch (InterpreterException x) {
            throw new RuntimeWrap(x);
        } catch (InterpreterMessage x) {
            throw new RuntimeWrap(x);
        }
    }

}