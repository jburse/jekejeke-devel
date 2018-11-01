package jekdev.reference.debug;

import jekpro.model.builtin.SpecialBody;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.BindCount;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.SpecialPred;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.proxy.FactoryAPI;
import jekpro.tools.term.SkelCompound;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>Provides a special predicate for index dump.</p>
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
public final class SpecialDump extends AbstractSpecial {
    private final static int SPECIAL_SYS_DUMP = 0;

    /**
     * <p>Create a index dump special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialDump(int i) {
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
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_SYS_DUMP:
                Object[] temp = ((SkelCompound) en.skel).args;
                BindCount[] ref = en.display;
                Predicate pick = SpecialPred.indicatorToProvable(temp[0], ref, en);
                Predicate.checkExistentProvable(pick, temp[0], ref);
                AbstractDelegate fun = pick.del;
                AbstractDefined.checkDefinedRead(fun, pick, en);
                Object obj = en.visor.curoutput;
                FactoryAPI.checkTextWrite(obj);
                Writer wr = (Writer) obj;
                dumpHeader(pick, wr);
                ((AbstractDefined) fun).inspectClauses(wr, en);
                SpecialLoad.newLineFlush(wr);
                return en.getNextRaw();
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Show the dump header.</p>
     *
     * @param defined The defined predicate.
     * @param wr      The writer.
     */
    private static void dumpHeader(Predicate defined, Writer wr)
            throws EngineMessage {
        try {
            wr.write("-------- ");
            wr.write(defined.getFun());
            wr.write("/");
            wr.write(Integer.toString(defined.getArity()));
            wr.write(" ---------\n");
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

}
