package jekpro.reference.bootload;

import jekpro.frequent.stream.ForeignStream;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;

/**
 * <p>This class represents options for the module toolkit.</p>
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
final class InitOpts {
    private final static String OP_PROMPT = "prompt";

    private final static String OP_INIT_OPTION = "init_option";

    private boolean prompt;

    /**
     * <p>Retrieve the prompt flag.</p>
     *
     * @return The prompt flag.
     */
    boolean getPrompt() {
        return prompt;
    }

    /**
     * <p>Set the prompt flag.</p>
     *
     * @param p The prompt flag.
     */
    private void setPrompt(boolean p) {
        prompt = p;
    }

    /**
     * <p>Decode the init options.</p>
     *
     * @param opt The init options term.
     * @return The init options.
     * @throws InterpreterMessage Validation error.
     */
    static InitOpts decodeInitOptions(Object opt)
            throws InterpreterMessage {
        InitOpts res = new InitOpts();
        while (opt instanceof TermCompound &&
                ((TermCompound) opt).getArity() == 2 &&
                ((TermCompound) opt).getFunctor().equals(
                        Knowledgebase.OP_CONS)) {
            Object temp = ((TermCompound) opt).getArg(0);
            if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_PROMPT)) {
                Object help = ((TermCompound) temp).getArg(0);
                res.setPrompt(ForeignStream.atomToBool(help));
            } else {
                InterpreterMessage.checkInstantiated(temp);
                throw new InterpreterMessage(InterpreterMessage.domainError(
                        OP_INIT_OPTION, temp));
            }
            opt = ((TermCompound) opt).getArg(1);
        }
        if (opt.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            InterpreterMessage.checkInstantiated(opt);
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    EngineMessage.OP_TYPE_LIST, opt));
        }
        return res;
    }

}
