package jekpro.reference.bootload;

import jekpro.frequent.stream.ForeignStream;
import jekpro.model.pretty.Foyer;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;

/**
 * <p>This class represent the init options.</p>
 *
 * @author Copyright 2012-2014, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.3 (a fast and small prolog interpreter)
 */
final class InitOpts {
    /* options */
    private final static String OP_PROMPT = "prompt";

    /* error terms */
    private final static String OP_INIT_OPTION = "init_option";
    private final static String OP_LIST = "list";

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
                    OP_LIST, opt));
        }
        return res;
    }

}
