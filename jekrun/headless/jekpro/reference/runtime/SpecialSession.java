package jekpro.reference.runtime;

import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.PrologWriter;
import jekpro.reference.structure.SpecialUniv;
import jekpro.reference.structure.SpecialVars;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

/**
 * <p>Provides built-in predicates for sessions.</p>
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
public final class SpecialSession extends AbstractSpecial {
    private final static int SPECIAL_SYS_QUOTED_VAR = 0;

    public final static int MASK_MODE_PRMT = 0x0000F000;

    public final static int MASK_PRMT_PROF = 0x00000000;
    public final static int MASK_PRMT_PCUT = 0x00001000;
    public final static int MASK_PRMT_PDBG = 0x00002000;
    public final static int MASK_PRMT_PRON = 0x00003000;

    public final static String OP_ANSWER_CUT = "answer_cut";
    public final static String OP_ASK_DEBUG = "ask_debug";

    /**
     * <p>Create a session special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialSession(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a term in a list of goals for the first time.</p>
     * <p>The term is passed via the skel and display of the engine.</p>
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
            case SPECIAL_SYS_QUOTED_VAR:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                String fun = SpecialUniv.derefAndCastString(temp[0], ref);
                if (!en.unifyTerm(temp[1], ref, sysQuoteVar(fun, en), Display.DISPLAY_CONST))
                    return false;
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /********************************************************************/
    /* Raw Variable Names                                               */
    /********************************************************************/

    /**
     * <p>Show a variable</p>
     *
     * @param fun The variable.
     * @param en  The engine.
     * @return The quoted variable.
     */
    private static SkelAtom sysQuoteVar(String fun, Engine en) {
        PrologWriter pw = new PrologWriter();
        pw.setSource(en.visor.peekStack());
        pw.setFlags(pw.getFlags() | PrologWriter.FLAG_QUOT);
        return new SkelAtom(pw.variableQuoted(fun));
    }

    /**
     * <p>Convert variable names.</p>
     * <p>Will not convert variables that have not yet been allocated.</p>
     * <p>Will not convert variables that have already been deallocated.</p>
     *
     * @param vars The var hash.
     * @param d    The term display.
     * @param en   The engine.
     * @return The Prolog association list.
     */
    public static Object hashToAssoc(MapHashLink<String, SkelVar> vars,
                                     Display d, Engine en) {
        Object end = en.store.foyer.ATOM_NIL;
        if (vars == null)
            return end;
        for (MapEntry<String, SkelVar> entry = vars.getLastEntry();
             entry != null; entry = vars.predecessor(entry)) {
            SkelVar sv = entry.value;
            if (sv.id >= d.bind.length || d.bind[sv.id] == null)
                continue;
            Object val = new SkelCompound(en.store.foyer.ATOM_EQUAL,
                    new SkelAtom(entry.key), sv);
            end = new SkelCompound(en.store.foyer.ATOM_CONS, val, end);
        }
        return end;
    }

    /*******************************************************************/
    /* Prompt Conversion                                               */
    /*******************************************************************/

    /**
     * <p>Convert a prompt mode to an atom.</p>
     *
     * @param m The prompt mode.
     * @return The atom.
     */
    public static Object promptToAtom(int m) {
        switch (m) {
            case SpecialSession.MASK_PRMT_PROF:
                return new SkelAtom(AbstractFlag.OP_OFF);
            case SpecialSession.MASK_PRMT_PCUT:
                return new SkelAtom(OP_ANSWER_CUT);
            case SpecialSession.MASK_PRMT_PDBG:
                return new SkelAtom(OP_ASK_DEBUG);
            case SpecialSession.MASK_PRMT_PRON:
                return new SkelAtom(AbstractFlag.OP_ON);
            default:
                throw new IllegalArgumentException("illegal mode");
        }
    }

    /**
     * <p>Convert an atom to a prompt mode.</p>
     *
     * @param t The atom skeleton.
     * @param d The atom display.
     * @return The prompt mode.
     */
    public static int atomToPrompt(Object t, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(t, d);
        if (fun.equals(AbstractFlag.OP_OFF)) {
            return SpecialSession.MASK_PRMT_PROF;
        } else if (fun.equals(OP_ANSWER_CUT)) {
            return SpecialSession.MASK_PRMT_PCUT;
        } else if (fun.equals(OP_ASK_DEBUG)) {
            return SpecialSession.MASK_PRMT_PDBG;
        } else if (fun.equals(AbstractFlag.OP_ON)) {
            return SpecialSession.MASK_PRMT_PRON;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    "prompt_mode", t), d);
        }
    }

}
