package jekpro.frequent.stream;

import jekpro.frequent.system.DomOpts;
import jekpro.model.pretty.Foyer;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.format.AbstractDom;

/**
 * <p>Helper for sheet options.</p>
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
class SheetOpts extends DomOpts {
    /* sheet options */
    public final static String OP_VARIABLE = "variable";

    /* error terms */
    private final static String OP_SHEET_OPTION = "sheet_option";

    MapHash<String, Object> variables;

    /**
     * <p>Retrieve the variables.</p>
     *
     * @return The variables.
     */
    MapHash<String, Object> getVariables() {
        return variables;
    }

    /**
     * <p>Set the variables.</p>
     *
     * @param vs The variables.
     */
    public void setVariables(MapHash<String, Object> vs) {
        variables = vs;
    }

    /**
     * <p>Decode the sheet options.</p>
     *
     * @param opt The sheet options term.
     * @return The dom options.
     * @throws InterpreterMessage Validation error.
     */
    public static SheetOpts decodeSheetOpts(Object opt)
            throws InterpreterMessage {
        try {
            SheetOpts res = new SheetOpts();
            res.setMask(AbstractDom.MASK_TEXT);
            while (opt instanceof TermCompound &&
                    ((TermCompound) opt).getArity() == 2 &&
                    ((TermCompound) opt).getFunctor().equals(Knowledgebase.OP_CONS)) {
                Object temp = ((TermCompound) opt).getArg(0);
                if (temp instanceof TermCompound &&
                        ((TermCompound) temp).getArity() == 1 &&
                        ((TermCompound) temp).getFunctor().equals(DomOpts.OP_ROOT)) {
                    Object help = ((TermCompound) temp).getArg(0);
                    int flags = DomOpts.atomToRoot(help);
                    res.setMask((res.getMask() & ~DomOpts.MASK_ROOT) | flags);
                } else if (temp instanceof TermCompound &&
                        ((TermCompound) temp).getArity() == 2 &&
                        ((TermCompound) temp).getFunctor().equals(DomOpts.OP_TYPE)) {
                    Object help = ((TermCompound) temp).getArg(0);
                    String key = InterpreterMessage.castString(help);
                    help = ((TermCompound) temp).getArg(1);
                    Integer type = Integer.valueOf(atomToType(help));
                    MapHash<String, Integer> control = res.getControl();
                    if (control == null) {
                        control = new MapHash<String, Integer>();
                        res.setControl(control);
                    }
                    MapEntry<String, Integer> entry = control.getEntry(key);
                    if (entry != null) {
                        entry.value = type;
                    } else {
                        control.add(key, type);
                    }
                } else if (temp instanceof TermCompound &&
                        ((TermCompound) temp).getArity() == 2 &&
                        ((TermCompound) temp).getFunctor().equals(OP_VARIABLE)) {
                    Object help = ((TermCompound) temp).getArg(0);
                    String key = InterpreterMessage.castString(help);
                    help = ((TermCompound) temp).getArg(1);
                    Object val;
                    if (!(help instanceof String)) {
                        Number num = InterpreterMessage.castNumber(help);
                        long x = SpecialEval.castLongValue(num);
                        val = Long.valueOf(x);
                    } else {
                        val = help;
                    }
                    MapHash<String, Object> variables = res.getVariables();
                    if (variables == null) {
                        variables = new MapHash<String, Object>();
                        res.setVariables(variables);
                    }
                    MapEntry<String, Object> entry = variables.getEntry(key);
                    if (entry != null) {
                        entry.value = val;
                    } else {
                        variables.add(key, val);
                    }
                } else {
                    InterpreterMessage.checkInstantiated(temp);
                    throw new InterpreterMessage(InterpreterMessage.domainError(
                            OP_SHEET_OPTION, temp));
                }
                opt = ((TermCompound) opt).getArg(1);
            }
            if (opt.equals(Foyer.OP_NIL)) {
                /* */
            } else {
                InterpreterMessage.checkInstantiated(opt);
                throw new InterpreterMessage(InterpreterMessage.typeError(
                        InterpreterMessage.OP_TYPE_LIST, opt));
            }
            return res;
        } catch (ClassCastException x) {
            throw new InterpreterMessage(
                    InterpreterMessage.representationError(x.getMessage()));
        }
    }

}