package jekpro.frequent.system;

import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.format.AbstractDom;

/**
 * <p>Helper for dom options.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public class DomOpts {
    /* mask values */
    private final static String OP_TREE = "tree";
    private final static String OP_TABLE = "table";
    private final static String OP_DOCUMENT = "document";
    private final static String OP_FRAGMENT = "fragment";

    /* control values */
    private final static String OP_NONE = "none";
    private final static String OP_EMPTY = "empty";
    private final static String OP_ANY = "any";

    /* dom options */
    public final static String OP_ROOT = "root";
    public final static String OP_TYPE = "type";
    public final static String OP_CONTEXT = "context";

    /* error terms */
    private final static String OP_DOM_OPTION = "dom_option";

    private int mask;
    private MapHash<String, Integer> control;
    private String context;

    /**
     * <p>Retrieve the mask.</p>
     *
     * @return The mask.
     */
    public int getMask() {
        return mask;
    }

    /**
     * <p>Set the mask.</p>
     *
     * @param m The mask.
     */
    public void setMask(int m) {
        mask = m;
    }

    /**
     * <p>Retrieve the control.</p>
     *
     * @return The control.
     */
    public MapHash<String, Integer> getControl() {
        return control;
    }

    /**
     * <p>Set the control.</p>
     *
     * @param c The control.
     */
    public void setControl(MapHash<String, Integer> c) {
        control = c;
    }

    /**
     * <p>Retrieve the context.</p>
     *
     * @return The context.
     */
    public String getContext() {
        return context;
    }

    /**
     * <p>Set the context.</p>
     *
     * @param c The context.
     */
    public void setContext(String c) {
        context = c;
    }

    /**
     * <p>Decode the dom options.</p>
     *
     * @param opt The dom options term.
     * @return The dom options.
     * @throws InterpreterMessage Validation error.
     */
    public static DomOpts decodeDomOpts(Object opt)
            throws InterpreterMessage {
        DomOpts res = new DomOpts();
        res.setMask(AbstractDom.MASK_LIST);
        res.setContext("");
        while (opt instanceof TermCompound &&
                ((TermCompound) opt).getArity() == 2 &&
                ((TermCompound) opt).getFunctor().equals(Knowledgebase.OP_CONS)) {
            Object temp = ((TermCompound) opt).getArg(0);
            if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_ROOT)) {
                Object help = ((TermCompound) temp).getArg(0);
                int mask = atomToMask(help);
                res.setMask(mask);
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 2 &&
                    ((TermCompound) temp).getFunctor().equals(OP_TYPE)) {
                Object help = ((TermCompound) temp).getArg(0);
                InterpreterMessage.checkInstantiated(help);
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
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals(OP_CONTEXT)) {
                Object help = ((TermCompound) temp).getArg(0);
                InterpreterMessage.checkInstantiated(help);
                String context = InterpreterMessage.castString(help);
                res.setContext(context);
            } else {
                InterpreterMessage.checkInstantiated(temp);
                throw new InterpreterMessage(
                        InterpreterMessage.domainError(OP_DOM_OPTION, temp));
            }
            opt = ((TermCompound) opt).getArg(1);
        }
        if (opt.equals(Knowledgebase.OP_NIL)) {
            /* */
        } else {
            InterpreterMessage.checkInstantiated(opt);
            throw new InterpreterMessage(InterpreterMessage.typeError(
                    InterpreterMessage.OP_TYPE_LIST, opt));
        }
        return res;
    }

    /**
     * <p>Convert an atom to a mask. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param t The mask term.
     * @return The mask value.
     * @throws InterpreterMessage Validation error.
     */
    public static int atomToMask(Object t) throws InterpreterMessage {
        InterpreterMessage.checkInstantiated(t);
        String val = InterpreterMessage.castString(t);
        if (val.equals(OP_TREE)) {
            return 0;
        } else if (val.equals(OP_TABLE)) {
            return AbstractDom.MASK_LIST;
        } else if (val.equals(OP_DOCUMENT)) {
            return AbstractDom.MASK_TEXT;
        } else if (val.equals(OP_FRAGMENT)) {
            return AbstractDom.MASK_LIST + AbstractDom.MASK_TEXT;
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    InterpreterMessage.OP_DOMAIN_FLAG_VALUE, t));
        }
    }

    /**
     * <p>Convert an atom to a type. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param t The type term.
     * @return The type value.
     * @throws InterpreterMessage Validation error.
     */
    public static int atomToType(Object t) throws InterpreterMessage {
        InterpreterMessage.checkInstantiated(t);
        String val = InterpreterMessage.castString(t);
        if (val.equals(OP_NONE)) {
            return AbstractDom.TYPE_NONE;
        } else if (val.equals(OP_EMPTY)) {
            return AbstractDom.TYPE_EMPTY;
        } else if (val.equals(OP_ANY)) {
            return AbstractDom.TYPE_ANY;
        } else {
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    InterpreterMessage.OP_DOMAIN_FLAG_VALUE, t));
        }
    }

}