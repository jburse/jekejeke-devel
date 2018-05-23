package jekpro.model.pretty;

import jekpro.model.builtin.Flag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>This class provides read options.</p>
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
public final class ReadOpts {
    public final static String OP_VALUE_ERROR = "error";
    public final static String OP_VALUE_CODES = "codes";
    public final static String OP_VALUE_CHARS = "chars";
    public final static String OP_VALUE_ATOM = "atom";
    public final static String OP_VALUE_VARIABLE = "variable";

    public final static int UTIL_CHARS = 2;
    public final static int UTIL_VARIABLE = 3;
    public final static int UTIL_CODES = 1;
    public final static int UTIL_ERROR = 0;
    public final static int UTIL_ATOM = 4;

    public int flags;
    public byte utildouble = UTIL_CODES;
    public byte utilback = UTIL_ERROR;
    public byte utilsingle = UTIL_ATOM;
    public AbstractSource source;

    /***************************************************************/
    /* Read Options                                                */
    /***************************************************************/

    /**
     * <p>Decode the given read parameters.</p>
     *
     * @param t The parameters skeleton.
     * @param d The parameters display.
     * @throws EngineMessage Shit happens.
     */
    public void decodeReadParameter(Object t, Display d, Engine en)
            throws EngineMessage {
        source = en.store.user;
        utildouble = (byte) en.store.foyer.getUtilDouble();
        utilback = (byte) en.store.foyer.getUtilBack();
        utilsingle = (byte) en.store.foyer.getUtilSingle();

        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        while (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONS)) {
            SkelCompound sc = (SkelCompound) t;
            en.skel = sc.args[0];
            en.deref();
            if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(PrologReader.OP_VARIABLES)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(PrologReader.OP_VARIABLE_NAMES)) {
                /* do nothing */
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(PrologReader.OP_SINGLETONS)) {
                /* do nothing */
                flags |= PrologWriter.FLAG_SING;
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_FLAG_DOUBLE_QUOTES)) {
                utildouble = (byte) ReadOpts.atomToUtil(((SkelCompound) en.skel).args[0], en.display, en);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_FLAG_BACK_QUOTES)) {
                utilback = (byte) ReadOpts.atomToUtil(((SkelCompound) en.skel).args[0], en.display, en);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Flag.OP_FLAG_SINGLE_QUOTES)) {
                utilsingle = (byte) ReadOpts.atomToUtil(((SkelCompound) en.skel).args[0], en.display, en);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(PrologReader.OP_ANNOTATION)) {
                int anno = WriteOpts.termToAnno(((SkelCompound) en.skel).args[0], en.display, en);
                if ((anno & WriteOpts.ANNO_MKDT) != 0) {
                    flags |= PrologWriter.FLAG_MKDT;
                } else {
                    flags &= ~PrologWriter.FLAG_MKDT;
                }
                if ((anno & WriteOpts.ANNO_FILL) != 0) {
                    flags |= PrologWriter.FLAG_FILL;
                } else {
                    flags &= ~PrologWriter.FLAG_FILL;
                }
                if ((anno & WriteOpts.ANNO_HINT) != 0) {
                    flags |= PrologWriter.FLAG_HINT;
                } else {
                    flags &= ~PrologWriter.FLAG_HINT;
                }
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(PrologReader.OP_SOURCE)) {
                en.skel = ((SkelCompound) en.skel).args[0];
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                String fun = EngineMessage.castString(en.skel, en.display);
                source = (!"".equals(fun) ? AbstractSource.keyToSource(fun, en.store) : null);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(PrologReader.OP_LINE_NO)) {
                /* do nothing */
            } else {
                EngineMessage.checkInstantiated(en.skel);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_READ_OPTION,
                        en.skel), en.display);
            }
            en.skel = sc.args[1];
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
        }
        if (t instanceof SkelAtom &&
                ((SkelAtom) t).fun.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_LIST, t), d);
        }
    }


    /**
     * <p>Set the read options.</p>
     *
     * @param pr The Prolog reader.
     */
    public void setReadOpts(PrologReader pr) {
        pr.setFlags(flags);
        pr.setUtilDouble(utildouble);
        pr.setUtilBack(utilback);
        pr.setUtilSingle(utilsingle);
        pr.setSource(source);
    }

    /**
     * <p>Convert an atom to an util value.</p>
     * <p>The following values are accepted:</p>
     * <ul>
     * <li><b>atom:</b> UTIL_ATOM.</li>
     * <li><b>codes:</b> UTIL_CODES.</li>
     * <li><b>variable:</b> UTIL_VARIABLE.</li>
     * </ul>
     *
     * @param m  The util value skel.
     * @param d  The util value display.
     * @param en The engine.
     * @return The util value.
     * @throws EngineMessage Shit happens.
     */
    public static int atomToUtil(Object m, Display d, Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        EngineMessage.checkInstantiated(en.skel);
        String fun = EngineMessage.castString(en.skel, en.display);
        if (fun.equals(OP_VALUE_ERROR)) {
            return UTIL_ERROR;
        } else if (fun.equals(OP_VALUE_CODES)) {
            return UTIL_CODES;
        } else if (fun.equals(OP_VALUE_CHARS)) {
            return UTIL_CHARS;
        } else if (fun.equals(OP_VALUE_ATOM)) {
            return UTIL_ATOM;
        } else if (fun.equals(OP_VALUE_VARIABLE)) {
            return UTIL_VARIABLE;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, en.skel));
        }
    }

}
