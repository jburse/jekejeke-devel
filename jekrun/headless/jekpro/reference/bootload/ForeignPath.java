package jekpro.reference.bootload;

import jekpro.model.rope.LoadOpts;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;
import matula.util.system.ForeignUri;

import java.io.IOException;
import java.net.MalformedURLException;
import java.nio.charset.CharacterCodingException;
import java.util.ArrayList;

/**
 * <p>The foreign predicates for the module path.</p>
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
public final class ForeignPath {
    /* prefix relationship flags */
    public static final int MASK_PRFX_LIBR = 0x00000001;
    public static final int MASK_PRFX_FRGN = 0x00000002;

    /* suffix relationship flags */
    public static final int MASK_SUFX_TEXT = 0x00000010;
    public static final int MASK_SUFX_BNRY = 0x00000020;
    public static final int MASK_SUFX_RSCS = 0x00000040;

    /* failure relationship flags */
    public static final int MASK_FAIL_READ = 0x00000100;
    public static final int MASK_FAIL_CHLD = 0x00000200;

    /* combined prefix, suffix and failure flags */
    public static final int MASK_MODL_LIBR = MASK_PRFX_LIBR | MASK_SUFX_TEXT;
    public static final int MASK_MODL_FRGN = MASK_PRFX_FRGN | MASK_SUFX_BNRY;
    public static final int MASK_MODL_AUTO = MASK_MODL_LIBR | MASK_MODL_FRGN | MASK_FAIL_CHLD;
    public static final int MASK_MODL_RSCS = MASK_PRFX_LIBR | MASK_SUFX_RSCS;
    public static final int MASK_MODL_VERB = MASK_MODL_LIBR | MASK_FAIL_CHLD;

    /**
     * <p>Find a write adr.</p>
     * <p>Resolve the given adr against the base url.</p>
     * <p>Always canonize the adr before returning the result.</p>
     *
     * @param inter The interpreter.
     * @param adr   The path.
     * @return The write path.
     * @throws InterpreterMessage       Validation error.
     * @throws CharacterCodingException File canonization problem.
     * @throws MalformedURLException    URL assembling problem.
     */
    public static String sysFindWrite(Interpreter inter, String adr)
            throws IOException, InterpreterMessage {
        /* make it absolute */
        if (ForeignUri.sysUriIsRelative(adr)) {
            String base = (String) inter.getProperty("base_url");
            if ("".equals(base))
                throw new InterpreterMessage(
                        InterpreterMessage.resourceError("baseurl_missing"));
            adr = ForeignUri.sysUriAbsolute(base, adr);
        }
        /* make it canonical */
        return ForeignUri.sysCanonicalUri(adr);
    }

    /**
     * <p>Find a prefix according to the auto loader.</p>
     *
     * @param inter The interpreter.
     * @param path  The path.
     * @param key   The call-site.
     * @param opt   The options list.
     * @return The prefixed name, or null.
     * @throws InterpreterMessage Shit happens.
     */
    public static String sysFindPrefix(Interpreter inter,
                                       String path, String key,
                                       Object opt)
            throws InterpreterMessage, IOException {
        int mask = decodeFindOptions(opt);
        return inter.findPrefix(path, key, mask);
    }

    /**
     * <p>Find a key according to the auto loader.</p>
     *
     * @param inter The interpreter.
     * @param path  The prefixed path.
     * @param key   The call-site.
     * @param opt   The options list.
     * @return The source key.
     * @throws InterpreterMessage Shit happens.
     */
    public static String sysFindKey(Interpreter inter,
                                    String path, String key,
                                    Object opt)
            throws InterpreterMessage {
        int mask = decodeFindOptions(opt);
        return inter.findKey(path, key, mask);
    }


    /**
     * <p>Revert a path back to a spec.</p>
     *
     * @param inter The interpreter.
     * @param path  The path.
     * @param key   The call-site.
     * @param opt   The options list.
     * @return The spec.
     */
    public static Object sysKeySpec(Interpreter inter,
                                    String path, String key,
                                    Object opt)
            throws InterpreterMessage {
        int mask = decodeFindOptions(opt);
        return inter.keyToSpec(path, key, mask);
    }

    /**
     * <p>Decode the find options.</p>
     *
     * @param opt The find options term.
     * @return The find options.
     */
    public static int decodeFindOptions(Object opt)
            throws InterpreterMessage {
        int mask = 0;
        while (opt instanceof TermCompound &&
                ((TermCompound) opt).getArity() == 2 &&
                ((TermCompound) opt).getFunctor().equals(
                        Knowledgebase.OP_CONS)) {
            Object temp = ((TermCompound) opt).getArg(0);
            if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals("package")) {
                Object help = ((TermCompound) temp).getArg(0);
                InterpreterMessage.checkInstantiated(help);
                String fun = InterpreterMessage.castString(help);
                if (fun.equals("none")) {
                    mask &= ~MASK_PRFX_LIBR;
                    mask &= ~MASK_PRFX_FRGN;
                } else if (fun.equals(LoadOpts.OP_PREFIX_LIBRARY)) {
                    mask |= MASK_PRFX_LIBR;
                    mask &= ~MASK_PRFX_FRGN;
                } else if (fun.equals(LoadOpts.OP_PREFIX_FOREIGN)) {
                    mask &= ~MASK_PRFX_LIBR;
                    mask |= MASK_PRFX_FRGN;
                } else if (fun.equals("both")) {
                    mask |= MASK_PRFX_LIBR;
                    mask |= MASK_PRFX_FRGN;
                } else {
                    throw new InterpreterMessage(
                            InterpreterMessage.domainError("fix_option", help));
                }
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals("file_extension")) {
                Object help = ((TermCompound) temp).getArg(0);
                InterpreterMessage.checkInstantiated(help);
                String fun = InterpreterMessage.castString(help);
                if (fun.equals("none")) {
                    mask &= ~MASK_SUFX_TEXT;
                    mask &= ~MASK_SUFX_BNRY;
                    mask &= ~MASK_SUFX_RSCS;
                } else if (fun.equals("file")) {
                    mask |= MASK_SUFX_TEXT;
                    mask |= MASK_SUFX_BNRY;
                    mask &= ~MASK_SUFX_RSCS;
                } else if (fun.equals("resource")) {
                    mask &= ~MASK_SUFX_TEXT;
                    mask &= ~MASK_SUFX_BNRY;
                    mask |= MASK_SUFX_RSCS;
                } else {
                    throw new InterpreterMessage(
                            InterpreterMessage.domainError("fix_option", help));
                }
            } else if (temp instanceof TermCompound &&
                    ((TermCompound) temp).getArity() == 1 &&
                    ((TermCompound) temp).getFunctor().equals("failure")) {
                Object help = ((TermCompound) temp).getArg(0);
                InterpreterMessage.checkInstantiated(help);
                String fun = InterpreterMessage.castString(help);
                if (fun.equals("none")) {
                    mask &= ~MASK_FAIL_READ;
                    mask &= ~MASK_FAIL_CHLD;
                } else if (fun.equals("read")) {
                    mask |= MASK_FAIL_READ;
                    mask &= ~MASK_FAIL_CHLD;
                } else if (fun.equals("child")) {
                    mask &= ~MASK_FAIL_READ;
                    mask |= MASK_FAIL_CHLD;
                } else {
                    throw new InterpreterMessage(
                            InterpreterMessage.domainError("fix_option", help));
                }
            } else {
                InterpreterMessage.checkInstantiated(temp);
                throw new InterpreterMessage(
                        InterpreterMessage.domainError("fix_option", temp));
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
        return mask;
    }

    /********************************************************/
    /* Class Path Modification & Access                     */
    /********************************************************/

    /**
     * <p>Add a class path.</p>
     *
     * @param inter The interpreter.
     * @param path  The class path.
     * @throws InterpreterMessage       Validation error.
     * @throws CharacterCodingException File canonization problem.
     * @throws MalformedURLException    URL assembling problem.
     */
    public static void sysAddPath(Interpreter inter, String path)
            throws InterpreterMessage, IOException {
        path = sysFindWrite(inter, path);
        inter.getKnowledgebase().addClassPath(path);
    }

    /**
     * <p>Retrieve the class paths.</p>
     *
     * @param inter The interpreter.
     * @return The list of class paths.
     */
    public static Object sysGetPaths(Interpreter inter) {
        ArrayList<String> paths = inter.getKnowledgebase().getClassPaths();
        Object end = Knowledgebase.OP_NIL;
        for (int i = paths.size() - 1; i >= 0; i--) {
            end = new TermCompound(Knowledgebase.OP_CONS,
                    paths.get(i), end);
        }
        return end;
    }

}
