package jekpro.platform.swing;

import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.regex.ScannerError;

import java.applet.Applet;

/**
 * Command line parser for the Swing version.
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
public final class CommandLine {
    public final static String OP_SYNTAX_UNKNOWN_OPTION = "unknown_option";
    public final static String OP_SYNTAX_ARG_MISSING = "arg_missing";
    public final static String OP_SYNTAX_DUPLICATE_OPTION = "duplicate_option";

    private final MapHash<String, String> decl = new MapHash<String, String>();
    private final MapHash<String, String> res = new MapHash<String, String>();

    /**
     * <p>Retrieve an option value.</p>
     *
     * @param name The name.
     * @return The value.
     */
    public String getValue(String name) {
        String val = res.get(name);
        if (val == null)
            val = decl.get(name);
        return val;
    }

    /**
     * <p>Define an option.</p>
     *
     * @param name The name.
     * @param def  The default, can be null.
     */
    public void defineOption(String name, String def) {
        if (name == null)
            throw new NullPointerException("name missing");
        if (def == null)
            throw new NullPointerException("def missing");
        decl.put(name, def);
    }

    /**
     * <p>Decode options.</p>
     *
     * @param args The arguments.
     * @throws ScannerError Shit happens.
     */
    public void decodeOptions(String[] args)
            throws ScannerError {
        for (int i = 0; i < args.length; i++) {
            String name = args[i];
            String def = decl.get(name);
            if (def == null)
                throw new ScannerError(CommandLine.OP_SYNTAX_UNKNOWN_OPTION, i);
            if (res.get(name) != null)
                throw new ScannerError(CommandLine.OP_SYNTAX_DUPLICATE_OPTION, i);
            i++;
            if (i >= args.length)
                throw new ScannerError(CommandLine.OP_SYNTAX_ARG_MISSING, i);
            res.put(name, args[i]);
        }
    }

    /**
     * <p>Decode options.</p>
     *
     * @param applet The applet.
     * @throws ScannerError Shit happens.
     */
    public void decodeOptions(Applet applet)
            throws ScannerError {
        for (MapEntry<String, String> entry = decl.getFirstEntry();
             entry != null; entry = decl.successor(entry)) {
            String val = applet.getParameter(entry.key);
            if (val == null)
                continue;
            res.put(entry.key, val);
        }
    }

}
