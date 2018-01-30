package jekpro.platform.swing;

import matula.util.data.ListArray;
import matula.util.data.MapHash;
import matula.util.regex.ScannerError;

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

    private final MapHash<String, OptionSpec> decl = new MapHash<String, OptionSpec>();
    private final MapHash<String, String[]> res = new MapHash<String, String[]>();

    /**
     * <p>Define an option.</p>
     *
     * @param name  The name.
     * @param arity The arity.
     * @param def   The default, can be null.
     */
    public void defineOption(String name, int arity, String[] def) {
        decl.put(name, new OptionSpec(arity, def));
    }

    /**
     * <p>Decode options.</p>
     *
     * @param args The arguments.
     * @throws ScannerError Shit happens.
     */
    public void decodeOptions(String[] args) throws ScannerError {
        for (int i = 0; i < args.length; i++) {
            String name = args[i];
            OptionSpec spec = decl.get(name);
            if (spec == null)
                throw new ScannerError(CommandLine.OP_SYNTAX_UNKNOWN_OPTION, i);
            if (res.get(name) != null)
                throw new ScannerError(CommandLine.OP_SYNTAX_DUPLICATE_OPTION, i);
            ListArray<String> values = new ListArray<String>();
            for (int j = 0; j < spec.getArity(); j++) {
                i++;
                if (i >= args.length)
                    throw new ScannerError(CommandLine.OP_SYNTAX_ARG_MISSING, i);
                values.add(args[i]);
            }
            String[] pack = new String[values.size()];
            values.toArray(pack);
            res.put(name, pack);
        }
    }

    /**
     * <p>Retrieve an option value.</p>
     *
     * @param name The name.
     * @return The value.
     */
    public String[] getValue(String name) {
        String[] val = res.get(name);
        if (val == null) {
            OptionSpec spec = decl.get(name);
            val = spec.getDefault();
        }
        return val;
    }

}
