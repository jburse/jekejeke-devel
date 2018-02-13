package jekpro.frequent.stream;

import jekpro.frequent.system.ForeignLocale;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.Date;
import java.util.Locale;
import java.util.Properties;

/**
 * <p>The foreign predicates for the module console.</p>
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
public final class ForeignConsole {

    /****************************************************************/
    /* Read Utils                                                   */
    /****************************************************************/

    /**
     * <p>Read a line.</p>
     *
     * @param obj The reader.
     * @return The line.
     * @throws IOException        IO error.
     */
    public static String sysReadLine(Reader obj)
            throws IOException {
        return readLine(obj);
    }

    /**
     * <p>Read a line.</p>
     *
     * @param lr The reader.
     * @return The line.
     * @throws IOException IO error.
     */
    public static String readLine(Reader lr)
            throws IOException {
        StringBuilder buf = new StringBuilder();
        int c = lr.read();
        while (c != '\n' && c != -1) {
            buf.append((char) c);
            c = lr.read();
        }
        if (c == -1 && buf.length() == 0)
            return null;
        return buf.toString();
    }

    /*******************************************************/
    /* Stack Trace Printing                                */
    /*******************************************************/

    /**
     * <p>Print an exception.</p>
     *
     * @param inter  The interpreter or null.
     * @param wr     The writer.
     * @param term   The exception term.
     * @param locstr The locale.
     * @param prop   The properties.
     * @throws IOException          IO error.
     * @throws InterpreterMessage   Not a text output.
     * @throws InterpreterException Shit happens.
     */
    public static void sysPrintStackTrace(Interpreter inter, Writer wr, Object term,
                                          String locstr, Properties prop)
            throws IOException, InterpreterMessage, InterpreterException {
        try {
            Locale locale = ForeignLocale.stringToLocale(locstr);
            Engine en = (Engine) inter.getEngine();
            EngineException.printStackTrace(wr, AbstractTerm.getSkel(term),
                    AbstractTerm.getDisplay(term), locale, prop, en);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
    }

    /**
     * <p>Some test cases.</p>
     */
    /*
    public static void main(String[] args) {
        Date date = new Date();
        System.out.format("date=%1$tF %1$tT", date);
    }
    */

}
