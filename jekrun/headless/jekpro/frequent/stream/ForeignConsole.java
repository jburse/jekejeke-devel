package jekpro.frequent.stream;

import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import matula.util.regex.CodeType;
import matula.util.regex.ScannerToken;
import matula.util.wire.LangProperties;

import java.io.*;
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignConsole {

    /****************************************************************/
    /* Read Line                                                    */
    /****************************************************************/

    /**
     * <p>Read a line.</p>
     *
     * @param reader The reader.
     * @return The line.
     * @throws IOException IO error.
     */
    public static String readLine(Reader reader)
            throws IOException {
        StringBuilder buf = new StringBuilder();
        int ch = ScannerToken.sysGetCode(reader);
        while (ch != CodeType.LINE_EOL && ch != CodeType.LINE_EOF) {
            buf.appendCodePoint(ch);
            ch = ScannerToken.sysGetCode(reader);
        }
        if (ch == CodeType.LINE_EOF && buf.length() == 0)
            return null;
        return buf.toString();
    }

    /**
     * <p>Read a line with maximum length.</p>
     *
     * @param reader The reader.
     * @param arg    The maximum length.
     * @return The line.
     * @throws IOException IO error.
     */
    public static String readLineMax(Reader reader, Integer arg)
            throws IOException {
        int len = arg.intValue();
        StringBuilder buf = new StringBuilder();
        int ch = (0 < len ? ScannerToken.sysGetCode(reader) : CodeType.LINE_EOL);
        while (ch != CodeType.LINE_EOL && ch != CodeType.LINE_EOF) {
            buf.appendCodePoint(ch);
            len--;
            ch = (0 < len ? ScannerToken.sysGetCode(reader) : CodeType.LINE_EOL);
        }
        if (ch == CodeType.LINE_EOF && buf.length() == 0)
            return null;
        return buf.toString();
    }

    /****************************************************************/
    /* Read Punch                                                   */
    /****************************************************************/

    /**
     * <p>Read a punch.</p>
     *
     * @param in The input stream.
     * @return The line.
     * @throws IOException IO error.
     */
    public static byte[] readPunch(InputStream in)
            throws IOException {
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        int ch = in.read();
        while (ch != CodeType.LINE_WIN && ch != CodeType.LINE_EOF) {
            buf.write(ch);
            ch = in.read();
        }
        if (ch == CodeType.LINE_EOF && buf.size() == 0)
            return null;
        if (ch != CodeType.LINE_WIN)
            throw new StreamCorruptedException("cr missing");
        ch = in.read();
        if (ch != CodeType.LINE_EOL)
            throw new StreamCorruptedException("lf missing");
        return buf.toByteArray();
    }

    /**
     * <p>Read a punch with maximum length.</p>
     *
     * @param in  The input stream.
     * @param arg The maximum length.
     * @return The line.
     * @throws IOException IO error.
     */
    public static byte[] readPunchMax(InputStream in, Integer arg)
            throws IOException {
        int len = arg.intValue();
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        int ch = (0 < len ? in.read() : CodeType.LINE_WIN);
        while (ch != CodeType.LINE_WIN && ch != CodeType.LINE_EOF) {
            buf.write(ch);
            len--;
            ch = (0 < len ? in.read() : CodeType.LINE_WIN);
        }
        if (ch == CodeType.LINE_EOF && buf.size() == 0)
            return null;
        if (!(0 < len))
            return buf.toByteArray();
        if (ch != CodeType.LINE_WIN)
            throw new StreamCorruptedException("cr missing");
        ch = in.read();
        if (ch != CodeType.LINE_EOL)
            throw new StreamCorruptedException("lf missing");
        return buf.toByteArray();
    }

    /****************************************************************/
    /* Exception Utilities                                          */
    /****************************************************************/

    /**
     * <p>Format a term from properties.</p>
     *
     * @param inter  The interpreter.
     * @param term   The message term.
     * @param locstr The locale.
     * @param obj    The properties.
     * @return The formatted term.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public static String sysErrorMake(Interpreter inter, Object term,
                                      String locstr, Properties obj)
            throws InterpreterMessage, InterpreterException {
        try {
            Locale locale = LangProperties.stringToLocale(locstr);
            Engine en = (Engine) inter.getEngine();
            return EngineException.errorMake(AbstractTerm.getSkel(term),
                    AbstractTerm.getDisplay(term), locale, obj, en);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
    }

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
            Locale locale = LangProperties.stringToLocale(locstr);
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
