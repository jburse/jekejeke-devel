package jekpro.frequent.system;

import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Lobby;
import jekpro.tools.term.TermCompound;
import matula.util.wire.XSelectFormat;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * <p>The foreign predicates for the module system/shell.</p>
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
public final class ForeignShell {

    /*****************************************************************/
    /* Environment Variables                                         */
    /*****************************************************************/

    /**
     * <p>List the environment variable names.</p>
     *
     * @return The environment variable names.
     */
    public static Object sysListEnv(Interpreter inter) {
        Lobby lobby = inter.getKnowledgebase().getLobby();
        Iterator<String> iter = System.getenv().keySet().iterator();
        Object res = lobby.ATOM_NIL;
        while (iter.hasNext()) {
            res = new TermCompound(lobby.ATOM_CONS,
                    iter.next(), res);
        }
        return res;
    }

    /*****************************************************************/
    /* Retrieve Date & Calendar                                      */
    /*****************************************************************/

    /**
     * <p>Retrieve a time zoned date.</p>
     *
     * @param time The time in milliseconds and current time zone.
     * @param zone The desired time zone.
     * @return The date.
     */
    public static Calendar sysGetTime(long time, String zone) {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone(zone));
        cal.setTimeInMillis(time);
        return cal;
    }

    /*****************************************************************/
    /* Format Date & Calendar                                        */
    /*****************************************************************/

    /**
     * <p>Convert a date or time zoned date to a string.</p>
     *
     * @param locstr The locale.
     * @param format The format.
     * @param date   The date.
     * @return The unparsed string.
     */
    public static String sysDateToString(String locstr,
                                         String format, Object date) {
        Locale locale = XSelectFormat.stringToLocale(locstr);
        SimpleDateFormat sf = new SimpleDateFormat(format, locale);
        if (date instanceof Calendar) {
            sf.setTimeZone(((Calendar) date).getTimeZone());
            return sf.format(((Calendar) date).getTime());
        } else {
            return sf.format(date);
        }
    }

    /**
     * <p>Convert a string to a date.</p>
     *
     * @param locstr The locale.
     * @param format The format.
     * @param str    The string.
     * @return The parsed date.
     */
    public static Object sysStringToDate(String locstr,
                                         String format, String str)
            throws InterpreterMessage {
        try {
            Locale locale = XSelectFormat.stringToLocale(locstr);
            SimpleDateFormat sf = new SimpleDateFormat(format, locale);
            return sf.parse(str);
        } catch (ParseException x) {
            throw new InterpreterMessage(InterpreterMessage.syntaxError(x.getMessage()));
        }
    }

    /**
     * <p>Some testing.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) {
        Properties prop = System.getProperties();
        Enumeration<Object> keys = prop.keys();
        while (keys.hasMoreElements()) {
            Object key = keys.nextElement();
            Object value = prop.get(key);
            System.out.println(key + "\t" + value);
        }
    }
    */

    /**
     * <p>Some testing.</p>
     *
     * @param args Not used.
     */
    public static void main(String[] args) throws ParseException {
        SimpleDateFormat sf = new SimpleDateFormat("dd MMM yyyy");
        Date date=sf.parse("29 Feb 2020");
        System.out.println("date="+date);

        Calendar calendar=Calendar.getInstance();
        calendar.setTime(date);
        calendar.add(Calendar.YEAR, -1);
        System.out.println("date="+calendar.getTime());

        calendar.add(Calendar.YEAR, 1);
        System.out.println("date="+calendar.getTime());

  /*
        long time = 1549463357000L;
        SimpleDateFormat sf = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz");
        sf.setTimeZone(TimeZone.getTimeZone("Europe/Berlin"));
        System.out.println("sf=" + sf.format(time)+ ", tmz=" + sf.getTimeZone().getID());

        sf = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz", Locale.ENGLISH);
        sf.setTimeZone(TimeZone.getTimeZone("GMT"));
        System.out.println("sf=" + sf.format(time)+ ", tmz=" + sf.getTimeZone().getID());

        sf = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz");
        Date date = sf.parse("Mi, 06 Feb 2019 15:29:17 MEZ");
        System.out.println("time=" + date.getTime() + ", tmz=" + sf.getTimeZone().getID());

        sf = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz", Locale.ENGLISH);
        date = sf.parse("Wed, 06 Feb 2019 14:29:17 GMT");
        System.out.println("time=" + date.getTime() + ", tmz=" + sf.getTimeZone().getID());
       */
    }

}
