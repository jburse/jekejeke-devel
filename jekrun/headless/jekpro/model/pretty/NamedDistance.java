package jekpro.model.pretty;

import jekpro.model.inter.Engine;
import jekpro.model.molec.BindVar;
import jekpro.tools.term.SkelVar;
import jekpro.tools.term.TermVar;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

/**
 * <p>This class registers the deref distance of a variable
 * name. This should give a little bit more natural printing
 * of variables.</p>
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
public final class NamedDistance {
    private String name;
    private int distance;

    /**
     * <p>Create a named distance</p>
     *
     * @param n The name.
     * @param d The distance.
     */
    private NamedDistance(String n, int d) {
        name = n;
        distance = d;
    }

    /**
     * <p>Retrieve the name.</p>
     *
     * @return The name.
     */
    public String getName() {
        return name;
    }

    /**
     * <p>Set the name.</p>
     *
     * @param n The name.
     */
    public void setName(String n) {
        name = n;
    }

    /**
     * <p>Retrieve the distance.</p>
     *
     * @return The distance.
     */
    private int getDistance() {
        return distance;
    }

    /**
     * <p>Set the distance.</p>
     *
     * @param d The distance.
     */
    private void setDistance(int d) {
        distance = d;
    }

    /**************************************************************/
    /* Print Map Helpers                                          */
    /**************************************************************/

    /**
     * <p>Dereference the term given by the current skeleton
     * and display, return the result in the current skeleton
     * and display.</p>
     *
     * @return The deref count.
     */
    public static int derefCount(Engine en) {
        int count = 0;
        BindVar b;
        while (en.skel instanceof SkelVar &&
                (b = en.display.bind[((SkelVar) en.skel).id]).display != null) {
            en.skel = b.skel;
            en.display = b.display;
            count++;
        }
        return count;
    }

    /**
     * <p>Add priorized to the map hash.</p>
     *
     * @param print The print map.
     * @param pair The variable.
     * @param name The variable name.
     * @param distance The variable distance.
     */
    public static void addPriorized(MapHashLink<TermVar, NamedDistance> print,
                                    TermVar pair,
                                    String name, int distance) {
        MapEntry<TermVar, NamedDistance> entry = print.getEntry(pair);
        if (entry == null) {
            NamedDistance nd = new NamedDistance(name, distance);
            print.add(pair, nd);
            return;
        }
        NamedDistance nd = entry.value;
        if (distance >= nd.getDistance())
            return;
        nd.setName(name);
        nd.setDistance(distance);
    }

    /**
     * <p>Add an anonymous variable.</p>
     *
     * @param print The print map.
     * @param pair The variable.
     * @param name The variable name.
     */
    public static void addAnon(MapHashLink<TermVar, NamedDistance> print,
                                    TermVar pair,
                                    String name) {
        NamedDistance nd = new NamedDistance(name, 0);
        print.add(pair, nd);
    }

}