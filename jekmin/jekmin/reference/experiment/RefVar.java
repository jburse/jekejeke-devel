package jekmin.reference.experiment;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.term.SkelVar;

/**
 * <p>This class wraps a variable skeleton and a display. Upon
 * hash code or equality the variable is not dereferenced.</p>
 *
 * @author Copyright 2014-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.1.1 (a fast and small prolog interpreter)
 */
final class RefVar implements Comparable<RefVar> {
    final SkelVar skel;
    final Display display;

    /**
     * <p>Creata a molec var from variable skeleton and display.
     *
     * @param s The variable skeleton.
     * @param d The display.
     */
    RefVar(SkelVar s, Display d) {
        skel = s;
        display = d;
    }

    /**
     * <p>Compute the hash.</p>
     *
     * @return The hash value.
     */
    public int hashCode() {
        return skel.hashCode(display);
    }

    /**
     * <p>Check equals another ref var.</p>
     *
     * @param o The other object.
     * @return True of equals other object, otherwise false.
     */
    public boolean equals(Object o) {
        if (!(o instanceof RefVar))
            return false;
        RefVar m = (RefVar) o;
        return (skel == m.skel && display == m.display);
    }

    /**
     * <p>Compare this ref var to another ref var.</p>
     *
     * @param o The other ref var.
     * @return <0 less, 0 equal, >0 greater
     */
    public int compareTo(RefVar o) {
        Engine en = Engine.getEngine();
        if (en == null)
            throw new ArithmeticException(EngineMessage.OP_EVALUATION_ORDERED);
        int i = display.bind[skel.id].getValue(en);
        int k = o.display.bind[o.skel.id].getValue(en);
        return i - k;
    }

}
