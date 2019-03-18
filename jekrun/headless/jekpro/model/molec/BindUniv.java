package jekpro.model.molec;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.inter.Engine;
import jekpro.tools.term.SkelVar;

/**
 * <p>This class provides a reference counted variable binder.</p>
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
public class BindUniv {
    public final static BindUniv[] BIND_CONST = new BindUniv[0];

    public Object skel;
    public Display display;
    public int refs = 1;

    /**
     * <p>Remove this bind from the engine.</p>
     *
     * @param en The engine.
     */
    void removeBind(Engine en) {
        /* do nothing */
    }

    /**
     * <p>Add this bind to the engine.</p>
     *
     * @param en The engine.
     */
    void addBind(Engine en) {
        /* do nothing */
    }

    /**
     * <p>Remove the variable from the binding list and unbind the term from it.</p>
     * <p>Decrement the dependent counts, and if zero recursively unbind.</p>
     * <p>Tail recursive implementation.</p>
     *
     * @param bc The bind univ.
     * @param en The engine.
     */
    public static void unbind(BindUniv bc, Engine en) {
        for (; ; ) {
            /* unbind variable */
            Display d = bc.display;
            Object t = bc.skel;
            bc.removeBind(en);
            bc.display = null;

            Object var = EngineCopy.getVar(t);
            if (var == null)
                break;
            SkelVar v;
            if (var instanceof SkelVar) {
                v = (SkelVar) var;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                int i = 0;
                for (; i < temp.length - 1; i++) {
                    v = temp[i];
                    bc = d.bind[v.id];
                    if ((--bc.refs) == 0) {
                        d.bind[v.id] = null;
                        if (bc.display != null)
                            BindUniv.unbind(bc, en);
                    }
                }
                v = temp[i];
            }
            bc = d.bind[v.id];
            if ((--bc.refs) == 0) {
                d.bind[v.id] = null;
                if (bc.display != null) {
                    continue;
                }
            }
            break;
        }
    }

    /**
     * <p>BindVar this variable with a term.</p>
     * <p>No occurs check is performed.</p>
     * <p>Possibly an attribute variable hook is called.</p>
     *
     * @param t  The term skeleton.
     * @param d  The term display.
     * @param d2 The variable display.
     * @param en The engine.
     * @throws EngineException Shit happens.
     */
    public boolean bindAttr(Object t, Display d, Display d2,
                            Engine en)
            throws EngineException {
        bindUniv(t, d, en);
        return true;
    }

    /**
     * <p>BindVar this variable with a term.</p>
     * <p>No occurs check is performed.</p>
     *
     * @param t  The term to bind to.
     * @param d  The display of the term.
     * @param en The engine.
     */
    public void bindUniv(Object t, Display d, Engine en) {
        /* bind variable */
        skel = t;
        display = d;
        addBind(en);

        Object var = EngineCopy.getVar(t);
        if (var == null)
            return;
        if (var instanceof SkelVar) {
            SkelVar v = (SkelVar) var;
            BindUniv bc = d.bind[v.id];
            bc.refs++;
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            int n = temp.length;
            for (int j = 0; j < n; j++) {
                SkelVar v = temp[j];
                BindUniv bc = d.bind[v.id];
                bc.refs++;
            }
        }
    }

    /**
     * <p>Same as engine remtab for univ builtins.</p>
     *
     * @param b  The display
     * @param en The engine.
     */
    public static void remTab(BindUniv[] b, Engine en) {
        int n = b.length;
        int k = 0;
        do {
            BindUniv bc = b[k];
            if ((--bc.refs) == 0) {
                b[k] = null;
                if (bc.display != null)
                    BindUniv.unbind(bc, en);
            }
            k++;
        } while (k < n);
    }

    /**
     * <p>Create a new display.</p>
     * <p>Fill the binds with bind univ.</p>
     *
     * @param s The size.
     * @return The new display.
     */
    public static BindUniv[] newUniv(int s) {
        if (s == 0)
            return BIND_CONST;
        BindUniv[] b = new BindUniv[s];
        for (int i = 0; i < s; i++)
            b[i] = new BindUniv();
        return b;
    }

    /**
     * <p>Create a new display.</p>
     * <p>Fill the binds with bind univ and bind lexical.</p>
     *
     * @param s The first size.
     * @param t The second size.
     * @return The new display.
     */
    public static BindUniv[] newUnivLexical(int s, int t) {
        t += s;
        if (t == 0)
            return BIND_CONST;
        BindUniv[] b = new BindUniv[t];
        for (int i = 0; i < t; i++) {
            if (i < s) {
                b[i] = new BindUniv();
            } else {
                b[i] = new BindLexical();
            }
        }
        return b;
    }

}