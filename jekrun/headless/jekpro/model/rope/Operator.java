package jekpro.model.rope;

import jekpro.frequent.system.ForeignThread;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.reference.reflect.SpecialOper;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;

import java.io.Reader;

/**
 * <p>This class provides an operator.
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
public class Operator {
    public final static int LEVEL_HIGH = 1200;
    public final static int LEVEL_MIDDLE = 949;

    public static final int TYPE_PREFIX = 0;
    public static final int TYPE_INFIX = 1;
    public static final int TYPE_POSTFIX = 2;

    public final static int MASK_OPER_LEFT = 0x00000001;
    public final static int MASK_OPER_RGHT = 0x00000002;
    public final static int MASK_OPER_TABR = 0x00000004;

    public final static int MASK_OPER_NSPL = 0x00000010;
    public final static int MASK_OPER_NSPR = 0x00000020;
    public final static int MASK_OPER_NEWR = 0x00000040;

    public final static int MASK_OPER_VSPR = 0x00000100;
    public final static int MASK_OPER_VSPU = 0x00000200;

    /* combined operator flags */
    public final static int MASK_OPER_MODE = MASK_OPER_LEFT | MASK_OPER_RGHT;
    public final static int MASK_OPER_VISI = MASK_OPER_VSPR | MASK_OPER_VSPU;

    private final String key;
    private final int type;
    private int flags;
    private int level;
    private AbstractSource scope;
    private AbstractSource source;
    private final String name;
    private String portray;
    private String alias;

    /**
     * <p>Create an operator.</p>
     *
     * @param t The type.
     * @param k The key.
     */
    public Operator(int t, String k) {
        key = k;
        if (CacheFunctor.isQuali(key)) {
            name = CacheFunctor.sepName(key);
        } else {
            name = key;
        }
        type = t;
    }

    /**
     * <p>Retrieve the operator type.</p>
     *
     * @return The operator type.
     */
    public int getType() {
        return type;
    }

    /**
     * <p>Retrieve the operator key.</p>
     *
     * @return The operator key.
     */
    public String getKey() {
        return key;
    }

    /**
     * <p>Set the level.</p>
     *
     * @param l The level.
     */
    public void setLevel(int l) {
        level = l;
    }

    /**
     * <p>Return the operator level.</p>
     *
     * @return The operator level.
     */
    public int getLevel() {
        return level;
    }

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public int getBits() {
        return flags;
    }

    /**
     * <p>Set a flag.</p>
     *
     * @param m The mask.
     */
    public void setBit(int m) {
        synchronized (this) {
            flags |= m;
        }
    }

    /**
     * <p>Set a flag.</p>
     *
     * @param m The mask.
     */
    public void resetBit(int m) {
        synchronized (this) {
            flags &= ~m;
        }
    }

    /**
     * <p>Set the source.</p>
     *
     * @param s The source.
     */
    public void setScope(AbstractSource s) {
        scope = s;
    }

    /**
     * <p>Retrieve the source.</p>
     *
     * @return The source.
     */
    public AbstractSource getScope() {
        return scope;
    }

    /**
     * <p>Retrieve the postion.</p>
     *
     * @return The postion, can be null.
     */
    public PositionKey getPosition() {
        return null;
    }

    /**
     * <p>Set the postion.</p>
     *
     * @param o The postion, can be null.
     */
    public void setPosition(PositionKey o) {
        /* */
    }

    /**
     * <p>Retrieve the home source.</p>
     *
     * @return The home source.
     */
    public AbstractSource getSource() {
        return source;
    }

    /**
     * <p>Set the home source.</p>
     *
     * @param s The home source.
     */
    public void setSource(AbstractSource s) {
        source = s;
    }

    /**
     * <p>Retrieve the portray.</p>
     *
     * @return The portray.
     */
    public String getPortray() {
        return portray;
    }

    /**
     * <p>Set the portray.</p>
     *
     * @param p The portray.
     */
    public void setPortray(String p) {
        portray = p;
    }

    /**
     * <p>Retrieve the alias.</p>
     *
     * @return The alias.
     */
    public String getAlias() {
        return alias;
    }

    /**
     * <p>Set the alias.</p>
     *
     * @param a The alias.
     */
    public void setAlias(String a) {
        alias = a;
    }

    /**************************************************************/
    /* Definition Handling                                        */
    /**************************************************************/

    /**
     * <p>Add a source definition.</p>
     * <p>Can veto that an operator is extended.</p>
     *
     * @param sa The call-site, non-null.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public void addDef(SkelAtom sa, Engine en)
            throws EngineMessage {
        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
        boolean ok;
        synchronized (this) {
            if (scope != src) {
                if (scope != null) {
                    ok = false;
                } else {
                    scope = src;
                    ok = true;
                }
            } else {
                return;
            }
        }
        if (ok) {
            src.addOperInv(this);
            setPosition(sa.getPosition());
            return;
        }
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_REDEFINE,
                EngineMessage.OP_PERMISSION_OPERATOR,
                SpecialOper.operToColonSkel(getKey(),
                        getSource().getStore().user, getType(), en)));
    }

    /**
     * <p>Remove a source definition.</p>
     *
     * @param s The source definition.
     */
    private void removeDef(AbstractSource s) {
        synchronized (this) {
            if (scope != s) {
                return;
            } else {
                scope = null;
            }
        }
        s.removeOperInv(this);
    }

    /**
     * <p>Clear a scope from an operator.</p>
     *
     * @param s The source.
     */
    public void clearOper(AbstractSource s) {
        removeDef(s);
        getSource().removeOper(getType(), getKey());
    }

    /**************************************************************/
    /* Parser Access                                              */
    /**************************************************************/

    /**
     * <p>Return the operator delta for the right argument.</p>
     * <p>Defined for infix and prefix.</p>
     *
     * @return The operator delta.
     */
    public int getRight() {
        if ((getBits() & MASK_OPER_RGHT) != 0) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * <p>Return the operator delta for the left argument.<p>
     * <p>Defined for infix and postfix.</p>
     *
     * @return The operator delta.
     */
    public int getLeft() {
        if ((getBits() & MASK_OPER_LEFT) != 0) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * <p>Retrieve the portray or the key.</p>
     *
     * @return The portray or key.
     */
    public String getPortrayOrName() {
        if (portray != null)
            return portray;
        return name;
    }

    /**
     * <p>Retrieve the alias or the key.</p>
     *
     * @return The alias or the key.
     */
    public String getAliasOrName() {
        if (alias != null)
            return alias;
        return name;
    }

    /**************************************************************/
    /* Printing Operators                                         */
    /**************************************************************/

    /**
     * <p>Assure that the operator is existent.</p>
     *
     * @param op The operator.
     * @param t  The skel.
     * @param d  The display.
     * @throws EngineMessage Shit happens.
     */
    public static void checkExistentOperator(Operator op, Object t, Display d)
            throws EngineMessage {
        if (op == null)
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_OPERATOR, t), d);
    }

    /************************************************************************/
    /* Style Checks Oper Initialization                                     */
    /************************************************************************/

    /**
     * <p>Perform a style check on the given operator.</p>
     * <p>This check is performed at the end of loading a module.</p>
     *
     * @param lr The readear.
     * @param op The operator.
     * @param en The engine.
     * @throws EngineMessage Printing error.
     */
    public static void checkOperInit(Reader lr, Operator op,
                                     Engine en)
            throws EngineMessage, EngineException {
        try {
            checkOperImplemented(op, en);
        } catch (EngineMessage x) {
            PositionKey pos = PositionKey.createPos(lr);
            EngineException y = new EngineException(x,
                    EngineException.fetchLoc(EngineException.fetchStack(en),
                            pos, en), EngineException.OP_WARNING);
            y.printStackTrace(en);
        }
    }

    /**
     * <p>Perform the implemented check.</p>
     *
     * @param oper The operator.
     * @param en   The engine.
     * @throws EngineMessage The warning.
     */
    private static void checkOperImplemented(Operator oper,
                                             Engine en)
            throws EngineMessage {
        if (oper.level != 0)
            return;
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_IMPLEMENTATION_OPER,
                SpecialOper.operToColonSkel(oper.getKey(),
                        oper.getSource().getStore().user, oper.getType(), en)));
    }

}
