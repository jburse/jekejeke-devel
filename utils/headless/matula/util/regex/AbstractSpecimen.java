package matula.util.regex;

/**
 * <p>This class provides the base class for a bounded string specimen.</p>
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
public abstract class AbstractSpecimen extends AbstractPattern {
    public static final int MATCH_NEGATIV = 0x00000001;
    public static final int MATCH_SENSITIV = 0x00000002;

    public static final int MASK_PAT_BOUNCE = 0x00000001;

    protected String pattern = "*";
    protected int flag;
    protected String target = "*";

    /**
     * <p>Create an abstract speciment.</p>
     */
    public AbstractSpecimen() {
        try {
            prepareMatch();
        } catch (ScannerError x) {
            throw new RuntimeException("shouldn't happen", x);
        }
    }

    /**
     * <p>Set the pattern.</p>
     *
     * @param p The pattern.
     */
    public void setPattern(String p) {
        pattern = p;
    }

    /**
     * <p>Retrieve the pattern.</p>
     *
     * @return The pattern.
     */
    public String getPattern() {
        return pattern;
    }

    /**
     * <p>Set the flags.</p>
     *
     * @param f The flags.
     */
    public final void setFlag(int f) {
        flag = f;
    }

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public final int getFlag() {
        return flag;
    }

    /**
     * <p>Set the target.</p>
     *
     * @param t The target.
     */
    public void setTarget(String t) {
        target = t;
    }

    /**
     * <p>Retrieve the target.</p>
     *
     * @return The target.
     */
    public String getTarget() {
        return target;
    }

    /******************************************************************/
    /* Preparation                                                    */
    /******************************************************************/

    /**
     * <p>Repare the pattern.</p>
     *
     * @throws ScannerError Parsing problem.
     */
    public abstract void prepareMatch()
            throws ScannerError;

    /**
     * <p>Move the pattern to the replace target.</p>
     *
     * @param match The pattern to move.
     */
    public void replaceTo(AbstractSpecimen match) {
        setTarget(match.getPattern());
    }

    /******************************************************************/
    /* Find                                                           */
    /******************************************************************/

    /**
     * <p>Return a copy of the matcher.</p>
     *
     * @return The copy.
     */
    public abstract AbstractSpecimen copyMatcher();

}
