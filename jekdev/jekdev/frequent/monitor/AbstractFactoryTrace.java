package jekdev.frequent.monitor;

import jekdev.model.builtin.CapabilityTrace;
import jekdev.model.pretty.FoyerTrace;
import jekdev.reference.system.ConnectionReaderTrace;
import jekdev.reference.system.ConnectionWriterTrace;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.pretty.Foyer;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.call.Capability;
import jekpro.tools.proxy.CapabilityAPI;

import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;

/**
 * <p>This class provides an abstract factory.</p>
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
public abstract class AbstractFactoryTrace extends AbstractFactory {

    /**
     * <p>Create an abstract factory trace.</p>
     */
    public AbstractFactoryTrace() {
        addPrologFlags(FlagFactoryTrace.DEFAULT);
    }

    /**
     * <p>Prepare the tool connections.</p>
     * <p>Can be overridden by subclasses.</p>
     */
    protected void prepareToolConnections() {
        Charset cs = getEncoding();
        toolinput = new ConnectionReaderTrace(new InputStreamReader(System.in, cs));
        tooloutput = new ConnectionWriterTrace(new OutputStreamWriter(System.out, cs));
        toolerror = new ConnectionWriterTrace(new OutputStreamWriter(System.err, cs));
    }

    /*******************************************************************/
    /* Bootstrap Capabilities                                          */
    /*******************************************************************/

    /**
     * <p>Retrieve the init branches.</p>
     *
     * @return The branches.
     */
    public AbstractBranch[] getInitBranches() {
        Capability cap = new CapabilityAPI();
        Capability cap2 = new CapabilityTrace();
        return new AbstractBranch[]{(AbstractBranch) cap.getBranch(),
                (AbstractBranch) cap2.getBranch()};
    }

    /**
     * <p>Retrieve the brand branch.</p>
     *
     * @return The branch.
     */
    public AbstractBranch getBrandBranch() {
        Capability cap2 = new CapabilityTrace();
        return (AbstractBranch) cap2.getBranch();
    }

    /*******************************************************************/
    /* Factory Methods                                                 */
    /*******************************************************************/

    /**
     * <p>Create a fioyer.</p>
     * <p>Can be overridden by subclasses.</p>
     *
     * @return The foyer.
     */
    public Foyer createFoyer() {
        Foyer foyer = new FoyerTrace();
        foyer.setFactory(this);
        return foyer;
    }

}