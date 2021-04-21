package jekpro.tools.bundle;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.tools.foreign.Tracking;
import matula.util.data.ListArray;
import matula.util.wire.FileExtension;

import java.io.IOException;

/**
 * <p>The internal implementation of a SWI package.</p>
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

/**
 * Obtained rights, copyright notice of SWI-Prolog 7.3.33 the
 * plweb-www GitHub package when we adopted SWI-Prolog icon.
 * <p>
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * <p>
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * <p>
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the
 * distribution.
 * <p>
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
final class BranchSWI extends AbstractBranch {
    private static final String PROLOG_DIR = "prolog/";

    /**
     * <p>Retrieve the parameters of this branch.</p>
     *
     * @return The parameters of this brach.
     */
    public String[] getParams() {
        return getArchiveRoots();
    }

    /**
     * <p>Create a branch SWI.</p>
     *
     * @param root The main root.
     */
    BranchSWI(String root) {
        setDescription(new DescriptionSWI(root));
//        setFlags(BaseBundle.MASK_BNDL_NACT);
        setArchiveRoots(new String[]{root});
    }

    /***************************************************************/
    /* Capability Properties                                       */
    /***************************************************************/

    /**
     * <p>Precompute the uris of a root.</p>
     *
     * @param res   The target list.
     * @param root  The root.
     * @param foyer The enforced.
     * @throws IOException Shit happens.
     */
    public void rootToAbsolute(ListArray<String> res, String root, Foyer foyer)
            throws IOException {
        ClassLoader loader = foyer.getRoot().getLoader();
        if (root.equals(getDescription().getMainRoot())) {
            Tracking.rootToAbsoluteCheck(res, root, loader, DescriptionSWI.MODEL_SWI + ".pl");
        } else {
            Tracking.rootToAbsoluteCheck(res, root, loader, "root.propertiesx");
        }
    }

    /**
     * <p>Init the store with this branch.</p>
     *
     * @param en     The engine.
     * @param prompt The prompt flag.
     * @param system The system flag.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final void initBranch(Engine en,
                                 boolean prompt, boolean system)
            throws EngineMessage, EngineException {
        super.initBranch(en, prompt, system);

        Store root = (Store) en.store.foyer.getRoot();
        root.addFileExtension(getDescription().getMainRoot() + PROLOG_DIR,
                new FileExtension(FileExtension.MASK_PCKG_LOAD));
    }

    /**
     * <p>Fini the store from this branch.</p>
     *
     * @param store  The store.
     * @param system The system flag.
     * @throws EngineMessage Shit happens.
     */
    public final void finiBranch(Store store, boolean system)
            throws EngineMessage, EngineException {
        Store root = (Store) store.foyer.getRoot();
        root.removeFileExtension(getDescription().getMainRoot() + PROLOG_DIR);

        super.finiBranch(store, system);
    }

}