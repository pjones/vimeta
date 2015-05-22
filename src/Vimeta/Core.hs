{-

This file is part of the vimeta package. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/vimeta/LICENSE. No part of the
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Vimeta.Core (module Export) where

--------------------------------------------------------------------------------
import Vimeta.Core.Config      as Export
import Vimeta.Core.Download    as Export
import Vimeta.Core.Format      as Export
import Vimeta.Core.MappingFile as Export hiding (Parser)
import Vimeta.Core.Process     as Export
import Vimeta.Core.Tagger      as Export

--------------------------------------------------------------------------------
-- Only export the public interface from the Vimeta module.
import Vimeta.Core.Vimeta as Export (Vimeta)
import Vimeta.Core.Vimeta as Export hiding (Vimeta)
