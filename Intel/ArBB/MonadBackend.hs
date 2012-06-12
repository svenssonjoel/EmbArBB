
module Intel.ArBB.MonadBackend where 

import Control.Monad
import Control.Monad.IO.Class

class (Monad m, MonadIO m) => MonadBackend m 
    -- TODO: figure out what is needed from this one.. 
    --    I suspect some variant of captureFun :: (a -> b) -> FunctionIdentifier 
    --    This is something I hope will enable us to 
    --    fix the "Map issue" in EmbArBB. 
