

{-# LANGUAGE CPP #-}  
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Intel.ArBB.Data where 

import Intel.ArBB.Types as T
import Intel.ArBB.Syntax
-- import qualified Intel.ArbbVM as VM

-- The basic data types supported
import Data.Word 
import Data.Int 
import Intel.ArBB.Data.Int
import Intel.ArBB.Data.Boolean

import qualified Foreign.Storable as S 

