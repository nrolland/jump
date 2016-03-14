{-# LANGUAGE NoImplicitPrelude #-}
-- | The primary module to be imported by end users. This is an alternate
-- prelude providing many conflicting names with the standard @Prelude@, and as
-- such, you should enable the @NoImplicitPrelude@ language pragma, e.g.:
--
-- @
-- {-\# LANGUAGE NoImplicitPrelude   #-}
-- module MyEndUserCode where
--
-- import Jump
-- @
--
-- == Recommended language extensions
--
-- There are a number of commonly used language extensions that work well with
-- Jump, and are generally recommended to be turned on unless there's a strong
-- reason to do otherwise. These are:
--
-- * @NoImplicitPrelude@
-- * @OverloadedStrings@
-- * @ScopedTypeVariables@
-- * @RankNTypes@
-- * @GADTs@
-- * @TypeFamilies@
module Jump
    ( module Start

    , module Jump.IO
    , module Jump.Lens
    , module Jump.Trans
    , module Jump.Vector
    ) where

import Start

import Jump.IO
import Jump.Lens
import Jump.Trans
import Jump.Vector
