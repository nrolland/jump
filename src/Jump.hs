{-# LANGUAGE NoImplicitPrelude #-}
-- | The primary module to be imported by end users. This is an alternate
-- prelude providing many conflicting names with the standard @Prelude@, and as
-- such, you should enable the @NoImplicitPrelude@ language pragma, e.g.:
--
-- @
-- {-# LANGUAGE NoImplicitPrelude   #-}
-- {-# LANGUAGE OverloadedString    #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- module MyEndUserCode where
--
-- import Jump
-- @
module Jump
    ( module Start
    ) where

import Start
