{-# LANGUAGE CPP #-}
module Import.NoFoundation
    ( module Import
    , renderForm
    , submitButton
    ) where

import ClassyPrelude.Yesod   as Import
import Model                 as Import
import Settings              as Import
import Settings.StaticFiles  as Import
import Yesod.Auth            as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import

import Yesod.Form.Bootstrap3

renderForm :: Monad m => FormRender m a
renderForm = renderBootstrap3 dims
  where
    dims   = BootstrapHorizontalForm (ColSm 0)
                                     (ColSm 4)
                                     (ColSm 0)
                                     (ColSm 6)

submitButton :: MonadHandler m => Text -> AForm m ()
submitButton name = bootstrapSubmit
                    $ BootstrapSubmit
                        (name :: Text)
                        "btn-default"
                        [("attribute-name","attribute-value")]
