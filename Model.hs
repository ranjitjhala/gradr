{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Yesod.Auth.Account

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")



instance PersistUserCredentials User where
  userUsernameF       = UserEmailAddress
  userEmailF          = UserEmailAddress
  userPasswordHashF   = UserPassword
  userEmailVerifiedF  = UserVerified
  userEmailVerifyKeyF = UserVerifyKey
  userResetPwdKeyF    = UserResetPasswordKey
  uniqueUsername      = UniqueUser

  userCreate name email key pwd
                      = User name pwd email False key ""
