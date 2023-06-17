module Main where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF)
import Effect (Effect)
import Effect.Console (log)





-- instance userRepositoryFree :: UserRepository FreeUserRepository where
--   findUserById userId = liftF $ FindUserById userId identity
--   findOptionById optionId = liftF $ FindOption optionId identity

-- type UserRepositoryFunctions = {
--   findUserById :: String -> User,
--   findOptionById :: String -> UserOption
-- }

-- runFreeDataStoreFunctions :: UserRepositoryFunctions -> FreeUserRepository ~> Effect
-- runFreeDataStoreFunctions f m = foldFree (goDataStoreFunctions f) m

-- goDataStoreFunctions :: UserRepositoryFunctions -> UserRepositoryF ~> Effect
-- goDataStoreFunctions f (FindUserById k next) = pure $ next $ f.findUserById k
-- goDataStoreFunctions f (FindOption k next) = pure $ next $ f.findOptionById k



main :: Effect Unit
main = do
  log "🍝"
