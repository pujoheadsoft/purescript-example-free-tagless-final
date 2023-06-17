module Example.TaglessFinalWithFree where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct(..))
import Data.Functor.Coproduct.Inject (inj)
import Example.Domain (User, UserOption)

class UserRepository m where
  findUserById :: String -> m User
  findOptionById :: String -> m UserOption

findUserOptionByUserId :: forall m. Monad m => UserRepository m => String -> m UserOption
findUserOptionByUserId userId = do
  user <- findUserById userId
  findOptionById user.optionId

data UserRepositoryF a
  = FindUserById String (User -> a)
  | FindOptionById String (UserOption -> a)

derive instance functorUserRepositoryF :: Functor UserRepositoryF

type FreeUserRepository = Free UserRepositoryF

instance userRepositoryFree :: UserRepository FreeUserRepository where
  findUserById userId = liftF $ FindUserById userId identity
  findOptionById optionId = liftF $ FindOptionById optionId identity


class Presenter m where
  display :: String -> m Unit

displayTrialStatus :: forall m. Monad m => Presenter m => UserOption -> m Unit
displayTrialStatus option = display $ "This User is " <> if option.isTrial then "Trial Account." else "Premium Account."

data PresenterF a = Display String a
derive instance functorPresenterF :: Functor PresenterF
type FreePresenter = Free PresenterF

instance presenterFree :: Presenter FreePresenter where
  display message = liftF $ Display message unit



findUserWithDisplayTrialStatus :: forall m. Monad m => UserRepository m => Presenter m => String -> m Unit
findUserWithDisplayTrialStatus userId = do
  option <- findUserOptionByUserId userId
  displayTrialStatus option


-- compose ---------
type UserRepositoryPresenterF = Coproduct UserRepositoryF PresenterF
type FreeUserRepositoryPresenter = Free UserRepositoryPresenterF

instance dataStoreCompose :: UserRepository FreeUserRepositoryPresenter where
  findUserById id = liftF <<< inj $ FindUserById id identity
  findOptionById id = liftF <<< inj $ FindOptionById id identity

instance keyFinderCompose :: Presenter FreeUserRepositoryPresenter where
  display msg = liftF <<< inj $ Display msg unit

-- util
or :: forall f g h a. (f a -> h a) -> (g a -> h a) -> Coproduct f g a -> h a
or fh gh = case _ of
  (Coproduct (Left left)) -> fh left
  (Coproduct (Right right)) -> gh right
