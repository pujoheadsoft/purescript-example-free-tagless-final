module Test.Example.TaglessFinalWithFreeSpec where

import Prelude

import Control.Monad.Free (foldFree)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Example.Domain (User, UserOption)
import Example.TaglessFinalWithFree (FreePresenter, FreeUserRepository, FreeUserRepositoryPresenter, PresenterF(..), UserRepositoryF(..), displayTrialStatus, findUserOptionByUserId, findUserWithDisplayTrialStatus, or)
import Test.PMock (any, fun, mock, mockFun, verify, (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type UserRepositoryFunctions = {
  findUserById :: String -> User,
  findOptionById :: String -> UserOption
}

runRepository :: UserRepositoryFunctions -> FreeUserRepository ~> Aff
runRepository f m = foldFree (interpretUserRepository f) m

interpretUserRepository ::  UserRepositoryFunctions -> UserRepositoryF ~> Aff
interpretUserRepository f (FindUserById id next) = pure $ next $ f.findUserById id
interpretUserRepository f (FindOptionById id next) = pure $ next $ f.findOptionById id

type DisplayFunctions = {
  display :: String -> Effect Unit
}

runPresenter :: DisplayFunctions -> FreePresenter ~> Aff
runPresenter f m = foldFree (interpretPresenter f) m

interpretPresenter :: DisplayFunctions -> PresenterF ~> Aff
interpretPresenter f (Display message a) = liftEffect $ f.display message >>= \_ -> pure a

runComposeFunctions :: UserRepositoryFunctions -> DisplayFunctions -> FreeUserRepositoryPresenter ~> Aff
runComposeFunctions f g m = foldFree (interpretUserRepository f `or` interpretPresenter g) m

spec :: Spec Unit
spec = do
  describe "Tagless Final With Free Spec" do
    describe "UserRepositryのテスト" do
      it "指定したIDのユーザーのオプションを取得することができる" do
        let
          functions = {
            findUserById: mockFun $ "userId" :> {id: "userId", name: "userName", optionId: "optionId"},
            findOptionById: mockFun $ "optionId" :> {optionId: "optionId", isTrial: true}
          }
        option <- findUserOptionByUserId "userId"
                  # runRepository functions 
        option `shouldEqual` {optionId: "optionId", isTrial: true}
    
    describe "Presenterのテスト" do
      it "トライアルユーザーかどうかを出力する - トライアルユーザーの場合" do
        let
          displayMock = mock $ any :> (pure unit :: Effect Unit)
        displayTrialStatus {optionId: "optionId", isTrial: true} 
          # runPresenter { display: fun displayMock } 
        verify displayMock "This User is Trial Account."

      it "トライアルユーザーかどうかを出力する - トライアルユーザでない場合" do
        let
          displayMock = mock $ any :> (pure unit :: Effect Unit)
        displayTrialStatus {optionId: "optionId", isTrial: false}
          # runPresenter { display: fun displayMock } 
        verify displayMock "This User is Premium Account."
    
    describe "UserRepository + Presenterのテスト" do
      it "指定したIDのユーザーが、トライアルユーザーかどうかを出力する - トライアルユーザーの場合" do
        let
          displayMock = mock $ any :> (pure unit :: Effect Unit)
          userRepositoryFunctions = {
            findUserById: mockFun $ "userId" :> {id: "userId", name: "userName", optionId: "optionId"},
            findOptionById: mockFun $ "optionId" :> {optionId: "optionId", isTrial: true}
          }
          presenterFunctions = { display: fun displayMock } 
        findUserWithDisplayTrialStatus "userId" 
          # runComposeFunctions userRepositoryFunctions presenterFunctions
        verify displayMock "This User is Trial Account."

      it "指定したIDのユーザーが、トライアルユーザーかどうかを出力する - トライアルユーザでない場合" do
        let
          displayMock = mock $ any :> (pure unit :: Effect Unit)
          userRepositoryFunctions = {
            findUserById: mockFun $ "userId" :> {id: "userId", name: "userName", optionId: "optionId"},
            findOptionById: mockFun $ "optionId" :> {optionId: "optionId", isTrial: false}
          }
          presenterFunctions = { display: fun displayMock } 
        findUserWithDisplayTrialStatus "userId" 
          # runComposeFunctions userRepositoryFunctions presenterFunctions
        verify displayMock "This User is Premium Account."