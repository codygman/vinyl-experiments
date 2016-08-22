{-# LANGUAGE ConstraintKinds, PartialTypeSignatures #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies, FlexibleContexts, FlexibleInstances#-}
{-# LANGUAGE NoMonomorphismRestriction, GADTs, TypeSynonymInstances, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables, DeriveDataTypeable, KindSignatures #-}
module Main where

import Data.Vinyl
import Control.Lens hiding (Identity)
import Data.Singletons.TH
import Data.Maybe
import Control.Monad
import Data.Vinyl.TypeLevel (RIndex)
import Data.Typeable
import GHC.Exts (Constraint)

data Fields = Id | Name | Age | ActivityName deriving Show

type Person = ['Id, 'Name, 'Age]
type Activity = ['Id, 'ActivityName]

type family ElF (f :: Fields) :: * where
  ElF 'Id = Int
  ElF 'Name = String
  ElF 'Age = Int
  ElF 'ActivityName = String

newtype Attr f = Attr { _unAttr :: ElF f }
makeLenses ''Attr
genSingletons [ ''Fields ]
instance Show (Attr 'Id) where show (Attr x) = "id: " ++ show x
instance Show (Attr 'Name) where show (Attr x) = "name: " ++ show x
instance Show (Attr 'Age) where show (Attr x) = "age: " ++ show x
instance Show (Attr 'ActivityName) where show (Attr x) = "activity: " ++ x

(=::) :: sing f -> ElF f -> Attr f
_ =:: x = Attr x

peopleRows :: [Rec Attr ['Id, 'Name, 'Age]]
peopleRows = [ (SId =:: 1)
          :& (SName =:: "Joy")
          :& (SAge =:: 28)
          :& RNil
          , (SId =:: 0)
          :& (SName =:: "Jon")
          :& (SAge =:: 23)
          :& RNil
          , (SId =:: 2)
          :& (SName =:: "Karen")
          :& (SAge =:: 15)
          :& RNil
          ]

activityRows :: [Rec Attr ['Id, 'ActivityName]]
activityRows  = [ (SId =:: 0)
                 :& (SActivityName =:: "football")
                 :& RNil
                 , (SId =:: 0)
                 :& (SActivityName =:: "dancing")
                 :& RNil
                 , (SId =:: 1)
                 :& (SActivityName =:: "racing")
                 :& RNil
                 ]

isInIdx field leftIdx rightRow =  any (== True) . map (== unAttrRightRow) $ leftIdx
  where unAttrRightRow = rightRow ^. rlens field . unAttr


-- exploration of mkJoinedRowHelper below
-- λ> :t (SId =:: 0)
-- (SId =:: 0) :: Attr 'Id
-- λ> :t (:&) (SId =:: 0)
-- (:&) (SId =:: 0) :: Rec Attr rs -> Rec Attr ('Id : rs)
-- λ> :t fmap (:&) [(SId =:: 0)]
-- fmap (:&) [(SId =:: 0)] :: [Rec Attr rs -> Rec Attr ('Id : rs)]
-- λ> :t fmap (:&) [(SId =:: 0)]

mkJoinedRowHelper
  :: (Functor f, RElem r rs (RIndex r rs),
      RElem 'ActivityName rs (RIndex 'ActivityName rs), ElF r ~ Int) =>
     sing r
     -> t
     -> f (Rec Attr rs)
     -> String
     -> Int
     -> f (Rec Attr ['Id, 'Name, 'Age, 'ActivityName])
mkJoinedRowHelper field rightRow filteredLeftRows name age = do
  let activityId actvy = actvy ^. rlens field . unAttr
  let activityName actvy = actvy ^. rlens SActivityName . unAttr
  (\actvy -> do
      (SId =:: activityId actvy)
        :& (SName =:: name)
        :& (SAge =:: age)
        :& (SActivityName =:: activityName actvy)
        :& RNil)
    <$> filteredLeftRows



-- TODO generalize mkJoinedRow if possible or require a typeclass instance of mkJoinedRow
-- TODO maybe we can just append fields or something
mkJoinedRow field leftRows rightRow = do
  let name = rightRow ^. rlens SName . unAttr
      age = rightRow ^. rlens SAge . unAttr

  let filteredLeftRows = filter (\r -> r ^. rlens field . unAttr == rightRow ^. rlens field . unAttr) leftRows
  case listToMaybe filteredLeftRows of
    Just _ -> do
      -- let activityId actvy = actvy ^. rlens field . unAttr
      -- let activityName actvy = actvy ^. rlens SActivityName . unAttr
      -- (\actvy -> do
      --     (SId =:: activityId actvy)
      --     -- (SId =:: actvy ^. rlens field . unAttr)
      --     :& (SName =:: name)
      --     :& (SAge =:: age)
      --     :& (SActivityName =:: activityName actvy)
      --     :& RNil)
      --   <$> filteredLeftRows
      mkJoinedRowHelper field rightRow filteredLeftRows name age
    Nothing -> []

innerJoinOn field people activities = do
  let peopleIdx =(\r -> r ^. rlens field . unAttr) <$> people
  let filteredActivites = filter (isInIdx field peopleIdx) activities
  join $ map (\p -> mkJoinedRow field filteredActivites p) people

-- printActvy :: ('ActivityName ∈ fields) => Rec Attr fields -> IO ()
printActvy
  :: (
      Show (f 'ActivityName),
      'ActivityName ∈ rs
     ) =>
     Rec f rs -> IO ()
printActvy r = print (r ^. rlens SActivityName)

main :: IO ()
main = mapM_ print $ innerJoinOn SId peopleRows activityRows

-- example of main running:
-- λ> peopleRows
-- [{id: 1, name: "Joy", age: 28},{id: 0, name: "Jon", age: 23},{id: 2, name: "Karen", age: 15}]
-- λ> activityRows
-- [{id: 0, activity: football},{id: 0, activity: dancing},{id: 1, activity: racing}]
-- λ> mapM_ print $ innerJoinOn SId peopleRows activityRows
-- {id: 1, name: "Joy", age: 28, activity: racing}
-- {id: 0, name: "Jon", age: 23, activity: football}
-- {id: 0, name: "Jon", age: 23, activity: dancing}
