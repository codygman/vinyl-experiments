{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction, GADTs, TypeSynonymInstances, TemplateHaskell, StandaloneDeriving #-}

module Main where

import Data.Vinyl
import Control.Lens hiding (Identity)
import Data.Singletons.TH
import Data.Maybe
import Control.Monad
import Data.Vinyl.TypeLevel (RIndex)

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

joy :: Rec Attr ['Id, 'Name, 'Age]
joy = (SId =:: 1)
   :& (SName =:: "Joy")
   :& (SAge =:: 28)
   :& RNil
jon :: Rec Attr ['Id, 'Name, 'Age]
jon = (SId =:: 0)
   :& (SName =:: "Jon")
   :& (SAge =:: 23)
   :& RNil

karen :: Rec Attr ['Id, 'Name, 'Age]
karen = (SId =:: 2)
   :& (SName =:: "Karen")
   :& (SAge =:: 15)
   :& RNil

jonFootball :: Rec Attr ['Id, 'ActivityName]
jonFootball = (SId =:: 0)
           :& (SActivityName =:: "football")
           :& RNil

jonDancing :: Rec Attr ['Id, 'ActivityName]
jonDancing = (SId =:: 0)
           :& (SActivityName =:: "dancing")
           :& RNil

joyRacing :: Rec Attr ['Id, 'ActivityName]
joyRacing = (SId =:: 1)
           :& (SActivityName =:: "racing")
           :& RNil

peopleRows :: [Rec Attr ['Id, 'Name, 'Age]]
peopleRows = [joy, jon, karen]

activitieRows :: [Rec Attr ['Id, 'ActivityName]]
activitieRows = [jonFootball, jonDancing, joyRacing]

printActvy :: ('ActivityName ∈ fields) => Rec Attr fields -> IO ()
printActvy r = print (r ^. rlens SActivityName)

isInPplIdx :: ('Id ∈ fields) => [Int] -> Rec Attr fields -> Bool
isInPplIdx peopleIdx actvyRow =  any (== True) . map (== actvyIdInt) $ peopleIdx
  where actvyIdInt = actvyRow ^. rlens SId . unAttr


mkJoinedRow :: (Eq (ElF r1),
                                RElem
                                  r1
                                  ['Id, 'Name, 'Age]
                                  (RIndex r1 ['Id, 'Name, 'Age]),
                                RElem
                                  r1
                                  ['Id, 'ActivityName]
                                  (RIndex r1 ['Id, 'ActivityName]),
                                ElF r1 ~ Int) => sing1 r1 -> [Rec Attr ['Id, 'ActivityName]] -> Rec Attr ['Id, 'Name, 'Age] ->  [Rec Attr ['Id, 'Name, 'Age, 'ActivityName]]
-- mkJoinedRow :: _ -> [Rec Attr ['Id, 'ActivityName]] -> Rec Attr ['Id, 'Name, 'Age] ->  [Rec Attr ['Id, 'Name, 'Age, 'ActivityName]]
mkJoinedRow field activities person = do
  let name = person ^. rlens SName . unAttr
      age = person ^. rlens SAge . unAttr

  let filteredActivities = filter (\r -> r ^. rlens field . unAttr == person ^. rlens field . unAttr) activities
  case listToMaybe filteredActivities of
    Just _ -> do
      let activityId actvy = actvy ^. rlens field . unAttr
          activityName actvy = actvy ^. rlens SActivityName . unAttr
      (\actvy -> (SId =:: activityId actvy) :& (SName =:: name) :& (SAge =:: age) :& (SActivityName =:: activityName actvy) :& RNil) <$> filteredActivities
    Nothing -> []

innerJoinOnId :: [Rec Attr ['Id, 'Name, 'Age]] -> [Rec Attr ['Id, 'ActivityName]] -> [Rec Attr ['Id, 'Name, 'Age, 'ActivityName]]
innerJoinOnId people activities = do
  let peopleIdx =(\r -> r ^. rlens SId . unAttr) <$> people
  let filteredActivites = filter (isInPplIdx peopleIdx) activities
  join $ map (\p -> mkJoinedRow SId filteredActivites p) people

main :: IO ()
main = mapM_ print $ innerJoinOnId peopleRows activitieRows
