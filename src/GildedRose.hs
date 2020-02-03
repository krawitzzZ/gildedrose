module GildedRose
  ( updateQuality
  , Item(..)
  , GildedRose
  )
where

import           Data.List

type GildedRose = [Item]
type LegendaryItemName = String

data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

sellInDelta :: Int
sellInDelta = 1

qualityDelta :: Int
qualityDelta = 1

maxQuality :: Int
maxQuality = 50

sulfuras :: LegendaryItemName
sulfuras = "Sulfuras, Hand of Ragnaros"

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityForItem

updateQualityForItem :: Item -> Item
updateQualityForItem (Item name sellIn quality) = Item name sellIn' quality' where
  sellIn'  = getUpdatedSellIn name sellIn
  quality' = getUpdatedQuality name sellIn quality

getUpdatedSellIn :: String -> Int -> Int
getUpdatedSellIn name sellIn =
  if sulfuras `isInfixOf` name then sellIn else sellIn - sellInDelta

getUpdatedQuality :: String -> Int -> Int -> Int
getUpdatedQuality name sellIn quality
  | sulfuras `isInfixOf` name      = updateLegendaryItemQuality quality
  | conjured `isInfixOf` name      = updateConjuredItemQuality sellIn quality
  | agedBrie `isInfixOf` name      = updateAgedBrieQuality sellIn quality
  | backstagePass `isInfixOf` name = updateBackstagePassesQuality sellIn quality
  | otherwise = updateCommonItemQuality sellIn quality qualityDelta
 where
  agedBrie      = "Aged Brie"
  backstagePass = "Backstage pass"
  conjured      = "Conjured"


updateLegendaryItemQuality :: Int -> Int
updateLegendaryItemQuality quality = quality

updateCommonItemQuality :: Int -> Int -> Int -> Int
updateCommonItemQuality sellIn quality delta =
  max (if sellIn > 0 then quality - delta else quality - (delta * 2)) 0

updateConjuredItemQuality :: Int -> Int -> Int
updateConjuredItemQuality sellIn quality =
  updateCommonItemQuality sellIn quality (qualityDelta * 2)

updateAgedBrieQuality :: Int -> Int -> Int
updateAgedBrieQuality sellIn quality = min
  (if sellIn > 0 then quality + qualityDelta else quality + (qualityDelta * 2))
  maxQuality

updateBackstagePassesQuality :: Int -> Int -> Int
updateBackstagePassesQuality sellIn quality
  | sellIn > 10                = getQuality $ quality + qualityDelta
  | sellIn > 5 && sellIn <= 10 = getQuality $ quality + (qualityDelta * 2)
  | sellIn > 0 && sellIn <= 5  = getQuality $ quality + (qualityDelta * 3)
  | otherwise                  = 0
  where getQuality quality' = min quality' maxQuality
