module GildedRoseSpecDefinitions where

import           Test.Hspec
import           GildedRose


iterateTimesAndCheck :: Int -> GildedRose -> GildedRose -> IO ()
iterateTimesAndCheck timesToIterate initial expected =
  actual `shouldBe` expected
  where actual = iterate updateQuality initial !! timesToIterate

initialInventory :: GildedRose
initialInventory =
  [ Item "+5 Dexterity Vest"          10 20
  , Item "Aged Brie"                  2  0
  , Item "Elixir of the Mongoose"     5  7
  , Item "Sulfuras, Hand of Ragnaros" 0  80
  , Item "Backstage passes to a TAFKAL80ETC concert" 15 20
  , Item "Conjured Mana Cake"         13 40
  ]

expectedInventoryInTenDays :: GildedRose
expectedInventoryInTenDays =
  [ Item "+5 Dexterity Vest"          0    10
  , Item "Aged Brie"                  (-8) 18
  , Item "Elixir of the Mongoose"     (-5) 0
  , Item "Sulfuras, Hand of Ragnaros" 0    80
  , Item "Backstage passes to a TAFKAL80ETC concert" 5 35
  , Item "Conjured Mana Cake"         3    20
  ]

expectedInventoryInTwentyDays :: GildedRose
expectedInventoryInTwentyDays =
  [ Item "+5 Dexterity Vest"          (-10) 0
  , Item "Aged Brie"                  (-18) 38
  , Item "Elixir of the Mongoose"     (-15) 0
  , Item "Sulfuras, Hand of Ragnaros" 0     80
  , Item "Backstage passes to a TAFKAL80ETC concert" (-5) 0
  , Item "Conjured Mana Cake"         (-7)  0
  ]
