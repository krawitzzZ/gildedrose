module GildedRoseSpec
  ( spec
  )
where

import           Test.Hspec
import           GildedRose
import           GildedRoseSpecDefinitions


spec :: Spec
spec = do
  describe "updateQuality for all items" $ do
    it "correctly updates `sellIn` and `quality` after 10 days"
      $ iterateTimesAndCheck 10 initialInventory expectedInventoryInTenDays

    it "correctly updates `sellIn` and `quality` after 20 days"
      $ iterateTimesAndCheck 20 initialInventory expectedInventoryInTwentyDays


  describe "updateQuality for `+5 Dexterity Vest`" $ do
    it "correctly updates `sellIn` and `quality` after 20 days"
      $ iterateTimesAndCheck 20
                             [Item "+5 Dexterity Vest" 10 32]
                             [Item "+5 Dexterity Vest" (-10) 2]

    it "decreases `sellIn` and `quality` by 1 after 1 day if `sellIn` > 0"
      $ iterateTimesAndCheck 1
                             [Item "+5 Dexterity Vest" 10 10]
                             [Item "+5 Dexterity Vest" 9 9]

    it "decreases `quality` by 2 if `sellIn` < 0" $ do
      iterateTimesAndCheck 1
                           [Item "+5 Dexterity Vest" 1 10]
                           [Item "+5 Dexterity Vest" 0 9]
      iterateTimesAndCheck 1
                           [Item "+5 Dexterity Vest" 0 9]
                           [Item "+5 Dexterity Vest" (-1) 7]

    it "does not set `quality` less than 0" $ iterateTimesAndCheck
      10
      [Item "+5 Dexterity Vest" 5 5]
      [Item "+5 Dexterity Vest" (-5) 0]


  describe "updateQuality for `Elixir of the Mongoose`" $ do
    it "correctly updates `sellIn` and `quality` after 20 days"
      $ iterateTimesAndCheck 20
                             [Item "Elixir of the Mongoose" 10 32]
                             [Item "Elixir of the Mongoose" (-10) 2]

    it "decreases `sellIn` and `quality` by 1 after 1 day if `sellIn` > 0"
      $ iterateTimesAndCheck 1
                             [Item "Elixir of the Mongoose" 10 10]
                             [Item "Elixir of the Mongoose" 9 9]

    it "decreases `quality` by 2 if `sellIn` < 0" $ do
      iterateTimesAndCheck 1
                           [Item "Elixir of the Mongoose" 1 10]
                           [Item "Elixir of the Mongoose" 0 9]
      iterateTimesAndCheck 1
                           [Item "Elixir of the Mongoose" 0 9]
                           [Item "Elixir of the Mongoose" (-1) 7]

    it "does not set `quality` less than 0" $ iterateTimesAndCheck
      10
      [Item "Elixir of the Mongoose" 5 5]
      [Item "Elixir of the Mongoose" (-5) 0]


  describe "updateQuality for `Aged Brie`" $ do
    it "correctly updates `sellIn` and `quality` after 20 days"
      $ iterateTimesAndCheck 20
                             [Item "Aged Brie" 10 15]
                             [Item "Aged Brie" (-10) 45]

    it
        "decreases `sellIn` and increases `quality` by 1 after 1 day if `sellIn` > 0"
      $ iterateTimesAndCheck 1 [Item "Aged Brie" 10 10] [Item "Aged Brie" 9 11]

    it "increases `quality` by 2 if `sellIn` < 0" $ do
      iterateTimesAndCheck 1 [Item "Aged Brie" 1 10] [Item "Aged Brie" 0 11]
      iterateTimesAndCheck 1 [Item "Aged Brie" 0 11] [Item "Aged Brie" (-1) 13]

    it "does not increase `quality` to more than 50" $ iterateTimesAndCheck
      60
      [Item "Aged Brie" 12 30]
      [Item "Aged Brie" (-48) 50]


  describe "updateQuality for `Sulfuras, Hand of Ragnaros`" $ do
    it "correctly updates `sellIn` and `quality` after 20 days"
      $ iterateTimesAndCheck 20
                             [Item "Sulfuras, Hand of Ragnaros" 10 15]
                             [Item "Sulfuras, Hand of Ragnaros" 10 15]

    it "never changes `sellIn` and `quality`" $ iterateTimesAndCheck
      10
      [Item "Sulfuras, Hand of Ragnaros" 5 10]
      [Item "Sulfuras, Hand of Ragnaros" 5 10]

    it "can have `quality` of 80" $ iterateTimesAndCheck
      10
      [Item "Sulfuras, Hand of Ragnaros" 5 80]
      [Item "Sulfuras, Hand of Ragnaros" 5 80]


  describe "updateQuality for `Backstage passes to a TAFKAL80ETC concert`" $ do
    it "correctly updates `sellIn` and `quality` after 20 days"
      $ iterateTimesAndCheck
          20
          [Item "Backstage passes to a TAFKAL80ETC concert" 10 15]
          [Item "Backstage passes to a TAFKAL80ETC concert" (-10) 0]

    it
        "decreases `sellIn` and increases `quality` by 1 after 1 day if `sellIn` > 10"
      $ do
          iterateTimesAndCheck
            5
            [Item "Backstage passes to a TAFKAL80ETC concert" 20 10]
            [Item "Backstage passes to a TAFKAL80ETC concert" 15 15]
          iterateTimesAndCheck
            1
            [Item "Backstage passes to a TAFKAL80ETC concert" 11 20]
            [Item "Backstage passes to a TAFKAL80ETC concert" 10 21]

    it
        "decreases `sellIn` by 1 and increases `quality` by 2 after 1 day if 10 > `sellIn` > 5"
      $ do
          iterateTimesAndCheck
            3
            [Item "Backstage passes to a TAFKAL80ETC concert" 10 10]
            [Item "Backstage passes to a TAFKAL80ETC concert" 7 16]
          iterateTimesAndCheck
            1
            [Item "Backstage passes to a TAFKAL80ETC concert" 6 10]
            [Item "Backstage passes to a TAFKAL80ETC concert" 5 12]

    it
        "decreases `sellIn` by 1 and increases `quality` by 3 after 1 day if 5 > `sellIn` > 0"
      $ do
          iterateTimesAndCheck
            2
            [Item "Backstage passes to a TAFKAL80ETC concert" 5 10]
            [Item "Backstage passes to a TAFKAL80ETC concert" 3 16]
          iterateTimesAndCheck
            1
            [Item "Backstage passes to a TAFKAL80ETC concert" 1 10]
            [Item "Backstage passes to a TAFKAL80ETC concert" 0 13]

    it "drops `quality` to 0 if `sellIn` < 0" $ do
      iterateTimesAndCheck
        3
        [Item "Backstage passes to a TAFKAL80ETC concert" 2 20]
        [Item "Backstage passes to a TAFKAL80ETC concert" (-1) 0]
      iterateTimesAndCheck
        1
        [Item "Backstage passes to a TAFKAL80ETC concert" 0 20]
        [Item "Backstage passes to a TAFKAL80ETC concert" (-1) 0]

    it "does not increase `quality` to more than 50" $ iterateTimesAndCheck
      30
      [Item "Backstage passes to a TAFKAL80ETC concert" 50 30]
      [Item "Backstage passes to a TAFKAL80ETC concert" 20 50]


  describe "updateQuality for `Conjured Mana Cake`" $ do
    it "correctly updates `sellIn` and `quality` after 20 days"
      $ iterateTimesAndCheck 20
                             [Item "Conjured Mana Cake" 18 50]
                             [Item "Conjured Mana Cake" (-2) 6]

    it "decreases `sellIn` by 1 and `quality` by 2 after 1 day if `sellIn` > 0"
      $ iterateTimesAndCheck 1
                             [Item "Conjured Mana Cake" 10 10]
                             [Item "Conjured Mana Cake" 9 8]

    it "decreases `quality` by 4 if `sellIn` < 0" $ do
      iterateTimesAndCheck 1
                           [Item "Conjured Mana Cake" 1 10]
                           [Item "Conjured Mana Cake" 0 8]
      iterateTimesAndCheck 1
                           [Item "Conjured Mana Cake" 0 8]
                           [Item "Conjured Mana Cake" (-1) 4]

    it "does not set `quality` less than 0" $ iterateTimesAndCheck
      10
      [Item "Conjured Mana Cake" 5 5]
      [Item "Conjured Mana Cake" (-5) 0]
