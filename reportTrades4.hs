data Trade = Buy { unitPrice :: Double, quantity :: Double }

reportTrades :: [Trade] -> [(Double, Double, Double)]
reportTrades xs = scanl reportTradesItem (0,0,0) xs

reportTradesItem :: (Double, Double, Double) -> Trade -> (Double, Double, Double)
reportTradesItem (totalCost, totalQuantity, averageCost) trade =
        let     newTotalCost = totalCost + unitPrice trade * quantity trade
                newTotalQuantity = totalQuantity + quantity trade
        in      (newTotalCost, newTotalQuantity, newTotalCost / newTotalQuantity)
