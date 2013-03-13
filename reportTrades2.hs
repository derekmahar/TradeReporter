data Trade = Trade { unitPrice :: Double, quantity :: Double }

reportTrades :: [Trade] -> [(Double,Double,Double)]
reportTrades xs = scanl reportTradesItem (0,0,0) xs

reportTradesItem :: (Double,Double,b) -> Trade -> (Double,Double,Double)
reportTradesItem (totalCost, totalQuantity, averageCost) Trade { unitPrice = unitPrice, quantity = quantity } =
        let     newTotalCost = totalCost + unitPrice * quantity
                newTotalQuantity = totalQuantity + quantity
        in      (newTotalCost, newTotalQuantity, newTotalCost / newTotalQuantity)
