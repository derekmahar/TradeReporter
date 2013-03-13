reportTrades :: Fractional b => [(b, b)] -> [(b, b, b)]
reportTrades xs = scanl reportTradesItem (0,0,0) xs

reportTradesItem :: Fractional a => (a,a,b) -> (a,a) -> (a,a,a)
reportTradesItem (totalCost, totalQuantity, averageCost) (unitPrice, quantity) =
        let     newTotalCost = totalCost + unitPrice * quantity
                newTotalQuantity = totalQuantity + quantity
        in      (newTotalCost, newTotalQuantity, newTotalCost / newTotalQuantity)
