data Num a => Trade a = Trade { unitPrice :: a, quantity :: a }

reportTrades :: Fractional b => [Trade b] -> [(b, b, b)]
reportTrades xs = scanl reportTradesItem (0,0,0) xs

reportTradesItem :: Fractional a => (a,a,b) -> Trade a -> (a,a,a)
reportTradesItem (totalCost, totalQuantity, averageCost) Trade { unitPrice = unitPrice, quantity = quantity } =
        let     newTotalCost = totalCost + unitPrice * quantity
                newTotalQuantity = totalQuantity + quantity
        in      (newTotalCost, newTotalQuantity, newTotalCost / newTotalQuantity)
