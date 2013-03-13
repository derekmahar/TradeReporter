data Num a => Trade a = Trade { unitPrice :: a, quantity :: a }

tradingReport :: Fractional b => [Trade b] -> [(b, b, b)]
tradingReport xs = scanl tradingReportItem (0,0,0) xs

tradingReportItem :: Fractional a => (a,a,b) -> Trade a -> (a,a,a)
tradingReportItem (totalCost, totalQuantity, averageCost) Trade { unitPrice = unitPrice, quantity = quantity } =
        let     newTotalCost = totalCost + unitPrice * quantity
                newTotalQuantity = totalQuantity + quantity
        in      (newTotalCost, newTotalQuantity, newTotalCost / newTotalQuantity)
