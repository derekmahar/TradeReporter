tradingReport :: Fractional b => [(b, b)] -> [(b, b, b)]
tradingReport xs = scanl tradingReportItem (0,0,0) xs

tradingReportItem :: Fractional a => (a,a,b) -> (a,a) -> (a,a,a)
tradingReportItem (totalCost, totalQuantity, averageCost) (unitPrice, quantity) =
        let     newTotalCost = totalCost + unitPrice * quantity
                newTotalQuantity = totalQuantity + quantity
        in      (newTotalCost, newTotalQuantity, newTotalCost / newTotalQuantity)
