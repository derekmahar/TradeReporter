data Trade = Trade { unitPrice :: Double, quantity :: Double }

tradingReport :: [Trade] -> [(Double,Double,Double)]
tradingReport xs = scanl tradingReportItem (0,0,0) xs

tradingReportItem :: (Double,Double,b) -> Trade -> (Double,Double,Double)
tradingReportItem (totalCost, totalQuantity, averageCost) Trade { unitPrice = unitPrice, quantity = quantity } =
        let     newTotalCost = totalCost + unitPrice * quantity
                newTotalQuantity = totalQuantity + quantity
        in      (newTotalCost, newTotalQuantity, newTotalCost / newTotalQuantity)
