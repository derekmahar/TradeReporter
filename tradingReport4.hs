data Trade = Buy { unitPrice :: Double, quantity :: Double }

tradingReport :: [Trade] -> [(Double, Double, Double)]
tradingReport xs = scanl tradingReportItem (0,0,0) xs

tradingReportItem :: (Double, Double, Double) -> Trade -> (Double, Double, Double)
tradingReportItem (totalCost, totalQuantity, averageCost) trade =
        let     newTotalCost = totalCost + unitPrice trade * quantity trade
                newTotalQuantity = totalQuantity + quantity trade
        in      (newTotalCost, newTotalQuantity, newTotalCost / newTotalQuantity)
