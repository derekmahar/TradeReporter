data Trade = Buy {unitPrice :: Double, quantity :: Double} deriving (Show)
data Summary = Summary {totalCost :: Double, totalQuantity :: Double, averageCost :: Double} deriving (Show)

reportTrades :: [Trade] -> [Summary]
reportTrades xs = scanl reportTradesItem Summary {totalCost = 0, totalQuantity = 0, averageCost = 0} xs

reportTradesItem :: Summary -> Trade -> Summary 
reportTradesItem summary trade =
        let     newTotalCost = totalCost summary + unitPrice trade * quantity trade
                newTotalQuantity = totalQuantity summary + quantity trade
        in      Summary {totalCost = newTotalCost, totalQuantity = newTotalQuantity, averageCost = newTotalCost / newTotalQuantity}
