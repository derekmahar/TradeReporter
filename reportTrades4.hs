data Trade = Buy {
	quantity :: Double,
	unitPrice :: Double
} | Sell {
	quantity :: Double,
	unitPrice :: Double
} deriving (Show)

data Summary = Summary {
	totalQuantity :: Double,
	totalCost :: Double,
	totalCostOfGoodsSold :: Double,
	totalSales :: Double,
	totalNetIncome :: Double,
	averageUnitCost :: Double
} deriving (Show)

reportTrades :: [Trade] -> [Summary]
reportTrades = scanl reportTradesItem Summary {totalQuantity = 0, totalCost = 0, totalCostOfGoodsSold = 0, totalSales = 0, totalNetIncome = 0, averageUnitCost = 0}

reportTradesItem :: Summary -> Trade -> Summary 
reportTradesItem summary ( Buy quantity unitPrice ) =
	let	newTotalQuantity = totalQuantity summary + quantity
		newTotalCost = totalCost summary + unitPrice * quantity
		newTotalCostOfGoodsSold = totalCostOfGoodsSold summary
		newTotalSales = totalSales summary
		newTotalNetIncome = newTotalSales - newTotalCostOfGoodsSold 
		newAverageUnitCost = newTotalCost / newTotalQuantity
	in	Summary {totalQuantity = newTotalQuantity, totalCost = newTotalCost, totalCostOfGoodsSold = newTotalCostOfGoodsSold, totalSales = totalSales summary, totalNetIncome = newTotalNetIncome, averageUnitCost = newAverageUnitCost}
reportTradesItem summary ( Sell quantity unitPrice ) =
	let	newTotalQuantity = totalQuantity summary - quantity
		newTotalCost = if newTotalQuantity > 0 then totalCost summary - averageUnitCost summary * quantity else 0
		newTotalCostOfGoodsSold = totalCostOfGoodsSold summary + averageUnitCost summary * quantity
		newTotalSales = totalSales summary + unitPrice * quantity
		newTotalNetIncome = newTotalSales - newTotalCostOfGoodsSold 
		newAverageUnitCost = if newTotalQuantity > 0 then newTotalCost / newTotalQuantity else 0 
	in	Summary {totalQuantity = newTotalQuantity, totalCost = newTotalCost, totalCostOfGoodsSold = newTotalCostOfGoodsSold, totalSales = newTotalSales, totalNetIncome = newTotalNetIncome, averageUnitCost = newAverageUnitCost}
