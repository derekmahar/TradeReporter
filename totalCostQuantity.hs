totalCostQuantity :: Num a => [(a,a)] -> [(a,a)]
totalCostQuantity xs =
        let     trade (totalCost, totalQuantity) (unitPrice, quantity) =
                        let     newTotalCost = totalCost + unitPrice * quantity
                                newTotalQuantity = totalQuantity + quantity
                        in (newTotalCost, newTotalQuantity)
        in      scanl trade (0,0) xs
