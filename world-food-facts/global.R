library("shiny")
library("treemap")
library("ggplot2")
library("radarchart")

foodCategories <- c("Unknown", "CerealsAndPotatoes", "Beverages", 
                    "SugarySnacks", "FruitsAndVegetables", 
                    "FatAndSauces", "FishMeatEggs", "CompositeFoods", 
                    "MilkAndDairy", "SaltySnacks")

additiveAdditionReasons <- c("Colorant", "Conservatives",
                             "Thickeners","Acid/Clot/Ferment",
                             "Flavouring", "Sweetening/Cosmetic") 