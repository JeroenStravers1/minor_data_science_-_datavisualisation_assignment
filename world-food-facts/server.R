
foodFacts <- read.csv("FoodFacts.csv")
foodTypesAmount = nrow(foodFacts)

# I've read that for-loops in R are a bad idea. Unfortunately, I don't see 
# any other way to split the multiple values in the additives_tags 
# column into single values (without using Python/Java; I wanted to limit myself to R. Using
# another programming language to preprocess the dataset would give me an even more unfair advantage
# over my non-computer science-studying fellow students than my prior experience in programming 
# already does).

# That said, this is not a user-friendly dataset.

# there are 1421 ENumbers* currently, with some having sub-classes (e321iii).
# I took a margin of 25% to compensate for this. 1421 * 1.25 = 1776.25. I need to initialize
# the vectors with a set length to minimize memory usage when parsing the existing
# dataset into a usable format (it's slow enough already!).
# *http://www.e-nummers-lijst.nl/en_US/e-nummers-lijst/ (1521 - 100)
additiveTotalOccurence  <- rep(0, 1777)
additiveGoal            <- rep(NA, 1777)
additiveIdentifier      <- rep(NA, 1777)
Unknown                 <- rep(0, 1777)
CerealsAndPotatoes      <- rep(0, 1777)
Beverages               <- rep(0, 1777)
SugarySnacks            <- rep(0, 1777)
FruitsAndVegetables     <- rep(0, 1777)
FatAndSauces            <- rep(0, 1777)
FishMeatEggs            <- rep(0, 1777)
CompositeFoods          <- rep(0, 1777)
MilkAndDairy            <- rep(0, 1777)
SaltySnacks             <- rep(0, 1777)
additiveRowNumber       <- rep(0, 1777)

totalsPerAdditive <- data.frame(additiveIdentifier, additiveTotalOccurence,
                                additiveGoal, Unknown, CerealsAndPotatoes,
                                Beverages, SugarySnacks, FruitsAndVegetables,
                                FatAndSauces, FishMeatEggs, CompositeFoods,
                                MilkAndDairy, SaltySnacks)
totalsLookupTable <- data.frame(additiveRowNumber) 
#* R mistakes totalsPerAdditive["e170", "FatSauce"] <- totalsPerAdditive["e170", "FatSauce"] + 1 as me trying to set a rowname as "e170", throwing
# an error. The lookup table circumvents this problem.

unwrittenRowInTotalsPerAdditive = 1

# add totals and additive identifiers to the dataframe
for(i in 1:foodTypesAmount) {
  
  # get additives in current food product as vector
  eNumbersInCurrentFoodType <- toString(foodFacts[i, "additives_tags"])
                
  if(is.na(eNumbersInCurrentFoodType) == FALSE & is.null(eNumbersInCurrentFoodType) == FALSE & (eNumbersInCurrentFoodType != "") == TRUE) {
    splitENumbersInCurrentFoodType = unlist(strsplit(eNumbersInCurrentFoodType, ","))
    additivesInCurrentProduct = length(splitENumbersInCurrentFoodType)
    
    # determine category of current food product, parse to value matching
    # dataframe column names
    spacelessCurrentFoodCategory = gsub(" ", "", foodFacts[i, "pnns_groups_1"])
    currentFoodCategory = gsub("-", "", spacelessCurrentFoodCategory)
    switch(currentFoodCategory,
           NULL                  = {currentFoodCategory <- "Unknown"},
           unknown               = {currentFoodCategory <- "Unknown"},
           Cerealsandpotatoes    = {currentFoodCategory <- "CerealsAndPotatoes"},            
           cerealsandpotatoes    = {currentFoodCategory <- "CerealsAndPotatoes"}, 
           Beverages             = {currentFoodCategory <- "Beverages"}, 
           Sugarysnacks          = {currentFoodCategory <- "SugarySnacks"}, 
           sugarysnacks          = {currentFoodCategory <- "SugarySnacks"}, 
           Fruitsandvegetables   = {currentFoodCategory <- "FruitsAndVegetables"}, 
           fruitsandvegetables   = {currentFoodCategory <- "FruitsAndVegetables"}, 
           Fatandsauces          = {currentFoodCategory <- "FatAndSauces"}, 
           FishMeatEggs          = {currentFoodCategory <- "FishMeatEggs"}, 
           Compositefoods        = {currentFoodCategory <- "CompositeFoods"}, 
           Milkanddairyproducts  = {currentFoodCategory <- "MilkAndDairy"}, 
           Saltysnacks           = {currentFoodCategory <- "SaltySnacks"}, 
           saltysnacks           = {currentFoodCategory <- "SaltySnacks"}
    )

    # loop through all additives found in this product
    for(rawENumber in 1:additivesInCurrentProduct) {
      eNumber = sub("en:", "", splitENumbersInCurrentFoodType[rawENumber])
      
      # if(!eNumber has own row): edit current 'empty' row, rowname = enumber for lookup
      if(is.na(totalsLookupTable[eNumber, 1])) {
        rownames(totalsLookupTable)[unwrittenRowInTotalsPerAdditive] <- eNumber
        totalsLookupTable[unwrittenRowInTotalsPerAdditive, 1] <- unwrittenRowInTotalsPerAdditive
        totalsPerAdditive[unwrittenRowInTotalsPerAdditive, "additiveIdentifier"] <- eNumber
        # determine and append goal* of additive addition
        # *based on *http://www.e-nummers-lijst.nl/en_US/e-nummers-lijst/
        numericAdditiveCode = sub("e", "", eNumber)
        eNumConsistsOfDigits = grepl('^[0-9]+$', numericAdditiveCode)
        
        # remove additions (letters a-e, v, i; 345v, 653d)
        if(eNumConsistsOfDigits == FALSE) {
          numericAdditiveCode = gsub('[a-z]+', '', numericAdditiveCode)
          # account for '14xx' additive code
          if(numericAdditiveCode == "14") {
            numericAdditiveCode = "1400"
          }
        }
        
        # interpret additive goal based on its numeric code
        numericAdditiveCode = as.integer(numericAdditiveCode)
        currentAdditiveGoal = ""
        
        if(numericAdditiveCode < 200) {
          currentAdditiveGoal = "Colorant"
        } else if (numericAdditiveCode < 400) { #*
          currentAdditiveGoal = "Conservatives"
        } else if (numericAdditiveCode < 500) {
          currentAdditiveGoal = "Thickeners"
        } else if (numericAdditiveCode < 586) {
          currentAdditiveGoal = "Acid/Clot/Ferment"
        } else if (numericAdditiveCode < 700) {
          currentAdditiveGoal = "Flavouring"
        } else {
          currentAdditiveGoal = "Sweetening/Cosmetic"
        }
        # * I merged antioxidants with conservatives/acids, they serve
        # the same purpose (preservation)
        
        totalsPerAdditive[unwrittenRowInTotalsPerAdditive, "additiveGoal"] <- currentAdditiveGoal
        unwrittenRowInTotalsPerAdditive <- unwrittenRowInTotalsPerAdditive + 1
      }
      
      # increment additive (total and foodcategory-specific totals)
      currentAdditiveRow <- totalsLookupTable[eNumber, 1]
      totalsPerAdditive[currentAdditiveRow, "additiveTotalOccurence"] <- totalsPerAdditive[currentAdditiveRow, "additiveTotalOccurence"] + 1
      totalsPerAdditive[currentAdditiveRow, currentFoodCategory] = totalsPerAdditive[currentAdditiveRow, currentFoodCategory] + 1
    }
  }
}

# get only rows that contain data (prevent empty rows from showing up in plots)
filteredFoodTotals <- subset(totalsPerAdditive, additiveIdentifier != "")


function(input, output) {
  
  output$treemapTitle <- renderText({
    "Distribution of additive use clustered by goal of additive use"
  })
  
  output$additiveGoalsTreemap <- renderPlot({
    
    treemap(
      filteredFoodTotals,
      index=c("additiveGoal", "additiveIdentifier"),
      vSize="additiveTotalOccurence",
      vColor="additiveTotalOccurence",
      type="value"
    )
  })
  
  
  output$additivesPerFoodtypeBarChart <- renderPlot({
    
    freqUnknown               <- sum(filteredFoodTotals$Unknown)
    freqCerealsAndPotatoes    <- sum(filteredFoodTotals$CerealsAndPotatoes)
    freqBeverages             <- sum(filteredFoodTotals$Beverages)
    freqSugarySnacks          <- sum(filteredFoodTotals$SugarySnacks)
    freqFruitsAndVegetables   <- sum(filteredFoodTotals$FruitsAndVegetables)
    freqFatAndSauces          <- sum(filteredFoodTotals$FatAndSauces)
    freqFishMeatEggs          <- sum(filteredFoodTotals$FishMeatEggs)
    freqCompositeFoods        <- sum(filteredFoodTotals$CompositeFoods)
    freqMilkAndDairy          <- sum(filteredFoodTotals$MilkAndDairy)
    freqSaltySnacks           <- sum(filteredFoodTotals$SaltySnacks)
    
    frequencyPerCategory <- c(freqUnknown, freqCerealsAndPotatoes, freqBeverages, 
                              freqSugarySnacks, freqFruitsAndVegetables, freqFatAndSauces, 
                              freqFishMeatEggs, freqCompositeFoods, freqMilkAndDairy,
                              freqSaltySnacks)
    
    additivesFoodCategories <- data.frame(foodCategories, frequencyPerCategory)
    
    ggplot(additivesFoodCategories, aes(x = foodCategories, y = frequencyPerCategory)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = sprintf("%.2f%%", frequencyPerCategory/sum(frequencyPerCategory) * 100)), 
                vjust = -.5) + 
      ggtitle("Relative and absolute distribution of total\nnumber of added additives per food category") + 
      theme(plot.title = element_text(lineheight=.8, face="bold"))
  })
  
  
  output$additiveGoalSpreadPerFoodCategoryBarChart <- renderPlot({
    
    goalsPerChosenFoodCategory <- subset(filteredFoodTotals, filteredFoodTotals$additiveGoal == input$selectAdditiveGoal)
    
    goalUnknown               <- sum(goalsPerChosenFoodCategory$Unknown)
    goalCerealsAndPotatoes    <- sum(goalsPerChosenFoodCategory$CerealsAndPotatoes)
    goalBeverages             <- sum(goalsPerChosenFoodCategory$Beverages)
    goalSugarySnacks          <- sum(goalsPerChosenFoodCategory$SugarySnacks)
    goalFruitsAndVegetables   <- sum(goalsPerChosenFoodCategory$FruitsAndVegetables)
    goalFatAndSauces          <- sum(goalsPerChosenFoodCategory$FatAndSauces)
    goalFishMeatEggs          <- sum(goalsPerChosenFoodCategory$FishMeatEggs)
    goalCompositeFoods        <- sum(goalsPerChosenFoodCategory$CompositeFoods)
    goalMilkAndDairy          <- sum(goalsPerChosenFoodCategory$MilkAndDairy)
    goalSaltySnacks           <- sum(goalsPerChosenFoodCategory$SaltySnacks)
    
    summedGoalFoodCategories <- c(goalUnknown, goalCerealsAndPotatoes, goalBeverages,
                                  goalSugarySnacks, goalFruitsAndVegetables, goalFatAndSauces,
                                  goalFishMeatEggs, goalCompositeFoods, goalMilkAndDairy,
                                  goalSaltySnacks)
    
    relativeGoalFoodCategories <- prop.table(summedGoalFoodCategories)

    barplot(relativeGoalFoodCategories, names.arg=foodCategories, 
            cex.names=0.75, las=1, xlab="categories of food", ylab="percentages",
            main="Relative distribution of additives by goal of addition over categories of food",
            col=c("antiquewhite1","aquamarine2","coral1","chartreuse3","cadetblue1","chocolate1",
                  "darkgoldenrod1","blueviolet","darkseagreen2","brown3"))
  })
  
  
  output$displayDisclaimer <- renderText({
    "***all graphs are based on the World Food Fact dataset from Kaggle, retrieved on 19 november 2016
    from https://www.kaggle.com/openfoodfacts/world-food-facts"
  })
}