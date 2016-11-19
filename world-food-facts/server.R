tester = data.frame(n<-c(2,3,4), s<-c(3,4,5), b<-c(5,6,7))
colnames(tester) <- c("one","two","three")

foodFacts <- read.csv("FoodFacts.csv")
foodTypesAmount = nrow(foodFacts)

# I've read that forloops in R are a bad idea. Unfortunately, I don't see 
# any other way to split the multiple values in the additives_tags 
# column into single values (without using Python/Java).

# This is not a user-friendly dataset.

# there are 1421 ENumbers* currently, with some having sub-classes (e321iii).
# I took a margin of 25% for this. 1421 * 1.25 = 1776.25. I need to initialize
# the vectors with a set length to minimize memory usage when parsing the existing
# dataset into a usable format (it's slow enough already ;) ).
# *http://www.e-nummers-lijst.nl/en_US/e-nummers-lijst/
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


function(input, output) {
  
  #read csv test
  #output$readDataset <- read.csv
  
  #treemap test --> WORKS
  #output$treemap <- renderPlot({
   # tm <-  treemap(
    #  tester,
     # index=c("three", "two"),
      #vSize="two",
      #vColor="one",
      #type="value"
    #)
  #})
  
  output$treemap <- renderPlot({
    tm <-  treemap(
      totalsPerAdditive,
      index=c("additiveGoal", "additiveIdentifier"),
      vSize="additiveTotalOccurence",
      vColor="additiveTotalOccurence",
      type="value"
  )
  })
  
  #output$testdf <- renderDataTable(totalsPerAdditive,
  #               options = list (
  #                 pageLength = 20
  #               )
  #            )
}