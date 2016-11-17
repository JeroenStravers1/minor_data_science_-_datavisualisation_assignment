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
Unknown                 <- rep(0, 1777)
CerealPotato            <- rep(0, 1777)
Beverage                <- rep(0, 1777)
SugarySnacks            <- rep(0, 1777)
FruitVegetable          <- rep(0, 1777)
FatSauce                <- rep(0, 1777)
FishMeatEggs            <- rep(0, 1777)
CompositeFoods          <- rep(0, 1777)
MilkDairy               <- rep(0, 1777)
SaltySnacks             <- rep(0, 1777)

totalsPerAdditive <- data.frame(additiveTotalOccurence, 
                                additiveGoal, Unknown, CerealPotato,
                                Beverage, SugarySnacks, FruitVegetable,
                                FatSauce, FishMeatEggs, CompositeFoods,
                                MilkDairy, SaltySnacks)

unwrittenRowInTotalsPerAdditive = 1
# add totals and additive identifiers to the dataframe
for(i in 1:foodTypesAmount) {
  # get additives in current food product as vector
  eNumbersInCurrentFoodType = foodFacts[i, "additives_tags"]
  if(is.na(eNumbersInCurrentFoodType) == FALSE & is.null(eNumbersInCurrentFoodType == FALSE)) {
    splitENumbersInCurrentFoodType = unlist(strsplit(eNumbersInCurrentFoodType, ","))
    additivesInCurrentProduct = length(splitENumbersInCurrentFoodType)
    
    # determine category of current food product, parse to value matching
    # dataframe column names
    currentFoodCategory = sub(" ", "", foodFacts[i, "pnns_groups_1"])
    currentFoodCategory = sub("-", "", currentFoodCategory)
    switch(currentFoodCategory,
           NULL                  = {currentFoodCategory = "Unknown"},
           unknown               = {currentFoodCategory = "Unknown"},
           Cerealsandpotatoes    = {currentFoodCategory = "CerealPotato"},            
           cerealsandpotatoes    = {currentFoodCategory = "CerealPotato"}, 
           Beverages             = {currentFoodCategory = "Beverage"}, 
           Sugarysnacks          = {currentFoodCategory = "SugarySnacks"}, 
           sugarysnacks          = {currentFoodCategory = "SugarySnacks"}, 
           Fruitsandvegetables   = {currentFoodCategory = "FruitVegetable"}, 
           fruitsandvegetables   = {currentFoodCategory = "FruitVegetable"}, 
           Fatandsauces          = {currentFoodCategory = "FatSauce"}, 
           FishMeatEggs          = {currentFoodCategory = "FishMeatEggs"}, 
           Compositefoods        = {currentFoodCategory = "CompositeFoods"}, 
           Milkanddairyproducts  = {currentFoodCategory = "MilkDairy"}, 
           Saltysnacks           = {currentFoodCategory = "SaltySnacks"}, 
           saltysnacks           = {currentFoodCategory = "SaltySnacks"}
    )

    # loop through all additives found in this product
    for(eNumber in 1:additivesInCurrentProduct) {
      eNumber = sub("en:", "", eNumber)
      
      # if(!eNumber has own row): edit current 'empty' row, rowname = enumber
      if(is.na(totalsPerAdditive[eNumber, 1])) {
        rownames(totalsPerAdditive)[unwrittenRowInTotalsPerAdditive] = eNumber
        unwrittenRowInTotalsPerAdditive = unwrittenRowInTotalsPerAdditive + 1
      }
      
      # increment times additive found, total and per food category
      totalsPerAdditive[eNumber, "additiveTotalOccurence"] = 
        totalsPerAdditive[eNumber, "additiveTotalOccurence"] + 1
      totalsPerAdditive[eNumber, currentFoodCategory] = 
        totalsPerAdditive[eNumber, currentFoodCategory] + 1
      
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
        currentAdditiveGoal = "Acidity/Clotting/Fermentation"
      } else if (numericAdditiveCode < 700) {
        currentAdditiveGoal = "Flavouring"
      } else {
        currentAdditiveGoal = "Sweetening/Cosmetic"
      }
      # * I merged antioxidants with conservatives/acids, they serve
      # the same purpose (preservation)
    }
  }
}



function(input, output) {
  
  #read csv test
  #output$readDataset <- read.csv
  
  #treemap test --> WORKS
  output$treemap <- renderPlot({
    tm <-  treemap(
      tester,
      index=c("three", "two"),
      vSize="two",
      vColor="one",
      type="value"
    )
  })
  
  #output$treemap <- renderPlot({
   # tm <-  treemap(
    #  totalsPerAdditive,
     # index=c("three", "two"),
      #vSize="two",
      #vColor="one",
      #type="value"
  #)
  #})

}