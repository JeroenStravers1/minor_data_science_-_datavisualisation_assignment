library("shiny")
library("treemap")

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
additiveTotalOccurence  <- rep(NA, 1777)
additiveGoal            <- rep(NA, 1777)
Unknown                 <- rep(NA, 1777)
CerealPotato            <- rep(NA, 1777)
Beverages               <- rep(NA, 1777)
SugarySnacks            <- rep(NA, 1777)
FruitVegetable          <- rep(NA, 1777)
FatSaucage              <- rep(NA, 1777)
FishMeatEggs            <- rep(NA, 1777)
CompositeFoods          <- rep(NA, 1777)
MilkDairy               <- rep(NA, 1777)
SaltySnacks             <- rep(NA, 1777)

# create the dataframe:
#       total goal  totalPerFoodCategory  
# eNum  x     y     zzz
totalsPerAdditive <- data.frame(additiveTotalOccurence, 
                                additiveGoal, Unknown, CerealPotato,
                                Beverages, SugarySnacks, FruitVegetable,
                                FatSaucage, FishMeatEggs, CompositeFoods,
                                MilkDairy, SaltySnacks)

unwrittenRowInTotalsPerAdditive = 1
# add totals and additive identifiers to the dataframe
for(i in 1:foodTypesAmount) {
  # get additive information
  eNumbersInCurrentFoodType = foodFacts[i, "additives_tags"]
  if(is.na(eNumbersInCurrentFoodType) == FALSE & is.null(eNumbersInCurrentFoodType == FALSE)) {
    splitENumbersInCurrentFoodType = unlist(strsplit(eNumbersInCurrentFoodType, ","))
    currentENumbers = length(splitENumbersInCurrentFoodType)
    currentFoodCategory = foodFacts[i, "pnns_groups_1"]
    
    for(eNumber in 1:currentENumbers) {
      eNumber = sub("en:", "", eNumber)
      
      # if eNumber doesnt have a row -> add row, rowname = enumber
      if(is.na(totalsPerAdditive[eNumber, 1])) {
        totalsPerAdditive[unwrittenRowInTotalsPerAdditive, 1] = 1
        rownames(totalsPerAdditive)[unwrittenRowInTotalsPerAdditive] = eNumber
        unwrittenRowInTotalsPerAdditive = unwrittenRowInTotalsPerAdditive + 1
      }
      
      # do group determination here
      
    }
  }
  # for string in splitString
  #totalsPerAdditive[string] += 1
}


#data <- read.csv(file = "FoodFacts.csv", na.strings =c("", "NA"))

# need a table/df: category (~_pnn_1)
#         E0001   n   "fish meat eggs" "additive_goal"
#         E0002   n

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

}