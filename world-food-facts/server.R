library("shiny")
library("treemap")

tester = data.frame(n<-c(2,3,4), s<-c(3,4,5), b<-c(5,6,7))
colnames(tester) <- c("one","two","three")

foodFacts <- read.csv("FoodFacts.csv")

# I've read that foRloops are a bad idea. Unfortunately, I don't see 
# any other way to split the multiple values in the additives_tags 
# column into single (usable!) values.
# This is not a user-friendly dataset.
additiveIdentifiers <- c()
additiveTotalOccurences <- c()
totalsPerAdditive <- data.frame(additiveIdentifiers, additiveTotalOccurences)
rowsAmount = nrow(foodFacts)

# add totals and additive identifiers to thedataframe
for(i in 1:rowsAmount) {
  
}


#data <- read.csv(file = "FoodFacts.csv", na.strings =c("", "NA"))

# need a table/df: category (~_pnn_1)
#         E0001   n
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