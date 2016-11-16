
tester = data.frame(n<-c(2,3,4), s<-c(3,4,5), b<-c(5,6,7))
colnames(tester) <- c("one","two","three")


function(input, output) {
  
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