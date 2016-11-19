
fluidPage(
  sliderInput(inputId = "num",
  label = "Choose a number",
  value = 25, min = 1, max = 100),

  plotOutput("treemap")
  #,getwd()
  
  #fluidRow(
  #  column(12,
  #         dataTableOutput('testdf')
  #  )
  #)
)

# start with a treemap displaying E-numbers - categories
# pnn_groups_1 has 9 unique usable dimensions, use radar plot to visualize number of additives/food category