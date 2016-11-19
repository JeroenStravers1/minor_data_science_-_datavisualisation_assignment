
fluidPage(

  titlePanel("0885941 INF4C Jeroen Stravers Datavisualisatie"),
  
  sidebarLayout(

    sidebarPanel(
      'Answer the following question: "Which additives are applied the most? Make categories 
      of reasons to add additives (improve flavour, mask decay, extend Best Before Date, ...)"',
      
      selectInput(inputId = "selectAdditiveGoal", "Examine which additive types are applied the most to which categories of food:", 
                  choices = additiveAdditionReasons, 
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL)
    ),
    mainPanel(
      plotOutput("additiveGoalSpreadPerFoodCategoryBarChart"),
      plotOutput("additivesPerFoodtypeBarChart"),
      textOutput("treemapTitle"),
      plotOutput("additiveGoalsTreemap"),
      textOutput("displayDisclaimer")
    )
  )
)