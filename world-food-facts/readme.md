# minor_data_science_-_datavisualisation_assignment

An R Shiny app using the world food fact dataset from Kaggle. Make sure the dataset is in the same directory as the R files.

--Requires: Shiny d3treeR

 - devtools::install_github("gluc/data.tree")
 - devtools::install_github("timelyportfolio/d3treeR")
 - manually install required packages using Rstudio Install Packages option

# My assignment:

Answer the following question: "Which additives are applied the most? Make categories of reasons to add additives (improve flavour, mask decay, extend Best Before Date, ...)"

I answered this question with a Treemap, I based the additive categories on information found online*. I added barplots to allow the user to further explore the usage of additives in different food categories.
 
*http://www.e-nummers-lijst.nl/en_US/e-nummers-lijst/ (also available in english)
