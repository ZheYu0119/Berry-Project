library(shiny)
library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(gridExtra)
rberry <- read.csv("rberry.csv",header = T)
unfood <- read.csv("unfood.csv",header = T)
unfood_1 <- read.csv("unfood_1.csv",header = T)
berries <- read.csv("berries.csv",header = T)
unfood$Year <- as.character(unfood$Year)
unfood_2 <- filter(unfood,Chemical=="(NITROGEN)"|Chemical=="(PHOSPHATE)"|Chemical=="(POTASH)")
ctype1 <- ggplot(unfood_2,mapping=aes(x=Chemical,y=Value))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13, face = "bold")) +
  labs(x = "Fertilizer")
unfood_3 <- filter(unfood,Chemical=="FUNGICIDE"|Chemical=="HERBICIDE"|Chemical=="INSECTICIDE"|Chemical=="OTHER")
ctype2 <- ggplot(unfood_3,mapping=aes(x=Chemical,y=Value))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13, face = "bold")) +
  coord_cartesian(ylim = c(0,2))+
  labs(x = "Agentia")
pc <- grid.arrange(ctype1,ctype2,nrow=1)

df <- unfood_1[,-c(1,2)]
pca <- prcomp(df,center = T,scale. = T)
pca2.1 <- pca
pca2.1$rotation <- -pca2.1$rotation
pca2.1$x <- -pca2.1$x
pt.line <- plot(pca, type="lines")
pt.bi <- biplot(pca2.1, scale = 0)

pc12 <- data.frame(pca2.1$rotation[, 1:2])
pc12$type <- rownames(pc12)
pt1 <- ggplot(pc12, aes(x = PC1, y = PC2))
pt1 <- pt1 + geom_point(size = 3) +
  geom_text(aes(label = type), vjust = 1) +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold"))
set.seed(7)
pc.km <- kmeans(pc12[, 1:2], 3, nstart = 100)
# ggplot clusters
pt.km <- ggplot(pc12, aes(x = PC1, y = PC2))
pt.km <- pt.km + geom_point(aes(colour = factor(pc.km$cluster)), size = 3) +
  scale_colour_manual(values = c("red", "blue", "green")) +
  geom_text(aes(label =type), vjust = 1) +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold")) +
  labs(colour = "Cluster")



###ui part
ui <- fluidPage(
  titlePanel("Raspberry"),
  
  tabsetPanel(
    tabPanel("Data Display",
             sidebarLayout(
               sidebarPanel(
                 helpText("show the data of raspberry,
                           information from USDA."),
                 
                 selectInput("dataset",
                             label = "Choose a dataset to display",
                             choices = c("raw data",
                                         "cleaned data",
                                         "PCA part"),
                             selected = "cleaned data")
                 
               ),
               
               mainPanel(
                 uiOutput("caption3"),
                 DT::dataTableOutput("selectdata"),
                 uiOutput("caption4")
               ) 
               
             )
    ),
    tabPanel("EDA",
             sidebarLayout(
               sidebarPanel(
                 helpText("show the results of EDA of raspberry,
               information from USDA."),
                 
                 selectInput("Var",
                             label = "Choose a plot to display",
                             choices = c("Value~Chemical",
                                         "Value~State",
                                         "Value~Year",
                                         "Pca-line",
                                         "Pca-biplot",
                                         "Pca-scatter plot"),
                             selected = "Chemical")
                 
               ),
               
               mainPanel(
                 uiOutput("caption"),
                 plotOutput("selectVar"),
                 textOutput("caption2")
               ) 
               
             )
    )
  )
  
)




server <- function(input, output){
  
  formulaText <- reactive({
    switch(input$Var,
           "Value~Chemical" = "Value vs Chemical type(agentia and fertilizer)",
           "Value~State" = "Value vs State",
           "Value~Year" = "Value vs Year",
           "Pca-line" = "Proportion of variance explained by each component displayed showing the cumulative proportion of variance",
           "Pca-biplot" = "Biplot representing the values assigned to the variables and data points by the first two principal components",
           "Pca-scatter plot" = "Scatter plot displaying the different types of chemical along with their respective clusters"
    )
  })
  
  plotinput <- reactive({
    switch(input$Var,
           "Value~Chemical" = pc <- grid.arrange(ctype1,ctype2,nrow=1),
           "Value~State" = ggplot(unfood,mapping=aes(x=State,y=Value))+geom_boxplot()+
             theme(axis.text.x = element_text(angle = 60, hjust = 1),
                   axis.text = element_text(size = 10),
                   axis.title = element_text(size = 13, face = "bold")) +
             labs(x = "State")+
             coord_cartesian(ylim = c(0,2))+
             facet_wrap(.~Year,scales = "free"),
           "Value~Year" = ggplot(unfood,mapping=aes(x=Year,y=Value))+geom_boxplot()+
             theme(axis.text.x = element_text(angle = 60, hjust = 1),
                   axis.text = element_text(size = 10),
                   axis.title = element_text(size = 13, face = "bold")) +
             labs(x = "Year")+
             coord_cartesian(ylim = c(0,2))+
             facet_wrap(.~State,scales = "free"),
           "Pca-line" = pt.line <- plot(pca, type="lines"),
           "Pca-biplot" = pt.bi <- biplot(pca2.1, scale = 0),
           "Pca-scatter plot" = pt.km)
  })
  
  expText <- reactive({
    switch(input$Var,
           "Value~Chemical" = "It is clear that the value of raspberry using fertilizer('nitrogen', 'phosphate' and 'potash') is much higher than those using agentia('fungicide', 'herbicide', 'insecticide' and 'other')(becasue 'other' also have a small range so I put it in the agentia).",
           "Value~State" = "Look at 2015, the median of value of Oregon and Washington was much the same, around 0.26, but the value of Washington was closer. In 2017, the median of value in Oregon is almost two times of that in Washington. In 2019, there was no obvious difference between the situation of California and Washington.",
           "Value~Year" = "
The dataset only have one year of value in California so there is no much to discuss. It seems that Oregon improves a lot from 2015 to 2017, for the median of value in 2017 is almost two times of that in 2015. And the value is closer as well. The same with Oregon, Washington also decreas the range of value, but there was not increasing in median, and even the median decreased in each year.",
           "Pca-line" = "The PCA provides 6 components and 90% of the total variance is attributed to the first 4 components. See in plot above.",
           "Pca-biplot" = "And in the biplot I can see the relationship between each variables. The size of the angle between vectors determines the correlation of the variables, which is the desired indicator to achieve the objective for this analysis. A small angle indicates a strong positive correlation, 90 degrees represents no correlation and 180 degrees represents a negative correlation. 

For example, Phosphate and potash is almost coincide; others and herbicide have negative correlation.",
           "Pca-scatter plot" = "In this plot, it is clear that chemical can be separate into to three part, just I mentioned before, apart from 'other', fertilizer('nitrogen', 'phosphate' and 'potash') gathered as cluster1, and agentia('fungicide', 'herbicide', 'insecticide') gathered as cluster3."
    )
  })
  
  datachoose <- reactive({
    switch (input$dataset,
            "raw data" = berries,
            "cleaned data" = rberry,
            "PCA part" = unfood_1)
  })
  
  output$selectdata <- DT::renderDataTable(DT::datatable({
    datachoose()
  }))
  
  dataTextTitle <- reactive({
    switch (input$dataset,
            "raw data" = "uncleaned data with three berries",
            "cleaned data" = "raspberry data only",
            "PCA part" = "data organized for PCA")
    
  })
  
  dataexpText <- reactive({
    switch (input$dataset,
            "raw data" = "The raw data is a mixed data with all the information of blueberry, raspberry and strawberry. But in this project I only want to use raspberry data. And there are some overlap in conlumns <code>Data Item</code>, <code>Domain</code> and <code>Domain Category</code>",
            "cleaned data" = "So it is the data that have been cleaned. There are some new columns \n   
<div><code>Type</code>: Generally a physical attribute of the commodity.</div>     
<div><code>Production</code>: The aspect of a commodity being measured.</div>     
<div><code>Avg</code>: Average. </div>       
<div><code>Measures</code>: The unit associated with the statistic category.</div>     
<div><code>Materials</code>: Categories or partitions within a domain. </div>   
<div><code>Chemical</code>: describes the type of chemical applied to thee commodity.</div>    ",
            "PCA part" = "The data was cleaned up, because the majority of raspberry is for application, so I filter rows of data in the same situation to be prepared for the EDA part. And use <code>pivot_wider</code> to make data match the using of PCA.")
    
  })
  
  output$caption4 <- renderUI({
    HTML(dataexpText())
  })
  
  output$caption3 <- renderUI({
    h4(dataTextTitle())
  })
  
  output$caption <- renderUI({
    h4(formulaText())
  })
  
  output$selectVar <- renderPlot({
    plotinput()
  })
  
  output$caption2 <- renderText({
    expText()
  })
}

shinyApp(ui = ui, server = server)
