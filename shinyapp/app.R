library(shiny)
library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(gridExtra)
rberry <- read.csv("rberry.csv",header = T)
unfood <- read.csv("unfood.csv",header = T)
unfood_1 <- read.csv("unfood_1.csv",header = T)
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
      textOutput("caption"),
      plotOutput("selectVar")
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
  
  output$caption <- renderText({
    formulaText()
  })
  
  output$selectVar <- renderPlot({
    plotinput()
  })
}

shinyApp(ui = ui, server = server)
