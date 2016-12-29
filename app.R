#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list = ls()) 

library(shiny)
library(dplyr)
library(tidytext)
library(knitr)
library(tidyr)
library(stringr)

pred_txt<-readRDS("pred_txt_RDSbi_small.RData")


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("What is your next word?"), align="center",
  navbarPage("",
             tabPanel("Web-app",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("letras", "Enter your text here:", "Example")),
                        mainPanel(
                          h4('These are your options:'),
                          dataTableOutput('table')
                        )
                      )
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  predict_text=function(text){ 
    # Takes the text, cleans it and determined lenght
    # Depending on the lenght of the input, it filters the data set into 
    # the approppriate data frame and runs the find word function
    # If there is no word found, it repeats the process removing the first word.
    new_txt=tolower(
      sub("\\s*$", "",gsub("[?.;!¡¿·',]", "",
                           gsub("\\s([0-9]+)","",text)))) # Removes last space in the input
    l=sapply(gregexpr("\\w+", new_txt), length)
    if (l>=4){ # for inputs with 3 or more words, take only the last three words
      new_txt = paste(word(new_txt,(l-2):l), collapse=" ") 
      nGram=5
    }
    if (l<=3) {
      nGram=l+1
    }
    df=pred_txt%>%filter(ngram==nGram)
    Predictions=find_word(text, new_txt, df)
    if (length(Predictions)==0 && l==1){
      Predictions="No prediction. What's on your mind?"
    }
    if (length(Predictions)==0 && l!=1){
      second_txt=paste(word(text,2:l), collapse=" ")
      Predictions=predict_text(second_txt)
    }
    if (length(Predictions)>0){
      Predictions=Predictions
    }
    as.data.frame(Predictions)
  }
  find_word = function(texto, new_texto, ngram_df){
    # Finds a match for the input text and returns the word next to the match
    size=sapply(gregexpr("\\w+", new_texto), length)
    pattern = paste("(\\b",new_texto,"\\b)\\s\\w+$", sep="")
    df=ngram_df%>%filter(grepl(pattern, gram))%>%arrange(-n)
    df_1=word(df$gram[1:10],size+1)
    df_1=df_1[!is.na(df_1)]
    return(df_1)
  }
  output$table <- renderDataTable({
    predict_text(input$letras)
  }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

