library(shiny)
library(shinyWidgets)
library(plotly)
library(ROCit)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Methylation risk score for COVID-19"),

    sidebarLayout(
        
        # Sidebar for input
        sidebarPanel(
            p("Input data must follow this template:"),
            downloadButton(outputId = "dataTemplate",
                           label = "Download template"),
            p(""),
            fileInput(inputId = "trainUpload", 
                      label = "Upload training set here",
                      multiple = F),
            fileInput(inputId = "testUpload", 
                      label = "Upload test set here",
                      multiple = F),
            uiOutput("selectCpGs"),
            awesomeCheckboxGroup(
                inputId = "selectPc",
                label = "Which PC will be used (could be multiple-choice)", 
                choices = c("PC1", "PC2", "PC3", "PC4"),
                selected = "PC1",
                inline = TRUE,
                status = "success"
            ),
            fluidRow(column(12, 
                        actionButton("submitTrain", "Train model"),
                        align = "center")
            )
        ),

        # Main panel for output
        mainPanel(
            fluidRow(
                splitLayout(
                    cellWidths = c("55%", "45%"),
                    plotOutput("roc"),
                    plotOutput("rainCloud")
                ),
                verbatimTextOutput("auc")
            )
        )
    )
))
