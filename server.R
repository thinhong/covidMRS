library(shiny)
library(shinyWidgets)
library(ggplot2)
library(ggdist)
library(gghalves)
library(ggpubr)
library(ROCit)

shinyServer(function(input, output) {
    
    output$dataTemplate <- downloadHandler(
        filename = "dataTemplate.tsv",
        content = function(file) {
            file.copy("www/dataTemplate.tsv", file)
        }
    )
    
    trainUpload <- reactive({
        req(input$trainUpload)
        read.table(file = input$trainUpload$datapath, sep = "\t",
                   header = T, stringsAsFactors = F, check.names = F, fill = T)
    })
    
    testUpload <- reactive({
        req(input$testUpload)
        read.table(file = input$testUpload$datapath, sep = "\t", 
                   header = T, stringsAsFactors = F, check.names = F, fill = T)
    })
    
    output$selectCpGs <- renderUI({
        # Get CpGs names from trainUpload
        req(input$trainUpload)
        cpgs <- colnames(trainUpload()[,grep("sample|class", 
                                             colnames(trainUpload()), 
                                             invert = T)])
        pickerInput(
            inputId = "selectCpGs",
            label = "Select CpGs to train", 
            choices = cpgs,
            options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE), 
            multiple = TRUE
        )
    })
    
    pcaTrain <- eventReactive(input$submitTrain, {
        req(trainUpload())
        pcaTrain <- prcomp(trainUpload()[,input$selectCpGs], scale. = TRUE)
        pcaTrain <- data.frame(pcaTrain$x)
        pcaTrain <- cbind(pcaTrain, class = factor(trainUpload()$class))
        pcaTrain
    })
    
    pcaTest <- reactive({
        req(testUpload())
        pcaTest <- prcomp(testUpload()[,input$selectCpGs], scale. = TRUE)
        pcaTest <- data.frame(pcaTest$x)
        pcaTest <- cbind(pcaTest, class = factor(testUpload()$class))
        pcaTest
    })
    
    df <- eventReactive(input$submitTrain, {
        req(pcaTrain())
        req(pcaTest())
        pcaTest <- pcaTest()
        logis1 <- glm(as.formula(paste0("class ~ ", paste(input$selectPc, collapse = " + "))), 
                      data = pcaTrain(), family = "binomial")
        pcaTest$score <- predict(logis1, newdata = pcaTest)
        logis2 <- glm(class ~ score, data = pcaTest, family = "binomial")
        pcaTest$MRS <- predict(logis2, newdata = pcaTest, type ="response")
        pcaTest
    })
    
    output$roc <- renderPlot({
        req(df())
        plot(rocit(score = df()$MRS, class = df()$class))
    })
    
    output$rainCloud <- renderPlot({
        req(df())
        ggplot(df(), aes(x = class, y = MRS, fill = class)) +
            geom_boxplot(width = 0.15, lwd = 1, outlier.color = NA) + theme_classic() +
            geom_signif(comparisons = list(c("Severe", "Mild")),textsize = 5, test = wilcox.test) +
            ggdist::stat_halfeye(aes(fill = class), adjust = .5, width = .3,
                                 justification = -.6,.width = 0, point_colour = NA) + 
            gghalves::geom_half_point(aes(color = class),side = "l", range_scale = .3, size = 3, alpha = .6) +
            labs(fill = "",color = "") + xlab("") + ylab("COVID-19 Severity Score") +
            scale_fill_manual(values=c("#00AFBB","#FC4E07")) +
            scale_color_manual(values = c("#00AFBB", "#FC4E07"))+
            theme(text = element_text(size = 20))
    })
    
    output$auc <- renderText({
        req(df())
        paste0("AUC = ", rocit(score = df()$MRS, class = df()$class)$AUC)
    })
})
