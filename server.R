library(shiny)
library(shinyjs)
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)
library(kernlab)
library(e1071)
library(cluster) 
library(FactoMineR)
library(randomGLM)
library(corrplot)
library(rpart)
library(RRF)
library(inTrees)
library(rattle)
library(party)
library(partykit)
library(xtable)


shinyServer(function(input, output, session) {
  
  observe({
    
    shinyjs::hide("downloadData") # Hide download button before input submission
    if(input$submitbutton>0)
      shinyjs::show("downloadData") # Show download button after input submission
    
  })
  
    datasetInput <- reactive({
    
    inFile <- input$file1 

    

    # Read data from uploaded file
    D <- read.csv(inFile$datapath, header = TRUE)
    
    # Removing missing data
    D = na.omit(D)
    
    # Write data to file
    write.table(D, "missing_data_removed.txt", sep="\t") 

    ########################### Mean, S.D. and t-test
    m = ncol(data)
    k = m-1
    data = data.frame(D[,-ncol(D)],class= as.numeric(D[,ncol(D)]))
    meanX  <- matrix(nrow = k, ncol = 1)
    sdX  <- matrix(nrow = k, ncol = 1)
    meanY  <- matrix(nrow = k, ncol = 1)
    sdY  <- matrix(nrow = k, ncol = 1)
    p.value  <- matrix(nrow = k, ncol = 1)
    
    for(i in 1:k){ 
      X <- subset(data.frame(data[,i],class =data[,m]), class == '1')
      Y <- subset(data.frame(data[,i],class =data[,m]), class == '2')
      meanX[i,]  = mean(as.numeric(X[,1]))
      sdX[i,]  = sd(as.numeric(X[,1]))
      meanY[i,]  = mean(as.numeric(Y[,1]))
      sdY[i,]  = sd(as.numeric(Y[,1]))
      p.value[i,]  = t.test(as.numeric(X[,1]),as.numeric(Y[,1]))$ p.value
    }
    
    data.frame (meanX,sdX,meanY,sdY,p.value)
    
    ########## PCA analysis
    n = ncol(D)-1
    DataPCA  <- matrix(nrow = nrow(D), ncol = n)
    
    for(i in 1:n){ 
      DataPCA[,i] = as.numeric(D[,i])
    }
    colnames(DataPCA) = colnames(D[,-ncol(D)])
    res.pca <- PCA(DataPCA)
    
    pca_variance <- data.frame (res.pca$eig) #### eigen value
    pca_loadings <- data.frame (res.pca$var$coord) #### loadings value
    pca_scores <- data.frame (res.pca$ind$coord) #### scores value
    
    # Write data to file
    write.table(pca_variance, "pca_variance.txt", sep="\t")
    write.table(pca_loadings, "pca_loadings.txt", sep="\t")
    write.table(pca_scores, "pca_scores.txt", sep="\t")
    
    ########## Generate DT DT (rpart package)
    pdf('decision_tree.pdf')
    tree <- rpart(class ~ ., data=D)
    fancyRpartPlot(tree)
    dev.off()
    
    ########## Generate rule from RF
    X <- D[,-ncol(D)]
    target <- D[,ncol(D)]
    rf <- RRF(X,as.factor(target),ntree=100) # build an ordinary RF
    treeList <- RF2List(rf)
    ruleExec <- extractRules(treeList,X)
    ruleExec <- unique(ruleExec)
    ruleMetric <- getRuleMetric(ruleExec,X,target) # measure rules
    ruleMetric <- pruneRule(ruleMetric,X,target) # prune each rule
    #ruleMetric <- selectRuleRRF(ruleMetric,X,target) # rule selection
    learner <- buildLearner(ruleMetric,X,target)
    pred <- applyLearner(learner,X)
    read <- presentRules(learner,colnames(X)) # more readable format
    # format the rule and metrics as a table in latex code
    rf_rule <- data.frame(xtable(read))
    
    write.table(rf_rule, "rf_rule.txt", sep="\t")

    # zip files
    files <- c("missing_data_removed.txt", "pca_variance.txt", "pca_loadings.txt", "pca_scores.txt", "rf_rule.txt", "decision_tree.pdf")
    zip("output.zip", files=paste(files, sep="/"))
    
    })
  
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } else {
      return("Server is ready for processing")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("output.zip", sep='')
    },
    content = function(file) {
      myfile <- paste0("output.zip", collapse = NULL)
      file.copy(myfile, file)
    }
  )
  
})
