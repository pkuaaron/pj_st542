library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(class)
library(tidyverse)
library(knitr)
library(rgl)
library(ggfortify)

library(readxl)
library(psych)
library(ggplot2)
library(MASS)

options(dplyr.print_min = 5)
options(tibble.print_min = 5)
opts_chunk$set(message = FALSE, cache = TRUE)
knit_hooks$set(webgl = hook_webgl)

shinyServer(function(input, output, session) {
  getData <- reactive({
    
    if(input$Gender =="all"){
      newData <- EFNEP_data
    }else{
      newData <- EFNEP_data %>% filter(Gender == input$Gender)
    }
  })
  
  getPcaResult<-reactive({
      pca_vars<-input$PcaSelectedColumns
      EFNEP_data_ana_pca<-EFNEP_data_ana
      for(p in predictors){
          colm<-colMeans(EFNEP_data_ana_pca[p])
          colsd<-sqrt(sum((EFNEP_data_ana_pca[p]-colm)^2)/(nrow(EFNEP_data_ana_pca)-1))
          EFNEP_data_ana_pca[,p]<-(EFNEP_data_ana_pca[,p]-colm)/colsd
      }
      PCs<-prcomp(EFNEP_data_ana_pca[,pca_vars], scale=T)
      return(list(pcas=PCs))
  })
  
  PCA.results<-reactive({
      t<-getPcaResult()
        t$pcas})
  
  getSupervisedResult<-reactive({
          
          set.seed(100)
          pct=input$TrainingPercent/100.0
          neighbors=input$NeighborCount
          predictors= input$SelectedColumns
          print(input$SelectedColumns)
          subjects<-unique(EFNEP_data_ana$Subject)
          
          train_sample_size<-floor(pct*length(subjects))
          train_subj<-sample(subjects,train_sample_size )
          train_EFNEP_data_ana<-filter(EFNEP_data_ana, Subject %in% train_subj)
          test_EFNEP_data_ana<-filter(EFNEP_data_ana, !(Subject %in% train_subj))
          
          
          for(p in c("Height","Weight","BMI","PI")){
              colm<-colMeans(train_EFNEP_data_ana[p])
              colsd<-sqrt(sum((train_EFNEP_data_ana[p]-colm)^2)/(nrow(train_EFNEP_data_ana)-1))
              train_EFNEP_data_ana[,p]<-(train_EFNEP_data_ana[,p]-colm)/colsd
              test_EFNEP_data_ana[,p]<-(test_EFNEP_data_ana[,p]-colm)/colsd
          }
          
          if(input$SupervisedMethod=='knn'){
              fitted.values <- knn(train = train_EFNEP_data_ana[,predictors],
                            test = test_EFNEP_data_ana[,predictors],
                            cl = train_EFNEP_data_ana$EFNEP_data,
                            k = neighbors, prob=TRUE) #could use CV to determine k
          }
          else{
              train_EFNEP_data_tmp<-train_EFNEP_data_ana[,c('EFNEP_data',predictors)]
              test <- multinom(EFNEP_data ~ ., data = train_EFNEP_data_tmp)
              fitted.values<-predict(test,newdata=test_EFNEP_data_ana)
          }
          
          fitInfo <- tbl_df(data.frame(predicted=fitted.values, test_EFNEP_data_ana[, c("EFNEP_data",predictors)]))
          contingencyTbl<-table(fitInfo$predicted,fitInfo$EFNEP_data)
          misClass <- 1 - sum(diag(contingencyTbl))/sum(contingencyTbl)
          
          return(list(contingencyTbl=as.data.frame.matrix(contingencyTbl), misclassified=misClass))
      })
  
  
  plotInput_local <- reactive({
    newData <- getData()
    entry_col<-paste(input$Question,'.entry',sep='')
    exit_col<-paste(input$Question,'.exit',sep='')
    crit1<-c(entry_col,exit_col)
    newData <- newData %>%  group_by_(.dots=crit1) %>% summarise(count=n())
    #create plot
    ggplot(newData, aes_string(x = entry_col, y = exit_col)) + geom_point(aes(size = count)) +
        ggtitle("Answers for entry and exit (Local food exposure)")
    
  })
  
  plotInput_non_local <- reactive({
      newData <- getData()
      entry_col<-paste(input$Question,'.entry',sep='')
      exit_col<-paste(input$Question,'.exit',sep='')
      crit1<-c(entry_col,exit_col)
      newData <- newData %>% filter(cat=='Non Local food exposed') %>% group_by_(.dots=crit1) %>% summarise(count=n())
      #create plot
      # browser()
      ggplot(newData, aes_string(x = entry_col, y = exit_col)) + geom_point(aes(size = count)) +
          ggtitle("Answers for entry and exit (Non Local food exposure)")
      
  })
  #create plot
  output$EFNEP_dataPlot_local <- renderPlot({
      plotInput_local()
  })
  
  output$EFNEP_dataPlot_non_local <- renderPlot({
      plotInput_non_local()
  })
  
  #create output of observations    
  output$table <- renderTable({
    getData()
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("EFNEP_data_",input$Highest_Grade, "_", input$Region_Name, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(getData(), file, row.names = FALSE)
      }
    )
    
    output$downloadPlot <- downloadHandler(
      filename = function() { paste("EFNEP_data_",input$Highest_Grade, "_", input$Region_Name, '.png', sep='') },
      content = function(file) {
        ggsave(file,plotInput())
      }
    )
    
    
    output$click_info <- renderPrint({
        # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
        # were a base graphics plot, we'd need those.
        nearPoints(getData(), input$plot1_click, addDist = TRUE)
    })
    
    output$brush_info <- renderPrint({
        brushedPoints(getData(), input$plot1_brush)
    })
    
    
    output$knn_output<-renderTable({
        rslt<-getSupervisedResult()
        rslt$contingencyTbl
    }, include.rownames=TRUE)
    
    output$misclassifiedPct<-renderText({
        rslt<-getSupervisedResult()
        print(paste0("Miss classified percentage: ",rslt$misclassified*100.0))
    })
    
    
    .theme<- theme(
        axis.line = element_line(colour = 'gray', size = .75), 
        panel.background = element_blank(),  
        plot.background = element_blank()
    )	
    
    
    #file info	
    output$filetable <- renderTable({
            tmp<-EFNEP_data_ana
            tmp<-tmp[,seq_along(1:ncol(tmp))<=10] # show max 10 columns and binf head tail calls
            rbind(head(tmp,10),tail(tmp,10))
        
    })
    
    
    #make componentplot
    output$componentplot <- renderPlot({
        #autoplot(getPcaResult()$pcas, data = EFNEP_data_ana, colour = 'EFNEP_data', loadings = TRUE,loadings.label = TRUE)
        PCs<-getPcaResult()$pcas
        st<-min(as.integer(input$NumberOfPcs),length(PCs)-1)
        
        # ggbiplot(PCs, obs.scale = 1, var.scale = 1, ellipse = input$plotEllipse, choices=st:(st+1), circle = input$plotCircle, var.axes=TRUE, labels=c(EFNEP_data_ana[,"EFNEP_data"]), groups=as.factor(c(EFNEP_data_ana[,"EFNEP_data"])))
            
    })
    
    output$screeplot <- renderPlot({
        pca_output <- getPcaResult()$pcas
        eig = (pca_output$sdev)^2
        variance <- eig*100/sum(eig)
        cumvar <- paste(round(cumsum(variance),1), "%")
        eig_df <- data.frame(eig = eig,
                             PCs = colnames(pca_output$x),
                             cumvar =  cumvar)
        ggplot(eig_df, aes(reorder(PCs, -eig), eig)) +
            geom_bar(stat = "identity", fill = "white", colour = "black") +
            geom_text(label = cumvar, size = 4,
                      vjust=-0.4) +
            theme_bw(base_size = 14) +
            xlab("PC") +
            ylab("Variances") +
            ylim(0,(max(eig_df$eig) * 1.1))
    })
})
