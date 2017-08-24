####################################################
#      Segmentation Discriminant and Targeting     #
####################################################

library("shiny")
library("cluster")
library("ggbiplot")
library("mclust")
library("MASS")

library("ggplot2")
library("scales")
library("gridExtra")

shinyServer(function(input, output) {
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      rownames(Dataset) = Dataset[,1]
      Dataset1 = Dataset[,2:ncol(Dataset)]
      #Dataset = t(Dataset)
      Dataset1 = scale(Dataset1, center = T, scale = T)
      return(Dataset1)
    }
  })
  
  Dataset2 <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      rownames(Dataset) = Dataset[,1]
      Dataset1 = Dataset[,2:ncol(Dataset)]
      return(Dataset1)
    }
  })
      
      discri_data <- reactive({
        if (is.null(input$file)) { return(NULL) }
        else{
          Dataset <- as.data.frame(read.csv(input$file1$datapath ,header=TRUE, sep = ","))
          rownames(Dataset) = Dataset[,1]
          Dataset1 = Dataset[,2:ncol(Dataset)]
          return(Dataset1)
        }
      })
    
      target_data <- reactive({
        if (is.null(input$file)) { return(NULL) }
        else{
          Dataset <- as.data.frame(read.csv(input$file2$datapath ,header=TRUE, sep = ","))
          rownames(Dataset) = Dataset[,1]
          Dataset1 = Dataset[,2:ncol(Dataset)]
          return(Dataset1)
        }
      })

      output$downloadData1 <- downloadHandler(
        filename = function() { "ConneCtorPDASegmentation.csv" },
        content = function(file) {
          write.csv(read.csv("data/ConneCtorPDASegmentation.csv"), file, row.names=F)
        }
      )
      
      output$downloadData2 <- downloadHandler(
        filename = function() { "ConneCtorPDADiscriminant.csv" },
        content = function(file) {
          write.csv(read.csv("data/ConneCtorPDADiscriminant.csv"), file, row.names=F)
        }
      )
      
      output$downloadData3 <- downloadHandler(
        filename = function() { "ConneCtorPDAClassification.csv" },
        content = function(file) {
          write.csv(read.csv("data/ConneCtorPDAClassification.csv"), file, row.names=F, col.names=F)
        }
      )
      
      t0 = reactive({
        set.seed(12345)
        if (input$select == "K-Means") ({
          
          if (is.null(input$file)) {
            # User has not uploaded a file yet
            return(data.frame())
          }
          
          else {
            fit = kmeans(Dataset(),input$Clust)
            Segment.Membership =  fit$cluster
            d = data.frame(r.name = row.names(Dataset2()),Segment.Membership,Dataset2())
            d
          }
        })
        
        else  if (input$select == "Model Based") ({
          
          if (is.null(input$file)) {
            # User has not uploaded a file yet
            return(data.frame())
          }
          
          else {
            fit = Mclust(Dataset(),input$Clust)
            Segment.Membership =  fit$classification
            d = data.frame(r.name = row.names(Dataset2()),Segment.Membership,Dataset2())
            d
          }
        })
        
        else if (input$select == "Hierarchical") ({
          if (is.null(input$file)) {
            # User has not uploaded a file yet
            return(data.frame())
          }
          else {
            distm <- dist(Dataset(), method = "euclidean") # distance matrix
            fit <- hclust(distm, method="ward") 
            Segment.Membership =  cutree(fit, k=input$Clust)
            d = data.frame(r.name = row.names(Dataset2()),Segment.Membership,Dataset2())
            d
          }
        })
      })
            
  output$table <- renderDataTable({
    t0()
  }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))
  
  output$caption1 <- renderText({
    if (input$select == "Model Based") return ("Model Based Segmentation -  Summary")
    else if (input$select == "K-Means") return ("K-Means Segmentation -  Summary")
    else if (input$select == "Hierarchical") return ("Hierarchical Segmentation -  Summary")
    else return (NULL)
  })
  
  output$caption2 <- renderText({
    if (input$select == "Model Based") 
    {
      fit0 = Mclust(Dataset())
      return(paste("Note - Optimal Segments Should be:",fit0$G,""))
    }
    else return(NULL)
  })
  
  output$summary <- renderPrint({
    
    set.seed(12345)
    
    if (input$select == "K-Means") ({
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        fit = kmeans(Dataset(),input$Clust)
        Segment.Membership = as.character(fit$cluster)
        clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
        Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans, Count = table(Segment.Membership) )
        Summary
      }
    })
    
    else  if (input$select == "Model Based") ({
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      else {
        fit = Mclust(Dataset(),input$Clust)
        Segment.Membership = as.character(fit$classification)
        clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
        Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans,ModelSumm = summary(fit) )
        Summary
      }
    })
    
    else if (input$select == "Hierarchical") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        d <- dist(Dataset(), method = "euclidean") # distance matrix
        fit <- hclust(d, method="ward") 
        Segment.Membership =  as.character(cutree(fit, k=input$Clust))
        clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
        Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans, Count = table(Segment.Membership), ModelSumm = fit )
        Summary
      }
    })  
  })
  
  discriminat <- reactive({
    set.seed(12345)
    if (input$select == "K-Means") ({
      fit = kmeans(Dataset(),input$Clust)
      Segment.Membership = as.character(fit$cluster)
    })
    else  if (input$select == "Model Based") ({
      fit = Mclust(Dataset(),input$Clust)
      Segment.Membership = as.character(fit$classification)
    })
    else if (input$select == "Hierarchical") ({
      d <- dist(Dataset(), method = "euclidean") # distance matrix
      fit <- hclust(d, method="ward") 
      Segment.Membership =  as.character(cutree(fit, k=input$Clust))
    })
    
    Classification = Segment.Membership
    
    data = discri_data()
    
    fit <- lda(Classification ~ . , data=data, 
               na.action = "na.omit", CV=TRUE)
    # fit0 <- lda(Classification ~ . , data=data)
    ct <- table(Classification, fit$class)
    Proportion = diag(prop.table(ct, 1))
    Percent.Correct = sum(diag(prop.table(ct)))*100
    
    lda <- lda(Classification ~ ., 
               data
               # prior = c(1,1,1)/3
               )
    
    pca <- prcomp(data,
                  center = TRUE,
                  scale. = TRUE) 
    
    plda <- predict(object = lda,
                    newdata = data)
    
    dataset = data.frame(Segment = Segment.Membership,
                         pca = pca$x, lda = plda$x)
    
    
    discri = list(confusion.matrix = ct, 
                  Proportion= Proportion, 
                  Percent.Correct = Percent.Correct,
                  modelout = fit,
                  dataset = dataset
                  )
    return(discri)
  })

  output$discriminatp <- renderPrint({
    discriminat()[1:4]
  })
  
  output$discplot = renderPlot({
    
    dataset = discriminat()$dataset
  
  p1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = Segment, shape = Segment), size = 2.5) + 
    labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
         y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))
  
  p2 <- ggplot(dataset) + geom_point(aes(pca.PC1, pca.PC2, colour = Segment, shape = Segment), size = 2.5) +
    labs(x = paste("PC1 (", percent(prop.pca[1]), ")", sep=""),
         y = paste("PC2 (", percent(prop.pca[2]), ")", sep=""))
  
  grid.arrange(p1, p2)
  
  })
  ############------------------------------------------------------------------------------------------#############
  output$targeting <- renderPrint({
    set.seed(12345)
    if (input$select == "K-Means") ({
      fit = kmeans(Dataset(),input$Clust)
      Segment.Membership = as.character(fit$cluster)
    })
    else  if (input$select == "Model Based") ({
      fit = Mclust(Dataset(),input$Clust)
      Segment.Membership = as.character(fit$classification)
    })
    else if (input$select == "Hierarchical") ({
      d <- dist(Dataset(), method = "euclidean") # distance matrix
      fit <- hclust(d, method="ward") 
      Segment.Membership =  as.character(cutree(fit, k=input$Clust))
    })
    
    Classification = Segment.Membership
    data = discri_data()
    
    fit = lda(Classification ~ ., data=data)
    # , na.action = "na.omit", CV=F)
    prediction <- predict(fit, newdata=target_data())
    return(prediction)
      })
  ###############3-------------------------------------------------------------------3############################################
  
  t1 = reactive({
    if (input$select == "K-Means") ({
      fit = kmeans(Dataset(),input$Clust)
      Segment.Membership = as.character(fit$cluster)
    })
    else  if (input$select == "Model Based") ({
      fit = Mclust(Dataset(),input$Clust)
      Segment.Membership = as.character(fit$classification)
    })
    else if (input$select == "Hierarchical") ({
      d <- dist(Dataset(), method = "euclidean") # distance matrix
      fit <- hclust(d, method="ward") 
      Segment.Membership =  as.character(cutree(fit, k=input$Clust))
    })
    
    Classification = Segment.Membership
    data = discri_data()
    
    fit = lda(Classification ~ ., data=data, na.action = "na.omit", CV=F)
    prediction <- predict(fit, newdata=target_data())
    
    Targeted.segment = prediction$class
    data.frame(Targeted.segment, target_data())
  })
  
  output$table1 <- renderDataTable({
    t1()

  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
    
  
  output$plotpca = renderPlot({ 
    
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    else {
    data.pca <- prcomp(Dataset(),center = TRUE,scale. = TRUE)
    plot(data.pca, type = "l"); abline(h=1)    
    }
    })
    
  output$plot = renderPlot({  
    set.seed(12345)
    
    if (input$select == "K-Means") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      fit = kmeans(Dataset(),input$Clust)
      
      classif1 = as.character(fit$cluster)
      data.pca <- prcomp(Dataset(),
                         center = TRUE,
                         scale. = TRUE)
      
      # plot(data.pca, type = "l"); abline(h=1)    
      
      g <- ggbiplot(data.pca,
                    obs.scale = 1,
                    var.scale = 1,
                    groups = classif1,
                    ellipse = TRUE,
                    circle = TRUE)
      
      g <- g + scale_color_discrete(name = '')
      g <- g + theme(legend.direction = 'horizontal',
                     legend.position = 'top')
      print(g)
      
    })
    
    else if (input$select == "Model Based") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      fit = Mclust(Dataset(),input$Clust)
      classif1 = as.character(fit$classification)
      data.pca <- prcomp(Dataset(),
                         center = TRUE,
                         scale. = TRUE)
      
      # plot(data.pca, type = "l"); abline(h=1)    
      
      g <- ggbiplot(data.pca,
                    obs.scale = 1,
                    var.scale = 1,
                    groups = classif1,
                    ellipse = TRUE,
                    circle = TRUE)
      
      g <- g + scale_color_discrete(name = '')
      g <- g + theme(legend.direction = 'horizontal',
                     legend.position = 'top')
      print(g)
      
    })
    
    else if (input$select == "Hierarchical") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      d <- dist(Dataset(), method = "euclidean") # distance matrix
      fit <- hclust(d, method="ward") 
      plot(fit) # display dendogram
      groups <- cutree(fit, k=input$Clust) # cut tree into 5 clusters
      # draw dendogram with red borders around the 5 clusters
      rect.hclust(fit, k=input$Clust, border="red") 
    })
  })
  
  output$downloadData4 <- downloadHandler(
    filename = function() { "segmentation.csv" },
    content = function(file) {
      write.csv(t0(), file, row.names=F)
    }
  )
  
  output$downloadData5 <- downloadHandler(
    filename = function() { "targeting.csv" },
    content = function(file) {
      write.csv(t1(), file, row.names=F)
    }
  )
  
  
})