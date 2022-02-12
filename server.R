library(plotly)
library(data.table)
library(DT)

function(input, output, session) { 
  #data <- read.csv("IND Videos.csv")
  #videodataServer<-data
  #videodataServer$publish_time <- as.Date(videodataServer$publish_time,format="%Y-%m-%d")
  #videodataServer <- subset(videodataServer,videodataServer$publish_time=='2017-11-12')
  dataset<-eventReactive(input$submit,{
    videodataServer<-videodata
    yearMonthData<-yearMonthDataFunc()
    tagSelectData<-tagSelectDataFunc()
    
    if(tagSelectData!="ALL"){
      videodataServer<-subset(videodataServer,videodataServer$tags_searh==tagSelectData)
    }
    yearMonthData<-as.Date(yearMonthData)
    yearMonthData<-format(yearMonthData, "%B-%Y")
    videodataServer<-subset(videodataServer,videodataServer$year_month==yearMonthData)
    if (dim(videodataServer)[1] == 0) {
      videodataServer <- videodataServer[FALSE,]
      videodataServercolname<-colnames(videodataServer)
      rowAdd<-c(0,0,"No Data","No Data",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
      videodataServer<-rbind(videodataServer, rowAdd)
      colnames(videodataServer)<-videodataServercolname
      videodataServer$views<-as.numeric(videodataServer$views)
      videodataServer$likes<-as.numeric(videodataServer$likes)
      videodataServer$comment_count<-as.numeric(videodataServer$comment_count)
      videodataServer$dislikes<-as.numeric(videodataServer$dislikes)
    }
    videodataServer
  })
  
  yearMonthDataFunc<-eventReactive(input$submit,{ data<-input$dateMonthYearInput})
  tagSelectDataFunc<-eventReactive(input$submit,{ data<-input$tagSelectNames})
  
  output$text1 <- renderText({
    videodataServerOut<-dataset()
    agg_data<-aggregate(videodataServerOut[c("views","likes","comment_count")],by = videodataServerOut["channel_title"],FUN=sum)
    agg_data<-agg_data[order(agg_data$views,decreasing = T), ]
    agg_data_Value<-agg_data$views[1]
    agg_data_Value<-prettyNum(agg_data_Value,big.mark=",", preserve.width="none")
    agg_data_Value
    
  })
  output$text4 <- renderText({
    videodataServerOut<-dataset()
    agg_data<-aggregate(videodataServerOut[c("views","likes","comment_count")],by = videodataServerOut["channel_title"],FUN=sum)
    agg_data<-agg_data[order(agg_data$views,decreasing = T), ]
    agg_data_Value<-agg_data$channel_title[1]
    agg_data_Value
    
  })
  output$text2 <- renderText({
    videodataServerOut<-dataset()
    agg_data<-aggregate(videodataServerOut[c("views","likes","comment_count")],by = videodataServerOut["channel_title"],FUN=sum)
    agg_data<-agg_data[order(agg_data$likes,decreasing = T), ]
    agg_data_Value<-agg_data$likes[1]
    agg_data_Value<-prettyNum(agg_data_Value,big.mark=",", preserve.width="none")
    agg_data_Value
    
  })
  output$text5 <- renderText({
    videodataServerOut<-dataset()
    agg_data<-aggregate(videodataServerOut[c("views","likes","comment_count")],by = videodataServerOut["channel_title"],FUN=sum)
    agg_data<-agg_data[order(agg_data$likes,decreasing = T), ]
    agg_data_Value<-agg_data$channel_title[1]
    agg_data_Value
    
  })
  output$text3 <- renderText({
    videodataServerOut<-dataset()
    agg_data<-aggregate(videodataServerOut[c("views","likes","comment_count")],by = videodataServerOut["channel_title"],FUN=sum)
    agg_data<-agg_data[order(agg_data$comment_count,decreasing = T), ]
    agg_data_Value<-agg_data$comment_count[1]
    agg_data_Value<-prettyNum(agg_data_Value,big.mark=",", preserve.width="none")
    agg_data_Value
    
  })
  output$text6 <- renderText({
    videodataServerOut<-dataset()
    agg_data<-aggregate(videodataServerOut[c("views","likes","comment_count")],by = videodataServerOut["channel_title"],FUN=sum)
    agg_data<-agg_data[order(agg_data$comment_count,decreasing = T), ]
    agg_data_Value<-agg_data$channel_title[1]
    
  })
  
  
  output$Plot1 <- renderPlotly({
  
    videodataServerOut<-dataset()
  
    agg_data<-aggregate(videodataServerOut[c("views","likes","comment_count")],by = videodataServerOut["channel_title"],FUN=sum)
    agg_data<-agg_data[order(agg_data$views,decreasing = T), ]
    agg_data_Values<-agg_data[1:10,]
    agg_data_Values$channel_title <- factor(agg_data_Values$channel_title, levels = unique(agg_data_Values$channel_title)[order(agg_data_Values$views, decreasing = TRUE)])
    
    fig <- plot_ly(
    x = agg_data_Values$channel_title,
    y = agg_data_Values$views,
    name = "Channel",
    type = "bar"
  )
  
  fig
  })
  
  output$Plot2 <- renderPlotly({
    
    videodataServerOut<-dataset()
    #videodataServerOut<-videodataServer
    agg_data<-aggregate(videodataServerOut[c("views","likes","comment_count")],by = videodataServerOut["channel_title"],FUN=sum)
    agg_data<-agg_data[order(agg_data$likes,decreasing = T), ]
    agg_data_Values<-agg_data[1:10,]
    agg_data_Values$channel_title <- factor(agg_data_Values$channel_title, levels = unique(agg_data_Values$channel_title)[order(agg_data_Values$likes, decreasing = TRUE)])
    
    fig <- plot_ly(
      x = agg_data_Values$channel_title,
      y = agg_data_Values$likes,
      name = "Channel",
      type = "bar"
    )
    
    fig
  })
  
  output$Plot3 <- renderPlotly({
    
    videodataServerOut<-dataset()
    
    agg_data<-aggregate(videodataServerOut[c("views","likes","comment_count")],by = videodataServerOut["channel_title"],FUN=sum)
    agg_data<-agg_data[order(agg_data$comment_count,decreasing = T), ]
    agg_data_Values<-agg_data[1:10,]
    agg_data_Values$channel_title <- factor(agg_data_Values$channel_title, levels = unique(agg_data_Values$channel_title)[order(agg_data_Values$comment_count, decreasing = TRUE)])
    
    fig <- plot_ly(
      x = agg_data_Values$channel_title,
      y = agg_data_Values$comment_count,
      name = "Channel",
      type = "bar"
    )
    
    fig
  })
  
  output$table1<-DT::renderDataTable({
    videodataServerOut<-dataset()
    overallData<-subset(videodataServerOut,select=c("title","channel_title","views","likes","dislikes","comment_count"))
    overallData$views<-prettyNum(overallData$views,big.mark=",", preserve.width="none")
    overallData$likes<-prettyNum(overallData$likes,big.mark=",", preserve.width="none")
    overallData$dislikes<-prettyNum(overallData$dislikes,big.mark=",", preserve.width="none")
    overallData$comment_count<-prettyNum(overallData$comment_count,big.mark=",", preserve.width="none")
    colnames(overallData)<-c("Title","Channel","Views","Likes","DisLikes","Comment Count")
    overallDataset<-datatable(overallData, filter = 'top', extensions = 'Buttons',options = list(
      pageLength = 10, autoWidth = TRUE,dom = 'Bfrtip',
      buttons = c('csv','excel')
      
    ))
    overallDataset
  })
  
  output$messageIcon1 <- renderMenu({
    videodataServerOut<-dataset()
    agg_data<-aggregate(videodataServerOut[c("views","likes","comment_count")],by = videodataServerOut["channel_title"],FUN=sum)
    agg_data<-agg_data[order(agg_data$views,decreasing = T), ]
    agg_data_Value<-head(agg_data, n = 1)
    agg_data_Value$views<-prettyNum(agg_data_Value$views,big.mark=",", preserve.width="none")
    agg_data_Value<-agg_data_Value[c("channel_title","views")]
    colnames(agg_data_Value)<-c("from","message")
    
    msgs <- apply(agg_data_Value, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    dropdownMenu(type = "messages", .list = msgs)
  })
  output$messageIcon2 <- renderMenu({
    videodataServerOut<-dataset()
    agg_data<-aggregate(videodataServerOut[c("views","likes","comment_count")],by = videodataServerOut["channel_title"],FUN=sum)
    agg_data<-agg_data[order(agg_data$likes,decreasing = T), ]
    agg_data_Value<-head(agg_data, n = 1)
    agg_data_Value$likes<-prettyNum(agg_data_Value$likes,big.mark=",", preserve.width="none")
    agg_data_Value<-agg_data_Value[c("channel_title","likes")]
    colnames(agg_data_Value)<-c("from","message")
    
    msgs <- apply(agg_data_Value, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    dropdownMenu(type = "messages", .list = msgs)
  })
  output$messageIcon3 <- renderMenu({
    videodataServerOut<-dataset()
    agg_data<-aggregate(videodataServerOut[c("views","likes","comment_count")],by = videodataServerOut["channel_title"],FUN=sum)
    agg_data<-agg_data[order(agg_data$comment_count,decreasing = T), ]
    agg_data_Value<-head(agg_data, n = 1)
    agg_data_Value$comment_count<-prettyNum(agg_data_Value$comment_count,big.mark=",", preserve.width="none")
    agg_data_Value<-agg_data_Value[c("channel_title","comment_count")]
    colnames(agg_data_Value)<-c("from","message")
    
    msgs <- apply(agg_data_Value, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    dropdownMenu(type = "messages", .list = msgs)
  })
  
}