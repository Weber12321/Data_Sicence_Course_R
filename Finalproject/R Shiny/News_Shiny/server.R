
library(imager)
library(shiny)
library(ggplot2)
library(magrittr)
# setwd("~/GitHub/NTU-CSX-DataScience--Group5/Finalproject/R Shiny/News_Shiny")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  News <- read.csv("News_sentiment.csv", encoding = "big5")
  FB_Taipei <- read.csv("FaceBookAPI-Taipei.csv", encoding = "big5")
  
  output$TrendPlot <- renderPlot({
    if(input$Candi==4){
      mood = input$mood %>% as.character()
      ggplot(data=FB_Taipei, aes(x=time%>% as.Date(), y=eval(parse(text = input$mood %>% as.character())), color=Candidate))+geom_point(size=3)+xlab("time")+ylab(mood)+scale_color_manual(values=c("blue", "green", "black"))
    }else{
      names = input$Candi %>% as.character()
      mood = input$mood %>% as.character()
      if(input$line==TRUE){
        qplot(time %>% as.Date(),eval(parse(text = input$mood %>% as.character())),data=FB_Taipei[FB_Taipei$Candidate==names,],xlab = "date",ylab=mood,geom = c("point", "smooth"))
      }else{
        qplot(time %>% as.Date(),eval(parse(text = input$mood %>% as.character())),data=FB_Taipei[FB_Taipei$Candidate==names,],xlab = "date",ylab=mood)
      }
    }
    
  })
  
  output$BoxPlot <- renderPlot({
    
    mood = input$mood2 %>% as.character()
    P1 <- ggplot(FB_Taipei, aes(x=Candidate, y=eval(parse(text = input$mood2 %>% as.character())),color = Candidate)) + 
      scale_color_manual(values=c("blue", "green", "red"))+ylab(mood)
    if(input$outlier == 0){
      P2 = P1 +  geom_boxplot()
      P2
    }else if(input$outlier == 1){
      if(input$mood2=="like"){
        Q = FB_Taipei$like %>%  quantile(.,0.75)
        Q = Q*2
      }else if (input$mood2=="share"){
        Q = FB_Taipei$share %>%  quantile(.,0.75)
        Q = Q*2
      }else if (input$mood2=="angry"){
        Q = FB_Taipei$angry %>%  quantile(.,0.75)
        Q = Q*2
      }else if (input$mood2=="sad"){
        Q = FB_Taipei$sad %>%  quantile(.,0.75)
        Q = Q*2
      }else if (input$mood2=="haha"){
        Q = FB_Taipei$haha %>%  quantile(.,0.75)
        Q = Q*2
      }else if (input$mood2=="love"){
        Q = FB_Taipei$love %>%  quantile(.,0.75)
        Q = 1000
      }else if (input$mood2=="wow"){
        Q = FB_Taipei$wow %>%  quantile(.,0.75)
        Q = Q*2
      }else if (input$mood2=="sentiment"){
        Q = FB_Taipei$sentiment %>%  quantile(.,0.75)
        Q = 1
      }
      
      
      P3 = P1 + geom_boxplot(outlier.shape = NA)+ylim(low=0, high=Q)+ylab(mood)
      P3
    }
    
    
  })
  
  output$TopText <- renderTable({
    
    if(input$Candi2=="柯文哲"){
      df = FB_Taipei[FB_Taipei$name=="Ko",]
    }else if(input$Candi2=="丁守中"){
      df = FB_Taipei[FB_Taipei$name=="Di",]
    }else if(input$Candi2=="姚文智"){
      df = FB_Taipei[FB_Taipei$name=="Yao",]
    }
    if(input$mood3=="like"){
      df2 <- df[,c(3,4,6)]
      df2[df2$like %>% order(.,decreasing = input$decrease), ] %>% head(.,n = input$shows)
      
    }else if (input$mood3=="share"){
      df2 <- df[,c(3,4,5)]
      df2[df2$share %>% order(.,decreasing = input$decrease), ] %>% head(.,n = input$shows)
      
    }else if (input$mood3=="angry"){
      df2 <- df[,c(3,4,11)]
      df2[df2$angry %>% order(.,decreasing = input$decrease), ] %>% head(.,n = input$shows)
      
    }else if (input$mood3=="sad"){
      df2 <- df[,c(3,4,9)]
      df2[df2$sad %>% order(.,decreasing = input$decrease), ] %>% head(.,n = input$shows)
      
    }else if (input$mood3=="haha"){
      df2 <- df[,c(3,4,8)]
      df2[df2$haha %>% order(.,decreasing = input$decrease), ] %>% head(.,n = input$shows)
      
    }else if (input$mood3=="love"){
      df2 <- df[,c(3,4,7)]
      df2[df2$love %>% order(.,decreasing = input$decrease), ] %>% head(.,n = input$shows)
      
    }else if (input$mood3=="wow"){
      df2 <- df[,c(3,4,10)]
      df2[df2$wow %>% order(.,decreasing = input$decrease), ] %>% head(.,n = input$shows)
      
    }else if (input$mood3=="sentiment"){
      df2 <- df[,c(3,4,12)]
      df2[df2$sentiment %>% order(.,decreasing = input$decrease), ] %>% head(.,n = input$shows)
      
    }
    
    
    
  })
  output$postLDA <- renderPlot({

    if(input$PostCandi=="柯文哲"){
      P <- load.image("PostLDA/Ko.png")
      plot(P,axes = FALSE)
      
    }else if(input$PostCandi=="姚文智"){
      P <- load.image("PostLDA/Yao.png")
      plot(P,axes = FALSE)
    }else if(input$PostCandi=="丁守中"){
      P <- load.image("PostLDA/Di.png")
      plot(P,axes = FALSE)
    }

  })
  
  output$TestPlot <- renderPlot({
    
    qplot(name,sentiment,data=News)
    
  })
  output$TestPlot2 <- renderPlot({
    
    qplot(name,sentiment,data=News)
    
  })
  output$NewsMood <- renderPlot({
    
    candi <- input$newsCandi %>% as.character()
    ggplot(News[News$candidate==candi,], aes(x=media, y=sentiment,color = media)) + 
      geom_boxplot()
    
    
  })
  
  output$NewsMood2 <- renderPlot({
    
    news <- input$newsName %>% as.character()
    ggplot(News[News$media==news,], aes(x=candidate, y=sentiment,color = media)) + 
      geom_boxplot()
    
    
  })
  
  output$newsLDA <- renderPlot({
    if(input$newsLDA=="LTN"){
      if(input$nameLDA=="柯文哲"){
        P <- load.image("NewsLDA/LTN/Ko.png")
        plot(P,axes = FALSE)
      }else if(input$nameLDA=="丁守中"){
        P <- load.image("NewsLDA/LTN/Di.png")
        plot(P,axes = FALSE)
      }else if(input$nameLDA=="姚文智"){
        P <- load.image("NewsLDA/LTN/Yao.png")
        plot(P,axes = FALSE)
      }
      
    }else if(input$newsLDA=="UDN"){
      if(input$nameLDA=="柯文哲"){
        P <- load.image("NewsLDA/UDN/Ko.png")
        plot(P,axes = FALSE)
      }else if(input$nameLDA=="丁守中"){
        P <- load.image("NewsLDA/UDN/Di.png")
        plot(P,axes = FALSE)
      }else if(input$nameLDA=="姚文智"){
        P <- load.image("NewsLDA/UDN/Yao.png")
        plot(P,axes = FALSE)
      }
    }else if(input$newsLDA=="Apple"){
      if(input$nameLDA=="柯文哲"){
        P <- load.image("NewsLDA/Apple/App_Ko.png")
        plot(P,axes = FALSE)
      }else if(input$nameLDA=="丁守中"){
        P <- load.image("NewsLDA/Apple/App_Ding.png")
        plot(P,axes = FALSE)
      }else if(input$nameLDA=="姚文智"){
        P <- load.image("NewsLDA/Apple/App_Yao.png")
        plot(P,axes = FALSE)
      }
    }else if(input$newsLDA=="CT"){
      if(input$nameLDA=="柯文哲"){
        P <- load.image("NewsLDA/CT/CT_Ko.png")
        plot(P,axes = FALSE)
      }else if(input$nameLDA=="丁守中"){
        P <- load.image("NewsLDA/CT/CT_Ding.png")
        plot(P,axes = FALSE)
      }else if(input$nameLDA=="姚文智"){
        P <- load.image("NewsLDA/CT/CT_Yao.png")
        plot(P,axes = FALSE)
      }
    }
    
  })
  
  
  output$CloudPlot <- renderPlot({
    # 柯文哲
    if(input$CandiXD == 1 && input$news == 1 && input$month ==0){
      P <- load.image("wc/LTN/Ko_wordcloudPNG/Ko_2018.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 1 && input$month ==1){
      P <- load.image("wc/LTN/Ko_wordcloudPNG/Ko_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 1 && input$month ==2){
      P <- load.image("wc/LTN/Ko_wordcloudPNG/Ko_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 1 && input$month ==3){
      P <- load.image("wc/LTN/Ko_wordcloudPNG/Ko_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 1 && input$month ==4){
      P <- load.image("wc/LTN/Ko_wordcloudPNG/Ko_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 1 && input$month ==5){
      P <- load.image("wc/LTN/Ko_wordcloudPNG/Ko_May.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 2 && input$month ==0){
      P <- load.image("wc/UDN/Ko_wordcloud/ko_2018.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 2 && input$month ==1){
      P <- load.image("wc/UDN/Ko_wordcloud/ko_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 2 && input$month ==2){
      P <- load.image("wc/UDN/Ko_wordcloud/ko_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 2 && input$month ==3){
      P <- load.image("wc/UDN/Ko_wordcloud/ko_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 2 && input$month ==4){
      P <- load.image("wc/UDN/Ko_wordcloud/ko_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 2 && input$month ==5){
      P <- load.image("wc/UDN/Ko_wordcloud/ko_May.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 3 && input$month ==0){
      P <- load.image("wc/CT/Ko_wordcloud/Ko_2018.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 3 && input$month ==1){
      P <- load.image("wc/CT/Ko_wordcloud/Ko_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 3 && input$month ==2){
      P <- load.image("wc/CT/Ko_wordcloud/Ko_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 3 && input$month ==3){
      P <- load.image("wc/CT/Ko_wordcloud/Ko_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 3 && input$month ==4){
      P <- load.image("wc/CT/Ko_wordcloud/Ko_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 3 && input$month ==5){
      P <- load.image("wc/CT/Ko_wordcloud/Ko_May.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 4 && input$month ==0){
      P <- load.image("wc/Ap/Ko_wordcloud/Ko_2018.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 4 && input$month ==1){
      P <- load.image("wc/Ap/Ko_wordcloud/Ko_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 4 && input$month ==2){
      P <- load.image("wc/Ap/Ko_wordcloud/Ko_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 4 && input$month ==3){
      P <- load.image("wc/Ap/Ko_wordcloud/Ko_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 4 && input$month ==4){
      P <- load.image("wc/Ap/Ko_wordcloud/Ko_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 1 && input$news == 4 && input$month ==5){
      P <- load.image("wc/Ap/Ko_wordcloud/Ko_May.png")
      plot(P,axes = FALSE)
    }
    # 丁守中
    if(input$CandiXD == 2 && input$news == 1 && input$month ==0){
      P <- load.image("wc/LTN/Di_wordcloudPNG/Di_2018.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 1 && input$month ==1){
      P <- load.image("wc/LTN/Di_wordcloudPNG/Di_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 1 && input$month ==2){
      P <- load.image("wc/LTN/Di_wordcloudPNG/Di_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 1 && input$month ==3){
      P <- load.image("wc/LTN/Di_wordcloudPNG/Di_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 1 && input$month ==4){
      P <- load.image("wc/LTN/Di_wordcloudPNG/Di_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 1 && input$month ==5){
      P <- load.image("wc/LTN/Di_wordcloudPNG/Di_May.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 2 && input$month ==0){
      P <- load.image("wc/UDN/Di_wordcloud/Di_2018.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 2 && input$month ==1){
      P <- load.image("wc/UDN/Di_wordcloud/Di_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 2 && input$month ==2){
      P <- load.image("wc/UDN/Di_wordcloud/Di_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 2 && input$month ==3){
      P <- load.image("wc/UDN/Di_wordcloud/Di_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 2 && input$month ==4){
      P <- load.image("wc/UDN/Di_wordcloud/Di_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 2 && input$month ==5){
      P <- load.image("wc/UDN/Di_wordcloud/Di_May.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 3 && input$month ==0){
      P <- load.image("wc/CT/Di_wordcloud/Di_2018.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 3 && input$month ==1){
      P <- load.image("wc/CT/Di_wordcloud/Di_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 3 && input$month ==2){
      P <- load.image("wc/CT/Di_wordcloud/Di_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 3 && input$month ==3){
      P <- load.image("wc/CT/Di_wordcloud/Di_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 3 && input$month ==4){
      P <- load.image("wc/CT/Di_wordcloud/Di_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 3 && input$month ==5){
      P <- load.image("wc/CT/Di_wordcloud/Di_May.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 4 && input$month ==0){
      P <- load.image("wc/Ap/Di_wordcloud/Di_2018.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 4 && input$month ==1){
      P <- load.image("wc/Ap/Di_wordcloud/Di_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 4 && input$month ==2){
      P <- load.image("wc/Ap/Di_wordcloud/Di_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 4 && input$month ==3){
      P <- load.image("wc/Ap/Di_wordcloud/Di_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 4 && input$month ==4){
      P <- load.image("wc/Ap/Di_wordcloud/Di_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 2 && input$news == 4 && input$month ==5){
      P <- load.image("wc/Ap/Di_wordcloud/Di_May.png")
      plot(P,axes = FALSE)
    }
    
    # 姚文智
    if(input$CandiXD == 3 && input$news == 1 && input$month ==0){
      P <- load.image("wc/LTN/Yao_wordcloudPNG/Yao_2018.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 1 && input$month ==1){
      P <- load.image("wc/LTN/Yao_wordcloudPNG/Yao_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 1 && input$month ==2){
      P <- load.image("wc/LTN/Yao_wordcloudPNG/Yao_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 1 && input$month ==3){
      P <- load.image("wc/LTN/Yao_wordcloudPNG/Yao_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 1 && input$month ==4){
      P <- load.image("wc/LTN/Yao_wordcloudPNG/Yao_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 1 && input$month ==5){
      P <- load.image("wc/LTN/Yao_wordcloudPNG/Yao_May.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 2 && input$month ==0){
      P <- load.image("wc/UDN/Yao_wordcloud/Yao_2018.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 2 && input$month ==1){
      P <- load.image("wc/UDN/Yao_wordcloud/Yao_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 2 && input$month ==2){
      P <- load.image("wc/UDN/Yao_wordcloud/Yao_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 2 && input$month ==3){
      P <- load.image("wc/UDN/Yao_wordcloud/Yao_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 2 && input$month ==4){
      P <- load.image("wc/UDN/Yao_wordcloud/Yao_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 2 && input$month ==5){
      P <- load.image("wc/UDN/Yao_wordcloud/Yao_May.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 3 && input$month ==0){
      P <- load.image("wc/CT/Yao_wordcloud/Yao_2018.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 3 && input$month ==1){
      P <- load.image("wc/CT/Yao_wordcloud/Yao_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 3 && input$month ==2){
      P <- load.image("wc/CT/Yao_wordcloud/Yao_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 3 && input$month ==3){
      P <- load.image("wc/CT/Yao_wordcloud/Yao_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 3 && input$month ==4){
      P <- load.image("wc/CT/Yao_wordcloud/Yao_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 3 && input$month ==5){
      P <- load.image("wc/CT/Yao_wordcloud/Yao_May.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 4 && input$month ==0){
      P <- load.image("wc/Ap/Yao_wordcloud/Yao_2018.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 4 && input$month ==1){
      P <- load.image("wc/Ap/Yao_wordcloud/Yao_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 4 && input$month ==2){
      P <- load.image("wc/Ap/Yao_wordcloud/Yao_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 4 && input$month ==3){
      P <- load.image("wc/Ap/Yao_wordcloud/Yao_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 4 && input$month ==4){
      P <- load.image("wc/Ap/Yao_wordcloud/Yao_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$CandiXD == 3 && input$news == 4 && input$month ==5){
      P <- load.image("wc/Ap/Yao_wordcloud/Yao_May.png")
      plot(P,axes = FALSE)
    }
    
  })
  
  output$FaceCloudPlot <- renderPlot({
    # 柯文哲
    if(input$candiXD == 1 && input$yue ==0){
      P <- load.image("wc/FB/Ko_wordcloud/Ko_2018.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 1 && input$yue ==1){
      P <- load.image("wc/FB/Ko_wordcloud/Ko_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 1 && input$yue ==2){
      P <- load.image("wc/FB/Ko_wordcloud/Ko_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 1 && input$yue ==3){
      P <- load.image("wc/FB/Ko_wordcloud/Ko_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 1 && input$yue ==4){
      P <- load.image("wc/FB/Ko_wordcloud/Ko_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 1 && input$yue ==5){
      P <- load.image("wc/FB/Ko_wordcloud/Ko_May.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 1 && input$yue ==6){
      P <- load.image("wc/FB/Ko_wordcloud/Ko_Jun.png")
      plot(P,axes = FALSE)
    }
    
    # 丁守中
    if(input$candiXD == 2 && input$yue ==0){
      P <- load.image("wc/FB/Di_wordcloud/Di_2018.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 2 && input$yue ==1){
      P <- load.image("wc/FB/Di_wordcloud/Di_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 2 && input$yue ==2){
      P <- load.image("wc/FB/Di_wordcloud/Di_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 2 && input$yue ==3){
      P <- load.image("wc/FB/Di_wordcloud/Di_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 2 && input$yue ==4){
      P <- load.image("wc/FB/Di_wordcloud/Di_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 2 && input$yue ==5){
      P <- load.image("wc/FB/Di_wordcloud/Di_May.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 2 && input$yue ==6){
      P <- load.image("wc/FB/Di_wordcloud/Di_Jun.png")
      plot(P,axes = FALSE)
    }
    # 姚文智
    if(input$candiXD == 3 && input$yue ==0){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_2018.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 3 && input$yue ==1){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 3 && input$yue ==2){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 3 && input$yue ==3){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 3 && input$yue ==4){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 3 && input$yue ==5){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_May.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 3 && input$yue ==6){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_Jun.png")
      plot(P,axes = FALSE)
    }
  })
})
