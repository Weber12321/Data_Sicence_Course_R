library(shiny)
library(ggplot2)
library(magrittr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  FB_Taipei <- read.csv("FaceBookAPI-Taipei.csv")
 
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
  
  output$TestPlot <- renderPlot({
    qplot(name,share,data=FB_Taipei)
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
  
  
  
})
