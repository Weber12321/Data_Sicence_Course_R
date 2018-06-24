library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  "FaceBook",
  
  #### 趨勢圖布局 ####
  tabPanel("趨勢圖",
           sidebarLayout(
      
             sidebarPanel(
               
               # Input: Choose dataset ----
               selectInput("Candi", "Choose a dataset:",
                           choices = list("柯文哲"="柯文哲", "丁守中"="丁守中", "姚文智"="姚文智","全部"=4))
               ,checkboxInput("line", label = "加上趨勢線", value = FALSE),
               radioButtons("mood", label = h3("Radio buttons"),
                            choices = c("like","share","angry","sad","haha","love","wow","sentiment"), 
                            selected = "like")
               
             ), mainPanel(
               
               plotOutput("TrendPlot")
               
             )
           
           )       
    ),
  
  
  #### 盒狀圖布局 ####
  tabPanel("盒狀圖",
           sidebarLayout(
             sidebarPanel(
        
               radioButtons("mood2", label = h4("Radio buttons"),
                           choices = c("like","share","angry","sad","haha","love","wow","sentiment"),
                           selected = "like"),
               checkboxInput("outlier", label = "去除極端值", value = FALSE)
             ),mainPanel(
               
               plotOutput("BoxPlot")
               
             )
             
           )
  ),

  #### Top ####
  tabPanel("Top文本",
           sidebarLayout(
             sidebarPanel(
               
               selectInput("Candi2", "Choose a dataset:",
                           choices = c("柯文哲", "丁守中", "姚文智"))
               ,numericInput('shows', '顯示項目', 5,
                             min = 1, max = 50),
               checkboxInput("decrease", label = "從大排到小", value = TRUE),
               radioButtons("mood3", label = h4("Radio buttons"),
                                                             choices = c("like","share","angry","sad","haha","love","wow","sentiment"),
                                                             selected = "like")
            
             ),mainPanel(
               
               h4("Observations"),
               tableOutput("TopText")
               
             )
             
             
             
             
           )
  ),
  
  #### 文字雲 ####
  tabPanel("文字雲",
           h1("施工中!!")
  ),
  
  #### LDA ####
  tabPanel("LDA",
           h1("施工中!!")
  )
  
  
  
  
  )
)
