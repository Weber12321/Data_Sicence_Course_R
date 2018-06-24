library(shiny)


shinyUI(navbarPage("選戰懶人包",

                   navbarMenu("FaceBook",
                              tabPanel("趨勢圖",sidebarLayout(
                                
                                sidebarPanel(
                                  
                                  # Input: Choose dataset ----
                                  selectInput("Candi", "北市候選人",
                                              choices = list("柯文哲"="柯文哲", "丁守中"="丁守中", "姚文智"="姚文智","全部"=4))
                                  ,checkboxInput("line", label = "加上趨勢線", value = FALSE),
                                  radioButtons("mood", label = h4("臉書表情"),
                                               choices = c("like","share","angry","sad","haha","love","wow","sentiment"), 
                                               selected = "like")
                                  
                                ), mainPanel(
                                  
                                  plotOutput("TrendPlot")
                                  
                                )
                                
                              )       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       ),
                              tabPanel("盒狀圖",sidebarLayout(
                                sidebarPanel(
                                  
                                  radioButtons("mood2", label = h4("臉書表情"),
                                               choices = c("like","share","angry","sad","haha","love","wow","sentiment"),
                                               selected = "like"),
                                  checkboxInput("outlier", label = "去除極端值", value = FALSE)
                                ),mainPanel(
                                  
                                  plotOutput("BoxPlot")
                                  
                                )
                                
                              )
                              ),
                              tabPanel("Top文本",
                                       sidebarLayout(
                                         sidebarPanel(
                                           
                                           selectInput("Candi2", "北市候選人",
                                                       choices = c("柯文哲", "丁守中", "姚文智"))
                                           ,numericInput('shows', '顯示項目', 5,
                                                         min = 1, max = 50),
                                           checkboxInput("decrease", label = "從大排到小", value = TRUE),
                                           radioButtons("mood3", label = h4("臉書表情"),
                                                        choices = c("like","share","angry","sad","haha","love","wow","sentiment"),
                                                        selected = "like")
                                           
                                         ),mainPanel(
                                           
                                           h4("Observations"),
                                           tableOutput("TopText")
                                           
                                         )
                                         
                                         
                                         
                                         
                                       )),
                              tabPanel("臉書文字雲",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput("candiXD", "北市候選人 :",
                                                       choices = list("柯文哲" = 1, "丁守中" = 2, "姚文智" = 3),
                                                       selected = 1),
                                           radioButtons("yue", label = h3("月份"),
                                                        choices = list("2018" = 0, "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6), 
                                                        selected = 0)
                                           
                                           
                                         ),
                                         mainPanel(
                                           
                                           h3("候選人粉專文字雲"),
                                           plotOutput("FaceCloudPlot", height = 600, width = 600)
                                           
                                         )
                                         
                                         
                                         
                                         
                                         
                                         
                                       )
                                       
                              ),
                              tabPanel("LDA",sidebarLayout(
                                sidebarPanel(
                                  selectInput("PostCandi", "北市候選人",
                                              choices = c("柯文哲", "丁守中", "姚文智"))
                                  
                                ),mainPanel(
                                  
                                  h4("LDA Result"),
                                  plotOutput("postLDA",height = 600, width = 1200)
                                  
                                )
                              )
                                       
                                       
                                       
                                       
                                       
                                       )
                              ),
    
                  navbarMenu("四大報",
                              tabPanel("情緒分析<以候選人分類>",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput("newsCandi", "北市候選人",
                                                       choices = c("柯文哲", "丁守中", "姚文智"))
                                           
                                         ),mainPanel(
                                           
                                           h4("情緒分析<以候選人分類>"),
                                           plotOutput("NewsMood")
                                           
                                         )
                                       )
                                       
                                       
                                       
                                       ),
                             
                             tabPanel("情緒分析<以報紙分類>",
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("newsName", "四大媒體",
                                                      choices = c("UDN", "CT", "LTN","Apple"))
                                          
                                        ),mainPanel(
                                          
                                          h4("情緒分析<以報紙分類>"),
                                          plotOutput("NewsMood2")
                                          
                                        )
                                      )
                                      
                                      
                                      
                             ),
                              tabPanel("四大報文字雲",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput("CandiXD", "北市候選人 :",
                                                       choices = list("柯文哲" = 1, "丁守中" = 2, "姚文智" = 3),
                                                       selected = 1),
                                           radioButtons("news", label = h3("媒體與社群"),
                                                        choices = list("自由" = 1, "聯合" = 2, "中時" = 3, "蘋果" = 4),
                                                        selected = 1),
                                           radioButtons("month", label = h3("月份"),
                                                        choices = list("2018" = 0, "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5), 
                                                        selected = 0)
                                           
                                           
                                         ),
                                         mainPanel(
                                           
                                           h3("候選人文字雲"),
                                           plotOutput("CloudPlot", height = 700, width = 700)
                                           
                                         )
                                         
                                         
                                         
                                         
                                         
                                         
                                       )
                                       
                              ),
                              tabPanel("LDA" ,
                                       
                                       sidebarLayout(
                                          sidebarPanel(
                                            selectInput("newsLDA", "四大媒體",
                                              choices = c("UDN", "CT", "LTN","Apple")),
                                            selectInput("nameLDA", "北市候選人",
                                              choices = c("柯文哲", "丁守中", "姚文智"))
                                  
                                        ),mainPanel(
                                  
                                              h4("LDA Result"),
                                              plotOutput("newsLDA",height = 600, width = 1200)
                                  
                                )
                              ))),
                  
                  tabPanel("討論")

  )
)