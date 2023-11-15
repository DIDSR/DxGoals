###########
legend.col <- function(col, lev){
        
        opar <- par
        
        n <- length(col)
        
        bx <- par("usr")
        
        box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
                    bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
        box.cy <- c(bx[3], bx[3])
        box.sy <- (bx[4] - bx[3]) / n
        
        xx <- rep(box.cx, each = 2)
        
        par(xpd = TRUE)
        for(i in 1:n){
                
                yy <- c(box.cy[1] + (box.sy * (i - 1)),
                        box.cy[1] + (box.sy * (i)),
                        box.cy[1] + (box.sy * (i)),
                        box.cy[1] + (box.sy * (i - 1)))
                polygon(xx, yy, col = col[i], border = col[i])
                
        }
        par(new = TRUE)
        plot(0, 0, type = "n",
             ylim = c(min(lev), max(lev)),
             yaxt = "n", ylab = "",
             xaxt = "n", xlab = "",
             
             frame.plot = FALSE)
        axis(side = 4, las = 2, tick = FALSE, line = .25)
        par <- opar
}
##########
Inferrence <- function(X){
        X <- as.matrix(X)
        t_hat <- X[2,]/colSums(X)
        Vt_hat <- t_hat*(1-t_hat)*colSums(X)
        
        ll0_hat <- log((1-t_hat[2])/(1-t_hat[1]))
        ll1_hat <- log(t_hat[2]/t_hat[1])
        
        Sp_hat <- 1-t_hat[1]
        Se_hat <- t_hat[2]
        sd_Sp <- sqrt(Sp_hat*(1-Sp_hat)/colSums(X)[1])
        sd_Se <- sqrt(Se_hat*(1-Se_hat)/colSums(X)[2])
        
        sd_ll1_hat <- sqrt(((1-t_hat[2])/X[2,2]) + ((1-t_hat[1])/X[2,1])) ### sd  
        sd_ll0_hat <- sqrt((t_hat[2]/X[1,2]) + (t_hat[1]/X[1,1])) ####
        
        return(c(Sp_hat,sd_Sp,Se_hat,sd_Se,ll0_hat,sd_ll0_hat,ll1_hat,sd_ll1_hat))
}

Inferrence2 <- function(X1,X2){
  
  Se_new <-rowSums(X2)[2]/sum(X2)
  Se_comp <-colSums(X2)[2]/sum(X2)
  
  Sp_new <-rowSums(X1)[1]/sum(X1)
  Sp_comp <-colSums(X1)[1]/sum(X1)
  
  l1_new <- Se_new/(1-Sp_new)
  l1_comp <- Se_comp/(1-Sp_comp)
  
  l0_new <- (1-Se_new)/(Sp_new)
  l0_comp <- (1-Se_comp)/(Sp_comp)
  
  V1 <- (X1[1,2]+X1[2,1])/(colSums(X1)[2]*rowSums(X1)[2])+(X2[1,2]+X2[2,1])/(colSums(X2)[2]*rowSums(X2)[2]) ## variance log((l1_new)/(l1_comp))
  
  V2 <- (X1[1,2]+X1[2,1])/(colSums(X1)[1]*rowSums(X1)[1])+(X2[1,2]+X2[2,1])/(colSums(X2)[1]*rowSums(X2)[1])# variance log((l0_new)/(l0_comp))
 
   A <- (log((l1_new-1)/(l1_comp-1))/log((l1_new)/(l1_comp)))^2
  V3 <- c(A,1)[1*(l1_new==l1_comp)+1]*V1 # variance log((l1_new-1)/(l1_comp-1)
  
  B <- (log((1-l0_new)/(1-l0_comp))/log((l0_new)/(l0_comp)))^2
  V4 <- c(B,1)[1*(l0_new==l0_comp)+1]*V2 # variance log((1-l0_new)/(1-l0_comp)
  
  return(c(Sp_new,Se_new,Sp_comp,Se_comp,l1_new,l0_new,l1_comp,l0_comp,sqrt(V1),sqrt(V2),sqrt(V3),sqrt(V4)))
  
}
####################
library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
        
        # App title ----
        titlePanel("Performance Goals for Diagnostic Test Classification Accuracy"),
        
        # Sidebar layout with input and output definitions ----
        
        sidebarPanel(
          p('* Please read instructions',style = "color:red"),
          radioButtons("Performance", "Choose a performance",
                       choices = c('Standalone' = "1",
                                   'Comparative' = "2"),selected = "1",
                       ),
          # selectInput("Performance", h4("Choose a performance"), 
          #             choices = list("Standalone" = 1, "Comparative" = 2), selected = 1),
          
          conditionalPanel(
            condition = "input.Performance == 2",
            selectInput("Hypothesis", "Hypothesis",
                        choices = list("Superiority" = 1, "Non-inferiority in risk difference" = 2,
                                       "Non-inferiority in relative risk" = 3), selected = 1)
          ),
          
                      selectInput("Goal", h5("Choose a Goal"), 
                                   choices = list("Rule-Out" = 1, "Rule-In" = 2,
                                                  "Rule-In & Rule-Out" = 3), selected = 1),
          conditionalPanel(
            condition = "input.Performance == 1",
                     numericInput(inputId = "Id100", 
                                  HTML(paste0("Prevalence p")), 
                                  min=0,max=1,step=0.001,
                                  value = 0.01)),
          conditionalPanel(
            condition = "input.Performance == 1 & input.Goal==1 |input.Performance == 1 &input.Goal==3",
                     numericInput(inputId = "Id101", 
                                  HTML(paste0("Rule-Out Risk p",tags$sub("0"))), 
                                  min=0,max=1,step=0.001,
                                  value = 0.005)),
          conditionalPanel(
            condition = "input.Performance == 1 & input.Goal==2 |input.Performance == 1 &input.Goal==3",
                     numericInput(inputId = "Id102", 
                                  HTML(paste0("Rule-In Risk p",tags$sub("1"))), 
                                  min=0,max=1,step=0.001,
                                  value = 0.02)),

               #      h5(HTML(paste0("Input Your Data X",tags$sub("td")))),
          
          conditionalPanel(
            condition = "input.Performance == 2 & input.Hypothesis != 1",
            sliderInput(inputId = "Id103",
                        label = "Choose non-inferiority margin level:", 
                        value = 0.95,
                        min=0.5,
                        max=1.0,
                        step = 0.01
            )),
          
          conditionalPanel(
            condition = "input.Performance == 1",
            h5(HTML(paste0("Input Your Data X",tags$sub("td")))),
                     fluidRow(
                             column(width = 3,
                                    numericInput(inputId = "X00", HTML(paste0("X",tags$sub("00"))),min=0,value = 10000)
                             ),
                             column(width = 3, 
                                    numericInput(inputId = "X01", HTML(paste0("X",tags$sub("01"))),min=0,value = 1)
                             )
                     ),
                     fluidRow(
                             column(width = 3,
                                    numericInput(inputId = "X10", HTML(paste0("X",tags$sub("10"))), min=0,value = 1)
                             ),
                             column(width = 3, 
                                    numericInput(inputId = "X11", HTML(paste0("X",tags$sub("11"))), min=0,value = 10000)
                             )
                     )),
          conditionalPanel(
            condition = "input.Performance == 2",
            h5(HTML(paste0("Input Your Data X",tags$sub("ts"), " (D=0: Non-disease subjects)"))),
            fluidRow(
              column(width = 3,
                     numericInput(inputId = "X00_0", HTML(paste0("X",tags$sub("00"))),min=0,value = 6912)
              ),
              column(width = 3, 
                     numericInput(inputId = "X01_0", HTML(paste0("X",tags$sub("01"))),min=0,value = 432)
              )
            ),
            fluidRow(
              column(width = 3,
                     numericInput(inputId = "X10_0", HTML(paste0("X",tags$sub("10"))), min=0,value = 864)
              ),
              column(width = 3, 
                     numericInput(inputId = "X11_0", HTML(paste0("X",tags$sub("11"))), min=0,value = 432)
              )
            ), 
            h5(HTML(paste0("Input Your Data X",tags$sub("ts"), " (D=1: Disease subjects)"))),
            
            fluidRow(
              column(width = 3,
                     numericInput(inputId = "X00_1", HTML(paste0("X",tags$sub("00"))),min=0,value = 27)
              ),
              column(width = 3, 
                     numericInput(inputId = "X01_1", HTML(paste0("X",tags$sub("01"))),min=0,value = 36)
              )
            ),
            fluidRow(
              column(width = 3,
                     numericInput(inputId = "X10_1", HTML(paste0("X",tags$sub("10"))), min=0,value = 45)
              ),
              column(width = 3, 
                     numericInput(inputId = "X11_1", HTML(paste0("X",tags$sub("11"))), min=0,value = 252)
              )
            )
            
            ),                    
                     tags$hr(),
                     actionButton('goButton', 'Get Results')
        ),
        mainPanel(# mainPanel will appear on the right side of the page
                tabsetPanel(
                  id = "tabs",
                        tabPanel("Guide",
                                 includeHTML(rmarkdown::render("Manual_Ty1.Rmd"))),#                   includeMarkdown(rmarkdown::render("Manual_Ty.Rmd"))),
                        tabPanel("Introduction",
                           includeHTML(rmarkdown::render("Manual_Ty2.Rmd"))),#   
                      tabPanel("Method",
                           includeHTML(rmarkdown::render("Manual_Ty3.Rmd"))),# 
                         # tabPanel("Input Data",fluidRow(column(12,verbatimTextOutput("Summary")),
                         #                                column(9,tableOutput("inputtable"))
                         #                                )), # this panel will show the output "inputtable", and the tab name is "Input Data"
                        
                        tabPanel("Result",
                                 fluidRow(
                                         column(12,
                                                conditionalPanel(
                                                  condition = "input.Performance == 1",
                                                plotOutput("LRGraph",width = "100%"
                                                           ,dblclick = "plot_click"
                                                ))
                                         )
                                 ),
                                 conditionalPanel(
                                   condition = "input.Performance == 1",
                                 tags$hr()
                                 ,
                                 tags$hr()),
                                 htmlOutput("LR")

                        ),
                        tabPanel("Extra_Result",
                                 tags$hr(),
                                 htmlOutput("Extra_Text"),
                                 fluidRow(
                                   column(12,
                                          plotOutput("Extra_Graph",width = "100%"
                                                     ,dblclick = "plot_click"
                                          )
                                   )
                                 )
  
                        )
                )
        )

)
server <- function(input, output, session) {
  observeEvent(input$goButton, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Result")
  })
        
        p <- reactive({input$Id100})
        p0 <- reactive({input$Id101})
        p1 <- reactive({input$Id102})
        
         Num_points <- 1001 #### number of value FPF for plotting 
         t0 <- seq(0,1,length.out=Num_points) ###FPF
         
         pre_odd <- reactive(p()/(1-p()))
         post_odd_0 <- reactive(p0()/(1-p0()))
         post_odd_1 <- reactive(p1()/(1-p1()))
        
         l0 <- reactive(post_odd_0()/pre_odd())
         l1 <- reactive(post_odd_1()/pre_odd())
         t1 <- reactive(1-l0()+l0()*t0)
         t2 <- reactive(l1()*t0)
         k1 <- reactive(which.min(abs(t0-(1/l1()))))
         
         t0_inter <- reactive((1-l0())/(l1()-l0()))
         t1_inter <- reactive(l1()*t0_inter())  
         
         
         k2 <- reactive(which.min(abs(t0-t0_inter())))
         k3 <- reactive(which.min(abs(t0-((l0()-1)/l0()))))
         ######## Plot triple with given l0,l1
         O <- t0/(1-t0)
         p0_st <- reactive(O*l0()/(1+O*l0()))
         p1_st <- reactive(O*l1()/(1+O*l1()))
         colr <- rev(heat.colors(Num_points))
         t1_st <- 101
         t2_st <- 501
        # 
        ################################## Data analysis Standalone#############
        x00 <- reactive({input$X00})
        x10 <- reactive({input$X10})
        x01 <- reactive({input$X01})
        x11 <- reactive({input$X11})
        X <- reactive({
                data <- as.data.frame(matrix(c(x00(),x10(),x01(),x11()),ncol = 2,nrow = 2))
                isolate({
                        colnames(data)<-c("D0","D1")
                        rownames(data)<-c("T0","T1")
                        
                })
                data})
        
        X_infer <- reactive(Inferrence(X())) ###c(Sp_hat,sd_Sp,Se_hat,sd_Sp,ll0_hat,sd_ll0_hat,ll1_hat,sd_ll1_hat)
        alpha <- 0.05
        l0_CI <-reactive({exp(c(X_infer()[5]+qnorm(alpha/2)*X_infer()[6],X_infer()[5]+qnorm(1-alpha/2)*X_infer()[6]))})
        l1_CI <-reactive({exp(c(X_infer()[7]+qnorm(alpha/2)*X_infer()[8],X_infer()[7]+qnorm(1-alpha/2)*X_infer()[8]))})
        
        t0_inter_1 <- reactive((1-l0_CI()[2])/(l1_CI()[1]-l0_CI()[2]))
        t1_inter_1 <- reactive(l1_CI()[1]*t0_inter_1())          

        t0_inter_2 <- reactive((1-l0_CI()[1])/(l1_CI()[2]-l0_CI()[1]))
        t1_inter_2 <- reactive(l1_CI()[2]*t0_inter_2())           
########################## Analysis Comparative
        ################################## Data analysis Standalone#############
        x00_0 <- reactive({input$X00_0})
        x10_0 <- reactive({input$X10_0})
        x01_0 <- reactive({input$X01_0})
        x11_0 <- reactive({input$X11_0})
        X0 <- reactive({
          data <- as.data.frame(matrix(c(x00_0(),x10_0(),x01_0(),x11_0()),ncol = 2,nrow = 2))
          isolate({
            colnames(data)<-c("S0","S1")
            rownames(data)<-c("T0","T1")
            
          })
          data})
        
        x00_1 <- reactive({input$X00_1})
        x10_1 <- reactive({input$X10_1})
        x01_1 <- reactive({input$X01_1})
        x11_1 <- reactive({input$X11_1})
        X1 <- reactive({
          data <- as.data.frame(matrix(c(x00_1(),x10_1(),x01_1(),x11_1()),ncol = 2,nrow = 2))
          isolate({
            colnames(data)<-c("S0","S1")
            rownames(data)<-c("T0","T1")
            
          })
          data})
        
        X2_infer <- reactive(Inferrence2(X0(),X1()))
        #c(Sp_new,Se_new,Sp_comp,Se_comp,l1_new,l0_new,l1_comp,l0_comp,sd1,sd2,sd3,sd4)
        
        gamma <- reactive({input$Id103})
        ######### Independent goals for comparative performance 
        del_sp <- reactive(X2_infer()[1]-X2_infer()[3]-1.96*sqrt((X2_infer()[1]*(1-X2_infer()[1])+X2_infer()[3]*(1-X2_infer()[3]))/sum(X0())))
        V_eta <- reactive((X2_infer()[2]*(1-X2_infer()[2])+gamma()^2*X2_infer()[4]*(1-X2_infer()[4]))/sum(X1())+(1-gamma())^2*X2_infer()[3]*(1-X2_infer()[3])/sum(X0()))
        del_se <- reactive(X2_infer()[2]-((1-gamma())*(1-X2_infer()[3])+gamma()*X2_infer()[4])-1.96*sqrt(V_eta()))
######################
#         output$inputtable <- renderTable({
#                 if(is.null(X())){return ()}
#        #         rownames(X) <- c("0","1")
# #                colnames(X) <- c("d0","d1")
#                 X()
#         },
#         include.rownames = T,
#         include.colnames = T)
# 
#          Goals <- c("Rule-Out","Rule-In","Rule-Out & Rule-In")
#          input_goal <- reactive({input$Goal})
#          output$Summary<-renderText({
#                  paste0("You choose ", Goals[as.numeric(input_goal())], " goal.","\n","Your prevalence p is ",round(p(),4),
#                         "\n","Your rule-out threshold is ",round(p0(),4),
#                         "\n","Your rule-in threshold is ",round(p1(),4),
#                         "\n","Your input data are shown below")
#          })
  ######### Display output in "Results" tab      
        output$LR <-renderText({
                if (input$goButton == 0) {return()}
                isolate({
                        if (input$Performance=="1" & input$Goal=="1"){
                          str1 <- paste("<font size=4.5, color=blue> <b>","NLR performance goal for a rule-out test is l<sub>0</sub> \u2264 ",round(l0(),3))
                          str2 <- paste("<font size=4.5> <b>","Your estimation \u00CE<sub>0</sub> = ",round(exp(X_infer()[5]),3))
                          str3 <- paste("<font size=4.5> <b>","95% confidence interval of \u00CE<sub>0</sub> is (",round(l0_CI()[1],3),",",round(l0_CI()[2],3),")")
                          str4 <- paste("<font size=4.5> <b>","Did your data meet the performance goal with statistical significance?",span(c("No","Yes")[1*(round(l0_CI()[2],3) <= round(l0(),3))+1],style = "color:red"))
                          str8 <- paste("")
                          
                          str5 <- paste("<font size=4.5, color=black> <b>","Your estimation \u015Ce =",round(X_infer()[3],3), " and \u015Cp =", round(X_infer()[1],3))
                          str6 <- paste("<font size=4.5> <b>","95% confidence interval of \u015Ce is (",round(max(0,X_infer()[3]+qnorm(alpha/2)*X_infer()[4]),3),",",round(min(1,X_infer()[3]+qnorm(1-alpha/2)*X_infer()[4]),3),")")
                          str7 <- paste("<font size=4.5> <b>","95% confidence interval of \u015Cp is (",round(max(0,X_infer()[1]+qnorm(alpha/2)*X_infer()[2]),3),",",round(min(1,X_infer()[1]+qnorm(1-alpha/2)*X_infer()[2]),3),")")
                          HTML(paste(str1,str2,str3,str4,str8,str5,str6,str7, sep = '<br/>'))
                          
                        }
                        else if (input$Performance=="1" & input$Goal=="2"){
                          str5 <- paste("<font size=4.5, color=black> <b>","Your estimation \u015Ce =",round(X_infer()[3],3), " and \u015Cp =", round(X_infer()[1],3))
                          str6 <- paste("<font size=4.5> <b>","95% confidence interval of \u015Ce is (",round(max(0,X_infer()[3]+qnorm(alpha/2)*X_infer()[4]),3),",",round(min(1,X_infer()[3]+qnorm(1-alpha/2)*X_infer()[4]),3),")")
                          str7 <- paste("<font size=4.5> <b>","95% confidence interval of \u015Cp is (",round(max(0,X_infer()[1]+qnorm(alpha/2)*X_infer()[2]),3),",",round(min(1,X_infer()[1]+qnorm(1-alpha/2)*X_infer()[2]),3),")")
                          str8 <- paste("")
                          str1 <- paste("<font size=4.5, color=blue> <b>","PLR performance goal for a rule-out test is l<sub>1</sub> \u2265 ",round(l1(),3)) 
                          str2 <- paste("<font size=4.5> <b>","Your estimation \u00CE<sub>1</sub> = ",round(exp(X_infer()[7]),3))
                          str3 <- paste("<font size=4.5> <b>","95% confidence interval of \u00CE<sub>1</sub> is (",round(l1_CI()[1],3),",",round(l1_CI()[2],3),")")
                          str4 <- paste("<font size=4.5> <b>","Did you data meet the performance goal with statistical significance?",span(c("No","Yes")[1*(round(l1_CI()[1],3) >= round(l1(),3))+1],style = "color:red"))
                          HTML(paste(str1, str2,str3,str4,str8,str5,str6,str7, sep = '<br/>'))
                          }  
                        else if (input$Performance=="1" & input$Goal=="3"){
                          str1 <- paste("<font size=4.5, color=black > <b>","Independent goals: Se \u2265",round(t1_inter(),3), " and Sp \u2265", round(1-t0_inter(),3))
                          str2 <- paste("<font size=4.5> <b>","Your estimation \u015Ce =",round(X_infer()[3],3), " and \u015Cp =", round(X_infer()[1],3))
                          str3 <- paste("<font size=4.5> <b>","95% confidence interval of \u015Ce is (",round(max(0,X_infer()[3]+qnorm(alpha/2)*X_infer()[4]),3),",",round(min(1,X_infer()[3]+qnorm(1-alpha/2)*X_infer()[4]),3),")")
                          str4 <- paste("<font size=4.5> <b>","95% confidence interval of \u015Cp is (",round(max(0,X_infer()[1]+qnorm(alpha/2)*X_infer()[2]),3),",",round(min(1,X_infer()[1]+qnorm(1-alpha/2)*X_infer()[2]),3),")")
                          str5 <- paste("<font size=4.5> <b>","Did your data meet the independent goals?",span(c("No","Yes")[1*((1-max(0,X_infer()[1]+qnorm(alpha/2)*X_infer()[2])<=t0_inter()) & (max(0,X_infer()[3]+qnorm(alpha/2)*X_infer()[4])>=t1_inter()))+1],style = "color:red"))
                          
                          str6 <- paste("")
                          
                          str7 <- paste("<font size=4.5, color=blue > <b>","The NLR and PLR performance goals are l<sub>0</sub> \u2264 ",round(l0(),3), "and l<sub>1</sub> \u2265 ",round(l1(),3))
                          str8 <- paste("<font size=4.5> <b>","Your estimation \u00CE<sub>0</sub> = ",round(exp(X_infer()[5]),3), "and \u00CE<sub>1</sub> = ",round(exp(X_infer()[7]),3))
                          str9 <- paste("<font size=4.5> <b>","95% confidence interval of \u00CE<sub>0</sub> is (",round(l0_CI()[1],3),",",round(l0_CI()[2],3),")")
                          str10 <- paste("<font size=4.5> <b>","95% confidence interval of \u00CE<sub>1</sub> is (",round(l1_CI()[1],3),",",round(l1_CI()[2],3),")")
                          str11 <- paste("<font size=4.5> <b>","Did you data meet the performance goals with statistical significance?",span(c("No","Yes")[1*(round(l0_CI()[2],3) <= round(l0(),3) & round(l1_CI()[1],3) >= round(l1(),3))+1],style = "color:red"))
                          
                          HTML(paste(str7,str8,str9,str10,str11,str6,str1, str2,str3,str4,str5, sep = '<br/>'))
                        }
                  else if (input$Performance=="2" & input$Hypothesis == "1" & input$Goal=="1"){
                    str0 <- paste("<font size=5, color=red > <b>", "NLR Analysis")
                    
              #      str1 <- paste("<font size=4.5> <b>","\u015Ce<sub>new</sub>=", round(X2_infer()[1],3))
               #     str2 <- paste("<font size=4.5> <b>","\u015Cp<sub>new</sub>=", round(X2_infer()[2],3))
                    str3 <- paste("<font size=4.5, color=blue > <b>","\u00CE<sub>0new</sub>=", round(X2_infer()[6],3))
                    
              #      str4 <- paste("<font size=4.5> <b>","\u015Ce<sub>comparator</sub>=", round(X2_infer()[3],3))
              #      str5 <- paste("<font size=4.5> <b>","\u015Cp<sub>comparator</sub>=", round(X2_infer()[4],3))
                    str6 <- paste("<font size=4.5> <b>","\u00CE<sub>0comparator</sub>=", round(X2_infer()[8],3))
                    
                    str7 <- paste("<font size=4.5> <b>","Ratio=\u00CE<sub>0new</sub>/\u00CE<sub>0comparator</sub>=", round(X2_infer()[6]/X2_infer()[8],3),"with 95% CI (",round(exp(log(X2_infer()[6]/X2_infer()[8])-1.96*X2_infer()[10]),3), ",",round(exp(log(X2_infer()[6]/X2_infer()[8])+1.96*X2_infer()[10]),3),")")
                    str8 <- paste("<font size=4.5> <b>","Is the new test superior to the comparator test in rule-out?",span(c("No","Yes")[1*(exp(log(X2_infer()[6]/X2_infer()[8])+1.96*X2_infer()[10])<1)+1],style = "color:red"))
                    
                    
                    HTML(paste(str0,str3,str6,str7,str8, sep = '<br/>'))
#       c(Sp_new,Se_new,Sp_comp,Se_comp,l1_new,l0_new,l1_comp,l0_comp,sd1,sd2)

                    
                  }
                  else if (input$Performance=="2" & input$Hypothesis == "1" & input$Goal=="2"){
              #      str1 <- paste("<font size=4.5> <b>","\u015Ce<sub>new</sub>=", round(X2_infer()[1],3))
              #      str2 <- paste("<font size=4.5> <b>","\u015Cp<sub>new</sub>=", round(X2_infer()[2],3))
                    str0 <- paste("<font size=5, color=red > <b>", "PLR Analysis")
                    str3 <- paste("<font size=4.5, color=blue > <b>","\u00CE<sub>1new</sub>=", round(X2_infer()[5],3))
                    
             #       str4 <- paste("<font size=4.5> <b>","\u015Ce<sub>comparator</sub>=", round(X2_infer()[3],3))
            #        str5 <- paste("<font size=4.5> <b>","\u015Cp<sub>comparator</sub>=", round(X2_infer()[4],3))
                    str6 <- paste("<font size=4.5> <b>","\u00CE<sub>1comparator</sub>=", round(X2_infer()[7],3))
                    
                    str7 <- paste("<font size=4.5> <b>","Ratio=\u00CE<sub>1new</sub>/\u00CE<sub>1comparator</sub>=", round(X2_infer()[5]/X2_infer()[7],3),"with 95% CI (",round(exp(log(X2_infer()[5]/X2_infer()[7])-1.96*X2_infer()[9]),3), ",",round(exp(log(X2_infer()[5]/X2_infer()[7])+1.96*X2_infer()[9]),3),")")
                    str8 <- paste("<font size=4.5> <b>","Is the new test superior to the comparator test in rule-in?",span(c("No","Yes")[1*(exp(log(X2_infer()[5]/X2_infer()[7])-1.96*X2_infer()[9])>1)+1],style = "color:red"))
                    
                    
                    HTML(paste(str0,str3,str6,str7,str8, sep = '<br/>'))
                    #       c(Sp_new,Se_new,Sp_comp,Se_comp,l1_new,l0_new,l1_comp,l0_comp,sd1,sd2)
                  }
                  else if (input$Performance=="2" & input$Hypothesis == "1" & input$Goal=="3"){
            #        str1 <- paste("<font size=4.5> <b>","\u015Ce<sub>new</sub>=", round(X2_infer()[1],3))
            #        str2 <- paste("<font size=4.5> <b>","\u015Cp<sub>new</sub>=", round(X2_infer()[2],3))
                    str0 <- paste("<font size=5, color=red > <b>", "LR Analysis")
                    str3 <- paste("<font size=4.5, color=blue > <b>","\u00CE<sub>1new</sub>=", round(X2_infer()[5],3))
                    str4 <- paste("<font size=4.5> <b>","\u00CE<sub>0new</sub>=", round(X2_infer()[6],3))
                    
                    
           #         str5 <- paste("<font size=4.5> <b>","\u015Ce<sub>comparator</sub>=", round(X2_infer()[3],3))
          #          str6 <- paste("<font size=4.5> <b>","\u015Cp<sub>comparator</sub>=", round(X2_infer()[4],3))
                    str7 <- paste("<font size=4.5> <b>","\u00CE<sub>1comparator</sub>=", round(X2_infer()[7],3))
                    str8 <- paste("<font size=4.5> <b>","\u00CE<sub>0comparator</sub>=", round(X2_infer()[8],3))
                    
                    str9 <- paste("<font size=4.5> <b>","Ratio=\u00CE<sub>0new</sub>/\u00CE<sub>0comparator</sub>=", round(X2_infer()[6]/X2_infer()[8],3),"with 95% CI (",round(exp(log(X2_infer()[6]/X2_infer()[8])-1.96*X2_infer()[10]),3), ",",round(exp(log(X2_infer()[6]/X2_infer()[8])+1.96*X2_infer()[10]),3),")")
                    
                    str10 <- paste("<font size=4.5> <b>","Ratio=\u00CE<sub>1new</sub>/\u00CE<sub>1comparator</sub>=", round(X2_infer()[5]/X2_infer()[7],3),"with 95% CI (",round(exp(log(X2_infer()[5]/X2_infer()[7])-1.96*X2_infer()[9]),3), ",",round(exp(log(X2_infer()[5]/X2_infer()[7])+1.96*X2_infer()[9]),3),")")
     #               str11 <- paste("<font size=4.5> <b>","Is the new test superior to the comparator test?",span(c("No","Yes")[1*(exp(log(X2_infer()[5]/X2_infer()[7])-1.96*X2_infer()[9])>1 & exp(log(X2_infer()[6]/X2_infer()[8])+1.96*X2_infer()[10])<1)+1],style = "color:red"))
                    str11 <- paste("<font size=4.5> <b>","Is the new test superior to the comparator test in rule-out?",span(c("No","Yes")[1*(exp(log(X2_infer()[6]/X2_infer()[8])+1.96*X2_infer()[10])<1)+1],style = "color:red"))
                    str12 <- paste("<font size=4.5> <b>","Is the new test superior to the comparator test in rule-in?",span(c("No","Yes")[1*(exp(log(X2_infer()[5]/X2_infer()[7])-1.96*X2_infer()[9])>1)+1],style = "color:red"))
                    
                    
                    HTML(paste(str0,str3,str4,str7,str8,str9,str10,str11,str12, sep = '<br/>'))
                    #       c(Sp_new,Se_new,Sp_comp,Se_comp,l1_new,l0_new,l1_comp,l0_comp,sd1,sd2)
                  }
                  
                  else if (input$Performance=="2" & input$Hypothesis == "2" & input$Goal=="1"){
                #    str1 <- paste("<font size=4.5> <b>","\u015Ce<sub>new</sub>=", round(X2_infer()[1],3))
              #      str2 <- paste("<font size=4.5> <b>","\u015Cp<sub>new</sub>=", round(X2_infer()[2],3))
                    str0 <- paste("<font size=5, color=red > <b>", "NLR Analysis")
                    str3 <- paste("<font size=4.5, color=blue > <b>","\u00CE<sub>0new</sub>=", round(X2_infer()[6],3))
                    
              #      str4 <- paste("<font size=4.5> <b>","\u015Ce<sub>comparator</sub>=", round(X2_infer()[3],3))
              #      str5 <- paste("<font size=4.5> <b>","\u015Cp<sub>comparator</sub>=", round(X2_infer()[4],3))
                    str6 <- paste("<font size=4.5> <b>","\u00CE<sub>0comparator</sub>=", round(X2_infer()[8],3))
                    
                    str7 <- paste("<font size=4.5> <b>","Ratio=(1-\u00CE<sub>0new</sub>)/(1-\u00CE<sub>0comparator</sub>)=", round((1-X2_infer()[6])/(1-X2_infer()[8]),3),"with 95% CI (",round(exp(log((1-X2_infer()[6])/(1-X2_infer()[8]))-1.96*X2_infer()[12]),3), ",",round(exp(log((1-X2_infer()[6])/(1-X2_infer()[8]))+1.96*X2_infer()[12]),3),")")
                    str8 <- paste("<font size=4.5> <b>","Is the new test non-inferior to the comparator test in rule-out with respect to margin",span(gamma(),style = "color:red"),"?",span(c("No","Yes")[1*(exp(log((1-X2_infer()[6])/(1-X2_infer()[8]))-1.96*X2_infer()[12])>gamma())+1],style = "color:red"))
                    
                    
                    HTML(paste(str0,str3,str6,str7,str8, sep = '<br/>'))
                    #       c(Sp_new,Se_new,Sp_comp,Se_comp,l1_new,l0_new,l1_comp,l0_comp,sd1,sd2,sd3,sd4)
                    
                  }
                  
                  else if (input$Performance=="2" & input$Hypothesis == "2" & input$Goal=="2"){
             #       str1 <- paste("<font size=4.5> <b>","\u015Ce<sub>new</sub>=", round(X2_infer()[1],3))
            #        str2 <- paste("<font size=4.5> <b>","\u015Cp<sub>new</sub>=", round(X2_infer()[2],3))
                    str0 <- paste("<font size=5, color=red > <b>", "PLR Analysis")
                    str3 <- paste("<font size=4.5, color=blue > <b>","\u00CE<sub>1new</sub>=", round(X2_infer()[5],3))
                    
           #         str4 <- paste("<font size=4.5> <b>","\u015Ce<sub>comparator</sub>=", round(X2_infer()[3],3))
          #          str5 <- paste("<font size=4.5> <b>","\u015Cp<sub>comparator</sub>=", round(X2_infer()[4],3))
                    str6 <- paste("<font size=4.5> <b>","\u00CE<sub>1comparator</sub>=", round(X2_infer()[7],3))
                    
                    str7 <- paste("<font size=4.5> <b>","Ratio=(\u00CE<sub>1new</sub>-1)/(\u00CE<sub>1comparator</sub>-1)=", round((X2_infer()[5]-1)/(X2_infer()[7]-1),3),"with 95% CI (",round(exp(log((X2_infer()[5]-1)/(X2_infer()[7]-1))-1.96*X2_infer()[11]),3), ",",round(exp(log((X2_infer()[5]-1)/(X2_infer()[7]-1))+1.96*X2_infer()[11]),3),")")
                    str8 <- paste("<font size=4.5> <b>","Is the new test non-inferior to the comparator test in rule-in with respect to margin",span(gamma(),style = "color:red"),"?",span(c("No","Yes")[1*(exp(log((X2_infer()[5]-1)/(X2_infer()[7]-1))-1.96*X2_infer()[11])>gamma())+1],style = "color:red"))
                    
                    
                    HTML(paste(str0,str3,str6,str7,str8, sep = '<br/>'))
                    #       c(Sp_new,Se_new,Sp_comp,Se_comp,l1_new,l0_new,l1_comp,l0_comp,sd1,sd2,sd3,sd4)
                    
                  }
                  else if (input$Performance=="2" & input$Hypothesis == "2" & input$Goal=="3"){
                    #       str1 <- paste("<font size=4.5> <b>","\u015Ce<sub>new</sub>=", round(X2_infer()[1],3))
                    #        str2 <- paste("<font size=4.5> <b>","\u015Cp<sub>new</sub>=", round(X2_infer()[2],3))
                    str0 <- paste("<font size=5, color=red > <b>", "LR Analysis")
                    
                    str2 <- paste("<font size=4.5, color=blue > <b>","\u00CE<sub>0new</sub>=", round(X2_infer()[6],3))
                    str3 <- paste("<font size=4.5> <b>","\u00CE<sub>1new</sub>=", round(X2_infer()[5],3))
                    
                    #         str4 <- paste("<font size=4.5> <b>","\u015Ce<sub>comparator</sub>=", round(X2_infer()[3],3))
                    #          str5 <- paste("<font size=4.5> <b>","\u015Cp<sub>comparator</sub>=", round(X2_infer()[4],3))
                    str5 <- paste("<font size=4.5> <b>","\u00CE<sub>0comparator</sub>=", round(X2_infer()[8],3))
                    str6 <- paste("<font size=4.5> <b>","\u00CE<sub>1comparator</sub>=", round(X2_infer()[7],3))
                   
                     str7 <- paste("<font size=4.5> <b>","Ratio=(1-\u00CE<sub>0new</sub>)/(1-\u00CE<sub>0comparator</sub>)=", round((1-X2_infer()[6])/(1-X2_infer()[8]),3),"with 95% CI (",round(exp(log((1-X2_infer()[6])/(1-X2_infer()[8]))-1.96*X2_infer()[12]),3), ",",round(exp(log((1-X2_infer()[6])/(1-X2_infer()[8]))+1.96*X2_infer()[12]),3),")")
                    
                    str8 <- paste("<font size=4.5> <b>","Ratio=(\u00CE<sub>1new</sub>-1)/(\u00CE<sub>1comparator</sub>-1)=", round((X2_infer()[5]-1)/(X2_infer()[7]-1),3),"with 95% CI (",round(exp(log((X2_infer()[5]-1)/(X2_infer()[7]-1))-1.96*X2_infer()[11]),3), ",",round(exp(log((X2_infer()[5]-1)/(X2_infer()[7]-1))+1.96*X2_infer()[11]),3),")")
                 #   str9 <- paste("<font size=4.5> <b>","Is the new test non-inferior to the comparator test for margin",span(gamma(),style = "color:red"),"?",span(c("No","Yes")[1*(exp(log((X2_infer()[5]-1)/(X2_infer()[7]-1))-1.96*X2_infer()[11])>gamma() & exp(log((1-X2_infer()[6])/(1-X2_infer()[8]))-1.96*X2_infer()[12])>gamma())+1],style = "color:red"))
                    str9 <- paste("<font size=4.5> <b>","Is the new test non-inferior to the comparator test in rule-out with respect to margin",span(gamma(),style = "color:red"),"?",span(c("No","Yes")[1*(exp(log((1-X2_infer()[6])/(1-X2_infer()[8]))-1.96*X2_infer()[12])>gamma())+1],style = "color:red"))
                    str92 <- paste("<font size=4.5> <b>","Is the new test non-inferior to the comparator test in rule-in with respect to margin",span(gamma(),style = "color:red"),"?",span(c("No","Yes")[1*(exp(log((X2_infer()[5]-1)/(X2_infer()[7]-1))-1.96*X2_infer()[11])>gamma())+1],style = "color:red"))
                    
                    str00 <- paste("")
                  
                    str10 <- paste("<font size=5, color=black > <b>","Independent goals")
                    
                    str11 <- paste("<font size=4.5> <b>","\u015Cp<sub>new</sub>=", round(X2_infer()[1],3))
                    str12 <- paste("<font size=4.5> <b>","\u015Ce<sub>new</sub>=", round(X2_infer()[2],3))
                    str13 <- paste("<font size=4.5> <b>","\u015Cp<sub>comparator</sub>=", round(X2_infer()[3],3))
                    str14 <- paste("<font size=4.5> <b>","\u015Ce<sub>comparator</sub>=", round(X2_infer()[4],3))
                    str15 <- paste("<font size=4.5> <b>","Lower bound of 95% confidence interval of (\u015Cp<sub>new</sub>-\u015Cp<sub>comparator</sub>)=", round(del_sp(),3))
                    str16 <- paste("<font size=4.5> <b>","Lower bound of 95% confidence interval of (\u015Ce<sub>new</sub>-([1-\u03B3](1-\u015Cp<sub>comparator</sub>)+\u03B3*\u015Ce<sub>comparator</sub>))=", round(del_se(),3))
                    str17 <- paste("<font size=4.5> <b>","Is the new test non-inferior to the comparator test with independent goals?",span(c("No","Yes")[1*(del_sp()>0 & del_se()>0)+1],style = "color:red"))
                  
                    HTML(paste(str0,str2,str3,str5,str6,str7,str8,str9,str92,str00,str10,str11,str12,str13,str14,str15,str16,str17, sep = '<br/>'))
                    #       c(Sp_new,Se_new,Sp_comp,Se_comp,l1_new,l0_new,l1_comp,l0_comp,sd1,sd2,sd3,sd4)
                    
                  }
                  
                  else if (input$Performance=="2" & input$Hypothesis == "3" & input$Goal=="1"){
                    str0 <- paste("<font size=5, color=red > <b>", "NLR Analysis")
                    
                    #      str1 <- paste("<font size=4.5> <b>","\u015Ce<sub>new</sub>=", round(X2_infer()[1],3))
                    #     str2 <- paste("<font size=4.5> <b>","\u015Cp<sub>new</sub>=", round(X2_infer()[2],3))
                    str3 <- paste("<font size=4.5, color=blue > <b>","\u00CE<sub>0new</sub>=", round(X2_infer()[6],3))
                    
                    #      str4 <- paste("<font size=4.5> <b>","\u015Ce<sub>comparator</sub>=", round(X2_infer()[3],3))
                    #      str5 <- paste("<font size=4.5> <b>","\u015Cp<sub>comparator</sub>=", round(X2_infer()[4],3))
                    str6 <- paste("<font size=4.5> <b>","\u00CE<sub>0comparator</sub>=", round(X2_infer()[8],3))
                    
                    str7 <- paste("<font size=4.5> <b>","Ratio=\u00CE<sub>0new</sub>/\u00CE<sub>0comparator</sub>-1=", round(X2_infer()[8]/X2_infer()[6]-1,3),"with 95% CI (",round(exp(log(X2_infer()[6]/X2_infer()[8])-1.96*X2_infer()[10])-1,3), ",",round(exp(log(X2_infer()[6]/X2_infer()[8])+1.96*X2_infer()[10])-1,3),")")
             #       str8 <- c(paste("<font size=4.5> <b>","The new test is non-inferior to the comparator test for margins up to",span(paste(100*round(exp(log(X2_infer()[6]/X2_infer()[8])+1.96*X2_infer()[10])-1,3),"%.",sep=""),style = "color:red")),"The new test is non-inferior to the comparator test for any margin.")[1*(exp(log(X2_infer()[6]/X2_infer()[8])+1.96*X2_infer()[10])<1)+1]
                    str9 <- paste("<font size=4.5> <b>","Is the new test non-inferior to the comparator test in rule-out with respect to margin",span(gamma(),style = "color:red"),"?",span(c("No","Yes")[1*(exp(log(X2_infer()[6]/X2_infer()[8])+1.96*X2_infer()[10])-1< gamma())+1],style = "color:red"))
                    
                    
                    HTML(paste(str0,str3,str6,str7,str9, sep = '<br/>'))
                    #       c(Sp_new,Se_new,Sp_comp,Se_comp,l1_new,l0_new,l1_comp,l0_comp,sd1,sd2)
                    
                    
                  }
                  
                  else if (input$Performance=="2" & input$Hypothesis == "3" & input$Goal=="2"){
                    str0 <- paste("<font size=5, color=red > <b>", "PLR Analysis")
                    
                    #      str1 <- paste("<font size=4.5> <b>","\u015Ce<sub>new</sub>=", round(X2_infer()[1],3))
                    #     str2 <- paste("<font size=4.5> <b>","\u015Cp<sub>new</sub>=", round(X2_infer()[2],3))
                    str3 <- paste("<font size=4.5, color= blue > <b>","\u00CE<sub>1new</sub>=", round(X2_infer()[5],3))
                    
                    #      str4 <- paste("<font size=4.5> <b>","\u015Ce<sub>comparator</sub>=", round(X2_infer()[3],3))
                    #      str5 <- paste("<font size=4.5> <b>","\u015Cp<sub>comparator</sub>=", round(X2_infer()[4],3))
                    str6 <- paste("<font size=4.5> <b>","\u00CE<sub>1comparator</sub>=", round(X2_infer()[7],3))
                    
                    str7 <- paste("<font size=4.5> <b>","Ratio=\u00CE<sub>1comparator</sub>/\u00CE<sub>1new</sub>-1=", round(X2_infer()[7]/X2_infer()[5]-1,3),"with 95% CI (",round(exp(log(X2_infer()[7]/X2_infer()[5])-1.96*X2_infer()[9])-1,3), ",",round(exp(log(X2_infer()[7]/X2_infer()[5])+1.96*X2_infer()[9])-1,3),")")
                 #   str8 <- c(paste("<font size=4.5> <b>","The new test is non-inferior to the comparator test for margins up to",span(paste(100*round(exp(log(X2_infer()[7]/X2_infer()[5])+1.96*X2_infer()[9])-1,3),"%.",sep=""),style = "color:red")),"The new test is non-inferior to the comparator test for any margin.")[1*(exp(log(X2_infer()[7]/X2_infer()[5])+1.96*X2_infer()[9])<1)+1]
                    str9 <- paste("<font size=4.5> <b>","Is the new test non-inferior to the comparator test in rule-in with respect to margin",span(gamma(),style = "color:red"),"?",span(c("No","Yes")[1*(exp(log(X2_infer()[7]/X2_infer()[5])+1.96*X2_infer()[9])-1< gamma())+1],style = "color:red"))
                    
                    
                    HTML(paste(str0,str3,str6,str7,str9, sep = '<br/>'))
                    #       c(Sp_new,Se_new,Sp_comp,Se_comp,l1_new,l0_new,l1_comp,l0_comp,sd1,sd2)
                    
                    
                  }
                  
                  else if (input$Performance=="2" & input$Hypothesis == "3" & input$Goal=="3"){
                    str0 <- paste("<font size=5, color=red > <b>", "LR Analysis")
                    
                    #      str1 <- paste("<font size=4.5> <b>","\u015Ce<sub>new</sub>=", round(X2_infer()[1],3))
                    str2 <- paste("<font size=4.5, color= blue > <b>","\u00CE<sub>0new</sub>=", round(X2_infer()[6],3))
                    str3 <- paste("<font size=4.5> <b>","\u00CE<sub>1new</sub>=", round(X2_infer()[5],3))
                    
                    #      str4 <- paste("<font size=4.5> <b>","\u015Ce<sub>comparator</sub>=", round(X2_infer()[3],3))
                    str5 <- paste("<font size=4.5> <b>","\u00CE<sub>0comparator</sub>=", round(X2_infer()[8],3))
                    str6 <- paste("<font size=4.5> <b>","\u00CE<sub>1comparator</sub>=", round(X2_infer()[7],3))
                    
                    str7 <- paste("<font size=4.5> <b>","Ratio=\u00CE<sub>0new</sub>/\u00CE<sub>0comparator</sub>-1=", round(X2_infer()[8]/X2_infer()[6]-1,3),"with 95% CI (",round(exp(log(X2_infer()[6]/X2_infer()[8])-1.96*X2_infer()[10])-1,3), ",",round(exp(log(X2_infer()[6]/X2_infer()[8])+1.96*X2_infer()[10])-1,3),")")
                    str8 <- paste("<font size=4.5> <b>","Ratio=\u00CE<sub>1comparator</sub>/\u00CE<sub>1new</sub>-1=", round(X2_infer()[7]/X2_infer()[5]-1,3),"with 95% CI (",round(exp(log(X2_infer()[7]/X2_infer()[5])-1.96*X2_infer()[9])-1,3), ",",round(exp(log(X2_infer()[7]/X2_infer()[5])+1.96*X2_infer()[9])-1,3),")")
                    
                    str9 <- paste("<font size=4.5> <b>","Is the new test non-inferior to the comparator test in rule-out with respect to margin",span(gamma(),style = "color:red"),"?",span(c("No","Yes")[1*(exp(log(X2_infer()[6]/X2_infer()[8])+1.96*X2_infer()[10])-1< gamma())+1],style = "color:red"))
                    str10 <- paste("<font size=4.5> <b>","Is the new test non-inferior to the comparator test in rule-in with respect to margin",span(gamma(),style = "color:red"),"?",span(c("No","Yes")[1*(exp(log(X2_infer()[7]/X2_infer()[5])+1.96*X2_infer()[9])-1< gamma())+1],style = "color:red"))
                    
                    
                    HTML(paste(str0,str2,str3,str5,str6,str7,str8,str9,str10, sep = '<br/>'))
                    #       c(Sp_new,Se_new,Sp_comp,Se_comp,l1_new,l0_new,l1_comp,l0_comp,sd1,sd2)
                    
                    
                  }

                })
        })
        
        output$Extra_Text <-renderText({
                if (input$goButton == 0) {return()}
                isolate({
                if (input$Performance=="1" & input$Goal=="3"){
                  str1 <- paste("With a given likelihood ratio goals l<sub>0</sub><sup>*</sup>, l<sub>1</sub><sup>*</sup>,
                                there are many triples (p<sub>0</sub><sup>*</sup>, p<sup>*</sup>, p<sub>1</sub><sup>*</sup>). ")}
  

                })
        })
        
        output$Extra_Graph<-renderPlot({  
          if (input$goButton == 0) {return()}
          isolate({
            if (input$Performance=="1" & input$Goal=="3")
            {
              par(mfrow=c(1,1),mai = c(1.0, 1.0, 1,1))
            ####### Plot Triple
            plot(p0_st(),p1_st(),pch=19,lwd=4,col=colr,ylim=c(0.0,1),xlim=c(0.0,1),
                 xaxs = "i",yaxs = "i",cex.axis=1.5,cex.lab=1.5
                 , xlab = expression(paste("Rule-Out Risk Threshold ",p["0"]^"*"))
                 , ylab = expression(paste("Rule-In Risk Threshold ",p["1"]^"*"))
             #    main = c(expression(bold(paste(" Risk Stratification Triples ( ",p["0"]^"*",",", p^"*",",", p["1"]^"*",")"))))
                 )
            mtext(expression(bold(paste(" Risk Stratification Triples ( ",p["0"]^"*",",", p^"*",",", p["1"]^"*",")"))),line=2.8,cex = 1.2)
            mtext(expression(bold('conferring same performance goals for')),line=1.7,cex = 1.2)
            mtext(expression(bold(paste('diagnostic likelihood ratios ', l[0], " and ", l[1] ))),line=0.6,cex = 1.2)

            segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1,lwd=1,col="gray")
            points(p0_st()[t1_st],p1_st()[t1_st],pch=19,lwd=4,col="blue")
            segments(x0 = p0_st()[t1_st], y0 = p1_st()[t1_st], x1 = 0, y1 = p1_st()[t1_st],lwd=2,lty=2)
            segments(x0 = p0_st()[t1_st], y0 = p1_st()[t1_st], x1 = p0_st()[t1_st], y1 = 0,lwd=2,lty=2)
            text(x=p0_st()[t1_st+100],y=p1_st()[t1_st],paste("p=",t0[t1_st]),cex=1.5)

            points(p0_st()[t2_st],p1_st()[t2_st],pch=19,lwd=4,col="blue")
            segments(x0 = p0_st()[t2_st], y0 = p1_st()[t2_st], x1 = 0, y1 = p1_st()[t2_st],lwd=2,lty = 2)
            segments(x0 = p0_st()[t2_st], y0 = p1_st()[t2_st], x1 = p0_st()[t2_st], y1 = 0,lwd=2,lty=2)
            text(x=p0_st()[t2_st+80],y=p1_st()[t2_st],paste("p=",t0[t2_st]),cex=1.5)
            text(x=p0_st()[Num_points*0.75],y=0.3*p1_st()[Num_points*0.75],bquote(l["0"]^"*" == .(round(l0(),3))),cex=1.5)
            text(x=p0_st()[Num_points*0.75],y=0.2*p1_st()[Num_points*0.75],bquote(l["1"]^"*" == .(round(l1(),3))),cex=1.5)
            grid(nx = 10, ny = 10,
                 lty = 2,      # Grid line type
                 col = "gray", # Grid line color
                 lwd = 1)      # Grid line width
            legend.col(col = colr, lev = t0)
            text(x = 1.25, y = 0.5, label = "Prevalence p",srt = 90,cex=1.5) # Rotation
            
            }
            
          })
        })

         output$LRGraph<-renderPlot({  
                if (input$goButton == 0) {return()}
                isolate({
                        if (input$Performance=="1" & input$Goal=="1")
                        {
                                par(mfrow=c(1,1),mai = c(1.0, 1.0, 0.5,0.3))

                                plot(t0,1-l0_CI()[1]+l0_CI()[1]*t0,type="l",lty=2,lwd=1,ylim=c(0.0,1),xlim=c(0.0,1),
                                     xaxs = "i",yaxs = "i",ylab="TPF (Sensitivity)",col="pink",
                                     xlab="FPF (1- Specificity)",cex.axis=1.5,cex.lab=1.5,
                                     main = "Likelihood Ratio Graph")

                                polygon(c(t0, rev(t0)), c(1-l0_CI()[2]+l0_CI()[2]*t0, rev(1-l0_CI()[1]+l0_CI()[1]*t0)), col = "pink", border = NA)
                                lines(t0,1-l0_CI()[2]+l0_CI()[2]*t0,type="l",lty=2,lwd=1,col="pink")
                                lines(t0,t1(),type="l",lty=2,lwd=3)
                                legend(0.41,0.40,legend=c("95% CI band of estimated NLR line"),fill = c("pink"),bty="n",cex=1.5)
                                legend(0.41,0.50,legend=c("Point Estimate and 95% CI for TPF and FPF"),pch = 8,col = "red",lwd=4,cex=1.5,bty="n")
                                
                                grid(nx = 10, ny = 10,
                                     lty = 2,      # Grid line type
                                     col = "gray", # Grid line color
                                     lwd = 1)      # Grid line width
                                
                                text(t0[800],0.85*t1()[800],"   NLR line",cex=1.5)
                                text(t0[round(0.5*(k3()+Num_points-50))],0.5*(t1()[round(0.5*(k3()+Num_points))]+1),"S",cex=2)
                                text(t0[round(0.5*(k3()+Num_points+50))],0.85*t1()[round(0.5*(k3()+Num_points))],"I",cex=2)
                                text(0.68,0.25,"S: NLR goal for confirming absence of target condition",cex=1.5)
                                text(0.60,0.15,"I: Unsatisfactory to confirm absence",cex=1.5)
                                points(1-X_infer()[1],X_infer()[3],pch=8,lwd=4,col="red")
                                arrows(x0=1-X_infer()[1],y0=max(X_infer()[3]+qnorm(alpha/2)*X_infer()[4],0),x1=1-X_infer()[1],y1=min(1,X_infer()[3]+qnorm(1-alpha/2)*X_infer()[4]),
                                       code=3,angle=90,length = 0.1,col="red",lwd=2)
                                
                                arrows(x0=max(1-X_infer()[1]+qnorm(alpha/2)*X_infer()[2],0),y0=X_infer()[3],x1=min(1,1-X_infer()[1]+qnorm(1-alpha/2)*X_infer()[2]),y1=X_infer()[3],
                                       code=3,angle=90,length = 0.1,col="red",lwd=2)
                                
                            #    arrows(0.8*(1-X_infer()[1]),X_infer()[3]-0.1,(1-X_infer()[1])*0.98,(X_infer()[3])*0.98,lwd=3,lty = 1,length = 0.08)
                                
                            #    text(0.8*(1-X_infer()[1]),X_infer()[3]-0.12,"You're here",cex=1.2,col="red",srt = 0)

                                
                        }
                  else if (input$Performance=="1" & input$Goal=="2")
                  {
                          par(mfrow=c(1,1),mai = c(1.0, 1.0, 0.5,0.3))
                          plot(t0,l1_CI()[1]*t0,type="l",lty=2,lwd=1,ylim=c(0.0,1),xlim=c(0.0,1),
                                xaxs = "i",yaxs = "i",ylab="TPF (Sensitivity)",
                                xlab="FPF (1- Specificity)",cex.axis=1.5,cex.lab=1.5,
                                main = "Likelihood Ratio Graph",col="pink")
                          polygon(c(t0, rev(t0)), c(l1_CI()[2]*t0, rev(l1_CI()[1]*t0)), col = "pink", border = NA)
                          lines(t0,l1_CI()[2]*t0,type="l",lty=2,lwd=1,col="pink")
                          lines(t0,t2(),type="l",lty=2,lwd=3)
                          legend(0.41,0.40,legend="95% CI band of estimated PLR line",fill = "pink",bty="n",cex=1.5)
                          legend(0.41,0.50,legend=c("Point Estimate and 95% CI for TPF and FPF"),pch = 8,col = "red",lwd=4,cex=1.5,bty="n")
                          grid(nx = 10, ny = 10,
                               lty = 2,      # Grid line type
                               col = "gray", # Grid line color
                               lwd = 1)      # Grid line width
                          
                          text(t0[150],0.25*t2()[150],"PLR line",cex=1.5)
                          text(t0[round(0.5*k1())],0.5*(t2()[round(0.5*k1())]+1),"S",cex=2)
                          text(t0[k1()],0.75*t2()[k1()],"I",cex=2)
                          text(0.68,0.25,"S: PLR goal for confirming presence of target condition",cex=1.5)
                          text(0.60,0.15,"I: Unsatisfactory to confirm presence",cex=1.5)
                          points(1-X_infer()[1],X_infer()[3],pch=8,lwd=3,col="red")
                          arrows(x0=1-X_infer()[1],y0=max(X_infer()[3]+qnorm(alpha/2)*X_infer()[4],0),x1=1-X_infer()[1],y1=min(1,X_infer()[3]+qnorm(1-alpha/2)*X_infer()[4]),
                                 code=3,angle=90,length = 0.1,col="red",lwd=2)
                          
                          arrows(x0=max(1-X_infer()[1]+qnorm(alpha/2)*X_infer()[2],0),y0=X_infer()[3],x1=min(1,1-X_infer()[1]+qnorm(1-alpha/2)*X_infer()[2]),y1=X_infer()[3],
                                 code=3,angle=90,length = 0.1,col="red",lwd=2)
                          
                    #      arrows(0.8*(1-X_infer()[1]),X_infer()[3]-0.1,(1-X_infer()[1])*0.98,(X_infer()[3])*0.98,lwd=3,lty = 1,length = 0.08)
                          
                    #      text(0.8*(1-X_infer()[1]),X_infer()[3]-0.12,"You're here",cex=1.2,col="red",srt = 0)

                  }
                        else if (input$Performance=="1" & input$Goal=="3")
                        {

                            #    par(mfrow=c(1,1),mai = c(1.0, 0.97, 0.9,1.0))
                                  par(mfrow=c(1,1),mai = c(1.0, 1.0, 0.5,0.3))
                          
                                plot(t0,t1()
                                     ,type="l",lty=2,lwd=3,ylim=c(0,1),xlim=c(0,1),xaxs = "i",yaxs = "i",
                                     ylab="TPF (Sensitivity)",xlab="FPF (1- Specificity)",cex.axis=1.5,cex.lab=1.5,
                                     main = "Likelihood Ratio Graph")
                                
                                grid(nx = 10, ny = 10,
                                     lty = 2,      # Grid line type
                                     col = "gray", # Grid line color
                                     lwd = 1)      # Grid line width
                                 rect(xleft = 0, xright = t0_inter(), ybottom = t1_inter(), ytop = 1,col="lightyellow",border = NA)
                                if(is.na(X())[1,1]=="FALSE" & is.na(X())[1,2]=="FALSE" & is.na(X())[2,1]=="FALSE"&is.na(X())[2,2]=="FALSE"){
                                  polygon(c(seq(0,t0_inter_1(),by=0.01),seq(t0_inter_1(),1,by=0.01), rev(c(seq(0,t0_inter_2(),by=0.01),seq(t0_inter_2(),1,by=0.01)))), c(1-l0_CI()[2]+l0_CI()[2]*seq(0,t0_inter_1(),by=0.01),l1_CI()[1]*seq(t0_inter_1(),1,by=0.01),
                                                                                                                                                                         rev(c(1-l0_CI()[1]+l0_CI()[1]*seq(0,t0_inter_2(),by=0.01),l1_CI()[2]*seq(t0_inter_2(),1,by=0.01)))), col = "pink", border = "pink",lty=2,lwd=3)
                                  # 
                                  # 
                                }

                                   text(t0[800],0.9*t1()[800],"NLR line",cex=1.5)
                                   lines(t0,t1(),type="l",lty=2,lwd=3)
                                   lines(t0,t2(),type="l",lty=1,lwd=3)
                                   text(t0[130],0.25*t2()[130],"PLR line",cex=1.5)
                                   # lines(c(seq(0,t0_inter_1(),by=0.01),seq(t0_inter_1(),1,by=0.01), rev(c(seq(0,t0_inter_2(),by=0.01),seq(t0_inter_2(),1,by=0.01)))),
                                   #       c(1-l0_CI()[2]+l0_CI()[2]*seq(0,t0_inter_1(),by=0.01),l1_CI()[1]*seq(t0_inter_1(),1,by=0.01),
                                   #         rev(c(1-l0_CI()[1]+l0_CI()[1]*seq(0,t0_inter_2(),by=0.01),l1_CI()[2]*seq(t0_inter_2(),1,by=0.01)))), lty=2,lwd=3,col="pink")

                                 segments(x0 = t0_inter(), y0 = 0, x1 = t0_inter(), y1 = t1_inter(),lwd=2,lty=3)
                                 segments(x0 = t0_inter(), y0 = t1_inter(), x1 = t0_inter(), y1 = 1,lwd=3,lty=2,col="gold")
                                 segments(x0 = t0_inter(), y0 = t1_inter(), x1 = 0, y1 = t1_inter(),lwd=3,lty=2,col="gold")
                                 
                                
                                 text(x=t0[round(0.5*k2())],y=0.5*(t1_inter()+1),"S",cex = 2)
                                # 
                                 text(x=t0[round(k2()/2)],y=0.5*(t1()[round(k2()/2)]+t2()[round(k2()/2)]),"P",cex = 2)
                                # 
                                 text(x=t0[round(0.5*(k1()+Num_points))],y=0.5*(t1()[round(0.5*(k1()+Num_points))]+1),"A",cex = 2)
                                 text(t0[k2()+50],0.75*t1()[k2()],"I",cex=2)
                                 legend(0.41,0.60,legend="TPF and FPF goals",fill = "lightyellow",bty="n",cex=1.5)
                            
                                legend(0.41,0.50,legend=c("Point Estimate and 95% CI for TPF and FPF"),pch = 8,col = "red",lwd=4,cex=1.5,bty="n")
                    
                                legend(0.41,0.40,legend="95% CI band of estimated NLR & PLR lines",fill = "pink",bty="n",cex=1.5)
                       
                                text(0.68,0.25,"S+P: PLR goal for confirming presence of target condition",cex=1.5)
                                text(0.68,0.15,"S+A: NLR goal for confirming absence of target condition",cex=1.5)
                                text(0.68,0.05,"I: Unsatisfactory to confirm either presence or absence",cex=1.5)
                                
                          #      text(0.60,0.25,"S: Rule-Out Risk Stratification is Met",cex=1.5)
                          #      text(0.62,0.15,"I: Rule-Out Risk Stratification is Not Met",cex=1.5)
                                
                                points(1-X_infer()[1],X_infer()[3],pch=8,lwd=3,col="red")
                            
                                arrows(x0=1-X_infer()[1],y0=max(X_infer()[3]+qnorm(alpha/2)*X_infer()[4],0),x1=1-X_infer()[1],y1=min(1,X_infer()[3]+qnorm(1-alpha/2)*X_infer()[4]),
                                       code=3,angle=90,length = 0.1,col="red",lwd=2)
                                
                                arrows(x0=max(1-X_infer()[1]+qnorm(alpha/2)*X_infer()[2],0),y0=X_infer()[3],x1=min(1,1-X_infer()[1]+qnorm(1-alpha/2)*X_infer()[2]),y1=X_infer()[3],
                                       code=3,angle=90,length = 0.1,col="red",lwd=2)
                                
                            #    arrows(0.8*(1-X_infer()[1]),X_infer()[3]-0.1,(1-X_infer()[1])*0.98,(X_infer()[3])*0.98,lwd=3,lty = 1,length = 0.08)
                                
                          #      text(0.8*(1-X_infer()[1]),X_infer()[3]-0.12,"You're here",cex=1.2,col="red",srt = 0)

                              #  mtext("*R.S.: Risk Stratification",line=-24,cex = 1)
                               # mtext('*R.S.: Risk Stratification', side=1, line= 4.2, at=0.2,cex=0.9)
                                
                                ######## Plot Triple
                                # plot(p0_st(),p1_st(),pch=19,lwd=4,col=colr,ylim=c(0.0,1),xlim=c(0.0,1),
                                #      xaxs = "i",yaxs = "i",cex.axis=1.5,cex.lab=1.5
                                #      , xlab = expression(paste("Rule-Out Risk Threshold ",p["0"]^"*"))
                                #      , ylab = expression(paste("Rule-In Risk Threshold ",p["1"]^"*"))
                                #  #    main = c(expression(bold(paste(" Risk Stratification Triples ( ",p["0"]^"*",",", p^"*",",", p["1"]^"*",")"))))
                                #      )
                                # mtext(expression(bold(paste(" Risk Stratification Triples ( ",p["0"]^"*",",", p^"*",",", p["1"]^"*",")"))),line=2.8,cex = 1.2)
                                # mtext(expression(bold('conferring same performance goals for')),line=1.7,cex = 1.2)
                                # mtext(expression(bold(paste('diagnostic likelihood ratios ', l[0], " and ", l[1] ))),line=0.6,cex = 1.2)
                                # 
                                # segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1,lwd=1,col="gray")
                                # points(p0_st()[t1_st],p1_st()[t1_st],pch=19,lwd=4,col="blue")
                                # segments(x0 = p0_st()[t1_st], y0 = p1_st()[t1_st], x1 = 0, y1 = p1_st()[t1_st],lwd=2,lty=2)
                                # segments(x0 = p0_st()[t1_st], y0 = p1_st()[t1_st], x1 = p0_st()[t1_st], y1 = 0,lwd=2,lty=2)
                                # text(x=p0_st()[t1_st+200],y=p1_st()[t1_st],paste("p=",t0[t1_st]),cex=1.5)
                                # 
                                # points(p0_st()[t2_st],p1_st()[t2_st],pch=19,lwd=4,col="blue")
                                # segments(x0 = p0_st()[t2_st], y0 = p1_st()[t2_st], x1 = 0, y1 = p1_st()[t2_st],lwd=2,lty = 2)
                                # segments(x0 = p0_st()[t2_st], y0 = p1_st()[t2_st], x1 = p0_st()[t2_st], y1 = 0,lwd=2,lty=2)
                                # text(x=p0_st()[t2_st+150],y=p1_st()[t2_st],paste("p=",t0[t2_st]),cex=1.5)
                                # text(x=p0_st()[Num_points*0.75],y=0.3*p1_st()[Num_points*0.75],bquote(l["0"]^"*" == .(round(l0(),3))),cex=1.5)
                                # text(x=p0_st()[Num_points*0.75],y=0.2*p1_st()[Num_points*0.75],bquote(l["1"]^"*" == .(round(l1(),3))),cex=1.5)
                                # grid(nx = 10, ny = 10,
                                #      lty = 2,      # Grid line type
                                #      col = "gray", # Grid line color
                                #      lwd = 1)      # Grid line width
                                # legend.col(col = colr, lev = t0)
                                # text(x = 1.5, y = 0.5, label = "Prevalence p",srt = 90,cex=1.5) # Rotation
                                
                        }

                })
                
        },width = 950,
        height = 450,)
}
shinyApp(ui = ui, server = server)

