library(shiny)
library(markdown)
library(ggplot2)
library(tidyverse)
library(plyr)
library(dplyr)
library(purrr)
library(ggpmisc)

library(shinythemes)

###	server.R :

server <- function(input, output) {
  
  mydata <- reactive({
    DA <- input$file

    DA_df <- read.csv2(DA$datapath,
                       header = T,
                       sep = ";")
  })
  
  
  ### Importing CSV file to the app :
  # You can access the value of the widget with input$file, e.g.
  output$mytable <- renderTable({
    
    DA <- input$file
    
    if(is.null(DA))
      return("Please select a csv file.")
    
    req(DA)
    
  # when reading semicolon separated files,
  # having a comma separator causes `read.csv` to error
    
    DA_df <- read.csv2(DA$datapath,
                       header = T,
                       sep = ";")
    
  })
  
  output$raw_summary <- renderPrint({
    
    DAs <- input$file
    
    DAs_df <- read.csv(DAs$datapath,
                      header = T,
                      sep = ";")
    
    summary(DAs_df)
    
    })
  
  output$raw_str <- renderPrint({
    
    DAstr <- input$file
    
    DAstr_df <- read.csv(DAstr$datapath,
                       header = T,
                       sep = ";")
    
    str(DAstr_df)
    
  })
  
  output$raw_model <- renderPrint({

    DA <- input$file

    DA_df <- read.csv(DA$datapath,
                      header = T,
                      sep = ";")

  ### Indexes of Cognition tests :
  
    MMSE_ind <- which(colnames(DA_df)=="Total.score..international.")
    DR_ind <- which(colnames(DA_df)=="Delayed.total.recall")
    IR_ind <- which(colnames(DA_df)=="Total_immediate")
  
  ### Indexes of DTI global parameters :
  
    FA_ind <- which(colnames(DA_df)=="FA")
    MD_ind <- which(colnames(DA_df)=="MD")
    AD_ind <- which(colnames(DA_df)=="AD")
    RD_ind <- which(colnames(DA_df)=="RD")
  
  ### Indexes of DTI global parameters :
    
    FA_C_ind <- which(colnames(DA_df)=="FA_C")
    MD_C_ind <- which(colnames(DA_df)=="MD_C")
    AD_C_ind <- which(colnames(DA_df)=="AD_C")
    RD_C_ind <- which(colnames(DA_df)=="RD_C")
  
  ### Indexes of DTI global parameters :
    
    FA_R_ind <- which(colnames(DA_df)=="FA_R")
    MD_R_ind <- which(colnames(DA_df)=="MD_R")
    AD_R_ind <- which(colnames(DA_df)=="AD_R")
    RD_R_ind <- which(colnames(DA_df)=="RD_R")
    
  ### Indexes of DTI global quadratic parameters :
    
    FA2_ind <- which(colnames(DA_df)=="FA2")
    MD2_ind <- which(colnames(DA_df)=="MD2")
    AD2_ind <- which(colnames(DA_df)=="AD2")
    RD2_ind <- which(colnames(DA_df)=="RD2")
    
  ### Indexes of DTI global log parameters :
    
    FA_L_ind <- which(colnames(DA_df)=="FA_L")
    MD_L_ind <- which(colnames(DA_df)=="MD_L")
    AD_L_ind <- which(colnames(DA_df)=="AD_L")
    RD_L_ind <- which(colnames(DA_df)=="RD_L")
    
  ### Indexes of Networks :
    
    DAN_ind <- which(colnames(DA_df)=="DAN")
    DMN_ind <- which(colnames(DA_df)=="DMN")
    FPN_ind <- which(colnames(DA_df)=="FPN")
    LIM_ind <- which(colnames(DA_df)=="LIM")
    SAL_ind <- which(colnames(DA_df)=="SAL")
    SMN_ind <- which(colnames(DA_df)=="SMN")
    VIS_ind <- which(colnames(DA_df)=="VIS")
    LC_ind <- which(colnames(DA_df)=="LC")
    YeLC_ind <- which(colnames(DA_df)=="YeLC")
    
  ### Indexes of quadratic Networks :
    
    DAN2_ind <- which(colnames(DA_df)=="DAN2")
    DMN2_ind <- which(colnames(DA_df)=="DMN2")
    FPN2_ind <- which(colnames(DA_df)=="FPN2")
    LIM2_ind <- which(colnames(DA_df)=="LIM2")
    SAL2_ind <- which(colnames(DA_df)=="SAL2")
    SMN2_ind <- which(colnames(DA_df)=="SMN2")
    VIS2_ind <- which(colnames(DA_df)=="VIS2")
    LC2_ind <- which(colnames(DA_df)=="LC2")
    YeLC2_ind <- which(colnames(DA_df)=="YeLC2")
    
  ### Indexes of log Networks :
    
    DAN_L_ind <- which(colnames(DA_df)=="DAN_L")
    DMN_L_ind <- which(colnames(DA_df)=="DMN_L")
    FPN_L_ind <- which(colnames(DA_df)=="FPN_L")
    LIM_L_ind <- which(colnames(DA_df)=="LIM_L")
    SAL_L_ind <- which(colnames(DA_df)=="SAL_L")
    SMN_L_ind <- which(colnames(DA_df)=="SMN_L")
    VIS_L_ind <- which(colnames(DA_df)=="VIS_L")
    LC_L_ind <- which(colnames(DA_df)=="LC_L")
    YeLC_L_ind <- which(colnames(DA_df)=="YeLC_L")

    MSE_df <- data.frame()
    PV_df <- data.frame()
    VE_df <- data.frame()
    Labs_df <- data.frame()

    if (input$corr == "DA_C_vs_DTI"){
      if (input$model == "linear") {
        if (input$corr_type == "G") {
          for (i in c(MMSE_ind, IR_ind, DR_ind)) {
            for (j in FA_ind:RD_ind){
              obj <- paste0(colnames(DA_df[i]), "_vs_", colnames(DA_df[j]))
              print(obj)
              LM_C_vs_DTI <- lm(DA_df[,i] ~ DA_df[,j], data=DA_df)
              mod_summ <- summary(LM_C_vs_DTI)
              print(mod_summ)
              VE <- mod_summ$r.squared
              cat("R² =", VE, "\n")
              VE_df <- rbind(VE_df, VE)
              MSE <- mean(mod_summ$residuals^2)
              cat("MSE =", MSE,"\n")
              MSE_df <- rbind(MSE_df, MSE)
              GP <- pf(mod_summ$fstatistic[1], mod_summ$fstatistic[2], mod_summ$fstatistic[3], lower.tail = FALSE)
              cat("Global p_value =", GP, "\n")
              PV_df <- rbind(PV_df, GP)
              Labs_df <- rbind(Labs_df, obj)
            }
          }
        }
        if (input$corr_type == "C") {
          for (i in c(MMSE_ind, IR_ind, DR_ind)) {
            for (j in FA_C_ind:RD_C_ind){
              obj <- paste0(colnames(DA_df[i]), "_vs_", colnames(DA_df[j]))
              print(obj)
              LM_C_vs_DTI <- lm(DA_df[,i] ~ DA_df[,j], data=DA_df)
              mod_summ <- summary(LM_C_vs_DTI)                
              print(mod_summ)
              VE <- mod_summ$r.squared
              cat("R² =", VE, "\n")
              VE_df <- rbind(VE_df, VE)
              MSE <- mean(mod_summ$residuals^2)
              cat("MSE =", MSE,"\n")
              MSE_df <- rbind(MSE_df, MSE)
              GP <- pf(mod_summ$fstatistic[1], mod_summ$fstatistic[2], mod_summ$fstatistic[3], lower.tail = FALSE)
              cat("Global p_value =", GP, "\n")
              PV_df <- rbind(PV_df, GP)
              Labs_df <- rbind(Labs_df, obj)
            }
          }
        }
        if (input$corr_type == "R") {
          for (i in c(MMSE_ind, IR_ind, DR_ind)) {
            for (j in FA_R_ind:RD_R_ind){
              obj <- paste0(colnames(DA_df[i]), "_vs_", colnames(DA_df[j]))
              print(obj)
              LM_C_vs_DTI <- lm(DA_df[,i] ~ DA_df[,j], data=DA_df)
              mod_summ <- summary(LM_C_vs_DTI)
              print(mod_summ)
              VE <- mod_summ$r.squared
              cat("R² =", VE, "\n")
              VE_df <- rbind(VE_df, VE)
              MSE <- mean(mod_summ$residuals^2)
              cat("MSE =", MSE,"\n")
              MSE_df <- rbind(MSE_df, MSE)
              GP <- pf(mod_summ$fstatistic[1], mod_summ$fstatistic[2], mod_summ$fstatistic[3], lower.tail = FALSE)
              cat("Global p_value =", GP, "\n")
              PV_df <- rbind(PV_df, GP)
              Labs_df <- rbind(Labs_df, obj)
            }
          }
        }
      }
      
      if (input$model == "quadratic") {
        for (i in c(MMSE_ind, IR_ind, DR_ind)) {
          for (j in FA2_ind:RD2_ind){
            obj <- paste0(colnames(DA_df[i]), "_vs_", colnames(DA_df[j]))
            print(obj)
            QM_C_vs_DTI <- lm(DA_df[,i] ~ DA_df[,j], data=DA_df)
            mod_summ <- summary(QM_C_vs_DTI)
            print(mod_summ)
            VE <- mod_summ$r.squared
            cat("R² =", VE, "\n")
            VE_df <- rbind(VE_df, VE)
            MSE <- mean(mod_summ$residuals^2)
            cat("MSE =", MSE,"\n")
            MSE_df <- rbind(MSE_df, MSE)
            GP <- pf(mod_summ$fstatistic[1], mod_summ$fstatistic[2], mod_summ$fstatistic[3], lower.tail = FALSE)
            cat("Global p_value =", GP, "\n")
            PV_df <- rbind(PV_df, GP)
            Labs_df <- rbind(Labs_df, obj)
          }
        }
      }
      
      if (input$model == "log") {
        for (i in c(MMSE_ind, IR_ind, DR_ind)) {
          for (j in FA_L_ind:RD_L_ind){
            obj <- paste0(colnames(DA_df[i]), "_vs_", colnames(DA_df[j]))
            print(obj)
            LOG_C_vs_DTI <- lm(DA_df[,i] ~ DA_df[,j], data=DA_df)
            mod_summ <- summary(LOG_C_vs_DTI)
            print(mod_summ)
            VE <- mod_summ$r.squared
            cat("R² =", VE, "\n")
            VE_df <- rbind(VE_df, VE)
            MSE <- mean(mod_summ$residuals^2)
            cat("MSE =", MSE,"\n")
            MSE_df <- rbind(MSE_df, MSE)
            GP <- pf(mod_summ$fstatistic[1], mod_summ$fstatistic[2], mod_summ$fstatistic[3], lower.tail = FALSE)
            cat("Global p_value =", GP, "\n")
            PV_df <- rbind(PV_df, GP)
            Labs_df <- rbind(Labs_df, obj)
          }
        }
      }
      
      DA_stats <- cbind(Labs_df, PV_df, VE_df, MSE_df)
      colnames(DA_stats) <- c("Correlations", paste0(input$model, "_", "p-value"), paste0(input$model, "_", "R2"), paste0(input$model, "_", "MSE"))
      write_csv(DA_stats, paste0("./stats_data/", input$corr, "_", input$model, ".csv"))
    }
    
    if (input$corr == "DA_N_vs_DTI"){
      if (input$model == "linear") {
        for (i in DAN_ind:YeLC_ind) {
          for (j in FA_ind:RD_ind){
            obj <- paste0(colnames(DA_df[i]), "_vs_", colnames(DA_df[j]))
            print(obj)
            LM_N_vs_DTI <- lm(DA_df[,i] ~ DA_df[,j], data=DA_df)
            mod_summ <- summary(LM_N_vs_DTI)
            print(mod_summ)
            VE <- mod_summ$r.squared
            cat("R² =", VE, "\n")
            VE_df <- rbind(VE_df, VE)
            MSE <- mean(mod_summ$residuals^2)
            cat("MSE =", MSE,"\n")
            MSE_df <- rbind(MSE_df, MSE)
            GP <- pf(mod_summ$fstatistic[1], mod_summ$fstatistic[2], mod_summ$fstatistic[3], lower.tail = FALSE)
            cat("Global p_value =", GP, "\n")
            PV_df <- rbind(PV_df, GP)
            Labs_df <- rbind(Labs_df, obj)
          }
        }
      }
      
      if (input$model == "quadratic") {
        for (i in DAN_ind:YeLC_ind) {
          for (j in FA2_ind:RD2_ind){
            obj <- paste0(colnames(DA_df[i]), "_vs_", colnames(DA_df[j]))
            print(obj)
            LM_N_vs_DTI <- lm(DA_df[,i] ~ DA_df[,j], data=DA_df)
            mod_summ <- summary(LM_N_vs_DTI)
            print(mod_summ)
            VE <- mod_summ$r.squared
            cat("R² =", VE, "\n")
            VE_df <- rbind(VE_df, VE)
            MSE <- mean(mod_summ$residuals^2)
            cat("MSE =", MSE,"\n")
            MSE_df <- rbind(MSE_df, MSE)
            GP <- pf(mod_summ$fstatistic[1], mod_summ$fstatistic[2], mod_summ$fstatistic[3], lower.tail = FALSE)
            cat("Global p_value =", GP, "\n")
            PV_df <- rbind(PV_df, GP)
            Labs_df <- rbind(Labs_df, obj)
          }
        }
      }
      
      if (input$model == "log") {
        for (i in DAN_ind:YeLC_ind) {
          for (j in FA_L_ind:RD_L_ind){
            obj <- paste0(colnames(DA_df[i]), "_vs_", colnames(DA_df[j]))
            print(obj)
            LM_N_vs_DTI <- lm(DA_df[,i] ~ DA_df[,j], data=DA_df)
            mod_summ <- summary(LM_N_vs_DTI)
            print(mod_summ)
            VE <- mod_summ$r.squared
            cat("R² =", VE, "\n")
            VE_df <- rbind(VE_df, VE)
            MSE <- mean(mod_summ$residuals^2)
            cat("MSE =", MSE,"\n")
            MSE_df <- rbind(MSE_df, MSE)
            GP <- pf(mod_summ$fstatistic[1], mod_summ$fstatistic[2], mod_summ$fstatistic[3], lower.tail = FALSE)
            cat("Global p_value =", GP, "\n")
            PV_df <- rbind(PV_df, GP)
            Labs_df <- rbind(Labs_df, obj)
          }
        }
      }
      
      DA_stats <- cbind(Labs_df, PV_df, VE_df, MSE_df)
      colnames(DA_stats) <- c("Correlations", paste0(input$model, "_", "p-value"), paste0(input$model, "_", "R2"), paste0(input$model, "_", "MSE"))
      write_csv(DA_stats, paste0("./stats_data/", input$corr, "_", input$model, ".csv"))
    }
    
    if (input$corr == "DA_C_vs_N"){
      if (input$model == "linear") {
        for (i in c(MMSE_ind, IR_ind, DR_ind)) {
          for (j in DAN_ind:YeLC_ind){
            obj <- paste0(colnames(DA_df[i]), "_vs_", colnames(DA_df[j]))
            print(obj)
            LM_N_vs_DTI <- lm(DA_df[,i] ~ DA_df[,j], data=DA_df)
            mod_summ <- summary(LM_N_vs_DTI)
            print(mod_summ)
            VE <- mod_summ$r.squared
            cat("R² =", VE, "\n")
            VE_df <- rbind(VE_df, VE)
            MSE <- mean(mod_summ$residuals^2)
            cat("MSE =", MSE,"\n")
            MSE_df <- rbind(MSE_df, MSE)
            GP <- pf(mod_summ$fstatistic[1], mod_summ$fstatistic[2], mod_summ$fstatistic[3], lower.tail = FALSE)
            cat("Global p_value =", GP, "\n")
            PV_df <- rbind(PV_df, GP)
            Labs_df <- rbind(Labs_df, obj)
          }
        }
      }
      
      if (input$model == "quadratic") {
        for (i in c(MMSE_ind, IR_ind, DR_ind)) {
          for (j in DAN2_ind:YeLC2_ind){
            obj <- paste0(colnames(DA_df[i]), "_vs_", colnames(DA_df[j]))
            print(obj)
            LM_N_vs_DTI <- lm(DA_df[,i] ~ DA_df[,j], data=DA_df)
            mod_summ <- summary(LM_N_vs_DTI)
            print(mod_summ)
            VE <- mod_summ$r.squared
            cat("R² =", VE, "\n")
            VE_df <- rbind(VE_df, VE)
            MSE <- mean(mod_summ$residuals^2)
            cat("MSE =", MSE,"\n")
            MSE_df <- rbind(MSE_df, MSE)
            GP <- pf(mod_summ$fstatistic[1], mod_summ$fstatistic[2], mod_summ$fstatistic[3], lower.tail = FALSE)
            cat("Global p_value =", GP, "\n")
            PV_df <- rbind(PV_df, GP)
            Labs_df <- rbind(Labs_df, obj)
          }
        }
      }
      
      if (input$model == "log") {
        for (i in c(MMSE_ind, IR_ind, DR_ind)) {
          for (j in DAN_L_ind:YeLC_L_ind){
            obj <- paste0(colnames(DA_df[i]), "_vs_", colnames(DA_df[j]))
            print(obj)
            LM_N_vs_DTI <- lm(DA_df[,i] ~ DA_df[,j], data=DA_df)
            mod_summ <- summary(LM_N_vs_DTI)
            print(mod_summ)
            VE <- mod_summ$r.squared
            cat("R² =", VE, "\n")
            VE_df <- rbind(VE_df, VE)
            MSE <- mean(mod_summ$residuals^2)
            cat("MSE =", MSE,"\n")
            MSE_df <- rbind(MSE_df, MSE)
            GP <- pf(mod_summ$fstatistic[1], mod_summ$fstatistic[2], mod_summ$fstatistic[3], lower.tail = FALSE)
            cat("Global p_value =", GP, "\n")
            PV_df <- rbind(PV_df, GP)
            Labs_df <- rbind(Labs_df, obj)
          }
        }
      }
      
      DA_stats <- cbind(Labs_df, PV_df, VE_df, MSE_df)
      colnames(DA_stats) <- c("Correlations", paste0(input$model, "_", "p-value"), paste0(input$model, "_", "R2"), paste0(input$model, "_", "MSE"))
      write_csv(DA_stats, paste0("./stats_data/", input$corr, "_", input$model, ".csv"))
    }

  })
  
  stats_data <- reactive({
    read.csv2(paste0("./stats_data/", input$corr, "_", input$model, ".csv"), header = T, sep = ",")
  })
  
  output$table_stats <- renderTable({
    stats_data()
  })
  
  x_val <- reactive({input$x_val})
  y_val <- reactive({input$y_val})
  
  
  
  output$corrplot <- renderPlot({
    
    cols = c("linear_model" = "blue", "quadratic_model" = "red", "log_model" = "green") #, "quadratic_model" = "red", "log_model" = "green"
    
    DA <- input$file
    
    DA_df <- read_csv(DA$datapath, 
                      col_names = T)
    
    DA_in_df_no_na <- na.omit(data.frame(cbind(paste0("DA_df$", y_val()), paste0("DA_df$", x_val()), paste0("DA_df$", y_val(), "2"), paste0("DA_df$", x_val(), "2")), paste0("DA_df$", y_val(), "_L"), paste0("DA_df$", x_val(), "_L")))
    colnames(DA_in_df_no_na) <- c(paste0(y_val()), paste0(x_val()), paste0(y_val(), "2"), paste0(x_val(), "2"), paste0(y_val(), "_L"), paste0(x_val(), "_L"))
    
    LM <- lm(DA_in_df_no_na[, 1] ~ DA_in_df_no_na[, 2])
    lmPC <- predict(LM)
    lmPC_df <- data.frame(PClmRD_C)
    lmPC_df$model <- "LM"
    colnames(lmPC_df) <- c("predict_values", "model")
    LM_coef<-as.data.frame(predict(LM, interval ="confidence"))
    
    QM <- lm(DA_in_df_no_na[, 3] ~ DA_in_df_no_na[, 2] + DA_in_df_no_na[, 4], list(DA_in_df_no_na[, 1], DA_in_df_no_na[, 3]))
    qmPC <- predict(QM)
    qmPC_df <- data.frame(PCqRD_C)
    qmPC_df$model <- "QM"
    colnames(qmPC_df) <- c("predict_values", "model")
    QM_coef<-as.data.frame(predict(QM, interval ="confidence"))
    
    LOGM <- lm(DA_in_df_no_na[, 5] ~ DA_in_df_no_na[, 6])
    logPC <- predict(logRD_C)
    logPC_df <- data.frame(PClogRD_C)
    logPC_df$model <- "LOGM"
    colnames(logPC_df) <- c("predict_values", "model")
    LOGM_C_coef<-as.data.frame(predict(LOGM, interval ="confidence"))
    
    ggplot(aes_string(y = y_val(), x = x_val()), data = DA_in_df_no_na) +
      geom_point() +
      geom_smooth(aes(y = predict_values, col = "linear_model"), data = lmPC_df) + #Linear model
      geom_smooth(aes(y = predict_values, col = "quadratic_model"), data = qmPC_df) + #quadratic model
      geom_smooth(aes(y = predict_values,  color = "log_model"), data = logPC_df) + #logarithmic model
      #scale_color_manual(values = c("blue", "red"))+
      #scale_x_continuous(breaks = seq(26, 30, 2))+
      ggtitle(paste0(y_val(), " vs ", x_val())) +
      xlab(x_val()) +
      ylab(y_val()) +
      theme(
        plot.background = element_rect(fill = "white", colour = "black"), 
        panel.background = element_rect(fill = "white", colour = "black", size = 0.5, linetype = "solid"),
        plot.margin = margin(1, 1 , 0.2, 0.2, "cm"),
        plot.title= element_text(hjust = 0.5, face="bold", colour="black", vjust=0.5, size=15),	
        panel.grid.major = element_line(colour = "grey90", size = 0.5, linetype = "solid"),
        axis.title.x = element_text(colour="black", size=12),
        axis.text.x  = element_text(colour="black", vjust=0.5, size=12),
        axis.title.y  = element_text(hjust=0.5, colour="black", size=12), 
        axis.text.y  = element_text(colour="black", vjust=0.5, size=12),
        legend.position = "top") +
      geom_ribbon(aes(ymin=LM_coef$lwr, ymax=LM_coef$upr), fill="blue",alpha=0.4) +
      geom_ribbon(aes(ymin=QM_coef$lwr, ymax=QM_coef$upr), fill="red",alpha=0.2) +
      geom_ribbon(aes(ymin=LOGM_coef$lwr, ymax=LOGM_coef$upr), fill="green",alpha=0.2) +
      labs(color = "Legend : ") +
      scale_color_manual(values = cols)
  })
  
  # output$statplot <- renderPlot({
  #   
  # })
  
}

###	ui.R :

ui <- fluidPage(
  shinythemes::themeSelector(),
  pageWithSidebar(
    headerPanel("Demographic Analysis of Cognition, Network and LC Diffusion data"),
    sidebarPanel(
      fileInput("file", label = h3("CSV File input : ")),
      hr(),
      radioButtons("corr", label = h3("Correlations : "), 
                         choices = list("Cognition tests vs DTI metrics" = "DA_C_vs_DTI", "Networks vs DTI metrics" = "DA_N_vs_DTI", "Cognition tests vs Networks" = "DA_C_vs_N"),
                         selected = "DA_C_vs_DTI"),
      hr(),
      radioButtons("model", label = h3("Model selection : "), 
                         choices = list("Linear model" = "linear", "Qudratic model" = "quadratic", "Logarithmic model" = "log"),
                         selected = "linear"),
      hr(),
      radioButtons("group", label = h3("Group selection : "), 
                         choices = list("All" = "all_subjs", "CU and CI" = "CU_CI"),
                         selected = "all_subjs"),
      radioButtons("corr_type", label = h3("DTI metrics type : "), 
                   choices = list("Global" = "G", "Caudal" = "C", "Rostral" = "R"),
                   selected = "G"),
      hr(),
      textInput("y_val", label = h3("Cognition test/Network"), value = "Enter y value..."),
      hr(),
      textInput("x_val", label = h3("DTI/Network"), value = "Enter x value..."),
      # selectInput("stat", label = h3("Model statistics to plot : "), 
      #                    choices = list("p-value" = 9, "R2" = 10, "MSE" = 11),
      #                    selected = 9),
      hr()
    ),
  # Copy the line below to make a file upload manager
  mainPanel(
    tabsetPanel(
      tabPanel("About", includeMarkdown("DA_shiny_app_about.md")),
      tabPanel("Data", tableOutput("mytable")),
      tabPanel("Summary", verbatimTextOutput("raw_summary")),
      tabPanel("Variables", verbatimTextOutput("raw_str")),
      tabPanel("Models", verbatimTextOutput("raw_model")),
      tabPanel("Model Statistics", tableOutput("table_stats")),
      tabPanel("Graphs", plotOutput("corrplot"))
      )
    )
  )
)

shinyApp(ui, server)


