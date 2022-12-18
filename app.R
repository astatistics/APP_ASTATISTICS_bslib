rm(list = ls(all=T))
gc(reset = T)
cat("\014")
options(java.parameters = "-Xmx4024m")
options(scipen = 99999)

#### PACKAGES ####

if (!require(pacman)) {install.packages("pacman")}
pacman::p_load("pacman","beepr","data.table","dplyr","googledrive","lubridate","stringr","googlesheets","matrixStats","plotly","readr","readxl","reshape2","rhandsontable","rJava","RJDBC","rlang","RODBC", "scales", "shiny","shinydashboard","sqldf","tidyr","xlsx", "utils","writexl")

#### CONNECTIONS ####

##### Functions #####

'%!in%' <- function(x,y)!('%in%'(x,y))

SecMaxF <- function (x) { max( x[x!=max(x)] ) }

Multiplicate <- function(DataFrameColumn, Value){
  DataFrameColumn * Value}

Divide <- function(DataFrameColumn, Value){
  DataFrameColumn / Value}

##### INFO #####

ui <- dashboardPage(
  # Application title
  dashboardHeader(title = div(img(src = "Nielsen.PNG", height = 40, width = 45), "DATA CHECK SCANP")),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebarid",
                #menuItem("HOME", tabName = "HOME",icon("th")),
                menuItem("DWH", tabName = "DWH",icon = icon("database")),
                menuItem("SIRVAL", tabName = "SIRVAL",icon = icon("question")),
                menuItem("SURGERY", tabName = "SURGERY",icon = icon("stumbleupon")),
                menuItem("LINKS", tabName = "LINKS",icon = icon("info")),
                conditionalPanel( 'input.sidebarid == "DWH"',
                                  br(),
                                  textInput("periodicity", "1 (WEEKLY), 2 (MONTHLY), 3(BIMESTRAL FOOD), 4 (BIMESTRAL DRUG)"),
                                  textInput("text_1", "Enter your periods separated by coma here"),
                                  textInput("text_2", "Enter your FDS"),
                                  textInput("text_3", "Enter your MAS_ID"),
                                  textInput("text_4", "Enter your TAG"),
                                  selectInput("text_5",
                                              label = "",
                                              choices =  "",
                                              selected = "",multiple = FALSE, width = "100%"),
                                  actionButton("go", "GET DWH"),
                                  br(),
                                  helpText(" Select the download format"),
                                  radioButtons("type_1","Format type:",
                                               choices = c("EXCEL(CSV)","EXCEL")),
                                  br(),
                                  helpText("DOWNLOAD DWH"),
                                  
                                  # Button
                                  downloadButton("downloadData_dwh", "Download")),
                conditionalPanel( 'input.sidebarid == "SIRVAL"',
                                  br(),
                                  textInput("periodicity_1", "1 (WEEKLY), 2 (MONTHLY), 3(BIMESTRAL FOOD), 4 (BIMESTRAL DRUG)"),
                                  textInput("text_1_1", "Enter your periods separated by coma here"),
                                  textInput("text_2_1", "Enter your FDS"),
                                  selectInput("text_3_1",label = "",
                                              choices = "",selected = "",multiple = F,width = "100%"),
                                  br(),
                                  actionButton("go_1", "GET SIRVAL QUESTION"),
                                  br(),
                                  helpText("DOWNLOAD SIRVAL QUESTIONING"),
                                  
                                  # Button
                                  downloadButton("downloadData_sirval", "Download")),
                conditionalPanel( 'input.sidebarid == "SURGERY"',
                                  br(),
                                  textInput("text_1_2", "Enter your adjust percent (%) "),
                                  selectInput("text_2_2",label = "TYPE OF CORRECTION",
                                              choices = c("PAST PERIOD","BALANCING","DELETE"),selected = "BALANCING",multiple = F,width = "100%"),
                                  br(),
                                  actionButton("go_2", "GET SURGERY"),
                                  br(),
                                  helpText("DOWNLOAD SURGERY FILE"),
                                  
                                  # Button
                                  downloadButton("downloadData_surgery", "Download"))
    )),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "DWH",
              h1("DWH",style = "text-align: center;"),
              fluidRow(column(7,DT::dataTableOutput('table_dwh',width = 620)))
      ),
      tabItem(tabName = "SIRVAL",strong('Download input headers'),
              downloadButton("Download_INPUT","Download"),
              fileInput("INSUMO","Load Input"),
              fluidRow(column(7,DT::dataTableOutput('table_output',width = 620))),
              p('You choice'),
              fluidRow(column(7,DT::dataTableOutput('table_input',width = 620)))
              
      ),
      tabItem(tabName = "SURGERY",
              p('You choice'),
              fluidRow(
                rHandsontableOutput("table_surgery")
              )
              
      ),
      tabItem(tabName = "LINKS",
              h1("LINKS SURGERY",style = "text-align: center;"),
              fluidRow(column(7,DT::dataTableOutput('links',width = 620)))
      )
    )           
  )
)

server <- function(input, output,session) {
  
  observe({
    
    if(input$text_2 != ""){
      
      FDS_TOT <- input$text_2
      
      UNI <- dbGetQuery(conn_m,paste0("SELECT DISTINCT FWC_PRIORITY as ID, b.FWC_CUN_DESCRIPTION AS UNIDAD
                                     FROM NRSP_V.TRAG_ASSIGNED_WEIGHT_VALUE a
                                     inner join NRSP_V.TRDS_FDS_WEIGHT_CODE b on (AWV_FDS_ID = FWC_FDS_ID AND FWC_CUN_ID = AWV_CUN_ID)
                                     WHERE AWV_FDS_ID IN (",FDS_TOT,") 
                                     order by 1
                                      "))
      
      
      UNID <- data.frame(FDS = FDS_TOT,ID = UNI$ID,UNIDAD = UNI$UNIDAD,stringsAsFactors = F)
      
      updateSelectInput(session,inputId = "text_5", label = paste0("Choose UNID: ",paste0(paste0(UNID$ID,"-",UNID$UNIDAD),collapse = ",")),
                        choices = unique(UNID$ID),selected = unique(UNID$ID)[1])
      
    }
    
  })
  
  dwh_out <- eventReactive(input$go, {
    
    
    withProgress(message = "Application loading", value = 0, {
      # some code
      # ... your code, e.g.:
      Sys.sleep(1)
      # increase progress by 20%
      incProgress(0.2, detail = "Do something")
      #do something
      
      
      CURRENT <- as.integer(unlist(str_split(input$text_1,",")))
      
      UNI <- dbGetQuery(conn_m,paste0("SELECT DISTINCT FWC_PRIORITY as ID, b.FWC_CUN_DESCRIPTION AS UNIDAD
                                     FROM NRSP_V.TRAG_ASSIGNED_WEIGHT_VALUE a
                                     inner join NRSP_V.TRDS_FDS_WEIGHT_CODE b on (AWV_FDS_ID = FWC_FDS_ID AND FWC_CUN_ID = AWV_CUN_ID)
                                     WHERE AWV_FDS_ID IN (",input$text_2,") AND
                                     FWC_PRIORITY = ",input$text_5,"
                                      "))
      UNID    <- data.frame(FDS = input$text_2,ID = input$text_5,UNIDAD = UNI$UNIDAD)
      
      TREND <- DWH_EFORTE2(PERIODICITY = as.integer(input$periodicity),
                           PERIODOS = CURRENT,
                           FDS_TOT  = input$text_2,
                           MERCADO  = input$text_3,
                           TAG      = input$text_4,
                           ID       = input$text_5)
      # ... your code, e.g.:
      Sys.sleep(2)
      # a_flag=1
      # increase 50%
      incProgress(0.5, detail = "Finish something")
      # finish something
      return(TREND)
      # ... your code
      incProgress(0.3, detail = "Done")
    })
    
  })
  
  fileext1 <- reactive({
    switch(input$type_1,
           "EXCEL(CSV)" = "csv", "EXCEL" = "xlsx")
    
  })
  
  output$table_dwh <- DT::renderDataTable({
    DT::datatable(data = dwh_out(),
                  options = list(scrollX = TRUE))
  })
  
  output$downloadData_dwh <- downloadHandler(
    filename = function() {
      paste(paste0('DWH_',min(unlist(str_split(input$text_1,","))),"-",max(unlist(str_split(input$text_1,","))),"_",input$text_2,"_",input$text_3), fileext1(), sep = ".")
    },
    content = function(file) {
      
      tip <- switch(input$type_1,"EXCEL(CSV)" = 1,"EXCEL" = 2)              
      
      if(tip == 1){
        
        write.csv(dwh_out(), file, row.names = FALSE)
        
      }  
      if(tip == 2){
        
        write_xlsx(list('Simulation' = dwh_out()), path = file)
        
        
      }
      
    }
  )
  
  INPUT_HEA <- data.frame("AC_NSHOPID" = NA,"F_NAN_KEY" = NA)
  
  output$Download_INPUT <- downloadHandler(
    filename = function() {
      paste("INPUT_HEADERS.csv")
    },
    content = function(file) {
      
      
      write.csv(INPUT_HEA, file, row.names = FALSE)
      
    }  
    
    
  )
  
  
  observe({
    
    if(input$text_2_1 != ""){
      
      FDS_TOT <- input$text_2_1
      
      UNI <- dbGetQuery(conn_m,paste0("SELECT DISTINCT FWC_PRIORITY as ID, b.FWC_CUN_DESCRIPTION AS UNIDAD
                                     FROM NRSP_V.TRAG_ASSIGNED_WEIGHT_VALUE a
                                     inner join NRSP_V.TRDS_FDS_WEIGHT_CODE b on (AWV_FDS_ID = FWC_FDS_ID AND FWC_CUN_ID = AWV_CUN_ID)
                                     WHERE AWV_FDS_ID IN (",FDS_TOT,") 
                                     order by 1
                                      "))
      
      
      UNID <- data.frame(FDS = FDS_TOT,ID = UNI$ID,UNIDAD = UNI$UNIDAD,stringsAsFactors = F)
      
      updateSelectInput(session,inputId = "text_3_1", label = paste0("Choose UNID: ",paste0(paste0(UNID$ID,"-",UNID$UNIDAD),collapse = ",")),
                        choices = unique(UNID$ID),selected = unique(UNID$ID)[1])
      
    }
    
  })
  
  
  sirval_out <- eventReactive(input$INSUMO, {
    
    
    withProgress(message = "Application loading", value = 0, {
      # some code
      # ... your code, e.g.:
      PERIODS <- as.integer(unlist(str_split(input$text_1_1,",")))
      CURRENT <- max(PERIODS)
      BEFORE  <- sort(PERIODS,decreasing = T)[2]
      
      Sys.sleep(1)
      # increase progress by 20%
      incProgress(0.2, detail = "Do something")
      #do something
      infile <- input$INSUMO
      
      if(is.null(infile))
        return("File not loaded")
      
      DF <- read.csv(infile$datapath,header = TRUE,sep = ",")
      
      PERIODICITY <- input$periodicity_1
      
      
      tam <- floor(length(unique(DF$AC_NSHOPID))/1000)
      
      if(tam == 0){
        
        
        RAW_DATA <- dbGetQuery(conn_s,paste0("SELECT A.*,B.AC_CREFDESCRIPTION FROM VLDSYS_BR.RAWDATA",ifelse(PERIODICITY == 1,"",
                                                                                                             ifelse(PERIODICITY == 2,"_mm",
                                                                                                                    ifelse(PERIODICITY == 3,"_bi","_bm")))," A
                                             FULL JOIN VLDSYS_BR.DESCRIPTIONS B ON  A.AC_CREF = B.AC_CREF AND A.NC_HASH_SIGNATURE = B.NC_HASH_SIGNATURE                                                 
                                             WHERE AC_NSHOPID IN (",paste0(paste0("'",unique(DF$AC_NSHOPID),"'"),collapse = ","),")
                                             AND NC_PERIODID IN (",paste0(c(PERIODS + 989),collapse = ","),")
                                             AND F_NAN_KEY IN (",paste0(unique(DF$F_NAN_KEY),collapse = ","),")"))
        
      }else{
        
        RAW_DATA <- data.frame()     
        
        BLOCKS <- matrix(unique(DF$AC_NSHOPID)[1:(1000*tam)],ncol = tam)
        
        print(paste0("Hay ",ifelse(length(unique(DF$AC_NSHOPID)) %% 1000 == 0,tam,tam + 1)," bloques de tiendas"))
        
        for(block in 1:(ifelse(length(unique(DF$AC_NSHOPID)) %% 1000 == 0,tam,tam + 1))){
          
          if(block != tam + 1){
            
            SHOP_AUX <- BLOCKS[,block]
            
          }else{
            
            SHOP_AUX <- unique(DF$AC_NSHOPID)[-(1:(1000*tam))]  
            
          }
          
          
          RAW_DATA_AUX <- dbGetQuery(conn_s,paste0("SELECT A.*,B.AC_CREFDESCRIPTION FROM VLDSYS_BR.RAWDATA",ifelse(PERIODICITY == 1,"",
                                                                                                                   ifelse(PERIODICITY == 2,"_mm",
                                                                                                                          ifelse(PERIODICITY == 3,"_bi","_bm")))," A
                                             FULL JOIN VLDSYS_BR.DESCRIPTIONS B ON  A.AC_CREF = B.AC_CREF AND A.NC_HASH_SIGNATURE = B.NC_HASH_SIGNATURE                                                 
                                             WHERE AC_NSHOPID IN (",paste0(paste0("'",SHOP_AUX,"'"),collapse = ","),")
                                             AND NC_PERIODID IN (",paste0(c(PERIODS + 989),collapse = ","),")
                                             AND F_NAN_KEY IN (",paste0(unique(DF$F_NAN_KEY),collapse = ","),")"))
          
          
          RAW_DATA <- rbind(RAW_DATA,RAW_DATA_AUX) 
          
          print(paste0("Se completaron ",block," bloques de tiendas faltan ",ifelse(length(unique(DF$AC_NSHOPID)) %% 1000 == 0,tam,tam + 1) - block," bloques de tiendas")) 
          
        }
        
      }
      
      RAW_DATA <- DF %>% inner_join(RAW_DATA,by = c("AC_NSHOPID","F_NAN_KEY"))
      
      CHAIN_N <- CHAIN_NAME(conn_m,CHANNEL = c("SCAN","WKSP","MONT","DRUG","FOOD"),SHOPS = unique(RAW_DATA$AC_NSHOPID))
      
      RAW_DATA <- RAW_DATA %>% left_join(CHAIN_N,by = c("AC_NSHOPID" = "SHO_EXTERNAL_CODE"))
      
      
      # ... your code, e.g.:
      Sys.sleep(2)
      # a_flag=1
      # increase 50%
      incProgress(0.5, detail = "Finish something")
      # finish something
      return(RAW_DATA)
      # ... your code
      incProgress(0.3, detail = "Done")
    })
    #CONCENTRATION <- TREND[[2]]
  })
  
  sirval_out_1 <- eventReactive(input$go_1,{
    
    
    withProgress(message = "Application loading", value = 0, {
      # some code
      # ... your code, e.g.:
      PERIODS <- as.integer(unlist(str_split(input$text_1_1,",")))
      CURRENT <- max(PERIODS)
      BEFORE  <- sort(PERIODS,decreasing = T)[2]
      
      Sys.sleep(1)
      # increase progress by 20%
      incProgress(0.2, detail = "Do something")
      #do something
      if(as.integer(input$periodicity_1) == 1){
        
        RETAILER_SHOP <- dbGetQuery(conn_s,paste0("SELECT A.AC_NSHOPID,A.AC_SHOPDESCRIPTION,A.AC_RETAILER,B.AC_cSHOPID AS RETAILER_SHOP FROM VLDSYS_BR.STORES A
                                           JOIN VLDSYS_BR.LBATCHSTORES B ON A.AC_NSHOPID = B.AC_NSHOPID
                                           WHERE A.AC_NSHOPID IN (",paste0(paste0("'",unique(sirval_out()$AC_NSHOPID),"'"),collapse = ","),")")) %>%
          filter(!RETAILER_SHOP %in% unique(sirval_out()$AC_NSHOPID))
        
        RAW_DATA  <- sirval_out() %>% mutate(PRECO = NC_SLOT2/(NC_SLOT1*NC_CONV)) %>% dplyr::select(NC_PERIODID,
                                                                                                    AC_NSHOPID,
                                                                                                    NAN_KEY = F_NAN_KEY,
                                                                                                    BARCODE = AC_CREF,
                                                                                                    VENDAS_UND = NC_SLOT1,
                                                                                                    VENDAS_VALOR = NC_SLOT2,
                                                                                                    PRECO,
                                                                                                    AC_CREFDESCRIPTION,
                                                                                                    NC_CONV,
                                                                                                    CADENA)
        
        
      }else{
        
        RETAILER_SHOP <- dbGetQuery(conn_s,paste0("SELECT A.AC_NSHOPID,A.AC_SHOPDESCRIPTION,A.AC_RETAILER,B.AC_cSHOPID AS RETAILER_SHOP FROM VLDSYS_BR.STORES A
                                           JOIN VLDSYS_BR.LBATCHSTORES B ON A.AC_NSHOPID = B.AC_NSHOPID
                                           WHERE A.AC_NSHOPID IN (",paste0(paste0("'",unique(sirval_out()$AC_NSHOPID),"'"),collapse = ","),")")) %>% unique()
        
        
        RAW_DATA  <- sirval_out() %>% filter(AC_DTGROUP %like% 'AUDIT_DTYPE') %>% 
          mutate(VENDAS_VALOR = NC_SLOT4*NC_SLOT3) %>% 
          dplyr::select(NC_PERIODID,
                        AC_NSHOPID,
                        NAN_KEY = F_NAN_KEY,
                        BARCODE = AC_CREF,
                        VENDAS_UND = NC_SLOT4,
                        VENDAS_VALOR,
                        PRECO = NC_SLOT3,
                        AC_CREFDESCRIPTION,
                        NC_CONV,
                        CADENA)
        
      }
      
      DESCRIPTION_NANKEY <- dbGetQuery(conn_s,paste0("SELECT F_NAD_NAN_KEY AS NAN_KEY,
                                                       F_NAD_DESC AS DESCRIPTION
                                                FROM VLDSYS_BR.NAN_ALT_DESCS 
                                                WHERE F_NAD_NAN_KEY in (",paste0(unique(sirval_out()$F_NAN_KEY),collapse = ","),")" ))
      
      if(as.integer(input$periodicity_1)  == 1){
        
        PERIOD_AUDIT <- get_sot_periods(conn_m,seq(CURRENT+5,min(PERIODS,na.rm = T)),2)
        
        
        NAME_PER <- dbGetQuery(conn_s,paste0("SELECT NC_PERIODID-989 AS PERIOD_AUDIT,
                                             NC_PERIODYEARPERSEQ AS MONTH_ID,
                                             NC_PERIODYEAR AS YEAR
                                      FROM vldsys_br.periods_mm
                                      WHERE NC_PERIODID - 989 IN (",paste0(unique(PERIOD_AUDIT$PERIOD_AUDIT[PERIOD_AUDIT$PERIOD_SOT %in% c(PERIODS)]),collapse = ","),")")) %>%
          left_join(data.frame(MONTH_ID = 1:12,NAME_PER = c('Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun','Jul','Ago','Set', 'Out', 'Nov','Dez'),stringsAsFactors = F)) %>% arrange(PERIOD_AUDIT)
        
        
        SOT <- PERIOD_AUDIT %>% left_join(NAME_PER %>% mutate(SOT = paste0(NAME_PER,"-",str_sub(YEAR,3,4))),)
        
      }else{
        
        NAME_PER <- dbGetQuery(conn_s,paste0("SELECT NC_PERIODID-989 AS PERIOD_AUDIT,
                                             NC_PERIODYEARPERSEQ AS MONTH_ID,
                                             NC_PERIODYEAR AS YEAR
                                      FROM vldsys_br.periods_mm
                                      WHERE NC_PERIODID - 989 IN (",paste0(PERIODS,collapse = ","),")")) %>%
          left_join(data.frame(MONTH_ID = 1:12,NAME_PER = c('Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun','Jul','Ago','Set', 'Out', 'Nov','Dez'),stringsAsFactors = F)) %>% arrange(PERIOD_AUDIT)
        
        
        SOT <- (NAME_PER %>% mutate(SOT = paste0(NAME_PER,"-",str_sub(YEAR,3,4)),
                                    PERIOD_SOT = PERIOD_AUDIT))
        
      }
      
      fc_temp <- dbGetQuery(conn_m,paste0("SELECT A.AWV_ITM_ID AS NAN_KEY,
                              A.AWV_WEIGHT_VALUE AS FATOR_CONV
                       FROM NRSP_V.TRAG_ASSIGNED_WEIGHT_VALUE A
                       INNER JOIN NRSP_V.TRDS_FDS_WEIGHT_CODE B 
                       ON (AWV_FDS_ID = FWC_FDS_ID AND FWC_CUN_ID = AWV_CUN_ID)
                       WHERE AWV_FDS_ID IN (",paste0(input$text_2_1,collapse = ","),")
                       AND FWC_PRIORITY = ",input$text_3_1)) 
      
      
      CUESTIONAMIENTO <- RAW_DATA %>% 
        left_join(RETAILER_SHOP) %>% 
        left_join(DESCRIPTION_NANKEY) %>% 
        left_join(fc_temp) %>% 
        left_join(SOT %>% mutate(NC_PERIODID = PERIOD_SOT + 989) %>% 
                    dplyr::select(NC_PERIODID,SOT) %>% unique()) %>% 
        arrange(AC_NSHOPID,NC_PERIODID) %>% 
        dplyr::select(AC_NSHOPID,AC_SHOPDESCRIPTION,AC_RETAILER,RETAILER_SHOP,CADENA,NC_PERIODID,NAN_KEY,DESCRIPTION,BARCODE,AC_CREFDESCRIPTION,
                      NC_CONV,VENDAS_UND,VENDAS_VALOR,PRECO,FATOR_CONV,SOT)
      
      
      # ... your code, e.g.:
      Sys.sleep(2)
      # a_flag=1
      # increase 50%
      incProgress(0.5, detail = "Finish something")
      # finish something
      return(CUESTIONAMIENTO)
      # ... your code
      incProgress(0.3, detail = "Done")
    })
    #CONCENTRATION <- TREND[[2]]
  })
  
  sirval_out_2 <- reactive({
    
    
    withProgress(message = "Application loading", value = 0, {
      # some code
      # ... your code, e.g.:
      
      Sys.sleep(1)
      # increase progress by 20%
      incProgress(0.2, detail = "Do something")
      #do something
      if(length(unique(sirval_out_1()$NC_PERIODID)) == 1){
        
        CURRENT <- max(sirval_out_1()$NC_PERIODID) - 989
        
        PIVOT <- sirval_out_1() %>% dplyr::select(TIENDA = AC_NSHOPID,ITEM = NAN_KEY,VENDAS_VALOR,VENDAS_UND)
        
        names(PIVOT)[3:4] <- c(paste0("VENDAS_VALOR_",CURRENT + 989),paste0("VENDAS_UND_",CURRENT + 989))
        
        PIVOT[,paste0("VENDAS_VALOR_",CURRENT - 1 + 989)] <- 0
        PIVOT[,paste0("VENDAS_UND_",CURRENT - 1 + 989)]   <- 0
        
        PIVOT$VAR_VALOR  <- PIVOT[,paste0("VENDAS_VALOR_",CURRENT + 989)]/PIVOT[,paste0("VENDAS_VALOR_",CURRENT - 1 + 989)] - 1 
        PIVOT$VAR_UNIDAD <- PIVOT[,paste0("VENDAS_UND_",CURRENT + 989)]/PIVOT[,paste0("VENDAS_UND_",CURRENT - 1 + 989)] - 1 
        
        
        PIVOT$VAR_VALOR  <- ifelse(PIVOT$VAR_VALOR == Inf,1,PIVOT$VAR_VALOR)
        PIVOT$VAR_UNIDAD <- ifelse(PIVOT$VAR_UNIDAD == Inf,1,PIVOT$VAR_UNIDAD)
        
        
      }else{
        
        SIRVAL_PERIODS <- sort(unique(sirval_out_1()$NC_PERIODID),decreasing = T)[1:2]
        
        CURRENT <- max(SIRVAL_PERIODS)
        BEFORE  <- min(SIRVAL_PERIODS)
        
        sirval_out_data <- sirval_out_1() %>% filter(NC_PERIODID %in% SIRVAL_PERIODS)
        
        PIVOT <- sirval_out_data %>% dplyr::select(NC_PERIODID,TIENDA = AC_NSHOPID,ITEM = NAN_KEY,VENDAS_VALOR,VENDAS_UND) %>%  
          pivot_wider(names_from = NC_PERIODID, values_from = c(VENDAS_VALOR,VENDAS_UND), 
                      values_fn = list(VENDAS_VALOR = sum,VENDAS_UND = sum)) %>% #Fazendo a pivotagem
          mutate(across(where(is.numeric), replace_na, 0)) %>% mutate(across(where(is.character), replace_na, "0")) %>% data.frame()
        
        PIVOT$VAR_VALOR  <- PIVOT[,paste0("VENDAS_VALOR_",CURRENT)]/PIVOT[,paste0("VENDAS_VALOR_",BEFORE)] - 1 
        PIVOT$VAR_UNIDAD <- PIVOT[,paste0("VENDAS_UND_",CURRENT)]/PIVOT[,paste0("VENDAS_UND_",BEFORE)] - 1 
        
        
        PIVOT$VAR_VALOR  <- ifelse(PIVOT$VAR_VALOR == Inf,1,PIVOT$VAR_VALOR)
        PIVOT$VAR_UNIDAD <- ifelse(PIVOT$VAR_UNIDAD == Inf,1,PIVOT$VAR_UNIDAD)
        
      }
      # ... your code, e.g.:
      Sys.sleep(2)
      # a_flag=1
      # increase 50%
      incProgress(0.5, detail = "Finish something")
      # finish something
      return(PIVOT)
      # ... your code
      incProgress(0.3, detail = "Done")
    })
    #CONCENTRATION <- TREND[[2]]
  })
  
  sirval_out_3 <- reactive({
    
    
    withProgress(message = "Application loading", value = 0, {
      # some code
      # ... your code, e.g.:
      
      
      Sys.sleep(1)
      # increase progress by 20%
      incProgress(0.2, detail = "Do something")
      #do something
      PERIODICITY <- input$periodicity_1
      
      PER.C <- unique(sirval_out_1()$NC_PERIODID)
      
      #WEEK.YEAR <- WY.TO.TPR(conn_s,1,PER.C)
      
      RMS <- sirval_out_1() %>% dplyr::select(AC_NSHOPID,NAN_KEY,NC_PERIODID,BARCODE)  
      
      RMS_FINAL <- RMS %>% group_by(AC_NSHOPID,NAN_KEY,BARCODE) %>% 
        summarise(MIN = min(NC_PERIODID,na.rm = T),
                  MAX = max(NC_PERIODID,na.rm = T)) %>%
        mutate(NC_PERIODID = paste0(MIN,"-",MAX)) %>% 
        dplyr::select(AC_NSHOPID,NAN_KEY,NC_PERIODID,BARCODE) %>%  as.data.frame() %>% unique()
      
      # ... your code, e.g.:
      Sys.sleep(2)
      # a_flag=1
      # increase 50%
      incProgress(0.5, detail = "Finish something")
      # finish something
      return(RMS_FINAL)
      # ... your code
      incProgress(0.3, detail = "Done")
    })
  })
  
  
  output$table_output <- DT::renderDataTable({
    DT::datatable(data = sirval_out_1(),
                  options = list(scrollX = TRUE))
  })
  
  output$table_input <- DT::renderDataTable({
    DT::datatable(data = sirval_out_2(),
                  options = list(scrollX = TRUE))
  })
  
  
  
  output$downloadData_sirval <- downloadHandler(
    filename = function() {
      paste0("CUESTIONAMIENTO_",input$periodicity_1,"_",input$text_2_1,".xlsx")
    },
    content = function(file) {
      
      write_xlsx(list('RAW DATA' = sirval_out(),
                      'TABLE' = sirval_out_1(),
                      'PIVOT' = sirval_out_2(),
                      'SIRVAL MAIL' = sirval_out_3()), path = file)
      
    }
  )
  
  surgery <- paste0("cor_",format(Sys.time(), "%d%m%y"))
  
  surgery_out <- eventReactive(input$go_2,{
    
    PERIODS <- as.integer(unlist(str_split(input$text_1_1,",")))
    CURRENT <- max(PERIODS)
    BEFORE  <- sort(PERIODS,decreasing = T)[2]
    
    
    withProgress(message = "Application loading", value = 0, {
      
      Sys.sleep(1)
      # increase progress by 20%
      incProgress(0.2, detail = "Do something")
      #do something
      
      PERIODICITY <- as.integer(input$periodicity_1)
      
      if(input$text_2_2 == "PAST PERIOD"){
        
        RawExtraction <- dbGetQuery(conn_s, paste0("select 
       'UC' as ac_operation, '",surgery,"', 'BR', '', a.nc_periodid, '', a.ac_nshopid, a.ac_cref, a.ac_crefsuffix, a.ac_dtgroup, a.f_nan_key, a.nc_conv, 
       a.nc_slot1,
       a.nc_slot2,
       a.nc_slot3, a.nc_slot4, a.nc_slot5, a.nc_slot6, a.nc_slot7, a.nc_slot8, a.nc_slot9, a.nc_slot10, a.ac_cslot1, a.ac_cslot2, 
       '",surgery,"', a.nc_hash_signature
       from vldsys_br.rawdata",ifelse(PERIODICITY == 1,"",ifelse(PERIODICITY == 2,"_mm",
                                                                 ifelse(PERIODICITY == 3,"_bi","_bm")))," a
       inner join vldsys_br.stores b on a.ac_nshopid = b.ac_nshopid
       WHERE a.nc_periodid IN (",paste0(c(BEFORE) + 989,collapse = ","),") 
       and a.ac_dtgroup = ",ifelse(PERIODICITY == 1,"'VOLUMETRIC'","'AUDIT_DTYPE'"),"
       AND a.AC_NSHOPID IN (",paste0(paste0("'",unique(sirval_out_1()$AC_NSHOPID),"'"),collapse = ","),")
       AND a.F_NAN_KEY IN (",paste0(unique(sirval_out_1()$NAN_KEY),collapse = ","),")"))
        
        
        RawExtraction$NC_PERIODID <- CURRENT  + 989
        
      }else if(input$text_2_2 == "BALANCING"){
        
        
        
        RawExtraction <- dbGetQuery(conn_s, paste0("select 
       'UC' as ac_operation, '",surgery,"', 'BR', '', a.nc_periodid, '', a.ac_nshopid, a.ac_cref, a.ac_crefsuffix, a.ac_dtgroup, a.f_nan_key, a.nc_conv, 
       a.nc_slot1,
       a.nc_slot2,
       a.nc_slot3, a.nc_slot4, a.nc_slot5, a.nc_slot6, a.nc_slot7, a.nc_slot8, a.nc_slot9, a.nc_slot10, a.ac_cslot1, a.ac_cslot2, 
       '",surgery,"', a.nc_hash_signature
       from vldsys_br.rawdata",ifelse(PERIODICITY == 1,"",ifelse(PERIODICITY == 2,"_mm",
                                                                 ifelse(PERIODICITY == 3,"_bi","_bm")))," a
       inner join vldsys_br.stores b on a.ac_nshopid = b.ac_nshopid
       WHERE a.nc_periodid IN (",paste0(c(PERIODS) + 989,collapse = ","),") 
       and a.ac_dtgroup = ",ifelse(PERIODICITY == 1,"'VOLUMETRIC'","'AUDIT_DTYPE'"),"
       AND a.AC_NSHOPID IN (",paste0(paste0("'",unique(sirval_out_1()$AC_NSHOPID),"'"),collapse = ","),")
       AND a.F_NAN_KEY IN (",paste0(unique(sirval_out_1()$NAN_KEY),collapse = ","),")"))
        
        ADJUST <- as.numeric(input$text_1_2)
        FA  <- (100 + ADJUST)/100
        
        RawExtraction$DIF      <- RawExtraction$NC_SLOT4*(1-FA)
        RawExtraction$NC_SLOT4 <- RawExtraction$NC_SLOT4*(FA)
        RawExtraction$NC_SLOT2 <- RawExtraction$NC_SLOT2 - RawExtraction$DIF
        RawExtraction$DIF      <- NULL
        
      }else if(input$text_2_2 == "DELETE"){
        
        RawExtraction <- dbGetQuery(conn_s, paste0("select 
       'DC' as ac_operation, '",surgery,"', 'BR', '', a.nc_periodid, '', a.ac_nshopid, a.ac_cref, a.ac_crefsuffix, a.ac_dtgroup, a.f_nan_key, a.nc_conv, 
       a.nc_slot1,
       a.nc_slot2,
       a.nc_slot3, a.nc_slot4, a.nc_slot5, a.nc_slot6, a.nc_slot7, a.nc_slot8, a.nc_slot9, a.nc_slot10, a.ac_cslot1, a.ac_cslot2, 
       '",surgery,"', a.nc_hash_signature
       from vldsys_br.rawdata",ifelse(PERIODICITY == 1,"",ifelse(PERIODICITY == 2,"_mm",
                                                                 ifelse(PERIODICITY == 3,"_bi","_bm")))," a
       inner join vldsys_br.stores b on a.ac_nshopid = b.ac_nshopid
       WHERE a.nc_periodid IN (",paste0(c(PERIODS) + 989,collapse = ","),") 
       and a.ac_dtgroup = ",ifelse(PERIODICITY == 1,"'VOLUMETRIC'","'AUDIT_DTYPE'"),"
       AND a.AC_NSHOPID IN (",paste0(paste0("'",unique(sirval_out_1()$AC_NSHOPID),"'"),collapse = ","),")
       AND a.F_NAN_KEY IN (",paste0(unique(sirval_out_1()$NAN_KEY),collapse = ","),")"))
        
        RawExtraction[,12:24] <- NA    
        
        
        
      }    
      #RawExtraction <- RawExtraction %>% mutate(NC_HASH_SIGNATURE = as.character(NC_HASH_SIGNATURE),
      #                                          AC_CREF           = as.character(AC_CREF) )
      RawExtraction$NC_HASH_SIGNATURE <- as.character(RawExtraction$NC_HASH_SIGNATURE)
      RawExtraction$AC_CREF           <- as.character(RawExtraction$AC_CREF)
      
      RawExtraction <- merge(sirval_out() %>% dplyr::select(AC_NSHOPID,F_NAN_KEY) %>% unique(),RawExtraction,by = c("AC_NSHOPID","F_NAN_KEY"))
      
      RawExtraction <- RawExtraction[,c(3:8,1,9:11,2,12:(ncol(RawExtraction)))]
      # ... your code, e.g.:
      Sys.sleep(2)
      # a_flag=1
      # increase 50%
      incProgress(0.5, detail = "Finish something")
      # finish something
      return(RawExtraction)
      # ... your code
      incProgress(0.3, detail = "Done")
    })
    #CONCENTRATION <- TREND[[2]]
  })
  
  values = reactiveValues()
  
  surgery_out_1 <- reactive({
    
    if (!is.null(input$table_surgery)) {
      DF = hot_to_r(input$table_surgery)
    } else {
      if (is.null(values[["DF"]]))
        DF = surgery_out()
      else
        DF = values[["DF"]]
    }
    
    
    values[["DF"]] = DF
    
    return(DF)
    
  })
  
  output$table_surgery <- renderRHandsontable({
    
    DF = surgery_out_1()
    
    if (!is.null(DF))
      rhandsontable(DF, stretchH = "all")
  })
  
  
  
  output$downloadData_surgery <- downloadHandler(
    filename = function() {
      paste0("SURGERY_",format(Sys.time(), "%d%m%y"),".txt")
    },
    content = function(file) {
      
      fwrite(surgery_out_1(),file = file,col.names = F)
      
    }
  )
  
  output$links <- DT::renderDataTable({
    
    data_link <- data.table(SURGERY = c("SERVICE NOW", "MODELO SURGERY","URL SERVICE NOW","RUTA WINDOWS","REFERENCIA SURGERY"),
                            LINKS = c("_id=8ae14f57db7ce3c0b0ed5a35dc961943&table=sc_cat_item",
                                      "NFiGUiZHaLjuQzUt_STY/edit#gid=1854362936",
                                      "HHH",
                                      "JJ",
                                      "OeSb72kmk4BbLRR5PyqoLtlOcM/edit#gid=0"),
                            stringsAsFactors = F)
    
    DT::datatable(data = data_link,
                  options = list(scrollX = TRUE))
    
    
  })
  
}

shinyApp(ui = ui, server = server)
