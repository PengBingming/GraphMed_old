
# remotes::install_github("cardiomoon/multipleROC")
library(multipleROC)
library(ggplot2)
library(rhandsontable)

lang <- Translator$new(translation_csvs_path = "./lang/info/")

rocUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny.i18n::usei18n(lang),
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("table") ) ) ),
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
                     h6(lang$t('格式：.csv .xlsx .xls')),
                     actionBttn( inputId = ns("show"), label = "Show Data",
                                 style = "fill", color = "primary", size = "sm" ),
                     hr(),
                     downloadButton(ns("downloadSampleData"),lang$t("参考数据"))
                 
               ) )  ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("图形"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot"),height = 500 ) ) ),
                 box(width=3,status="success",
                     actionBttn( inputId = ns("submit"), label = lang$t("开始画图"),
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     dropdownButton(circle=FALSE, label=lang$t("参数设置"), br(),br(),
                     selectInput(ns("outcome"),   lang$t("结局"), c("") ),
                     selectInput(ns("var"), lang$t("选择变量"), c(""),multiple = T ),
                     selectInput(ns("merge.var"),  lang$t("合并变量"), c("yes","no"),
                                 selected = 'no') ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("图形标签"),icon = icon('image'),  br(),br(),
                                    selectInput(ns('theme'),lang$t('主题'),selected = 'bw',
                                                choices =c('bw','classic','classic2', 'cleveland',
                                                           'dark','light','get', 'gray') ),
                                    selectInput(ns('point'),label = lang$t('点'),selected = 'show',
                                                choices = c('show','hide') ) ,
                                    selectInput(ns('eta'),label = 'lr.eta',selected = 'show',
                                                choices = c('show','hide') ) ,
                                    selectInput(ns('sen'),label = lang$t('灵敏度'),selected = 'show',
                                                choices = c('show','hide') ) ,
                                    selectInput(ns('auc'),label = "AUC",selected = 'show',
                                                choices = c('show','hide') ),
                                    selectInput(ns('facet'),label = lang$t('分面'), selected = 'none',
                                                choices = c('none','row','column') ),
                                    textInput(ns('legend'),label = lang$t('图例'),value = '' ) 
                     ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w'),label = lang$t('下载图宽'),value = 15),
                                    numericInput(inputId = ns('h'),label = lang$t('下载图高'),value = 15),
                                    numericInput(inputId = ns('ppi'),label = lang$t('分辨率'),value = 72),
                                    downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE )
                     ) )
                 
               ) )
    ) ) }



rocServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        
        # Load the data # 读取数据
        df <- reactive({
          file1 <- input$file1
          if ( is.null(file1) ){
            df <- read.csv('./www/roc.csv')
          } 
          else{
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
            if( d=='csv' ){
              df <- data.frame( read.csv(file1$datapath,header=T, stringsAsFactors = FALSE, fileEncoding = 'GB18030') )
            } else{
              df <- data.frame( read_excel(file1$datapath,1) )
            } 
          } # else
          return( df )
        })
        
        # 输入数据
        if(!is.null(df() ) ){
          output$table <- renderRHandsontable(
            rhandsontable(df(),rowHeaderWidth = 22,  height = 400) %>% 
              hot_cols(columnSorting = TRUE)
          )
        }
        
        observe({
          if(!is.null(input$table ) ){
            df <- as.data.frame(hot_to_r( input$table ) )
            
            # 实验组与对照组情况
            updateSelectInput(session, "outcome", label = lang$t('结局'), choices = colnames(df), 
                              selected = colnames(data)[1]  )
            updateSelectInput(session, "var"  , label = lang$t('选择变量'), choices = colnames(df)  , 
                              selected = colnames(df)[2]  )
          }
        } ) 
        
        observeEvent(input$submit, {
          
          plot <- reactive({
            df <- hot_to_r(input$table)
            df[,input$outcome] <- as.factor(df[,input$outcome])
            
            var <- input$var[1]
            if(length(input$var)==1 ){
              expr <- paste0("multipleROC(",input$outcome,"~",var,',data=df)')
              p <- expr 
            }
            else if(length(input$var)>1  ){
              
              if(input$merge.var=='yes'){
                for (i in 2:length(input$var)) {
                  var <- paste(var,'+',input$var[i])
                }
                expr <- paste0("multipleROC(",input$outcome,"~",var,',data=df)')
                p <- expr 
              }
              else if(input$merge.var=='no'){
                
                expr <- paste0("multipleROC(",'outcome',"~",input$var,',data=df)' )
                p <- paste0( "eval( parse(text = expr[1] ) )" )

                for (i in 2:length(input$var)) {
                  p <- paste0(p,',','eval( parse(text = expr[',i,'] ) )')
                  }
                  p <-paste0('list(',p,')')
              }
            }

              p <-  plot_ROC(eval( parse(text = p ) ), 
                             show.points = c(input$point=='show'), 
                             show.eta = c(input$eta=='show'), 
                             show.sens = c(input$sen=='show'), 
                             show.AUC =  c(input$auc=='show'),
                             facet = F )
              if(input$merge.var=='yes'){
                if(input$facet=='row'){
                  # New facet label names for supp variable
                  supp.labs <- var # 这个是我们希望展示出来的标签名
                  names(supp.labs) <-  1:length(var) # 这个是我们希望隐藏的标签名
                  
                  p <-  p + facet_grid(no~.,labeller = labeller(no=supp.labs) )
                }
                else  if(input$facet=='column'){
                  # New facet label names for supp variable
                  supp.labs <- var # 这个是我们希望展示出来的标签名
                  names(supp.labs) <-  1:length(var) # 这个是我们希望隐藏的标签名
                  p <-  p + facet_grid(.~no,labeller = labeller(no=supp.labs) )
                }
              }
             else if(input$merge.var=='no'){
                if(input$facet=='row'){
                  # New facet label names for supp variable
                  supp.labs <- input$var # 这个是我们希望展示出来的标签名
                  names(supp.labs) <-  1:length(input$var) # 这个是我们希望隐藏的标签名
                  
                  p <-  p + facet_grid(no~.,labeller = labeller(no=supp.labs) )
                }
                else  if(input$facet=='column'){
                  # New facet label names for supp variable
                  supp.labs <- input$var # 这个是我们希望展示出来的标签名
                  names(supp.labs) <-  1:length(input$var) # 这个是我们希望隐藏的标签名
                  p <-  p + facet_grid(.~no,labeller = labeller(no=supp.labs) )
                }
                
              }

            if(input$theme=='bw'){
                p <- p + theme_bw()
              }
            else if(input$theme=='classic'){
              p <- p + theme_classic()
            }
            else if(input$theme=='classic2'){
              p <- p + theme_classic2()
            }
            else if(input$theme=='cleveland'){
              p <- p + theme_cleveland()
            }
            else if(input$theme=='dark'){
              p <- p + theme_dark()
            }
            else if(input$theme=='light'){
              p <- p + theme_light()
            }
            else if(input$theme=='get'){
              p <- p + theme_get()
            }
            else if(input$theme=='gray'){
              p <- p + theme_gray()
            }
              
   
                if(input$legend==''){
                  p <- p +
                    theme(text = element_text(size = 25),
                          axis.title = element_text(size = 25,colour = 'gray40'),
                          axis.text =  element_text(size = 10),
                          legend.position='none'
                    )
                }
                else{
                  
                  if(input$merge.var=='yes'){
                    p <- p +
                      theme(text = element_text(size = 25),
                            axis.title = element_text(size = 25,colour = 'gray40'),
                            axis.text =  element_text(size = 10),
                            legend.position='none'
                      )
                  }
                  else if(input$merge.var=='no'){
                    p <- p +
                      theme(text = element_text(size = 25),
                            axis.title = element_text(size = 25,colour = 'gray40'),
                            axis.text =  element_text(size = 10),
                            legend.text = element_text(size = 10),
                            legend.title = element_text(color = "black", size = 15)
                      ) +
                      scale_colour_discrete( name=input$legend,
                                             breaks = 1:length(input$var),
                                             labels =  input$var)
                  }
               
                }

            return(p)
            
            })
          
          output$plot <- renderPlot({
            return(plot() )
          })
          
          if(T){
            output$pdf <- downloadHandler(
              filename="plot.pdf",
              content = function(file){
                pdf(file,width=input$w,height=input$h)
                print(plot() )
                dev.off()
              }
            )
            output$png <- downloadHandler(
              filename="plot.png",
              content = function(file){
                png(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }
            )
            output$jpeg <- downloadHandler(
              filename="plot.jpeg",
              content = function(file){
                jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }
            )
            output$tiff <- downloadHandler( 
              filename="plot.tiff",
              content = function(file){
                tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(plot() )
                dev.off()
              }
            )
          }
          
        } )
        
        
      } )

      # 2、下载参考数据
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('roc.csv')
        } ,
        content = function(file) {
          data <- read.csv('./www/roc.csv')
          write.csv(data , file, row.names = F, fileEncoding = "GB18030")
        }  )
      
    } ) }
        