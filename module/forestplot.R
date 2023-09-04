
library(shinyWidgets)
library(showtext) # 解决画图中文乱码
showtext_auto()

# library(AER)
library(readxl) # 读取 Excel
library(rhandsontable)
if(T){
  sample <-  gsub('\n','\t','rating	0	0.63	0.52	0.75
religiousness	0	0.72	0.6	0.86
age	0.015	0.96	0.92	0.99
education	0.677	1.02	0.93	1.13
occupation	0.667	1.03	0.9	1.19
yearsmarried	0.003	1.1	1.03	1.17
gendermale	0.241	1.32	0.83	2.12
childrenyes	0.173	1.49	0.85	2.66')
  
  
  sample <- as.data.frame(matrix(unlist(strsplit(sample,'\t')),ncol = 5,byrow = T))
  colnames(sample) <- c('Var','Pvalue','OR','Lower','Upper')
  
  sample[,-1] <- apply(sample[,-1],2,as.numeric)
}

if(T){
  # 森林图画图函数
  myforestplot <- function(data,input){
    
    library(ggplot2)
    df1 <- na.omit(data)
    
    df1$Factor <- ifelse(df1$Lower > 1,'Risk',ifelse(df1$Upper < 1,'Protective','Not sig.'))
    df1$`OR (95% CI)` <- paste0(df1$OR,'(',df1$Lower,'-',df1$Upper,')')
    
    df1$Pvalue.sig <- ifelse(df1$Pvalue >= 0.001,df1$Pvalue,'<0.001')
    
    df1 <- df1[order(df1$OR,decreasing = T), ]
    m <- df1$Var
    df1$Var <- factor(df1$Var,levels = m)
    # df1$Var1 <- factor( 1:nrow(df1) )
    
    annotation <- data.frame(matrix("",ncol = 3,nrow = 2*nrow(df1)+3 ))
    colnames(annotation) <- c('x','y','label')
    
    annotation$label <- c('OR (95% CI)','Odds Ratio','P Value',paste0(df1$OR,'(',df1$Lower,'-',df1$Upper,')'),df1$Pvalue.sig)
    
    annotation$x <- c( c(-0.3,1,-0.7),rep(-0.3, nrow(df1) ),rep(-0.7,nrow(df1) ) )
    
    annotation$y <- c(rep(nrow(df1)+ 0.4,3),seq(1, nrow(df1), 1),seq(1, nrow(df1), 1)  )
    
    plot <-  ggplot(df1, aes(OR, Var)) +
      geom_point(size=3.6, aes(col=Factor)) +
      geom_errorbarh(aes(xmax =Upper, xmin = Lower), col=input$lines, height = 0.4)+
      geom_vline(aes(xintercept=1),col=input$zero)+
      scale_x_continuous(limits=c(-0.7, max(df1$Upper)), breaks=seq(0,max(df1$Upper) , 0.5)) +
      theme_bw() + 
      theme(legend.position ="top") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill =input$background),
            axis.text.y = element_text(size=20,colour = 'black') ,
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size=20,colour = 'black'),
            legend.text = element_text(size=20),
            legend.title = element_text(size=20),
            plot.title  =  element_text(size=30,hjust = 0.5, face = "bold",colour = 'black')) +
      labs(x='',y='',title = input$title)+
      geom_text(data=annotation,aes(x=x,y=y,label=label) ,size=6)
    
    return(plot)
  }
}

forestplotUI <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     rHandsontableOutput(ns('table') ) ),
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
                     h6(lang$t('格式：.csv .xlsx .xls')),
                     actionBttn( inputId = ns("show"), label = "Show Data",
                                 style = "fill", color = "primary", size = "sm" ),
                     hr(),
                     downloadButton(ns("downloadtable"),lang$t("参考数据")),br(),br(),
                     dropdownButton( label = lang$t("使用说明"), icon = icon('tachometer-alt'), circle = FALSE,br(),br(),
                                     h6("1、参考数据中，列名大小写保持一致，不要改变。"),
                                     h6("2、使用："),
                                     h6("方法一：点击“Analyze Data”后将自己的数据粘贴到“输入数据”表格。
                             若需要增减行数，可在输入表格框内右键。"),
                                     h6("方法二：下载数据后编辑上传，再点击“Analyze Data” ")
                     ) ) )
               ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("森林图"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot"),width = 810,height = 540 ) ) ),
                 box(width=3,status="success",
                     actionBttn( inputId = ns("submit"), label = "Analyze Data",
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     dropdownButton( label = lang$t("图形标签"),icon = icon('image'), circle = FALSE,br(),br(),
                                     textInput(inputId = ns('title'),label = lang$t('标题'),value="Forestplot"),
                                     selectInput(ns("background"),  lang$t("背景颜色"), colors() , selected = "skyblue" ),
                                     selectInput(ns("zero"),        lang$t("竖线颜色"), colors() , selected = "black"   ),
                                     selectInput(ns("lines"),       lang$t("横线颜色"), colors() , selected = "black"   )
                                     
                     ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w'),label = lang$t('下载图宽'),value = 18),
                                    numericInput(inputId = ns('h'),label = lang$t('下载图高'),value = 12),
                                    numericInput(inputId = ns('ppi'),label = lang$t('分辨率'),value = 72),
                                    downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE ) )
                 )
               ) # fluidRow
      )
      )
  ) # tagList
} # function(id)


forestplotServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    observeEvent(input$show, {
      # 数据：读取输入文件或参考数据
      df1 <- reactive({
        
        file1 <- input$file1
        
        if(is.null(file1)){
          df1 <- sample
        } 
        else{
          
          d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
          
          if(d=='csv'){
            df1 <- data.frame( read.csv(file1$datapath,fileEncoding = "GB18030") )
          } else{
            df1 <- data.frame( read_excel(file1$datapath,1) )
          } 
          
        } # else
        
        return(df1)
        
      })
      
      df <- na.omit(df1() )
      
      output$table <- renderRHandsontable(
        
        df %>% rhandsontable( rowHeaderWidth = 22,height = 300 )%>% 
          hot_cols(columnSorting = TRUE) %>% 
          hot_col("Lower",type = 'numeric') %>%
          hot_col("Upper", type = 'numeric') %>%
          hot_col('OR', type = 'numeric')
      )
      
      observeEvent(input$submit, {

          forestplot <- reactive({
            mydata <- hot_to_r( input$table )
            plot <- myforestplot(mydata,input)
            
            return( plot )
          })
          
          output$plot <- renderPlot({
            
            plot <- forestplot()
            return(plot)
            
          } ) 

          if(T){
            output$pdf <- downloadHandler(
              filename="Forestplot.pdf",
              content = function(file){
                pdf(file,width=input$w,height=input$h)
                print( forestplot() )
                dev.off()
              }
            )
            output$png <- downloadHandler(
              filename="Forestplot.png",
              content = function(file){
                png(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print( forestplot() )
                dev.off()
              }
            )
            output$jpeg <- downloadHandler(
              filename="Forestplot.jpeg",
              content = function(file){
                jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print( forestplot() )
                dev.off()
              }
            )
            output$tiff <- downloadHandler( 
              filename="Forestplot.tiff",
              content = function(file){
                tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print( forestplot() )
                dev.off()
              }
            )
          }

          
        } )
        
        
      } )
      # 1、下载 结果
      output$downloadtable <- downloadHandler(
        filename = function() {
          paste('Forestplot.csv')
        },
        content = function(file) {
          
          if( is.null(input$table ) ){
            mydata <- sample
          }
          else{
            mydata <- hot_to_r( input$table )
          }
          write.csv(mydata, file,  row.names = F, fileEncoding = 'GB18030')
          
        } )
      
    } # function(input, output, session)
  ) # moduleServer
} # function(id)