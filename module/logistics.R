
library(rhandsontable)
# 编写函数
myfun_fp <- function(data){
  
  # 整理数据
  fit <- data
  
  df1 <- cbind( exp( coef(fit) ) ,  # OR
                summary(fit)$coefficients[,4], # P
                exp( confint(fit) ) # 置信区间
  ) 
  df1 <- data.frame(df1)[-1,]
  df1 <- cbind('Var'=rownames(df1),df1)
  colnames(df1)[-1] <- c("OR","Pvalue","OR_lower","OR_upper")
  
  df2 <- df1
  df2$OR_mean <- df2$OR
  df2$OR <- paste0(   round(df2$OR,2),   # OR
                      "(", round(df2$OR_lower,2), # OR_lower
                      "~", round(df2$OR_upper,2), # OR_upper
                      ")")
  df2$Pvalue <- ifelse( df2$Pvalue>=0.001 , round(df2$Pvalue,3), "<0.001")
  df2
  
  fp <- rbind("lables"=NA,df2)
  fp[1, 1:3]  <-  c('Variable', 'OR(95% CI)', 'Pvalue')
  
  return(fp)
}

# 画图
myfun_plot <-  function(data,input){
  library(ggplot2)
  fit <- data
  df1 <- cbind( 'OR' = round(exp( coef(fit) ), 2) ,  # OR
                "Pvalue"= round( summary(fit)$coefficients[,4], 3), # P
                "Lower"= round(exp( confint(fit) )[,1], 2 ), # 置信区间
                "Upper"= round(exp( confint(fit) )[,2] , 2)
  ) 
  
  df1 <- data.frame(df1)[-1,]
  df1 <- cbind('Var'=rownames(df1),df1)
  
  df1$Factor <- ifelse(df1$Lower>1,'Risk',ifelse(df1$Upper<1,'Protective','Not sig.'))
  
  df1$Pvalue1 <- ifelse(df1$Pvalue >= 0.001,df1$Pvalue,'<0.001')
  
  df1 <- df1[order(df1$OR,decreasing = T),]
  df1$Var1 <- factor(1:nrow(df1))
  
  annotation <- data.frame(matrix("",ncol = 3,nrow = c(3*nrow(df1)+3) ))
  colnames(annotation) <- c('x','y','label')
  annotation$label <- c('OR (95% CI)','Odds  Ratio','P Value',paste0(df1$OR,'(',df1$Lower,'-',df1$Upper,')'),df1$Pvalue1, df1$Var)
  annotation$x <- c( c(-0.3,1,-0.75),rep(-0.3, nrow(df1) ),rep(-0.75,nrow(df1) ), rep(-1.1, nrow(df1)) )
  annotation$y <- c(rep(nrow(df1)+0.47,3),seq(1, nrow(df1), 1),seq(1, nrow(df1), 1) ,seq(1, nrow(df1),1 ) )

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

logisUI <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("输入数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), rHandsontableOutput(ns("df")) ) 
                 ), 
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("输入文件"),multiple = FALSE ),
                     h6(lang$t('格式：.csv .xlsx .xls')),
                     actionBttn( inputId = ns("show"), label = "Show Data", 
                                 style = "fill", color = "primary", size = "sm" ), hr(),
                     downloadButton(ns("downloadtable"),lang$t("参考数据") ), br(),br(),
                     dropdownButton(circle=FALSE, label=lang$t("参数设置"),  br(),br() ,
                                    selectInput(ns("group"), lang$t("y：二分类"), c("") ),
                                    selectInput(ns("num"),    lang$t("x1：计量"),  multiple = T,c("") ),
                                    selectInput(ns("factor"), lang$t("x2：计数"),  multiple = T,c("") ),
                                    selectInput(ns("type"), lang$t("筛选变量："), selected = "full",
                                                c('all variables'='full',  'step regression'='step') )
                     ),br(),
                     dropdownButton( label = lang$t("使用说明"), icon = icon('tachometer-alt'), circle = FALSE,br(),br(),
                                     h6("1、参考数据中，因变量为二分类，在参数设置中选定（y），
                             默认第一列。其余列为自变量，可为连续变量、分类变量。"),
                                     h6("2、自变量为连续变量（计量）时，不要有缺失值。"),
                                     h6("3、自变量为分类变量（计数）时，建议设置其为字母而非数值。"),
                                     h6("4、可在参数设置中对这两类变量进行选定，然后点击“Analyze Data”。")
                     )
                 ) )
               ),
      tabPanel(title = 'Result',
               fluidRow(
                 box(title=lang$t("Logistics 结果"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), dataTableOutput(ns("contents1"),height = 200 ) ) 
                 ),
                 box(width = 3,status="success",
                     actionBttn( inputId = ns("submit1"), label = "Analyze Data", 
                                 style = "fill", color = "primary", size = "sm" ), hr(),
                     downloadButton(ns("downloadtable1"),lang$t("结果数据") )
                 )
                 )
      ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("森林图"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), plotOutput(ns('plot') ) )  
                 ) ,
                 box(width = 3,status="success",
                     dropdownButton(circle=FALSE, label=lang$t("图形参数"), icon = icon("image"), br(),br() ,
                                    textInput(inputId = ns('title'),label = lang$t('标题'),value="Forestplot"),
                                    selectInput(ns("background"),  lang$t("背景颜色"), colors() , selected = "skyblue" ),
                                    selectInput(ns("zero"),        lang$t("竖线颜色"), colors() , selected = "black" ),
                                    selectInput(ns("lines"),       lang$t("横线颜色"), colors() , selected = "black" ) 
                     ),br() ,
                     dropdownButton(circle=FALSE, label=lang$t("下载图形"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w0'),label = lang$t('下载图宽'),value = 15),
                                    numericInput(inputId = ns('h0'),label = lang$t('下载图高'),value = 15),
                                    numericInput(inputId = ns('ppi0'),label = lang$t('分辨率'),value = 72),
                                    downloadBttn(outputId = ns("pdf0") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png0") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg0"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff0"), label = "TIFF", size='sm', block=TRUE )
                     )
                 ) )
      ),
      tabPanel(title = 'Plot data',
               fluidRow(
                 box(title=lang$t("Forestplot 数据"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), dataTableOutput(ns("contents2"),height = 200 ) ) 
                 ),
                 box(width = 3,status="success",
                     downloadButton(ns("downloadtable2"),lang$t("图形数据") )
                 )
               )
               )
      )
  ) #  tagList
} # function(id)
          
logisServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    observeEvent(input$show, {
      # 数据：读取输入文件或参考数据
      df <- reactive({
        
        file1 <- input$file1
        
        if(is.null(file1)){
          df1 <- read.csv('./www/logistics.csv')
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
      
      output$df <- renderRHandsontable(
        rhandsontable(df(),rowHeaderWidth = 50, height = 370) %>% 
          hot_cols(columnSorting = TRUE)
      ) 
      
      observe({
        df <- df()
        
        updateSelectInput(session, 'group',  label = "y：二分类",     choices = c(colnames(df) ),selected = colnames(df)[1] )
        updateSelectInput(session, 'num',    label = "x1：计量", choices = c(colnames(df) ) )
        updateSelectInput(session, 'factor', label = "x2：计数", choices = c(colnames(df) ) )
        
      })

      observeEvent(input$submit1, {
        
        myfun <- reactive({
          if(is.null(input$df)){return(NULL)}
          df <- hot_to_r( input$df )
          n  <- which(colnames(df) == input$group ) 
          
          # logistics回归
          colnames(df)[n] <- 'group_yn'
          df$group_yn <- factor(df$group_yn )
          
          # 数值型数据
          if(length(input$num)==1){
            df[,input$num ] <- as.numeric(df[,input$num ]  )
          }
          else if(length(input$num)>1){
            df[,input$num ] <- apply( df[,input$num ] ,2, as.numeric)
          }
          
          # 因子型数据
          if( length(input$factor)==1 ){
            df[,input$factor ] <- factor(df[,input$factor ]  )
          }
          else if( length(input$factor)>1 ){
            df[,input$factor ] <- apply( df[,input$factor ] ,2, factor)
          }
          
          fit.full <- glm(group_yn~ . , # 所有变量
                          data = df, # 数据集
                          family = binomial(link='logit') # 拟合方式
          )
          
          fit.step <- step(object = fit.full,trace = 0)
          
          if(input$type == 'full'){
            
            plot <- myfun_plot(fit.full,input)
            fit <- as.data.frame(summary( fit.full )$coefficients)
            fp <- myfun_fp( data = fit.full )
            
          }
          else if(input$type == 'step'){
            
            plot <- myfun_plot(fit.step,input)
            fit <- as.data.frame(summary( fit.step )$coefficients)
            fp <- myfun_fp( data = fit.step )
            
          }

          fit <- cbind( 'Var'=rownames(fit), fit )
          
          results <-list()
          
          results$fp <- fp
          results$fit <- fit 
          results$plot <- plot
          
          return( results )
          
        })
        
        fp <- reactive({
          if(is.null(myfun())){return(NULL)}
          return(myfun()$fp)
        })
        
        fit <- reactive({
          if(is.null(myfun())){return(NULL)}
          return(myfun()$fit )
        })

        plot <- reactive({
          if(is.null(myfun())){return(NULL)}
          return(myfun()$plot)
        })
        
        # 森林图图形
        output$plot <- renderPlot({
          return(plot() )
        }  )
        
        # 展示 logistic 结果表格
        output$contents1 <- renderDataTable({
          return(fit() )
        })  
        
        # 展示森林图表格
        output$contents2 <- renderDataTable({
          return(fp() )
        })  
        
        if(T){
          output$pdf0 <- downloadHandler(
            filename="logisfp.pdf",
            content = function(file){
              pdf(file,width=input$w0,height=input$h0)
              print( plot() )
              dev.off()
            }
          )
          output$png0 <- downloadHandler(
            filename="logisfp.png",
            content = function(file){
              png(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
              print( plot() )
              dev.off()
            }
          )
          output$jpeg0 <- downloadHandler(
            filename="logisfp.jpeg",
            content = function(file){
              jpeg(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
              print(plot())
              dev.off()
            }
          )
          output$tiff0 <- downloadHandler( 
            filename="logisfp.tiff",
            content = function(file){
              tiff(file,width=input$w0,height=input$h0,units="in",res=input$ppi0)
              print( plot())
              dev.off()
            }
          )
        }
        
        
        # 1、下载logistic回归结果
        output$downloadtable1 <- downloadHandler(
          filename = function() {
            paste('logistic.csv')
          },
          content = function(file) {
            write.csv(fit() , file,  row.names = F, fileEncoding = 'GB18030')
          } )
        
        # 2、下载森林图结果
        output$downloadtable2 <- downloadHandler(
          filename = function() {
            paste('plot.csv')
          },
          content = function(file) {
            write.csv(fp() , file,  row.names = F, fileEncoding = 'GB18030')
          } )
        
        
      })
      
      
      
      
    })

       # 0、下载参考数据
      output$downloadtable <- downloadHandler(
        filename = function() {
          paste('sample.csv')
        },
        content = function(file) {
          df <- read.csv('./www/logistics.csv')
          write.csv(df , file,  row.names = F, fileEncoding = 'GB18030')
        } )
      
      
    } # function(input, output, session)
  )  # moduleServer
} # function(id)
          