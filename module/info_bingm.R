# # File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

contactUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  fluidRow(box(width=12,
               title="联系我们",solidHeader=TRUE,status='primary',background = "white",height=800,
               tags$h2("Our website"),
               tags$a(href="https://stu.chcmu.asia/", "我们的网站"),
               tags$hr(),
               tags$p("秉持热诚、坚守初心。"),
               tags$hr(),
               tags$h2("GraphMed"),
               tags$p("GraphMed 是由重医儿院研究生 彭炳明 编写的一个数据分析与可视化的网页工具，
                      它旨在提供简便易用、实用性高的数据分析与可视化服务，降低研究人员的技术学习成本，
                      使得其可将精力集中于“科学问题”而非“技能学习”，帮助其更加便捷、快速地处理数据。
                      若使用时存在问题或对此感兴趣，可通过上方链接“Bingm 的个人网站”进入本人的个人网站，注册后反馈问题，
                      也可通过下方二维码联系本人（请备注信息，但不一定通过）或对我的工作进行打赏，欢迎加入、感谢支持。"),
               tags$hr(),
               fluidRow(
                 box(width=6,background = "white",status='primary',title=lang$t("微信"),solidHeader=TRUE,
                     tags$img(src = "wechat.jpg", width = "220",height="300",style= "display:block;margin:auto;")),
                 box(width=6,background = "white",status='primary',title=lang$t("支付宝"),solidHeader=TRUE,
                     tags$img(src = "Alipay.jpg", width = "220",height="300",style= "display:block;margin:auto;")) )
  )
  )
}

faqUI <-  function(id) {
  ns <- NS(id)
  fluidRow(box(width=12,
               title="常见问题",solidHeader=TRUE,status='primary',background = "white",height=800,
               tags$h2("常见问题"),
               tags$hr(),
               tags$p("1、上传格式报错，如“列名”的大小写不符合。")
               
  )
  )
  
}

helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$p("GraphMed 为使用 R 语言 shiny 构建的网页临床、实验与生信数据分析工具，
                           旨在以一种简单高效的方式帮助科研人员快速完成数据探索及可视化。"),
                 # tags$img(src="x1.png",width="50%",height="50%"),
                 tags$p("1、若初次使用，可先运行示例、下载参考数据，了解数据格式与要求。
                        对于参考数据的“列名”，使用时需慎重，非必要不可修改参考数据的“列名”，
                        各“列名”大小写保持一致，仅仅替换其数据内容，否则可能报错。"),
                 tags$p("2、部分工具使用方法："),
                 tags$p("方法一：点击“Show Data”后将自己的数据粘贴到“输入数据”表格。
                                若需要增减行数，可在输入表格框内右键。"),
                 tags$p("方法二：下载数据后编辑上传，再点击“Analyze Data”。")
                 
    )
    ) )
  
}