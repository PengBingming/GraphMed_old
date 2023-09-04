# # File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

contactUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
fluidRow(box(width=12,
             title="联系我们",solidHeader=TRUE,status='primary',background = "white",height=800,
             tags$h3("重庆医科大学附属儿童医院呼吸病学研究室(非官方)"),
             tags$a(href="http://stu.chcmu.asia", "Laboratory of Pediatric Respiratory Medicine,
                           Children's Hospital of Chongqing Medical University"),
             tags$hr(),
             tags$p("重庆医科大学附属儿童医院呼吸专业成立于80年代初，
                    1992年获重庆市儿童哮喘防治中心，
                    2005年成立重庆医科大学小儿过敏性疾病诊疗中心，
                    2007年成立重庆医科大学附属儿童医院呼吸中心，
                    2008年获重庆市医学专业重点学科，2011年获得卫生部临床重点专科。
                    目前正致力于建设成为西部一流、国内领先的小儿呼吸系统疾病
                    诊断、治疗、研究与培训中心。（官网摘抄！）"),
             tags$hr(),
             tags$h3("GraphMed"),
             tags$p("GraphMed 是由重医儿院研究生 彭炳明 同学编写的一个数据分析及可视化的网页工具，
                    旨在提供简便易用、实用性高的数据分析与可视化服务，降低相关技能学习成本，
                    使得研究人员可将精力集中于“科学问题”而非“技能学习”，帮助其更加便捷、快速地处理数据。
                    感兴趣者通过邮箱联系，另外也可对此工作进行打赏。"),
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
             tags$p("1. 上传格式报错")
             
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
                 tags$p("1、若初次使用，可先运行示例，并下载参考数据，了解数据格式与要求，
                        建议除非必要，勿修改参考数据的“列名”，列名大小写保持一致，仅仅替换其数据内容。"),
                 tags$p("2、使用："),
                 tags$p("方法一：点击“Show Data”后将自己的数据粘贴到“输入数据”表格。
                                若需要增减行数，可在输入表格框内右键。"),
                 tags$p("方法二：了解数据格式后编辑文件上传，再点击“Show Data”查看、“Analyze Data”运行。")
                 
    )
    ) )
  
}