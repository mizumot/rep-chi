library(shiny)
library(shinyAce)
library(exact2x2) #
library(vcd)
library(reshape2) #
library(coin) #



shinyUI(bootstrapPage(


    headerPanel("McNemar's Test and Cochran’s Q Test"),



########## Adding loading message #########

tags$head(tags$style(type="text/css", "
#loadmessage {
position: fixed;
top: 0px;
left: 0px;
width: 100%;
padding: 10px 0px 10px 0px;
text-align: center;
font-weight: bold;
font-size: 100%;
color: #000000;
background-color: #CCFF66;
z-index: 105;
}
")),

conditionalPanel(condition="$('html').hasClass('shiny-busy')",
tags$div("Loading...",id="loadmessage")),

########## Added up untill here ##########



    mainPanel(
        tabsetPanel(position = "left", selected = "McNemar's Test (Tabulated data)",

        tabPanel("McNemar's Test (Raw data)",

            h2("McNemar's Test (Raw data)"),

            h4("Paired nominal data (2 × 2 contingency table)"),
            h4("[Within-subjects chi-squared test]"),

            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row. Missing values should be indicated by a period (.) or NA.</div></b>")),

            aceEditor("text1", value="Pretest\tPosttest\nPass\tPass\nPass\tPass\nPass\tPass\nPass\tPass\nPass\tPass\nPass\tPass\nPass\tFail\nFail\tPass\nFail\tPass\nFail\tPass\nFail\tPass\nFail\tPass\nFail\tPass\nFail\tPass\nFail\tPass\nFail\tFail\nFail\tFail\nFail\tFail\nFail\tFail\nFail\tFail", mode="r", theme="cobalt"),

            br(),

            h3("Contingency table"),
            verbatimTextOutput("data1.out"),

            br(),

            h3("Test result"),
            verbatimTextOutput("test1.out"),

            br(),

            h3("Plot"),

            plotOutput("pPlot1"),

            br(),

            plotOutput("mPlot1", height = "550px"),

            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info1.out")
            ),









        tabPanel("McNemar's Test (Tabulated data)",

            h2("McNemar's Test (Tabulated data)"),

            h4("Paired nominal data (2 × 2 contingency table)"),
            h4("[Within-subjects chi-squared test]"),

            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row. Missing values should be indicated by a period (.) or NA.</div></b>")),

            aceEditor("text2", value="\tPost_Pass\tPost_Fail\nPre_Pass\t6\t1\nPre_Fail\t8\t5", mode="r", theme="cobalt"),

            br(),

            h3("Contingency table"),
            verbatimTextOutput("data2.out"),

            br(),

            h3("Test result"),
            verbatimTextOutput("test2.out"),

            br(),

            h3("Plot"),

            plotOutput("pPlot2"),

            br(),

            plotOutput("mPlot2", height = "550px"),

            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info2.out")
            ),










        tabPanel("Cochran’s Q Test (Raw data)",

            h2("Cochran’s Q Test (Raw data)"),

            h4("Related samples (repeated measures) with nominal data"),

            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row and the person's IDs in the first column. Missing values should be indicated by a period (.) or NA.</div></b>")),

            aceEditor("text3", value="Participant\tPre\tPost\tDelayed\n1\t1\t0\t1\n2\t0\t0\t1\n3\t0\t1\t0\n4\t0\t1\t1\n5\t0\t0\t1\n6\t1\t1\t1\n7\t0\t0\t1\n8\t0\t1\t1\n9\t0\t1\t1\n10\t0\t1\t1",mode="r", theme="cobalt"),

            br(),

            h3("Contingency table"),
            verbatimTextOutput("data3.out"),

            br(),

            h3("Test result"),
            verbatimTextOutput("test3.out"),

            br(),

            h3("Plot"),

            plotOutput("pPlot3"),

            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info3.out")
            ),










        tabPanel("About",

            strong('Note'),
            p('This web application is developed with',
            a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
            ''),

            br(),

            strong('List of Packages Used'), br(),
            code('library(shiny)'),br(),
            code('library(shinyAce)'),br(),
#code('library(pwr)'),br(),
#code('library(vcd)'),br(),

            br(),

            strong('Code'),
            p('Source code for this application is based on',
            a('"The handbook of Research in Foreign Language Learning and Teaching" (Takeuchi & Mizumoto, 2012).', href='http://mizumot.com/handbook/', target="_blank")),

            p('The code for this web application is available at',
            a('GitHub.', href='https://github.com/mizumot/rep-chi', target="_blank")),

            p('If you want to run this code on your computer (in a local R session), run the code below:',
            br(),
            code('library(shiny)'),br(),
            code('runGitHub("rep-chi","mizumot")')
            ),

            p('I referred to',
            a("this website", href="http://oku.edu.mie-u.ac.jp/~okumura/stat/mcnemar.html", target="_blank"),
            'for some parts of the codes. I would like to thank the author.'),

            br(),

            strong('Recommended'),
            p('To learn more about R, I suggest this excellent and free e-book (pdf),',
            a("A Guide to Doing Statistics in Second Language Research Using R,", href="http://cw.routledge.com/textbooks/9780805861853/guide-to-R.asp", target="_blank"),
            'written by Dr. Jenifer Larson-Hall.'),

            p('Also, if you are a cool Mac user and want to use R with GUI,',
            a("MacR", href="https://sites.google.com/site/casualmacr/", target="_blank"),
            'is defenitely the way to go!'),

            br(),

            strong('Author'),
            p(a("Atsushi MIZUMOTO,", href="http://mizumot.com", target="_blank"),' Ph.D.',br(),
            'Associate Professor of Applied Linguistics',br(),
            'Faculty of Foreign Language Studies /',br(),
            'Graduate School of Foreign Language Education and Research,',br(),
            'Kansai University, Osaka, Japan'),

            br(),

            a(img(src="http://i.creativecommons.org/p/mark/1.0/80x15.png"), target="_blank", href="http://creativecommons.org/publicdomain/mark/1.0/"),

            p(br())

)
)
)
))