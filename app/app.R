### GLOBAL SHINY APP (INCLUDING UI AND SERVER SCRIPTS)

## 1. LOAD  REQUIRED LIBRARIES
require("utils") # To unzip files
require("qdapRegex") # Regular expression removal, extraction, and
# replacement tools to clean training set
require("tm") # Basic framework for text mining applications within R
require("slam") # To compute frequencies from tm Term-Document Matrices
require("textreg") # To convert tm corpus into character vector
require("parallel") # For parallel computation
require("RWeka") # To tokenize words from text
require("stringr") # To split columns from matrix as part of the process to
# make ngrams
require("digest") # To apply cryptographical hash functions to benchmark text
require("data.table") # For faster data manipulation

# Load Shiny app packages
require("shiny") # To create Shiny apps
require("DT") # To customize data tables

## 2. UI SCRIPT

# Define user interface for text predictive application
ui <- shinyUI(
        
        navbarPage(title = "",
                   
                   tabPanel(icon = icon("dashboard"),
                            title = "Application",
                            
                            includeCSS(path = "styles.css"),
                            
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"), # To turn off output errors in red while data is flowing
                            
                            titlePanel(title = div(img(src='title1.png',
                                                       align = "center"),
                                                   style="text-align: center;"),
                                       windowTitle = "Magic Keys, a Coursera Data Science Capstone Project"),
                            
                            h4("a Coursera Data Science Capstone Project",
                               align = "center"),
                            
                            hr(),
                            
                            fluidRow(align = "center",
                                     
                                     div(style="display: inline-block;vertical-align:top; width: 100px;",
                                         actionButton(inputId = "nextwordButton1",
                                                      label = textOutput(outputId = "nextword1"),
                                                      style = "color: #fff; background-color: #136689; border-color: #2e6da4; 'font-size:200%'")),
                                     
                                     div(style="display: inline-block;vertical-align:center; width: 25px;",
                                         HTML("<br>")),
                                     
                                     div(style="display: inline-block;vertical-align:top; width: 100px;",
                                         actionButton(inputId = "nextwordButton2",
                                                      label = textOutput(outputId = "nextword2"),
                                                      style = "color: #fff; background-color: #136689; border-color: #2e6da4; 'font-size:200%'")),
                                     
                                     div(style="display: inline-block;vertical-align:center; width: 25px;",
                                         HTML("<br>")),
                                     
                                     div(style="display: inline-block;vertical-align:top; width: 100px;",
                                         actionButton(inputId = "nextwordButton3",
                                                      label = textOutput(outputId = "nextword3"),
                                                      style = "color: #fff; background-color: #136689; border-color: #2e6da4; 'font-size:200%'")),
                                     
                                     div(style="display: inline-block;vertical-align:center; width: 25px;",
                                         HTML("<br>")),
                                     
                                     div(style="display: inline-block;vertical-align:top; width: 100px;",
                                         actionButton(inputId = "nextwordButton4",
                                                      label = textOutput(outputId = "nextword4"),
                                                      style = "color: #fff; background-color: #136689; border-color: #2e6da4; 'font-size:200%'")),
                                     
                                     div(style="display: inline-block;vertical-align:center; width: 25px;",
                                         HTML("<br>")),
                                     
                                     div(style="display: inline-block;vertical-align:top; width: 100px;",
                                         actionButton(inputId = "nextwordButton5",
                                                      label = textOutput(outputId = "nextword5"),
                                                      style = "color: #fff; background-color: #136689; border-color: #2e6da4; 'font-size:200%'"))
                                     
                            ),
                            
                            fluidRow(align = "center",
                                     textInput(inputId = "phrase",
                                               label = "",
                                               value = "",
                                               width = "25%",
                                               placeholder = "Type here if you believe in magic")
                                     
                            ),
                            
                            hr(),
                            
                            fluidRow(align = "center",
                                     
                                     div(style="display: inline-block;vertical-align:center; width: 200px;",
                                         selectInput(inputId = "language",
                                                     label = "Select language",
                                                     choices = list("English" = "english"),
                                                     selected = "english")
                                     ),
                                     
                                     div(style="display: inline-block;vertical-align:center; width: 100px;",
                                         HTML("<br>")),
                                     
                                     div(style="display: inline-block;vertical-align:center; width: 300px;",
                                         sliderInput(inputId = "numWords", 
                                                     label = h4("Number of words to be predicted"), 
                                                     value = 5,
                                                     min = 1, 
                                                     max = 5)
                                     ),
                                     
                                     div(style="display: inline-block;vertical-align:center; width: 100px;",
                                         HTML("<br>")),
                                     
                                     div(style="display: inline-block;vertical-align:center; width: 200px;",
                                         checkboxInput(inputId = "noSwearWords",
                                                       label = "Please, hide swear words",
                                                       value = TRUE)
                                     )
                                     
                            ),
                            
                            hr(),
                            
                            fluidRow(align = "center",
                                     
                                     div(style="display: inline-block;vertical-align:center; width: 700px;",
                                         wellPanel(align = "left",
                                                   h4("Instructions"),
                                                   
                                                   p("To try the app, just enter text in the upper text box. The prediction of the next word occurs instantly. Five candidates are provided above according to their backoff scores from left to right. The green buttons can be pressed to enter that candidate in the text field and start the process again. In addition, a table is displayed at bottom right corner to show the candidates and their respective scores."),
                                                   
                                                   p("Moreover, the widgets on the central part of the app allows you to control for the language-currently only English is available-, the number of words to be predicted-the maximum is five words-, and the option to show/hide bad words."),
                                                   
                                                   p("For more information about the statistical model behind this application and other additional issues, click on the tab 'Documentation' above. Enjoy the app!")
                                                   
                                         )
                                     ),
                                     
                                     div(style="display: inline-block;vertical-align:center; width: 100px;",
                                         HTML("<br>")),
                                     
                                     div(style="display: inline-block;vertical-align:top; width: 400px; color:#136689",
                                         conditionalPanel(condition = "input.phrase != 0",
                                                          dataTableOutput(outputId = "predictionTable"),
                                                          style='font-size:100%'))
                                     
                            ),
                            
                            hr(),
                            
                            fluidRow(align = "center",
                                     div(style="display: inline-block;vertical-align:top; width: 700px;",
                                         p("DEVELOPED by ",
                                           a("Antonio Serrano",
                                             href = "https://github.com/AntonioSerrano",
                                             target="_blank"),
                                           " | DATA obtained from ",
                                           a("HC Corpora",
                                             href = "http://www.corpora.heliohost.org/",
                                             target="_blank")
                                         )
                                     )
                                     
                            )
                   ),
                   
                   tabPanel(tags$style(HTML("
        .tabs-above > .nav > li[class=active] > a {
           background-color: #136689;
           color: #FFF;
        }")), # Change subtabs colors within about tab
                            icon = icon("newspaper-o"),
                            title = "Documentation",
                            tabsetPanel(
                                    
                                    tabPanel("Introduction",
                                             mainPanel(includeMarkdown("documentation1.Rmd"))),
                                    
                                    tabPanel("Data and Sample",
                                             mainPanel(includeMarkdown("./documentation2.Rmd"))),
                                    
                                    tabPanel("Text transformations",
                                             mainPanel(includeMarkdown("./documentation3.Rmd"))),
                                    
                                    tabPanel("Unknown words and profanity treatment",
                                             mainPanel(includeMarkdown("./documentation4.Rmd"))),
                                    
                                    tabPanel("N-gram model and smoothing technique",
                                             mainPanel(includeMarkdown("./documentation5.Rmd"))),
                                    
                                    tabPanel("Benchmarks",
                                             mainPanel(includeMarkdown("./documentation6.Rmd"))),
                                    
                                    tabPanel("R session info",
                                             mainPanel(includeMarkdown("./documentation7.Rmd"))),
                                    
                                    tabPanel("References",
                                             mainPanel(includeMarkdown("./documentation8.Rmd"))),
                                    
                                    tabPanel("About me",
                                             mainPanel(includeMarkdown("./documentation9.Rmd")))
                                    
                            )
                   )
        )
)

## 3. SERVER SCRIPT

# Import input data from previous script "PredictionModel.R"
load("./inputData.RData", envir = .GlobalEnv)

# Generate functions to upper predictive keys above the input text bar
scoreNgramsVector1 <- function(phrase, language, numWords, noSwearWords) {
        if (language == "english" & numWords >= 1) {
                return(myPredictiveModel(x = phrase, nrows = 20, showNresults = 1, removeProfanity = noSwearWords, outputType = TRUE)[1])
        }
}

scoreNgramsVector2 <- function(phrase, language, numWords, noSwearWords) {
        if (language == "english" & numWords >= 2) {
                return(myPredictiveModel(x = phrase, nrows = 20, showNresults = 2, removeProfanity = noSwearWords, outputType = TRUE)[2])
        }
}

scoreNgramsVector3 <- function(phrase, language, numWords, noSwearWords) {
        if (language == "english" & numWords >= 3) {
                return(myPredictiveModel(x = phrase, nrows = 20, showNresults = 3, removeProfanity = noSwearWords, outputType = TRUE)[3])
        }
}

scoreNgramsVector4 <- function(phrase, language, numWords, noSwearWords) {
        if (language == "english" & numWords >= 4) {
                return(myPredictiveModel(x = phrase, nrows = 20, showNresults = 4, removeProfanity = noSwearWords, outputType = TRUE)[4])
        }
}

scoreNgramsVector5 <- function(phrase, language, numWords, noSwearWords) {
        if (language == "english" & numWords >= 5) {
                return(myPredictiveModel(x = phrase, nrows = 20, showNresults = 5, removeProfanity = noSwearWords, outputType = TRUE)[5])
        }
}

scoreNgramsTable <- function(phrase, language, numWords, noSwearWords) {
        if (language == "english") {
                return(myPredictiveModel(x = phrase, nrows = 20, showNresults = numWords, removeProfanity = noSwearWords, outputType = FALSE))
        }
}

# Define server logic for text predictive application
server <- shinyServer(function(input, output, session) {
        
        phraseGo <- reactive(input$phrase)
        
        output$nextword1 <- renderText({
                result <- scoreNgramsVector1(phraseGo(), input$language, input$numWords, input$noSwearWords)
                paste0(result)
        })
        
        observeEvent(input$nextwordButton1, {
                result <- scoreNgramsVector1(phraseGo(), input$language, input$numWords, input$noSwearWords)
                result <- paste0(result)
                pastedPhrase <- paste(input$phrase, result)
                updateTextInput(session, "phrase", value = pastedPhrase)
        }
        )
        
        output$nextword2 <- renderText({
                result <- scoreNgramsVector2(phraseGo(), input$language, input$numWords, input$noSwearWords)
                paste0(result)
        })
        
        observeEvent(input$nextwordButton2, {
                result <- scoreNgramsVector2(phraseGo(), input$language, input$numWords, input$noSwearWords)
                result <- paste0(result)
                pastedPhrase <- paste(input$phrase, result)
                updateTextInput(session, "phrase", value = pastedPhrase)
        }
        )
        
        output$nextword3 <- renderText({
                result <- scoreNgramsVector3(phraseGo(), input$language, input$numWords, input$noSwearWords)
                paste0(result)
        })
        
        observeEvent(input$nextwordButton3, {
                result <- scoreNgramsVector3(phraseGo(), input$language, input$numWords, input$noSwearWords)
                result <- paste0(result)
                pastedPhrase <- paste(input$phrase, result)
                updateTextInput(session, "phrase", value = pastedPhrase)
        }
        )
        
        output$nextword4 <- renderText({
                result <- scoreNgramsVector4(phraseGo(), input$language, input$numWords, input$noSwearWords)
                paste0(result)
        })
        
        observeEvent(input$nextwordButton4, {
                result <- scoreNgramsVector4(phraseGo(), input$language, input$numWords, input$noSwearWords)
                result <- paste0(result)
                pastedPhrase <- paste(input$phrase, result)
                updateTextInput(session, "phrase", value = pastedPhrase)
        }
        )
        
        output$nextword5 <- renderText({
                result <- scoreNgramsVector5(phraseGo(), input$language, input$numWords, input$noSwearWords)
                paste0(result)
        })
        
        observeEvent(input$nextwordButton5, {
                result <- scoreNgramsVector5(phraseGo(), input$language, input$numWords, input$noSwearWords)
                result <- paste0(result)
                pastedPhrase <- paste(input$phrase, result)
                updateTextInput(session, "phrase", value = pastedPhrase)
        }
        )
        
        output$predictionTable <- renderDataTable({
                datatable(
                        data = scoreNgramsTable(phraseGo(), input$language, input$numWords, input$noSwearWords),
                        options = list(searching = FALSE, paging=FALSE, bInfo=FALSE)
                )
        }
        )
        
})

## 4. RUN THE APP

shinyApp(ui = ui, server = server) # Finish!!! ;)
