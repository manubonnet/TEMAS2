##################################STEP 2 ############################???
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("shinyWidgets")
library(stringr)
options(shiny.maxRequestSize=1000*1024^2)



# Define UI for data upload app ----
ui <- fluidPage(
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    # App title ----
    titlePanel("First sort"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            fluidRow(
                column(6,
                       # Input: Select separator ----
                       radioButtons("sep", "Separator",
                                    choices = c(Comma = ",",
                                                Semicolon = ";",
                                                Tab = "\t"),
                                    selected = ",")     
                ),
                column(6,
                       # Input: Select quotes ----
                       radioButtons("quote", "Quote",
                                    choices = c(None = "",
                                                "Double Quote" = '"',
                                                "Single Quote" = "'"),
                                    selected = '"')
                )
            ),
            
            # Input: Select number of rows to display ----
            actionButton(inputId = "sauve", label = "Upload database"),
            uiOutput("tab")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            # tableOutput("contents"),
            fluidRow(
                h3("Step 1: Select the number of terms to remove"),
                sliderInput("slider1" , label="", min = 0, 
                            max = 6, value = 0),
                
                
            ),
            fluidRow(
                conditionalPanel( "input.slider1 > 0",
                                  hr(),
                                  h3("Step 2: Enter the term(s) to remove")
                ),
            ),
            fluidRow(
                column(4,
                       conditionalPanel(
                           condition = "input.slider1 > 0",
                           textInput("text1", label = h5("term to remove 1"), value = "NULL")),
                       conditionalPanel(
                           condition = "input.slider1 > 3",
                           textInput("text4", label = h5("term to remove 4"), value = "NULL"))),
                column(4,
                       conditionalPanel(
                           condition = "input.slider1 > 1",
                           textInput("text2", label = h5("term to remove 2"), value = "NULL")),
                       conditionalPanel(
                           condition = "input.slider1 > 4",
                           textInput("text5", label = h5("term to remove 5"), value = "NULL"))),
                column(4,
                       conditionalPanel(
                           condition = "input.slider1 > 2",
                           textInput("text3", label = h5("term to remove 3"), value = "NULL")),
                       conditionalPanel(
                           condition = "input.slider1 > 5",
                           textInput("text6", label = h5("term to remove 6"), value = "NULL")))),
            fluidRow(
                hr(),
                conditionalPanel( "input.slider1 == 0",
                                  h3("Step 2: Management of articles without abstract")
                ),
                conditionalPanel( "input.slider1 > 0",
                                  h3("Step 3: Management of articles without abstract")
                ),
                checkboxInput("checkbox", label = "Check the box to remove articles without abstracts (strongly recommended)", value = T),
                hr(),
                conditionalPanel( "input.slider1 == 0",
                                  h3("Step 3: Sort Database")
                ),
                conditionalPanel( "input.slider1 > 0",
                                  h3("Step 4: Sort Database")
                ),
                actionButton(inputId = "sort", label = "Sort database"),
                conditionalPanel( "input.sort",
                                  hr(),
                                  h3("Sort result :"),
                                  h4(textOutput("avant_tri")),
                                  h4(textOutput("tri")),
                                  downloadButton("downloadData", "Download sorted database") 
                )
                
            )
        )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
    
    data = reactiveValues()
    
    observeEvent(input$sauve, {
        sendSweetAlert(
            session = session,
            btn_labels = NA,
            title = "Saving database",
            text = "Please wait until \"Done !\" appears on your screen.",
            closeOnClickOutside = F,
            type = "warning"
        )
        data$table = read.csv(input$file1$datapath,
                              header = input$header,
                              sep = input$sep,
                              quote = input$quote)
        sendSweetAlert(
            session = session,
            title = "Done !",
            text = "Database saved !",
            type = "success"
        )  
        
    })
    output$test <- renderPrint({"attente tri"})
    data2 <- reactiveValues()
    observeEvent(input$sort, {
        sendSweetAlert(
            session = session,
            btn_labels = NA,
            title = "Sorting database",
            text = "Please wait until \"Done !\" appears on your screen.",
            closeOnClickOutside = F,
            type = "warning"
        )
        if (input$slider1>0) {
            memory <- c(input$text1, input$text2, input$text3, input$text4, input$text5,input$text6)
            for (i in 1:input$slider1) {
                data$table[,i+2] <- str_detect(data$table[,2] ,memory[i])
            }
            data$table[,input$slider1+3] <- F
            for (j in 1:nrow(data$table)) {
                i <- 1
                while(i <= input$slider1 & !data$table[j,(input$slider1+3)]) {
                    if (data$table[j,(i+2)]){data$table[j,(input$slider1+3)] <- T}
                    i <- i+1
                }
            }
            a <- 0
            if(input$checkbox){
                
                for (j in 1:nrow(data$table)) {
                    #               if (!data$table[j,(input$slider1+3)]) {
                    if (str_detect(str_sub(data$table[j,2],str_length(data$table[j,2])-2,str_length(data$table[j,2])),"na")){
                        data$table[j,(input$slider1+3)] <- T
                        a <- a+1
                        
                    }  
                    #                }
                    
                }
                a_mem <- reactiveValues()
                a_mem$a <- a
                print(a)
            }
            
        }
        if (input$slider1==0) {
            a <- 0
            data$table[,input$slider1+3] <- F
            if(input$checkbox){
                
                for (j in 1:nrow(data$table)) {
                    if (str_detect(str_sub(data$table[j,2],str_length(data$table[j,2])-2,str_length(data$table[j,2])),"na")){
                        data$table[j,3] <- T
                        a <- a+1
                    }  
                }
                a_mem <- reactiveValues()
                a_mem$a <- a
                print(a_mem$a)
                print(a)
            }
        }
        data2$tri <- subset(data$table, data$table[,(input$slider1+3)] == F)
        #           names(data2$tri<- c("uid","abstract",memory,"to remove"))
        data2$tri2 <- data2$tri[,1:2]
        print(a_mem$a)
        # data2$tri2[,2] <- gsub("['`^~\"]", " ", data2$tri2[,2])
        # data2$tri2[,2] <- iconv(data2$tri2[,2], to="ASCII//TRANSLIT")
        # data2$tri2[,2] <- gsub("['`^~,-><?!|\"]", "", data2$tri2[,2])
        # # data2$tri2[,2] <- gsub("\\d+\\s+", " ", data2$tri2[,2])
        # # data2$tri2[,2] <- gsub("\\s+\\d+", " ", data2$tri2[,2])
        # data2$tri2[,2] <- gsub("\\s+", " ", gsub("^\\s+|\\s+$", "", data2$tri2[,2]))
        data2$tri2[,2] <- iconv(data2$tri2[,2], to="ASCII//TRANSLIT//IGNORE")
        data2$tri2[,2] <- gsub("['`^~,-><?$!|\"]", "", data2$tri2[,2])
        data2$tri2[,2] <- gsub("[azertyuiopmlkjhgfdsqwxcvbn]+[AZERTYUIOPMLKJHGFDSQWXCVBN]+[azertyuiopmlkjhgfdsqwxcvbn]+", "", data2$tri2[,2])
        data2$tri2[,2] <- gsub("[AZERTYUIOPMLKJHGFDSQWXCVBN]+", "", data2$tri2[,2])
        data2$tri2[,2] <- gsub("[azertyuiopmlkjhgfdsqwxcvbn]*[a]{2,}[azertyuiopmlkjhgfdsqwxcvbn]*", "", data2$tri2[,2])
        data2$tri2[,2] <- gsub("\\s+", " ", gsub("^\\s+|\\s+$", "", data2$tri2[,2]))
        
        
        n_entr <- nrow(data2$tri2)
        data2$tri3 <- paste(data2$tri2[,1],data2$tri2[,2],sep="---")
        data2$tri4 <- paste(rep("**** *abstract_",n_entr),1:n_entr,"\n",sep = "")
        data2$tri5 <- paste(data2$tri4,data2$tri3,collapse = "\n",sep = "")
        
        
        sendSweetAlert(
            session = session,
            title = "Done !",
            text = "The database has been correctly sorted !",
            type = "success"
        )  
        
        
       
        value <- reactiveValues(download = 0)
        
        print(a_mem$a)
        output$test <-renderPrint({
            head(data2$tri2)})
        output$avant_tri <-renderText({paste(nrow(data$table), "articles before sorting,")})
        
        output$tri <-renderText({paste(nrow(data2$tri),"articles after sorting \n (",a_mem$a,
                                       "articles without abstracts, and",(as.numeric(nrow(data$table)-nrow(data2$tri))-as.numeric(a_mem$a)),"articles containing off-topic words removed)")})
        value$download <- 1
        
    })
    date_jour <- str_sub(date(),start = 9,end = 10)
    date_mois <- str_sub(date(),start = 5,end = 7)
    date_annee <- str_sub(date(),start = 21,end = 24)
    date_heure <- str_c(str_sub(date(),start = 12,end = 13),"h", str_sub(date(),start = 15,end = 16))
    
    name_id <- str_c("shiny.step2_",date_jour,"_",date_mois, "_" , date_annee,"_" ,date_heure,".txt")
    output$downloadData <- downloadHandler(
        filename = function() {
            name_id
        },
        content = function(file) {
            write.table(data2$tri5,file,row.names = F, col.names = F,quote = F)
        }
    )
    url <- a("step_Classif", href=" https://step3.temas-bonnet.site/TEMASC") 
    output$tab <- renderUI({ 
        tagList("Link to step_classif:", url) 
    }) 
}
# Create Shiny app ----
shinyApp(ui, server)