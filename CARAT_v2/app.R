#Author: Natalia Levshina
#Date of last change: 03.08.2018
#Version: 0.2
#New:
#1. Open the file with the corpus from Shiny
#2. Correction: if no nominal A or P -> no pronoun/noun and no subcategory (NA, NA, NA)
#3. Reordered some of the default values, for convenience
#4. Changed identifiability


#To-do list: 
#1. Go to sentence (?)
#2. Re-write the same sentence instead of adding a new line

#Instructions for working with different languages:
#To represent non-ascii characters:
#Russian: type the following line in RStudio (without the hash): 
#Sys.setlocale(locale = "Russian")
#for ngh: I haven't found the right locale yet.


library(shiny)



#datasets <- c("test_corpus", "SBC004", "ngh_sample", "rus_sample") #now downloadable by the user

ui = fluidPage(

  tags$head(
    tags$style(
      HTML(".shiny-notification {
           position:fixed;
           top: calc(40%);
           left: calc(40%);
          height: 50px;
          width: 200px;
           }
           "
      )
      )
      ),
  
  
tabsetPanel(
      
    id = "inTabset",
              
tabPanel(title = "Choose the data", value = "panel1",
      
             # Sidebar layout with input and output definitions ----
        sidebarLayout(
               
               # Sidebar panel for inputs ----
          sidebarPanel(
                 
                 # Input: Select a file ----
                 fileInput("file1", "Choose file",
                           multiple = FALSE,
                           accept = c("text/plain")),
                 
                 # Horizontal line ----
                 tags$hr(),
                

               textInput(inputId = 'inputsLocation', 
                         label = h4('Choose the file to save results'), value = "user_inputs.txt")
             ),                
               
               # Main panel for displaying outputs ----
mainPanel(

#selectInput("dataset", h4("Choose a dataset"),
#                      choices = datasets),
          
        wellPanel(
          h4("Current sentence:"),
               textOutput("sentenceText")
        ),
               
               #choose the sentence ID to start with, 1 by default
        fluidRow(
          column(width = 4,
                 actionButton("buttonPrevious", h4("Previous"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                 ), 
          column(width = 4, actionButton("buttonNext", h4("Next"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                 )
          ),
               

               #choose the sentence ID to start with, 1 by default
        #numericInput("goID", "Go to the sentence", 1),    
        #       actionButton("confirmSentenceID", "Go"),
        # 
          hr(),     
          "Code the sentence if it is transitive.", br(),
          actionButton("buttonCode", h4("Code the sentence"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               
          hr(),
               
               # Output: Data file ----
          wellPanel(h4("Corpus"),
               tableOutput("contents")         
               )
        )
        )),


tabPanel(
    title = "Check predicate", value = "panel2",               
    wellPanel(h4("Current sentence:"), textOutput("sentenceText2")),
    hr(),
    fluidRow(column(width = 4, textInput(inputId = "Predicate", label = h4("Specify the Predicate")
                                         )
                    )
             ),
    hr(),
    h4("Check if the predicate meets all of the criteria below:"),
    tags$ul(
              tags$li(HTML("The predicate is active, e.g. <em>Mary built a house</em>, and not <em>The house was built by Mary</em>.")),
              
              tags$li(HTML("The predicate is NOT a reflexive or reciprocal verb, as in <em>He washes himself</em> or <em>They hugged each other.</em>")),
              
              #tags$li(HTML("The predicate is NOT in the imperative mood, as in <em>Cook the dinner!</em>")),
              
              tags$li(HTML("The meaning of the predicate, A and P is compositional, e.g. <em>Mary builds a house</em>, but not <em>The house caught fire.</em>")),
              
              tags$li(HTML("The predicate is either simple or complex with the same subject, e.g. <em>Mary builds a house</em> or <em>Mary intends to build a house</em>, but not <em>Mary wants John to build a house</em>."))
              
              #tags$li("Neither A nor P are expressed by complementizers or relativizers, e.g. This is the house that (P) Jack (A) built.")
              ),
              
        "Are all the criteria fulfilled?",
        actionButton("buttonValidity", h4("Yes!"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
      
tabPanel(
  
    title = "Code A", value = "panel3",                
    wellPanel(h4("Current sentence:"), textOutput("sentenceText3")
    ),
  fluidRow(column(width = 4, selectInput(inputId = "A_Person", label = h4("Person of A"), 
                                choices = list("1st Person" = "A_1P", "2nd Person" = "A_2P", "3rd Person" = "A_3P", "Difficult to say" = "A_undefP", "Impersonal" = "A_impersP", "Clause" = "A_clauseP"), 
                                selected = 1)
                  ),
        column(width = 4, selectInput(inputId = "A_Number", label = h4("Number of A"), 
                                choices = list("Singular" = "A_SG", "Plural" = "A_PL", "Difficult to say" = "A_undefN", "Impersonal" = "A_impersN", "Clause" = "A_clauseN"), 
                                selected = 1)
               )
        ),
    
  fluidRow(column(width = 4, selectInput(inputId = "A_Sem", label = h4("Semantic class of A"), 
                                choices = list("Human" = "A_Hum", "Animal" = "A_Anim", "Physical Object" = "A_Phys", "Abstract entity" = "A_Abstr", "Event" = "A_Event", "Organization" = "A_Org", "Anthropomorphic" = "A_Anthro", "Kinship term" = "A_Kin", "Unspecified animate" = "A_UndefAnimate", "Unspecified inanimate" = "A_UndefInanimate", "Difficult to say" = "A_UndefSem", "Clause" = "A_clauseSem"), 
                                selected = 1)
                  ),

          column(width = 4, selectInput(inputId = "A_Ident", label = h4("Identifiability of A"), 
                                choices = list("A is unique in its kind, anaphoric or identifiable from context" = "A_Def", "One can replace A with 'a certain/particular A'" = "A_Spec", "Generic use" = "A_Gen", "Other" = "A_NonSpec", "Difficult to say" = "A_UndefIdent", "Not applicable (interrogative and impersonal pronouns, clauses, etc.)" = "A_NAIdent"), 
                                selected = 1)
                 ),
          column(width = 4, selectInput(inputId = "A_Given", label = h4("Givenness of A"), 
                                            choices = list("Given (mentioned previously or inferrable from context)" = "A_Given", "New (neither mentioned, nor inferrable)" = "A_New", "Difficult to say" = "A_UndefGiven", "Not applicable (interrogative and impersonal pronouns, clauses, etc.)" = "A_NAGiven"), 
                                            selected = 1)
                 )
          ),
      fluidRow(column(width = 2, wellPanel(radioButtons("A_radio", label = h4("Is A a nominal?"),
                            choices = list("Yes" = 1, "No" = 2), 
                            selected = 2)
                            )
                      ), 
               column(width = 10, wellPanel(uiOutput("ui_explicit")
                                            )
                      )
               ),
  
  actionButton("buttonGoToP", h4("Next"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")  
               #textInput(inputId = "A_head", label = "Enter A")
),

tabPanel(
    title = "Code P", value = "panel4",                
    wellPanel(h4("Current sentence:"), textOutput("sentenceText4")),   
    

    fluidRow(column(width = 4, selectInput(inputId = "P_Person", label = h4("Person of P"), 
                                           choices = list("1st Person" = "P_1P", "2nd Person" = "P_2P", "3rd Person" = "P_3P", "Difficult to say" = "P_undefP", "Clause" = "P_clauseN"), 
                                           selected = 1)
                     ),
             column(width = 4, selectInput(inputId = "P_Number", label = h4("Number of P"), 
                                           choices = list("Singular" = "P_SG", "Plural" = "P_PL", "Difficult to say" = "P_undefN", "Clause" = "P_clauseN"), 
                                           selected = 1)
                    )
             ),
    
    fluidRow(column(width = 4,selectInput(inputId = "P_Sem", label = h4("Semantic class of P"), 
                choices = list("Human" = "P_Hum", "Animal" = "P_Anim", "Physical Object" = "P_Phys", 
                               "Abstract entity" = "P_Abstr", "Event" = "P_Event", "Organization" = "P_Org", 
                               "Anthropomorphic" = "P_Anthro", "Kinship terms" = "P_Kin", "Unspecified animate" = "P_UndefAnimate", "Unspecified inanimate" = "P_UndefInanimate", "Difficult to say" = "P_UndefSem", "Clause" = "P_clauseSem"), 
                selected = 1)
                ),
    
     column(width = 4, selectInput(inputId = "P_Ident", label = h4("Identifiability of P"), 
                                           choices = list("P is unique in its kind, anaphoric or identifiable from context" = "P_Def", "One can replace P with 'a certain/particular P'" = "P_Spec", "Generic use" = "P_Gen", "Other" = "P_NonSpec", "Difficult to say" = "P_UndefIdent", "Not applicable (interrogative and impersonal pronouns, clauses, etc.)" = "P_NAIdent"), 
                                           selected = 1)
            ),
    column(width = 4, selectInput(inputId = "P_Given", label = h4("Givenness of P"), 
                                           choices = list("Given (mentioned previously or inferrable from context)" = "P_Given", "New (neither mentioned, nor inferrable)" = "P_New", "Difficult to say" = "P_UndefGiven", "Not applicable (interrogative and impersonal pronouns, clauses, etc.)" = "P_NAGiven"), 
                                           selected = 1)
           )
    ),
    
    fluidRow(column(width = 2, wellPanel(radioButtons("P_radio", label = h4("Is P a nominal?"),
                           choices = list("Yes" = 1, "No" = 2), 
                           selected = 2)
                           )
                    )
             ,
    
            column(width = 10, wellPanel(uiOutput("ui_explicitP")
                                         )
                   )
            ),
            
    actionButton("goToClause", h4("Next"), 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  ),

tabPanel(
  
  title = "Code clause properties", value = "panel5",   
  
  wellPanel(h4("Current sentence:"), textOutput("sentenceText5")
            ),   
  
  fluidRow(column(width = 4, wellPanel(radioButtons("ClauseType", label = h4("Type of clause"),
                                                    choices = list("Main" = "Clause_Main", "Subordinate" = "Clause_Sub"), 
                                                    selected = "Clause_Main")
                                       )
                  ),
           column(width = 4, wellPanel(radioButtons("ClauseNeg", label = h4("Is there negation?"),
                                                    choices = list("No" = "Clause_Pos", "Yes" = "Clause_Neg"), 
                                                    selected = "Clause_Pos")
                                        )
                  ),
           column(width = 4, wellPanel(radioButtons("ClauseTense", label = h4("Tense"),
                                                    choices = list("Present" = "Clause_Pres", "Past" = "Clause_Past", "Other" = "Clause_Other"), 
                                                    selected = "Clause_Pres")
           )
           )
  ),
  
  actionButton("saveCoding", h4("Save the results"), icon("paper-plane"), 
               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
               ),
  hr(),
  actionButton("goToStart", h4("Choose the next sentence"), 
               style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  
)
)
)
 
server <- function(input, output, session) {

selectedDataset <- eventReactive(input$file1, {  

  # when reading semicolon separated files,
  # having a comma separator causes `read.csv` to error
tryCatch(
    {
      readLines(input$file1$datapath, encoding = "UTF-8")
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )

  
})

  
  
v <- reactiveValues()
v$sentenceID <- 1

#selectedDataset <- reactive({
#switch(input$dataset, "test_corpus" = test_corpus, "SBC004" = SBC004, "ngh_sample" = ngh_sample, "rus_sample" = rus_sample)  #!!!CORPUS!!! The corpus should be here.
#})


observeEvent(input$buttonNext, {
  v$sentenceID <- v$sentenceID + 1
  })

observeEvent(input$buttonPrevious, {
  v$sentenceID <- v$sentenceID - 1
})

output$sentenceText <-  renderText({
  selectedDataset()[v$sentenceID]
  })

output$sentenceText2 <-  renderText({
  selectedDataset()[v$sentenceID]
})

output$sentenceText3 <-  renderText({
  selectedDataset()[v$sentenceID]
})

output$sentenceText4 <-  renderText({
  selectedDataset()[v$sentenceID]
})

output$sentenceText5 <-  renderText({
  selectedDataset()[v$sentenceID]
})

output$contents <- renderText({selectedDataset()})

observeEvent(input$buttonCode, {
    updateTabsetPanel(session, inputId = "inTabset",
                      selected = "panel2")})


observeEvent(input$buttonValidity, {
  updateTabsetPanel(session, inputId = "inTabset",
  selected = "panel3")
  }
  )


output$ui_explicit <- renderUI({
  if (input$A_radio == 1){
    fluidRow(column(width = 4, textInput(inputId = "A_Head", label = h4("Specify A Head")
                                         )
                    ),
           column(width = 4, selectInput(inputId = "A_POS", label = h4("Part of Speech of A Head"), 
                                                   choices = list("Pronoun" = "A_pronoun", "Noun" = "A_noun", "Other" = "A_other"), 
                                                   selected = 1)
                  ),
          column(width = 4, selectInput(inputId = "A_Subcat", label = h4("Subcategory of A Head"), 
                                                   choices = list("Personal Pronoun" = "A_PersPro", "Common Noun" = "A_ComNoun", "Proper Noun (persons, institutions, places)" = "A_PropNoun", "Possessive Pronoun" = "A_PossPro", "Demonstrative Pronoun" = "A_DemPro", "Interrogative Pronoun" = "A_InterPro", "Other" = "A_OthPro"), 
                                                   selected = 1
                                                   )
                  )
           )
  } 
  else{return(NULL)
}  
  })

output$ui_explicitP <- renderUI({
  if (input$P_radio == 1){
    fluidRow(column(width = 4, textInput(inputId = "P_Head", label = h4("Specify P Head")
                                         )
                    ),
            column(width = 4, selectInput(inputId = "P_POS", label = h4("Part of Speech of P Head"), 
                                                   choices = list("Pronoun" = "P_pronoun", "Noun" = "P_noun", "Adjective" = "P_adj", "Other" = "P_other"), 
                                                   selected = 1)
                   ),
          column(width = 4, selectInput(inputId = "P_Subcat", label = h4("Subcategory of P Head"), 
                                                   choices = list("Personal Pronoun" = "P_PersPro", "Common Noun" = "P_ComNoun", "Proper Noun (persons, institutions, places)" = "P_PropNoun", "Possessive Pronoun" = "P_PossPro", "Demonstrative Pronoun" = "P_DemPro", "Interrogative Pronoun" = "P_InterPro", "Other" = "P_OthPro"), 
                                                   selected = 1)
                 )
          )
  } 
  else{return(NULL)}
})

observeEvent(input$buttonGoToP, {
  updateTabsetPanel(session, inputId = "inTabset",
                    selected = "panel4")
})

observeEvent(input$goToClause, {
  updateTabsetPanel(session, inputId = "inTabset",
                    selected = "panel5")
}
)


A_text1 <- reactive({input$A_Person})
A_text2 <- reactive({input$A_Number})
A_text3 <- reactive({input$A_Sem})
A_text4 <- reactive({input$A_Ident})
A_text5 <- reactive({input$A_Given})
A_text6 <- reactive({input$A_Head})
A_text7 <- reactive({input$A_POS})
A_text8 <- reactive({input$A_Subcat})  

P_text1 <- reactive({input$P_Person})
P_text2 <- reactive({input$P_Number})
P_text3 <- reactive({input$P_Sem})
P_text4 <- reactive({input$P_Ident})
P_text5 <- reactive({input$P_Given})
P_text6 <- reactive({input$P_Head})
P_text7 <- reactive({input$P_POS})
P_text8 <- reactive({input$P_Subcat})


C_text1 <- reactive({input$ClauseType})
C_text2 <- reactive({input$ClauseNeg})
C_text3 <- reactive({input$ClauseTense})
Predicate_text <- reactive({input$Predicate})

observeEvent(input$saveCoding, 
{  showNotification("The results are saved to the file!", duration = 3, type = "message")             
outmatrixClause <-  matrix(NA, ncol = 5)
elementsClause <- c(v$sentenceID, C_text1(), C_text2(), C_text3(), Predicate_text())
for (i in 1:5){
if (isTruthy(elementsClause[i])){
  outmatrixClause[i] <- elementsClause[i]  
}  
}
outmatrixA <- matrix(NA, ncol = 8) 
elementsA <- c(A_text1(), A_text2(), 
              A_text3(), A_text4(), A_text5(), A_text6(), A_text7(), 
              A_text8())
for (i in 1:8){
  if (isTruthy(elementsA[i])){
    outmatrixA[i] <- elementsA[i]  
  }  
}

if (input$A_radio == 2){
outmatrixA[6] <- "NA"
outmatrixA[8] <- "NA"
outmatrixA[7] <- "NA"
}

outmatrixP <- matrix(NA, ncol = 8) 
elementsP <- c(P_text1(), P_text2(), 
              P_text3(), P_text4(), P_text5(), P_text6(), 
              P_text7(), P_text8())
for (i in 1:8){
  if (isTruthy(elementsP[i])){
    outmatrixP[i] <- elementsP[i]  
  }  
}

if (input$P_radio == 2){
  outmatrixP[6] <- "NA"
  outmatrixP[8] <- "NA"
  outmatrixP[7] <- "NA"
}



write.table(data.frame(t(c(outmatrixClause, outmatrixA, outmatrixP))), input$inputsLocation, row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE, sep = ",")
})

observeEvent(input$goToStart, {
  updateTabsetPanel(session, inputId = "inTabset",
                    selected = "panel1")
}
)

}


#sentenceID <- eventReactive(input$buttonPrevious, {startID - 1}) 

    
# input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
#req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
  #tryCatch(
  #    {
  #      df <- read.csv(input$file1$datapath,
  #                     header = input$header,
  #                     sep = input$sep,
  #                     quote = input$quote)
  #   },
  #    error = function(e) {
  #     # return a safeError if a parsing error occurs
  #      stop(safeError(e))
  #    }
  #  )
  #  
  #  if(input$disp == "head") {
  #    return(head(df))
  #  }
  #  else {
  #    return(df)
  #  }
  #  
  #})




shinyApp(ui = ui, server = server)
