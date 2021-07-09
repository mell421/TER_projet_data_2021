library(shiny)

if(require(shiny)){

    library(wordcloud2)
    #library('rsconnect')
    suppressWarnings(source("./fctR/sources.R"))
    suppressWarnings(library(tidyverse))

    # Define the UI
    ui <- fluidPage(
        titlePanel("text"),
        sidebarLayout(
            sidebarPanel(
                selectInput("fct","source:",
                            list("demofreq"="demofreq",
                                 "demofreqc"="demofreqc",
                                 "hp"="hp",
                                 "spring"="spring",
                                 "texte"="texte",
                                 "file"="file",
                                 "sheet"="file2"
                            )
                ),

                selectInput("form","form:",
                            list("circle"="circle",
                                 "cardioid"="cardioid",
                                 "diamond"="diamond",
                                 "triangle-forward"="triangle-forward",
                                 "triangle"="triangle",
                                 "pentagon"="pentagon",
                                 "star"="star"
                            )
                ),
                conditionalPanel(condition = "input.fct == 'texte'",
                    textAreaInput("texte","text:","")
                ),
                conditionalPanel(condition = "input.fct == 'file'",
                    textInput("file", "file address(txt/md):","./txt/")
                ),
                conditionalPanel(condition = "input.fct == 'file2'",
                    textInput("file2", "your file address(sheets):","https://docs.google.com/spreadsheets/d/")
                ),

                numericInput(inputId ="size", 'freq min in the table', 1),
                numericInput(inputId ="size3", 'nb in barplot', 30),

                textInput(inputId ="color","color(random-light,random-dark,other)", "random-light"),


                textInput(inputId ="bgc","backgroundcolor", "black")
            ),
            mainPanel(
                tabsetPanel(
                    type = "tabs",
                    tabPanel("wordcloud",
                             wordcloud2Output('wordcloud2')
                    ),
                    tabPanel("table",
                             dataTableOutput('table')
                    ),
                    tabPanel("barplot",
                             plotOutput('barplot')
                    ),
                    tabPanel("heat",
                             plotOutput('heat')
                    )
                )


            )
        )


    )


    # Define the server code
    server <- function(input, output) {
        output$wordcloud2 <- renderWordcloud2({
            if(input$fct == "demofreq"){
                data <- demoFreq
                data <- data %>% filter(freq >= input$size)
                return(wordcloud2(data, color = input$color, size=1,backgroundColor=input$bgc,shape=input$form))
            } else if(input$fct == "demofreqc"){
                data <- demoFreqC
                data <- data %>% filter(freq >= input$size)
                return(wordcloud2(data, color = input$color, size=1,backgroundColor=input$bgc,shape=input$form))
            } else if(input$fct == "hp"){
                hp <- read_lines("./txt/HP1.txt")
                texte <- hp
            } else if(input$fct == "spring"){
                spring <- read_lines("./txt/spring.txt")
                texte <- spring
            } else if(input$fct == "file"){
                tex <- input$file
                texte <- read_lines(tex)
            } else if(input$fct == "file2"){
                table <- suppressWarnings(gsheet2tbl(input$file2))
                texte <- data.frame(table)
            } else if(input$fct == "texte"){
                text <- input$texte
                texte <- read_lines(text)
            }

            main <- function(texte){
                TextDoc <- Corpus(VectorSource(texte))

                #Replacing "/", "@" and "|" with space
                toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
                removeSpace <- content_transformer(function (x , pattern ) gsub(pattern, "", x))
                TextDoc <- tm_map(TextDoc, toSpace, "/")
                TextDoc <- tm_map(TextDoc, toSpace, "@")
                TextDoc <- tm_map(TextDoc, toSpace, "\\|")
                # Convert the text to lower case
                TextDoc <- tm_map(TextDoc, content_transformer(tolower))
                # Remove numbers
                TextDoc <- tm_map(TextDoc, removeNumbers)
                # Remove english common stopwords
                TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))

                # Remove your own stop word
                # specify your custom stopwords as a character vector
                TextDoc <- tm_map(TextDoc, removeWords, c("the","and","-"))
                # Remove punctuations
                TextDoc <- tm_map(TextDoc, removePunctuation)
                # Eliminate extra white spaces
                TextDoc <- tm_map(TextDoc, stripWhitespace)
                # Eliminate spaces
                # TextDoc <- gsub("[[:blank:]]", "", TextDoc)


                # Build a term-document matrix
                TextDoc_dtm <- TermDocumentMatrix(TextDoc)
                dtm_m <- as.matrix(TextDoc_dtm)
                # Sort by descearing value of frequency
                dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
                dtm_d <- data.frame(word = names(dtm_v) ,freq=dtm_v)
                # Display the top 20 most frequent words
                head(dtm_d, 30)
                dtm_d <- dtm_d %>% filter(freq >= input$size)
                #generate word cloud

                wordcloud2(data = dtm_d, color = input$color, size=1,backgroundColor=input$bgc,shape=input$form)
            }
            main(texte)
        })

        output$table <- renderDataTable({
            if(input$fct == "demofreq"){
                return(demoFreq)
            } else if(input$fct == "demofreqc"){
                return(demoFreqC)
            } else if(input$fct == "hp"){
                hp <- read_lines("./txt/HP1.txt")
                texte <- hp
            } else if(input$fct == "spring"){
                spring <- read_lines("./txt/spring.txt")
                texte <- spring
            } else if(input$fct == "file"){
                tex <- input$file
                texte <- read_lines(tex)
            } else if(input$fct == "file2"){
                table <- suppressWarnings(gsheet2tbl(input$file2))
                texte <- data.frame(table)
            } else if(input$fct == "texte"){
                text <- input$texte
                texte <- read_lines(text)
            }

            main <- function(texte){
                TextDoc <- Corpus(VectorSource(texte))

                #Replacing "/", "@" and "|" with space
                toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
                removeSpace <- content_transformer(function (x , pattern ) gsub(pattern, "", x))
                TextDoc <- tm_map(TextDoc, toSpace, "/")
                TextDoc <- tm_map(TextDoc, toSpace, "@")
                TextDoc <- tm_map(TextDoc, toSpace, "\\|")


                # Convert the text to lower case
                TextDoc <- tm_map(TextDoc, content_transformer(tolower))
                # Remove numbers
                TextDoc <- tm_map(TextDoc, removeNumbers)
                # Remove english common stopwords
                TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
                TextDoc <- tm_map(TextDoc, removeWords, stopwords("french"))

                # Remove your own stop word
                # specify your custom stopwords as a character vector
                TextDoc <- tm_map(TextDoc, removeWords, c("the","and","-"))
                # Remove punctuations
                TextDoc <- tm_map(TextDoc, removePunctuation)
                # Eliminate extra white spaces
                TextDoc <- tm_map(TextDoc, stripWhitespace)
                # Eliminate spaces
                # TextDoc <- gsub("[[:blank:]]", "", TextDoc)


                # Build a term-document matrix
                TextDoc_dtm <- TermDocumentMatrix(TextDoc)
                dtm_m <- as.matrix(TextDoc_dtm)
                # Sort by descearing value of frequency
                dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
                dtm_d <- data.frame(word = names(dtm_v) ,freq=dtm_v)
                # Display the top 20 most frequent words

                dtm_d %>% filter(freq >= input$size)
            }
            main(texte)
        })
        output$barplot <- renderPlot({
            if(input$fct == "demofreq"){
                return(barplot(demoFreq$freq, las = 2, names.arg = demoFreq$word,
                               col =brewer.pal(8, "Dark2"), main = paste("Top",max,input$fct,sep = " "),
                               ylab = "Word frequencies"))

            } else if(input$fct == "hp"){
                hp <- readLines("./txt/HP1.txt")
                texte <- hp
            } else if(input$fct == "spring"){
                spring <- read_lines("./txt/spring.txt")
                texte <- spring
            } else if(input$fct == "file"){
                tex <- input$file
                texte <- read_lines(tex)
            } else if(input$fct == "file2"){
                table <- suppressWarnings(gsheet2tbl(input$file2))
                texte <- data.frame(table)
            } else if(input$fct == "texte"){
                text <- input$texte
                texte <- read_lines(text)
            }

            max <- input$size3
            TextDoc <- Corpus(VectorSource(texte))

            #Replacing "/", "@" and "|" with space
            toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
            removeSpace <- content_transformer(function (x , pattern ) gsub(pattern, "", x))
            TextDoc <- tm_map(TextDoc, toSpace, "/")
            TextDoc <- tm_map(TextDoc, toSpace, "@")
            TextDoc <- tm_map(TextDoc, toSpace, "\\|")
            # Convert the text to lower case
            TextDoc <- tm_map(TextDoc, content_transformer(tolower))
            # Remove numbers
            TextDoc <- tm_map(TextDoc, removeNumbers)
            # Remove english common stopwords
            TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))

            # Remove your own stop word
            # specify your custom stopwords as a character vector
            TextDoc <- tm_map(TextDoc, removeWords, c("the","and","-"))
            # Remove punctuations
            TextDoc <- tm_map(TextDoc, removePunctuation)
            # Eliminate extra white spaces
            TextDoc <- tm_map(TextDoc, stripWhitespace)
            # Eliminate spaces
            # TextDoc <- gsub("[[:blank:]]", "", TextDoc)


            # Build a term-document matrix
            TextDoc_dtm <- TermDocumentMatrix(TextDoc)
            dtm_m <- as.matrix(TextDoc_dtm)
            # Sort by descearing value of frequency
            dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
            dtm_d <- data.frame(word = names(dtm_v) ,freq=dtm_v)
            # Display the top 20 most frequent words
            head(dtm_d, 30)
            dtm_d <- dtm_d %>% filter(freq >= input$size)

            barplot(dtm_d[1:max,]$freq, las = 2, names.arg = dtm_d[1:max,]$word,
                    col =brewer.pal(8, "Dark2"), main = paste("Top",max,input$fct,sep = " "),
                    ylab = "Word frequencies")
        })
        # output$name <- renderDataTable({

        # })
        output$heat <- renderPlot({
            data <- as.matrix(eurodist)
            # Default Heatmap
            heatmap(data, scale="column")
        })


    }
    # Return a Shiny app object
    shinyApp(ui = ui, server = server)
}
