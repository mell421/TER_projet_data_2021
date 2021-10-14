library(shiny)

if(require(shiny)){
    suppressWarnings(source("./fctR/sources.R"))

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
                conditionalPanel(condition = "input.fct == 'filePicker'",
                                 textInput("filePicker", "file (txt/md):")
                ),
                conditionalPanel(condition = "input.fct == 'file2'",
                    textInput("file2", "your file address(sheets):","https://docs.google.com/spreadsheets/d/")
                ),

                numericInput(inputId ="size", 'freq min in the table', 1),

                textInput(inputId ="color","color(random-light,random-dark,other)", "random-light"),


                textInput(inputId ="bgc","backgroundcolor", "black"),
                conditionalPanel(condition = "input.fct == 'maps'",
                    selectInput("maps","maps:",
                            list("Nasa"="NASAGIBS.ViirsEarthAtNight2012",
                                 "Google map"="Esri.WorldImagery",
                                 "Gray"="Esri.WorldGrayCanvas",
                                 "Terrain"="Esri.WorldTerrain",
                                 "Topo Map"="Esri.WorldTopoMap"
                            )
                    ),
                    numericInput(inputId ="zoomM", 'niveau de zoom', 4),
                )


            ),
            mainPanel(
                tabsetPanel(
                    type = "tabs",
                    tabPanel("wordcloud",
                             wordcloud2Output('wordcloud2')
                    ),
                    tabPanel("table",
                             dataTableOutput('table')
                    )
                )


            )
        )


    )


    # Define the server code
    server <- function(input, output) {
        choix <- renderDataTable({

        })
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
                dtm_d <- wc(texte)
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
                dtm_d <- wc(texte)
                # Display the top 20 most frequent words

                dtm_d %>% filter(freq >= input$size)
            }
            main(texte)
        })
        output$analyseS <- renderDataTable({
            library(tidyverse)      # data manipulation & plotting
            library(stringr)        # text cleaning and regular expressions
            library(tidytext)
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
            main <- function(texte){

                dtm_d <- wc(texte)
                # Display the top 20 most frequent words
                head(dtm_d, 30)
                dtm_d <- dtm_d %>% filter(freq >= input$size)
                #generate word cloud

                wordcloud2(data = dtm_d, color = input$color, size=1,backgroundColor=input$bgc,shape=input$form)

                findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.25)
                # run nrc sentiment analysis to return data frame with each row classified as one of the following
                # emotions, rather than a score:
                # anger, anticipation, disgust, fear, joy, sadness, surprise, trust
                # It also counts the number of positive and negative emotions found in each row
                d<-get_nrc_sentiment(text)

                #transpose
                td<-data.frame(t(d))
                #The function rowSums computes column sums across rows for each level of a grouping variable.
                td_new <- data.frame(rowSums(td[2:253]))
                #Transformation and cleaning
                names(td_new)[1] <- "count"
                td_new <- cbind("sentiment" = rownames(td_new), td_new)
                rownames(td_new) <- NULL
                td_new2<-td_new[1:8,]
                #Plot One - count of words associated with each sentiment
                quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")


            }
            main(texte)



        })
        # output$name <- renderDataTable({

        # })
        output$maps <- renderLeaflet({

            m <- leaflet() %>%
                addTiles() %>%
                setView( lng = 2.34, lat = 48.85, zoom = input$zoomM ) %>%
                addProviderTiles(input$maps)
            m
        })

        output$heat <- renderPlot({
            data <- as.matrix(eurodist)
            # Default Heatmap
            heatmap(data, scale="column")
        })
        output$testDecTree <- renderPlot({
        # output$decisiontree <- renderPlot({
        #     #Loading libraries
        #     library(rpart,quietly = TRUE)
        #     library(caret,quietly = TRUE)
        #     library(rpart.plot,quietly = TRUE)
        #     library(rattle)
        #
        #     #Reading the data set as a dataframe
        #     mushrooms <- read.csv ("https://drive.google.com/file/d/15UjPu68RbjRIVG5SuQ6nSMjmjyX7sN1A/view?usp=sharing")
        #
        #     # number of rows with missing values
        #     nrow(mushrooms) - sum(complete.cases(mushrooms))
        #
        #     # deleting redundant variable `veil.type`
        #     mushrooms$veil.type <- NULL
        #
        #     number.perfect.splits <- apply(X=mushrooms[-1], MARGIN = 2, FUN = function(col){
        #         t <- table(mushrooms$class,col)
        #         sum(t == 0)
        #     })
        #
        #     # Descending order of perfect splits
        #     order <- order(number.perfect.splits,decreasing = TRUE)
        #     number.perfect.splits <- number.perfect.splits[order]
        #
        #     # Plot graph
        #     par(mar=c(10,2,2,2))
        #     barplot(number.perfect.splits,
        #             main="Number of perfect splits vs feature",
        #             xlab="",ylab="Feature",las=2,col="wheat")
        #
        # })

    #     #data splicing
    #     set.seed(12345)
    #     train <- sample(1:nrow(mushrooms),size = ceiling(0.80*nrow(mushrooms)),replace = FALSE)
    #     # training set
    #     mushrooms_train <- mushrooms[train,]
    #     # test set
    #     mushrooms_test <- mushrooms[-train,]
    #
    #     # penalty matrix
    #     penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)
    #
    #     # building the classification tree with rpart
    #     tree <- rpart(class~.,
    #                   data=mushrooms_train,
    #                   parms = list(loss = penalty.matrix),
    #                   method = "class")
    #
    #     # Visualize the decision tree with rpart.plot
    #     rpart.plot(tree, nn=TRUE)
    # }
        })
    }

    # Return a Shiny app object
    shinyApp(ui = ui, server = server)
}
