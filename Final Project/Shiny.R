library(shiny)
library(tm)
library(wordcloud)
library(memoise)

review <<- list("1 star" = "1-star.txt", "2 star" = "2-star.txt", "3 star" = "3-star.txt", "4 star" = "4-star.txt", "5 star" = "5-star.txt")

getTM <- memoise(function(review) {
    if (!(review %in% review))
        stop("Unknown info")
    text <- readLines(review, encoding = "UTF-8")
    cp = Corpus(VectorSource(text))
    cp = tm_map(cp, content_transformer(tolower))
    cp = tm_map(cp, removePunctuation)
    cp = tm_map(cp, removeNumbers)
    cp <- tm_map(cp, removeWords, c("I", "he", "she", "we", "today", "come", "go", "get", "have", "order", "just", "said", "can", "came", "people", "one", "place", "will", "told", "got", "went", "know", "take", "took", "ordered", "minutes", "service", "food", "time", "the", "and", "was", "were", "for", "that", "with", "this", "you", "had", "when", "are", "all", "back", "from", "them", "about", "they", "there", "not", "but", "would", "our", "out", "there", "here", "her", "him"))
    cp <- tm_map(cp, removeWords, stopwords("english"))
    tdm = TermDocumentMatrix(cp, control = list(minWordLength = 1))
    a = as.matrix(tdm)
    sort(rowSums(a), decreasing = TRUE)
})

ui <- fluidPage(
    titlePanel("Word Cloud of Yelp Review"),
    sidebarLayout(
        sidebarPanel(
            selectInput("selection", "Choose a star level:", choices = review),
            actionButton("update", "Change"),
            hr(),
            sliderInput("freq", "Minimum Frequency:", min = 1,  max = 100, value = 50),
            sliderInput("max", "Maximum Number of Words:", min = 1,  max = 300,  value = 100)
        ),
        mainPanel(
            plotOutput("plot")
        )
    )
)

server <- function(input, output, session) {
    terms <- reactive({
        input$update
        isolate({
            withProgress({
                setProgress(message = "Processing corpus...")
                getTermMatrix(input$selection)
            })
        })
    })
    wordcloud_rep <- repeatable(wordcloud)
    output$plot <- renderPlot({
        t <- terms()
        wordcloud_rep(names(t), t, scale=c(3, 0.2), min.freq = input$freq, max.words=input$max, random.order = F, colors=brewer.pal(8, "Dark2"))
    })
}

shinyApp(ui = ui, server = server)
