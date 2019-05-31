## Capstone: Coursera Data Science
## Final Project 
## Name - Nikhil

library(shiny)
library(markdown)

## SHINY UI
shinyUI(
        fluidPage(
                titlePanel("Data Science Capstone Project - Word Prediction using NLP"),br(),
                img(src = 'coursera_logo.png', height = "auto", width = "auto"), img(src = 'swiftkey_logo.jpg', height = "auto", width = "auto"),
                sidebarLayout(
                        sidebarPanel(
                                helpText(h2("Next Word Prediction based on N-Gram")),
                                hr(),
                                textInput("inputText", "Enter the text/word/sentence in the box",value = ""),
                                hr(),
                                helpText("Note : This application uses Natural Language Processing techniques for predicting next word.", 
                                         hr(),
                                         "Dataset used for this project is provided by SwiftKey and is from 3-different sources; News, Blogs and Twitter.",
                                         hr(),
                                         "Dataset were created by using text mining technique of cleaning, sampling, and tokenization. After cleaning of the data, N-Gram Tokenizer used for predection."),
                                         
                                
                                hr(),
                                hr()
                                ),
                mainPanel(
                        h2("Next predicted word is :"),
                        verbatimTextOutput("prediction"),
                        strong("Entered Text \ Word \ Sentence is :"),
                        strong(code(textOutput('sentence1'))),
                        br(),
                        strong("N-Gram used to show next word is:"),
                        strong(code(textOutput('sentence2'))),
                        hr(),
                        helpText(h2("Word predection rule:"),hr(),
                        "1. If the supplied text is greater than 2 words, it take the last three words of the text and search the trigram/unigram pairs.",
                        br(),
                        "2. If the supplied text is 2 words, take the two words and search the bigram/unigram pairs.",
                        br(),
                        "3. If the supplied text is 1 word, search for that word in the unigram/unigram pairs."),
                        hr()
                        )
                )
        )
)

