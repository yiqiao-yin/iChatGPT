library(shiny)
library(httr)
library(sass)
library(markdown)
library(waiter)
library(shinyjs)
library(shinyCopy2clipboard)

css <- sass(sass_file("www/chat.scss"))
jscode <- 'var container = document.getElementById("chat-container");
if (container) {
  var elements = container.getElementsByClassName("user-message");
  if (elements.length > 1) {
    var lastElement = elements[elements.length - 1];
    lastElement.scrollIntoView({
      behavior: "smooth"
    });
  }
}'


cosine_similarity <- function(str1, str2) {
  # Tokenize strings into words
  words1 <- strsplit(str1, " ")[[1]]
  words2 <- strsplit(str2, " ")[[1]]
  
  # Compute word frequencies for each string
  freq1 <- table(words1)
  freq2 <- table(words2)
  
  # Create a set of all unique words in both strings
  all_words <- union(names(freq1), names(freq2))
  
  # Compute dot product and norms for each frequency vector
  dot_product <- sum(data.frame(freq1[all_words] * freq2[all_words])[2], na.rm = TRUE)
  norm1 <- sqrt(sum(data.frame(freq1[all_words]^2)[2], na.rm = TRUE))
  norm2 <- sqrt(sum(data.frame(freq2[all_words]^2)[2], na.rm = TRUE))
  
  # Compute cosine similarity score (range [-1, 1])
  if (norm1 == 0 | norm2 == 0) {
    score <- 0
  } else {
    score <- dot_product / (norm1 * norm2)
  }
  
  return(score)
}


levenshtein_dist <- function(str1, str2) {
  n <- nchar(str1)
  m <- nchar(str2)
  d <- matrix(0, n + 1, m + 1)
  
  for (i in 1:(n + 1)) {
    d[i, 1] <- i - 1
  }
  
  for (j in 1:(m + 1)) {
    d[1, j] <- j - 1
  }
  
  for (j in 2:(m + 1)) {
    for (i in 2:(n + 1)) {
      if (substring(str1, i - 1, i - 1) == substring(str2, j - 1, j - 1)) {
        substitution_cost <- 0
      } else {
        substitution_cost <- 1
      }
      
      d[i, j] <- min(d[i - 1, j] + 1,          # deletion
                     d[i, j - 1] + 1,          # insertion
                     d[i - 1, j - 1] + substitution_cost)   # substitution
    }
  }
  
  return (1 - d[n + 1, m + 1] / max(n, m)) * 100
}

chatGPT_R <- function(apiKey, prompt, model = "gpt-3.5-turbo") {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", apiKey)),
    content_type("application/json"),
    encode = "json",
    body = list(model = model,
                messages = list(list(
                  role = "user", content = prompt
                )))
  )
  
  if (status_code(response) > 200) {
    result <- trimws(content(response)$error$message)
  } else {
    result <- trimws(content(response)$choices[[1]]$message$content)
  }
  
  return(result)
  
}

execute_at_next_input <-
  function(expr, session = getDefaultReactiveDomain()) {
    observeEvent(once = TRUE,
                 reactiveValuesToList(session$input),
                 {
                   force(expr)
                 },
                 ignoreInit = TRUE)
  }

# Define UI for application
ui <- fluidPage(
  useWaiter(),
  useShinyjs(),
  use_copy(),
  tags$head(tags$style(css)),
  shinythemes::themeSelector(),
  navbarPage("Main",
             tabPanel(
               "ChatGPT",
               sidebarPanel(
                 textInput("apiKey", "API Key", "sk-xxxxxxxxxxxxxxxxxxxx"),
                 tags$h5("Disclaimer: We don't save your API Key."),
                 selectInput(
                   "model",
                   "Model",
                   choices = c("gpt-3.5-turbo", "gpt-3.5-turbo-0301"),
                   selected = "gpt-3.5-turbo"
                 ),
                 selectInput(
                   "domain",
                   "Domain",
                   choices = c("General", "Labcorp Annual Report 2022"),
                   selected = "General"
                 ),
                 selectInput(
                   "similarity",
                   "Similarity",
                   choices = c("Cosine", "Levenshtein"),
                   selected = "Cosine"
                 ),
                 style = "background-color: #fff; color: #333; border: 1px solid #ccc;"
               ),
               
               mainPanel(
                 tags$div(
                   id = "chat-container",
                   tags$div(
                     id = "chat-header",
                     tags$img(src = "TnUa864.png", alt = "AI Profile Picture"),
                     tags$h3("AI Assistant")
                   ),
                   tags$div(id = "chat-history",
                            uiOutput("chatThread")),
                   tags$div(id = "chat-input",
                            tags$form(column(
                              12,
                              textAreaInput(
                                inputId = "prompt",
                                label = "",
                                placeholder = "Type your message here... 请在此输入信息...",
                                width = "100%"
                              )
                            ),
                            fluidRow(
                              tags$div(
                                style = "margin-left: 1.5em;",
                                actionButton(
                                  inputId = "submit",
                                  label = "Send",
                                  icon = icon("paper-plane")
                                ),
                                actionButton(
                                  inputId = "remove_chatThread",
                                  label = "Clear History",
                                  icon = icon("trash-can")
                                ),
                                CopyButton(
                                  "clipbtn",
                                  label = "Copy",
                                  icon = icon("clipboard"),
                                  text = ""
                                )
                              )
                            )))
                 )
               )
             ))
)

# Define server logic
server <- function(input, output, session) {
  historyALL <- reactiveValues(df = data.frame() , val = character(0))
  
  # On click of send button
  observeEvent(input$submit, {
    if (nchar(trimws(input$prompt)) > 0) {
      # Spinner
      w <- Waiter$new(id = "chat-history",
                      html = spin_3(),
                      color = transparent(.5))
      w$show()
      
      # Get context
      if (input$domain == "Labcorp Annual Report 2022") {
        some_df <- read.csv('lh_ar_2022.csv')
        user_question <- input$prompt
        if (input$similarity == "Cosine") {
          some_df['questions_l_score'] <- sapply(1:nrow(some_df), function(i){cosine_similarity(some_df[i, 3], user_question)})
          some_df['answers_l_score'] <- sapply(1:nrow(some_df), function(i){cosine_similarity(some_df[i, 4], user_question)})
        } else if (input$similarity == "Levenshtein") {
          some_df['questions_l_score'] <- sapply(1:nrow(some_df), function(i){levenshtein_dist(some_df[i, 3], user_question)})
          some_df['answers_l_score'] <- sapply(1:nrow(some_df), function(i){levenshtein_dist(some_df[i, 4], user_question)})
        } else {
          some_df['questions_l_score'] <- sapply(1:nrow(some_df), function(i){cosine_similarity(some_df[i, 3], user_question)})
          some_df['answers_l_score'] <- sapply(1:nrow(some_df), function(i){cosine_similarity(some_df[i, 4], user_question)})
        }
        top_que_df <- some_df[order(some_df$questions_l_score, decreasing=TRUE),]
        updated_prompt <- paste0(
          'Use the following as background context: ',
          paste0(paste(top_que_df$answers[1:5], " "), collapse=" "),
          ' and then answer the question: ',
          user_question
        )
      } else {
        updated_prompt <- input$prompt
      }
      
      # Response
      chatGPT <- chatGPT_R(input$apiKey, updated_prompt, input$model)
      historyALL$val <- chatGPT
      history <- data.frame(
        users = c("Human", "AI"),
        content = c(input$prompt, markdown::mark_html(text =
                                                        chatGPT)),
        stringsAsFactors = FALSE
      )
      historyALL$df <- rbind(historyALL$df, history)
      updateTextInput(session, "prompt", value = "")
      
      # Conversation Interface
      output$chatThread <- renderUI({
        conversations <- lapply(seq_len(nrow(historyALL$df)), function(x) {
          tags$div(class = ifelse(historyALL$df[x, "users"] == "Human",
                                  "user-message",
                                  "bot-message"),
                   HTML(paste0(
                     ifelse(
                       historyALL$df[x, "users"] == "Human",
                       "<div class='img-wrapper'><img src='girl.avif' class='img-wrapper2'></div>",
                       "<div class='img-wrapper'><img src='boy3.avif' class='img-wrapper2'></div>"
                     ),
                     historyALL$df[x, "content"]
                   )))
        })
        do.call(tagList, conversations)
      })
      
      w$hide()
      execute_at_next_input(runjs(jscode))
    }
  })
  
  observeEvent(input$remove_chatThread, {
    output$chatThread <- renderUI({
      return(NULL)
    })
    historyALL$df <- NULL
    updateTextInput(session, "prompt", value = "")
  })
  
  observe({
    req(input$clipbtn)
    CopyButtonUpdate(
      session,
      id = "clipbtn",
      label = "Copy",
      icon = icon("clipboard"),
      text = as.character(historyALL$val)
    )
  })
  
  
}


# Run the application
shinyApp(ui=ui, server=server)


