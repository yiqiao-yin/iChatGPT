# Building a ChatGPT Clone Using R Shiny App

## Credit

Credits @ [deepanshu88](https://github.com/deepanshu88/shinyChatGPT) 

I used [How to build ChatGPT Clone in Shiny App](https://www.listendata.com/2023/04/how-to-build-chatgpt-clone-in-shiny.html)

## App

Here's my app uploaded live: [Chatbot-IO](https://y-yin.shinyapps.io/chatbot-io/)

## Motivation

The objective of this repository is to provide a guide on creating a custom UI using R Shiny app for building a ChatGPT clone. This allows users to have a unique interface that suits their specific needs, such as incorporating their company logo. Furthermore, the chatbot feature can be integrated into their website without relying on ChatGPT website's monthly paid subscription. This method proves cost-effective for a large number of users, as APIs cost less than the monthly subscription.

## Chat History

To build a chatbot, handling chat history is crucial. We use reactive function to collect and display chat history effectively. For instance, the following line initializes a reactive object called historyALL with two variables - df and val:

```
historyALL <- reactiveValues(df = data.frame(), val = character(0))
```

Here, df is initiated as an empty data frame while val is an empty character vector. Now, the next few lines create a variable - chatGPT, which contains the response from OpenAI API. The chatGPT_R function requires three arguments - apiKey, prompt, and model, which are obtained from user input using `input$apiKey`, `input$prompt`, and `input$model`. 

```
chatGPT <- chatGPT_R(input$apiKey, input$prompt, input$model)
```

Next, we create a data frame called history containing two columns - users and content. Here, users column contains the names of the users involved in the conversation (Human or AI), while the content column contains the text of the conversation - input$prompt for the users' messages and `markdown::mark_html(text=chatGPT)` formats the chatbot's response in HTML. The history data frame is then appended to the df variable in the historyALL reactive object using the rbind() function.

```
history <- data.frame(users = c("Human", "AI"), content = c(input$prompt, markdown::mark_html(text=chatGPT)), stringsAsFactors = FALSE)
historyALL$df <- rbind(historyALL$df, history)
```

This process stores the conversation history in df, which can be displayed to the user.