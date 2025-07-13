# load libraries ----

library(shiny)     # For building the Shiny app
library(ggplot2)   # For creating the line plot
library(scales)    # For formatting y-axis as currency

# define function ----

calc_payment_outcomes <- function(apr, principal, pay) {
  r <- apr ^ (1/12) / 100  # monthly interest rate from annual %
  remaining <- principal
  total_pay <- 0
  total_months <- 0
  cumulative_interest <- 0
  
  timeline <- data.frame(
    Month = integer(),
    Remaining = numeric(),
    CumulativeInterest = numeric()
  )
  
  # Check that payment is enough to cover at least monthly interest
  if (pay <= remaining * r) {
    stop("Monthly payment is too low to cover interest. Debt will grow forever.")
  }
  
  while (remaining > 0) {
    interest <- remaining * r
    principal_payment <- pay - interest
    
    remaining <- remaining - principal_payment
    cumulative_interest <- cumulative_interest + interest
    total_pay <- total_pay + pay
    total_months <- total_months + 1
    
    if (remaining < 0) {
      # Adjust final payment if overpaid
      total_pay <- total_pay + remaining
      remaining <- 0
    }
    
    timeline <- rbind(timeline, data.frame(
      Month = total_months,
      Remaining = max(remaining, 0),
      CumulativeInterest = cumulative_interest
    ))
  }
  
  return(list(
    total_months = total_months,
    total_paid = round(total_pay, 2),
    total_interest = round(cumulative_interest, 2),
    timeline = timeline
  ))
  
}

# define UI ----

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Helvetica Neue', sans-serif;
        background-color: #f8f9fa;
        margin-bottom: 50px;
      }
      .result-box {
        background-color: white;
        border-radius: 12px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.1);
        padding: 25px;
        margin-top: 20px;
        text-align: center;
      }
      .result-box h4 {
        margin: 10px 0 20px 0;
        color: #333;
      }
      .result-label {
        font-weight: bold;
        color: #555;
      }
      .result-value {
        font-size: 1.5em;
        color: #2a9d8f;
      }
      .container-fluid {
        max-width: 800px;
        margin: 0 auto;
      }
      @media (max-width: 600px) {
        .result-value {
          font-size: 1.2em;
        }
      }
    "))
  ),
  
  titlePanel("💰 Loan Payment Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("apr", "APR (%)", value = 2.3, step = 0.1),
      numericInput("principal", "Principal ($)", value = 16500, step = 100),
      numericInput("payment", "Monthly Payment ($)", value = 650, step = 10),
      actionButton("go", "Calculate", class = "btn btn-primary")
    ),
    
    mainPanel(
      uiOutput("styledResults"),
      plotOutput("timelinePlot")
    )
  )
)

# Server ----

server <- function(input, output) {
  results <- reactiveVal(NULL)
  
  observeEvent(input$go, {
    outcome <- calc_payment_outcomes(
      apr = input$apr,
      principal = input$principal,
      pay = input$payment
    )
    results(outcome)
  })
  
  output$styledResults <- renderUI({
    req(results())
    res <- results()
    div(class = "result-box",
        h4("Summary"),
        div(
          span(class = "result-label", "Total Months: "),
          span(class = "result-value", res$total_months)
        ),
        div(
          span(class = "result-label", "Total Paid: $"),
          span(class = "result-value", format(res$total_paid, big.mark = ","))
        ),
        div(
          span(class = "result-label", "Total Interest Paid: $"),
          span(class = "result-value", format(res$total_interest, big.mark = ","))
        )
    )
  })
  
  output$timelinePlot <- renderPlot({
    req(results())
    df <- results()$timeline
    
    ggplot(df, aes(x = Month)) +
      geom_line(aes(y = Remaining), color = "#2a9d8f", size = 1.2, linetype = "solid") +
      geom_line(aes(y = CumulativeInterest), color = "#b30000", size = 1.2, linetype = "dashed") +
      theme_minimal(base_family = "Helvetica") +
      labs(
        title = "Remaining Balance vs. Cumulative Interest Over Time",
        x = "Month",
        y = "Amount ($)"
      ) +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none"
      )
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
