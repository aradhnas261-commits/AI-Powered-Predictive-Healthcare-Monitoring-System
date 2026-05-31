library(shiny)
library(ggplot2)
library(bslib)

ui <- fluidPage(
 theme = bs_theme(
    version = 5,
    bootswatch = "darkly"
  ),
  
  titlePanel("🏥 AI Powered Predictive Healthcare Monitoring and Recommondation System"),
  
  br(),
  
  fluidRow(
   
    column(
      width = 3,
      
      div(
        style="
        background-color:#1e1e1e;
        padding:20px;
        border-radius:15px;
        box-shadow:0px 0px 15px cyan;
        ",
        
        h3("📝 Patient Details"),
        
        numericInput(
          "bp",
          "Blood Pressure",
          120
        ),
        
        numericInput(
          "sugar",
          "Sugar Level",
          90
        ),
        
        numericInput(
          "pulse",
          "Heart Rate",
          72
        ),
        
        numericInput(
          "weight",
          "Weight (kg)",
          60
        ),
        
        numericInput(
          "height",
          "Height (m)",
          1.7
        ),
        
        sliderInput(
          "stress",
          "Stress Level",
          min = 1,
          max = 10,
          value = 3
        ),
        
        sliderInput(
          "sleep",
          "Sleep Hours",
          min = 1,
          max = 12,
          value = 7
        ),
        
        sliderInput(
          "activity",
          "Daily Activity",
          min = 1,
          max = 10,
          value = 5
        ),
        
        br(),
        
        actionButton(
          "analyze",
          "🔍 Analyze Health",
          class = "btn-primary btn-lg"
        )
      )
    ),
    
    
    
    column(
      width = 9,
      
      fluidRow(
         column(
          width = 6,
          
          div(
            style="
            background-color:#1e1e1e;
            padding:20px;
            border-radius:15px;
            box-shadow:0px 0px 10px #00ff99;
            ",
            
            h3("🩺 Health Status"),
            verbatimTextOutput("status")
          )
        ),
         column(
          width = 6,
          
          div(
            style="
            background-color:#1e1e1e;
            padding:20px;
            border-radius:15px;
            box-shadow:0px 0px 10px #ffcc00;
            ",
            
            h3("⚖ BMI Analysis"),
            verbatimTextOutput("bmi")
          )
        )
      ),
      
      br(),
      
      fluidRow(
         column(
          width = 6,
          
          div(
            style="
            background-color:#1e1e1e;
            padding:20px;
            border-radius:15px;
            box-shadow:0px 0px 10px #ff66ff;
            ",
            
            h3("⭐ Health Score"),
            verbatimTextOutput("score")
          )
        ),
         column(
          width = 6,
          
          div(
            style="
            background-color:#1e1e1e;
            padding:20px;
            border-radius:15px;
            box-shadow:0px 0px 15px red;
            ",
            
            h3("🚨 Early Warning Alert"),
            verbatimTextOutput("alert")
          )
        )
      ),
      
      br(),
      
      fluidRow(
        
        # PREDICTION
        column(
          width = 6,
          
          div(
            style="
            background-color:#1e1e1e;
            padding:20px;
            border-radius:15px;
            box-shadow:0px 0px 10px orange;
            ",
            
            h3("🤖 Future Health Prediction"),
            verbatimTextOutput("prediction")
          )
        ),
        
         column(
          width = 6,
          
          div(
            style="
            background-color:#1e1e1e;
            padding:20px;
            border-radius:15px;
            box-shadow:0px 0px 10px cyan;
            ",
            
            h3("🧠 Behaviour Analytics"),
            verbatimTextOutput("behaviour")
          )
        )
      ),
      
      br(),
      
      fluidRow(
        
        column(
          width = 12,
          
          div(
            style="
            background-color:#1e1e1e;
            padding:20px;
            border-radius:15px;
            box-shadow:0px 0px 10px #00bfff;
            ",
            
            h3("💡 Smart Recommendation Engine"),
            verbatimTextOutput("recommendation")
          )
        )
      ),
      
      br(),
      
      fluidRow(
        
         column(
          width = 12,
          
          div(
            style="
            background-color:#1e1e1e;
            padding:20px;
            border-radius:15px;
            box-shadow:0px 0px 10px lime;
            ",
            
            h3("📄 AI Health Summary"),
            verbatimTextOutput("summary")
          )
        )
      ),
      
      br(),
      
      fluidRow(
        
        column(
          width = 6,
          
          div(
            style="
            background-color:#1e1e1e;
            padding:15px;
            border-radius:15px;
            ",
            
            h3("📊 Health Parameters"),
            plotOutput("barPlot")
          )
        ),
        
         column(
          width = 6,
          
          div(
            style="
            background-color:#1e1e1e;
            padding:15px;
            border-radius:15px;
            ",
            
            h3("🥧 Health Distribution"),
            plotOutput("piePlot")
          )
        )
      ),
      
      br(),
      
      fluidRow(
        
        column(
          width = 12,
          
          div(
            style="
            background-color:#1e1e1e;
            padding:15px;
            border-radius:15px;
            ",
            
            h3("📈 Health Trend Analysis"),
            plotOutput("linePlot")
          )
        )
      )
    )
  )
)



server <- function(input, output) {
  
  observeEvent(input$analyze, {
    
     bmi_value <- reactive({
      input$weight / (input$height^2)
    })
    
     output$status <- renderText({
      
      if(input$bp > 140 || input$sugar > 180){
        
        "⚠ High Health Risk Detected"
        
      } else {
        
        "✅ Patient Health Condition is Stable"
      }
    })
    
     output$bmi <- renderText({
      
      bmi <- bmi_value()
      
      status <- ""
      
      if(bmi < 18.5){
        
        status <- "Underweight"
        
      } else if(bmi < 25){
        
        status <- "Normal"
        
      } else if(bmi < 30){
        
        status <- "Overweight"
        
      } else {
        
        status <- "Obese"
      }
      
      paste(
        "BMI =", round(bmi,2),
        "-", status
      )
    })
    
     output$score <- renderText({
      
      score <- 100
      
      if(input$bp > 140){
        score <- score - 20
      }
      
      if(input$sugar > 180){
        score <- score - 20
      }
      
      if(input$stress > 7){
        score <- score - 15
      }
      
      if(input$sleep < 5){
        score <- score - 15
      }
      
      if(bmi_value() > 25){
        score <- score - 15
      }
      
      paste(
        "Overall Health Score =",
        score,
        "/100"
      )
    })
    
   output$alert <- renderText({
      
      if(input$bp > 180 ||
         input$sugar > 300 ||
         input$pulse > 120){
        
        "🚨 EMERGENCY RISK DETECTED"
        
      } else if(input$bp > 160){
        
        "⚠ Sudden BP Spike Detected"
        
      } else if(input$sugar > 250){
        
        "⚠ Critical Sugar Level Alert"
        
      } else {
        
        "✅ No Emergency Detected"
      }
    })
    
     output$prediction <- renderText({
      
      if(input$stress > 7 &&
         input$sleep < 5){
        
        "⚠ Future Stress & Hypertension Risk is High"
        
      } else if(input$sugar > 150){
        
        "⚠ Future Diabetes Risk Detected"
        
      } else if(bmi_value() > 28){
        
        "⚠ Obesity Related Health Risk Detected"
        
      } else {
        
        "✅ Future Health Risk is Low"
      }
    })
    
    output$behaviour <- renderText({
      
      if(input$stress > 7){
        
        "⚠ High stress is affecting your health."
        
      } else if(input$sleep < 5){
        
        "⚠ Poor sleep pattern detected."
        
      } else if(input$activity < 4){
        
        "⚠ Low physical activity detected."
        
      } else {
        
        "✅ Healthy lifestyle behaviour detected."
      }
    })
    
   output$recommendation <- renderText({
      
      if(input$bp > 140){
        
        "Reduce salt intake and perform daily exercise."
        
      } else if(input$sugar > 180){
        
        "Avoid sugary foods and monitor glucose regularly."
        
      } else if(input$stress > 7){
        
        "Meditation and stress management recommended."
        
      } else if(input$sleep < 5){
        
        "Improve sleep schedule for better health."
        
      } else if(input$activity < 4){
        
        "Increase physical activity and walking."
        
      } else {
        
        "Maintain your healthy lifestyle."
      }
    })
    
   output$summary <- renderText({
      
      paste(
        "Patient health analysis completed.",
        "Stress Level =", input$stress,
        ", Sleep Hours =", input$sleep,
        ", Activity Level =", input$activity,
        ". The system generated health predictions, alerts, and recommendations successfully."
      )
    })
    health_data <- data.frame(
      
      Category = c(
        "BP",
        "Sugar",
        "Pulse",
        "Stress",
        "Activity"
      ),
      
      Value = c(
        input$bp,
        input$sugar,
        input$pulse,
        input$stress * 10,
        input$activity * 10
      )
    )
     output$barPlot <- renderPlot({
      
      ggplot(
        health_data,
        aes(
          x = Category,
          y = Value,
          fill = Category
        )
      ) +
        
        geom_bar(
          stat = "identity"
        ) +
        
        theme_dark() +
        
        ggtitle("Health Parameter Analysis")
    })
     output$piePlot <- renderPlot({
      
      pie(
        health_data$Value,
        labels = health_data$Category,
        main = "Health Distribution"
      )
    })
     output$linePlot <- renderPlot({
      
      ggplot(
        health_data,
        aes(
          x = Category,
          y = Value,
          group = 1
        )
      ) +
        
        geom_line(
          linewidth = 2
        ) +
        
        geom_point(
          size = 5
        ) +
        
        theme_dark() +
        
        ggtitle("Health Trend Analysis")
    })
  })
}

shinyApp(ui = ui, server = server)