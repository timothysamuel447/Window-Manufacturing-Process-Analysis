# ui.R

library(shiny)
library(ggplot2)
library(lpSolveAPI)
library(lpSolve)

read_data <- function(file_path) {
  read.csv(file_path)
}


ui <- fluidPage(
  
  titlePanel("Window Manufacturing Analytics"),
  
  tabsetPanel(
    tabPanel("Problem Framing", 
             h3("Business Problem"),
             p("In the context of our manufacturing process for windows, we are faced with the challenge of minimizing the breakage rate of windows during production.
               The occurrence of breakages not only leads to financial losses but also impacts the overall efficiency and customer satisfaction. 
               The key stakeholder and end-user of our decision support system (DSS) is the window manufacturing technician responsible for overseeing the production process. 
               To address this issue, we've identified several constraints, including limitations on manufacturing process settings, quality standards from suppliers, and specific customer window specifications. 
               We acknowledge that certain assumptions have been made to simplify the model, such as assuming consistent material quality from suppliers and stable environmental conditions during production"),
             
             h3("Analytics Problem"), 
             p("In the business scenario, we're tackling the challenge of predicting breakage rates in a manufacturing process. To do this, we're using a linear regression model that relates factors like Ambient temperature, 
               Cut speed, Glass thickness, Edge deletion rate,Silicon viscosity,and Window size to breakage rates. We're making some assumptions, such as a linear relationship and the representativeness of historical data for future conditions.
               To assess the predictive model's success, we'll use metrics like R-squared and Mean Squared Error (MSE). Higher R-squared and lower MSE values indicate better predictions.
               In the optimization model, we distinguish between controllable (adjustable) and non-controllable (external) variables. We apply constraints to ensure realistic and feasible solutions.
               The Shiny app allows users to optimize controllable variables using the lpSolve optimization model, providing optimal actions to minimize breakage rates in the manufacturing process."),
             
             h3("Data"),
             
             p("C:\\Users\\HP\\Documents\\Fall 1\\R PROGRAMMING\\Final project\\Window_Manufacturing_xl.xlsx"),
             tableOutput("data_dict")),

    
    tabPanel("Descriptive Analytics",
             plotOutput("plot1"),
             plotOutput("plot2"),
             plotOutput("plot3"),
             plotOutput("plot4"),
             plotOutput("plot5"),
             plotOutput("plot6")),
    
    tabPanel("Predictive Analytics",
             sidebarPanel(
               actionButton(inputId = "run", label = "Run")), 
             tableOutput(outputId="coef_table"),
             plotOutput("model_plot"),
             tableOutput(outputId = "model_parameters")
             
             ),
    
    tabPanel("Prescriptive Analytics",
             sidebarPanel(
               actionButton(inputId = "Optimize", label = "Optimize")),
             tableOutput(outputId="opt_Result"), 
             tableOutput(outputId="opt_Decisions"),
             sliderInput("Spacer", "Spacer Distance", min=2.69, max=5.81, value=4.258),
             sliderInput("Color", "Window color", min=7.55, max=98.05, value=66.92),
             sliderInput("Viscosity", "Silicon Viscosity", min=7.806, max=16.196, value=12.037)
)
    
    ))

# server.R

library(shiny)

data <- read_data("C:\\Users\\HP\\Documents\\Fall 1\\R PROGRAMMING\\Final project\\Window_Manufacturing_CSV.csv")

server <- function(input, output) {
  
  # Data dictionary
  output$data_dict <- renderTable({
    data_dict <- data.frame(
      Variable = names(data),
      Type = sapply(data, class),
      Description = c("Breakage rate for a window", "size of a window", "Thickness of glass", "Ambient temperature",  
                      "Cut speed", "Edge deletion rate","Spacer distance","color of window","type of window",
                      "Supplier of glass","Silicon viscosity","location of glass supplier")
    )
  })
  
  # Descriptive plots
  output$plot1 <- renderPlot({
    ggplot(data, aes(x=Window.Size, y=Breakage.Rate)) +
      geom_point()+geom_smooth(method = "lm", formula = y ~ x, color = "red")
  })
  
  output$plot2 <- renderPlot({
    ggplot(data, aes(x=Glass.thickness, y=Breakage.Rate)) +
      geom_point()+geom_smooth(method = "lm", formula = y ~ x, color = "red")
    
  })
  
  output$plot3 <- renderPlot({
    ggplot(data, aes(x=Ambient.Temp, y=Breakage.Rate)) +
      geom_point()+geom_smooth(method = "lm", formula = y ~ x, color = "red")
    
  })
  
  output$plot4 <- renderPlot({
    ggplot(data, aes(x=Cut.speed, y=Breakage.Rate)) +
      geom_point()+geom_smooth(method = "lm", formula = y ~ x, color = "red")
    
  })
  output$plot5 <- renderPlot({
    ggplot(data, aes(x=Silicon.Viscosity, y=Breakage.Rate)) +
      geom_point()+geom_smooth(method = "lm", formula = y ~ x, color = "red")
    
  })
  
  output$plot6 <- renderPlot({
    ggplot(data, aes(x=Edge.Deletion.rate, y=Breakage.Rate)) +
      geom_point()+geom_smooth(method = "lm", formula = y ~ x, color = "red")
    
  })
  
  # Predictive model
  
  model <- eventReactive(input$run, {
  lm_model <- lm(Breakage.Rate ~., data = data)
  str(lm_model)
  coefs <- summary(lm_model)$coefficients  
  pvals <- coefs[,4]
  data.frame(variables=rownames(coefs), coefficient=coefs[,1], pvalue=pvals)
  })
  
  
  output$coef_table <- renderTable({
  model()
  })


  output$model_plot<- renderPlot({ 
    set.seed(123) 
    train_ind <- sample(nrow(data), 0.8*nrow(data))
    train <- data[train_ind,]
    test <- data[-train_ind,]
    
    train_pred <- predict(lm_model, train)     
    test_pred <- predict(lm_model, test)     
    rsq_train <- cor(train$Breakage.Rate, train_pred, use = "complete.obs")^2     
    rsq_test <- cor(test$Breakage.Rate, test_pred, use = "complete.obs")^2     
    mse_train <- mean((train$Breakage.Rate - train_pred)^2, na.rm = TRUE)     
    mse_test <- mean((test$Breakage.Rate - test_pred)^2, na.rm = TRUE)          
    

    
  output$model_parameters <- renderTable({
      
      data.frame(
        rsq_train = rsq_train,
        rsq_test = rsq_test,
        mse_train = mse_train,
        mse_test = mse_test )  
      
    })   
   
    par(mfrow=c(1,2)) 
    
    plot(rsq_train, ylim=c(0,1), pch=19, col="blue", main="R-squared",ylab="Value")     
    points(rsq_test, pch=19, col="red")     
    legend("topleft", legend=c("Train", "Test"),col=c("blue", "red"), pch=19)          
    
    plot(mse_train, pch=19, col="blue", main="MSE",ylab="Value")     
    points(mse_test, pch=19, col="red")     
    legend("topleft", legend=c("Train", "Test"),col=c("blue", "red"), pch=19)

  
   })
   
  
  
  # Prescriptive optimization
  
    optmodel <- eventReactive(input$Optimize, {
   
    (lps.model <- make.lp(nrow=0, ncol=7))
    
    set.type(lps.model, columns=1:7, type="real")
    
    get.type(lps.model) # to see what types are defined for each D.V.
    
    name.lp(lps.model, name="Minimize breakage rate")
    
    lp.control(lps.model, sense="min")
    
    obj <- set.objfn(lps.model, obj=c(1,-0.1738737,-572.5616615,0.0941819,0.4060236,0.0803458,0.9963117))
    
    # define constraints
    add.constraint(lps.model,c(1,0,0,0,0,0,0), "=", 285.9108438)
    add.constraint(lps.model,c(0,1,0,0,0,0,0), ">=", 51.92)
    add.constraint(lps.model,c(0,1,0,0,0,0,0), "<=", 75.57)
    
    add.constraint(lps.model,c(0,0,1,0,0,0,0), "<=", 0.5231)
    add.constraint(lps.model,c(0,0,1,0,0,0,0), ">=", 0.4797)
    
    add.constraint(lps.model,c(0,0,0,1,0,0,0), "<=", 24.098)
    add.constraint(lps.model,c(0,0,0,1,0,0,0), ">=", 8.393)
    
    add.constraint(lps.model,c(0,0,0,0,1,0,0), "<=", 3.218)
    add.constraint(lps.model,c(0,0,0,0,1,0,0), ">=", 0.2964)
    
    add.constraint(lps.model,c(0,0,0,0,0,1,0), "<=", 16.196)
    add.constraint(lps.model,c(0,0,0,0,0,1,0), ">=", 7.806)
    
    add.constraint(lps.model,c(0,0,0,0,0,0,1), "<=", 17.75)
    add.constraint(lps.model,c(0,0,0,0,0,0,1), ">=", 13.75)
    
    lps.model
    
    
  })
                
  output$opt_Result <- renderTable({
    solve(optmodel())
    paste("Optimal Objective:",round(get.objective(optmodel()),2))
  }) 
 
  output$opt_Decisions <- renderTable({
    solve(optmodel())
    paste("Decisions:",data.frame(get.variables(optmodel())))
  }) 

}

shinyApp(ui, server)
