library(shiny)
library(shinyMatrix)
library(expm)

# Simulation of the popualtion growth
matrix_model <- function(mat,init,generations) {
  pop <- matrix(init,nrow=1)
  for (gen in generations) {
    pop_t  <- (mat %^% gen) %*% init
    pop <- rbind(pop, t(pop_t))
  }
  return(structure(list(pop_size = pop, time = c(0,generations)),class="mod_results"))
}
# mmm <- matrix(c(0,0.8,1.3,0),nrow=2)
# init <- c(50,50)
# rrr <- matrix_model(mmm,init,1:100)
plot.mod_results <- function(mod, display_cohorts = FALSE) {
  sums <- rowSums(mod$pop_size)
  plot(sums~mod$time,type="l",ylim=c(0,max(sums)),xlab="generace",ylab="velikost populace",lwd=2)
  if (display_cohorts) {
    n_coh <- ncol(mod$pop_size)
    for (i in 1:n_coh) {
      lines(mod$time,mod$pop_size[,i],lty=i+1)
    }
    legend("topleft",legend = paste("kohorta",1:n_coh),lty=2:(n_coh+1))
  }
}
# plot(rrr, TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Věkově specifický model populačního růstu"),

    # Sidebar with a slider input for number of bins 
    fluidRow(plotOutput("pop_growth")),
    fluidRow(
      column(
        12,
        h5("Rychlost růstu populace (vlastní číslo):"),
        uiOutput("lambda"),
        h5("Dominantní vlastní vektor matice"),
        uiOutput("eigvek")
      )
    ),
    hr(),
    fluidRow(
      column(
        3,
         numericInput("n_cohorts","Počet věkových kohort", value=2,min=1),
         sliderInput(
           "generations",
            "Délka simulace (počet generací)",
             min = 1,
             max = 250,
             value = 100
          ),
        checkboxInput("display_cohorts","Zobrazit věkové kohorty v grafu")
       ),
      column(
        6,
        h5("Projekční matice"),
        matrixInput("pop_matrix", value = matrix(c(0,0.8,1.3,0),nrow=2), rows=list(names=F),cols=list(names=F),class="numeric"),
      ),
      column(3,
        h5("Počáteční velikost populace (pro jednotlivé kohorty)"),
        matrixInput("init_vector", value = matrix(rep(50,2),ncol=1), rows=list(names=F),cols=list(names=F),class="numeric")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observeEvent(input$n_cohorts, {
      mat <- input$pop_matrix 
      vec <- input$init_vector
      newdim <- input$n_cohorts
      if (dim(mat)[1] > newdim) {
        new <- matrix(mat[1:newdim,1:newdim],nrow=newdim,ncol=newdim)
        new_vec <- matrix(vec[1:newdim,1],nrow=newdim,ncol=1)
      } else if (dim(mat)[1]==newdim) {
        new <- mat
        new_vec <- vec
      } else {
        new <- matrix(0, ncol=newdim, nrow=newdim)
        new[1:dim(mat)[1], 1:dim(mat)[1]] <- mat
        new_vec <- matrix(0, ncol=1, nrow=newdim)
        new_vec[1:dim(mat)[1],1] <- vec
      }
      updateMatrixInput(session, "pop_matrix", new)
      updateMatrixInput(session, "init_vector", new_vec)
    })
    output$pop_growth <- renderPlot({
        # generate bins based on input$bins from ui.R
      plot(matrix_model(input$pop_matrix,input$init_vector,1:input$generations),input$display_cohorts)
    })
    output$lambda <- renderUI({
      lambd <- sort(eigen(input$pop_matrix)$values,decreasing=T)[1]
      paste("λ = ",lambd,sep="")
      })
    output$eigvek <- renderUI({
      vecs <- round(eigen(input$pop_matrix)$vectors[,order(eigen(input$pop_matrix)$values,decreasing = T)],3)
      paste("(",vecs[1,1],"; ",vecs[2,1],")",sep="")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
