library(shiny)

shinyServer(
  function(input, output){
    output$plot = renderPlot({
#------
      tempo = input$tempo					
      espaco.simulacao = input$espaco			
      dt =  tempo / (espaco.simulacao - 1)	
      escala.tempo = seq(0, tempo, dt)	
      
      mu = input$media				
      sigma = input$desvio			
      S0 = sample(0:500, 1)
      
      B = matrix(0:0, 10, espaco.simulacao)
      S = matrix(0:0, 10, espaco.simulacao)
      
      for (i in 1 : 10)
        S[i, 1] = S0					
      
      
      for (i in 1:10)
        for (j in 2 : espaco.simulacao){
          B[i, j] = B[i, j - 1] + sqrt(dt) * rnorm(1)
          S[i, j] = S[i, 1] * exp((mu - (sigma^2) / 2) * (i - 1)*dt + sigma * B[i, j])
        }
      
      espaco.grafico = range(S, B)		
      
      plot(escala.tempo, S[1, ], ylim = espaco.grafico, type = "l", col = "1", xlab = NA, ylab = NA)
      lines(escala.tempo, S[2, ], col = "2")
      lines(escala.tempo, S[3, ], col = "3")
      lines(escala.tempo, S[4, ], col = "4")
      lines(escala.tempo, S[5, ], col = "5")
      lines(escala.tempo, S[6, ], col = "6")
      lines(escala.tempo, S[7, ], col = "7")
      lines(escala.tempo, S[8, ], col = "8")
      lines(escala.tempo, S[9, ], col = "9")
      lines(escala.tempo, S[10, ], col = "10")
      
      title(main = "", xlab = "tempo", ylab = "espaco")
#------
    })
  }
)