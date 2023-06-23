

# SERVER ####
server_contri <- function(input, output) {
  
  output$eat2_contri <- renderPlotly({
    f_eat2_plot_contri(df = df_eat2_ct,
                       subs_input = input$eat2_single_subs_contri)
  })
  
}
