

# SERVER ####
server_contri <- function(input, output) {
  
  output$eat2_contri_pie <- renderPlotly({
    f_eat2_plot_contri_pie(df = df_eat2_ct,
                       subs_input = input$eat2_single_subs_contri)
  })
  
  output$eat2_contri_bar <- renderPlot({
    f_eat2_plot_contri_bar(df = df_eat2_ct_expanded,
                           subs_input = input$eat2_single_subs_contri,
                           hyp_input = input$eat2_contri_bar_hyp)
  })

}
