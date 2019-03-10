observeEvent(input$okexact_res,{
  
  res<-if(input$fixed_exact=="FE"){                                                                 ####fixed_exact in ui_meta_exact.R
    rma.glmm(xi=xi, ni=ni, method=input$fixed_exact, data=vals$datar, measure="PLO")
  }else if(input$fixed_exact=="RE"){                                                                ####fixed_exact in ui_meta_exact.R
    rma.glmm(xi=xi, ni=ni, method=input$rand_est2, data=vals$datar, measure="PLO")
  }
  
  # output$forest_norm<-renderPlot({
  #   conflevel<-as.numeric(as.character(input$conflevel2))
  # 
  #     forest(res, alim=c(0,1), refline=NA, digits=input$digits2, level=conflevel)
  # 
  # })
  
  output$msummary_exact<-renderPrint({
    print(res)
  })
  
  # output$temp<-renderDataTable({
  #   vals$datar
  # })
  
})


########################
##       effect2      ##
########################

dataModal5 <- function(failed = FALSE) {
  modalDialog(
    selectInput("type2", "Type of data", 
                choices=c("Proportion", "Mean", "Two proportions (2X2)"),
                selected="Proportion"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okexact_data", "OK")
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$effect_exact, {
  showModal(dataModal5())
})

observeEvent(input$okexact_data,{ 
  output$dataexact<-renderTable({
    if(!is.null(vals$datar)){
      vals$datar
    }
  })
  removeModal()
})








##########################
##     dynamic UI       ##
##########################
output$rand_estimation2<-renderUI({
  # if(input$type=="."){
  #   NULL
  # }else 
  
  if(input$fixed_exact=="FE"){
    selectInput("fixed_est2",
                "Estimation method",
                choices=c("Inverse-variance"),
                selected="Inverse-variance")
  }else if(input$fixed_exact=="RE"){
    selectInput("rand_est2",
                "Estimation method",
                choices=c(Maximum_likelihood="ML"),
                selected="ML")
  }
})