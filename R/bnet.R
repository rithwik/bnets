#' Make shiny page to build bayesian network structures
#' 
#' This function takes a data.frame, and spins ups a shiny one page app
#' where you can create bayesian networks using various algorithms
#' from the bnlearn package to learn the structures
#' @param df The data.frame passed in. No defaults.
#' @keywords bnet
#' @import dplyr bnlearn visNetwork infotheo shiny
#' @export
#' @examples
#' bnet()

bnet <- function(df) {
  
  df[, sapply(df, is.integer)] <- Map(as.numeric, df[, sapply(df, is.integer)])
  df[, sapply(df, is.character)] <- Map(as.factor, df[, sapply(df, is.character)])
  
  shinyApp(
    ### UI SECTION ###
    ui = fluidPage(
      titlePanel('bnlearn'),
      sidebarPanel(
        selectInput(
          'algo',
          label = 'Algorithm',
          choices = c('iamb', 'hc', 'gs', 'inter.iamb', 'fast.iamb'),
          selected = 'iamb'
        )
      ),
      mainPanel(visNetworkOutput('network'))
    ),
    
    ### SERVER SECTION ###
    server = function(input, output) {
      output$network = renderVisNetwork({
        net <- do.call(input$algo, list(df))
        nodelist <- data.frame(id = nodes(net),
                               label = nodes(net))
        edges <- data.frame(arcs(net))
        edges[] <- Map(as.character, Filter(is.factor, edges))
        edges$label <- apply(edges, 1,
                             function(x) {
                               round(mutinformation(df[x[1]], df[x[2]]), 2)
                             })
        visNetwork(nodelist, edges) %>%
          visEdges(
            arrows = list(to = list(enabled = TRUE)),
            color = list(color = "lightblue", highlight = "red")
          ) %>%
          visOptions(
            nodesIdSelection = TRUE,
            #manipulation = TRUE,
            highlightNearest = list(enabled = TRUE, degree = 2)
          )
      })
    }
  )
}
