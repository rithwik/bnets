#' Preprocess
#'
#' @param df Dataframe to be processed
#' @export
#' @importFrom infotheo discretize
preprocess <- function(df) {
  ids <- colnames(df)
  df[, sapply(df, is.character)] <- Map(as.factor, df[, sapply(df, is.character)])
  df[, sapply(df, is.numeric)] <- data.frame(
    lapply(df[, sapply(df, is.numeric)], infotheo::discretize)
  )
  df[, sapply(df, is.integer)] <- Map(as.numeric, df[, sapply(df, is.integer)])
  names(df) <- ids
  df
}

#' Draw the network
#'
#' Uses visNetwork to draw a network
#' @param df Preprocessed dataframe (i.e. discretized numerics and factors). Defaults to wbcd.
#' @param algo Name of algorithm to be used for structure learning
#' @importFrom dplyr %>%
#' @importFrom infotheo mutinformation
#' @import visNetwork
#' @export
draw_network <- function(df = wbcd, algo) {
  df <- preprocess(df)
  net <- do.call(algo, list(df))

  nodelist <- data.frame(id = nodes(net),
                         label = nodes(net))
  edges <- data.frame(arcs(net))
  edges[] <- Map(as.character, Filter(is.factor, edges))
  edges$label <- apply(edges, 1,
                       function(x) {
                         round(mutinformation(df[x[1]], df[x[2]]), 2)
                       })

  visNetwork(nodes = nodelist, edges = edges) %>%
    visEdges(
      arrows = list(to = list(enabled = TRUE)),
      color = list(color = "lightblue", highlight = "red")
    ) %>%
    visOptions(
      nodesIdSelection = TRUE,
      #manipulation = TRUE,
      highlightNearest = list(enabled = TRUE, degree = 2)
    )
}

#' Make App
#'
#' Make shiny page to create bayesian networks
#' @param df The data.frame passed in. Defaults to wbcd.
#' @export
#' @importFrom dplyr %>%
#' @import bnlearn visNetwork shiny
make_app <- function(df = wbcd) {

  df <- preprocess(df)

  shinyApp(
    ### UI SECTION ###
    ui = fluidPage(
      titlePanel('BNets playground'),
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
        draw_network(df, input$algo)
      })
    }
  )
}
