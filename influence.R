library(debug)
library(stats)

make.graph <- function(...)
  structure(as.environment(list(...)),
            class='graph')

as.list.graph <- as.list.environment

## We're essentially dealing with cliques, aren't we; or strongly
## connected components?
nodes.from <- function(graph, whence, whither) {
  nodes <- c()
  ## In this case, thank God for the unmemoized nature of `visited'!
  ## Doesn't it resemble `amb'?
  search <- function(whence, visited) {
    if (whence == whither)
      nodes <<- union(visited, nodes)
    neighbors <- graph[[whence]]
    for (neighbor in setdiff(neighbors, visited))
      search(neighbor, c(visited, neighbor))
  }
  search(whence, whence)
  nodes
}

make.complete.undirected.graph <- function(nodes)
  do.call(make.graph,
          structure(Reduce(function(node, graph)
                           c(list(setdiff(nodes, node)), graph),
                           nodes,
                           c(),
                           right=TRUE),
                    names=nodes))

## Check both directions?
is.adjacent <- function(graph, whence, whither)
  whither %in% graph[[whence]] || whence %in% graph[[whither]]

## With checks for continued adjacency, since the graph may be
## mutating from under our feet.
for.each.adjacent.pair <- function(graph, f) {
  for (whence in ls(graph))
    for (whither in graph[[whence]])
      if (is.adjacent(graph, whence, whither))
        f(graph, whence, whither)
}

prune.conditional.independencies <- function(graph, data, cardinality=0) {
  there.exist.adjacent.nodes.of.sufficient.cardinality <- FALSE

  for.each.adjacent.pair(graph, function(graph, whence, whither) {
    debug(cardinality, whence, whither)
    ## This version calculates all paths from whence to whither:
    ## 
    ## nodes <- setdiff(intersect(union(graph[[whence]], graph[[whither]]),
    ##                            nodes.from(graph, whence, whither)),
    ##                  c(whence, whither))
    ##
    ## This is the Kalisch-version, dealing only with adjacency:
    ## 
    nodes <- setdiff(union(graph[[whence]], graph[[whither]]),
                     c(whence, whither))
    if (length(nodes) >= cardinality) {
      there.exist.adjacent.nodes.of.sufficient.cardinality <<- TRUE
      for (m in cardinality:length(nodes)) {
        if (m == 0) {
          if (cor.test(data[,whence], data[,whither])$p.value < 0.05) {
            graph[[whence]] <- setdiff(graph[[whence]], whither)
            graph[[whither]] <- setdiff(graph[[whither]], whence)
            break
          }          
        } else {
          ## Does failure of lm or cor.test to return imply an
          ## orthogonality such that we can break the dependence? That
          ## would be nice.
          ##
          ## lapply-based parallelization here could create a
          ## combination -> indepedence map, which we could then use
          ## to break links.
          ##
          ## Is it problematic, though, that we mutate the graph in
          ## real-time? Possibly.
          ##
          ## Let's throw a big machine at it and see what happens.
          for (subset in combn(nodes, m)) {
            resid.whence <-
              tryCatch(resid(lm(with(data,
                                     as.formula(sprintf("%s ~ %s",
                                                        whence,
                                                        paste(nodes, collapse='+')))))),
                       error=function(e) {
                         warning(sprintf('lm failed: %s; breaking.',
                                         conditionMessage(e)),
                                 immediate.=TRUE)
                         NA
                       });
            if (is.na(resid.whence))
              break
            
            resid.whither <-
              tryCatch(resid(lm(with(data,
                                     as.formula(sprintf("%s ~ %s",
                                                        whither,
                                                        paste(nodes, collapse='+')))))),
                       error=function(e) {
                         warning(sprintf('lm failed: %s; breaking.',
                                         conditionMessage(e)),
                                 immediate.=TRUE)
                         NA
                       });
            if (is.na(resid.whither))
              break
            
            ## Can't yet get these to work:
            ## 
            ## lm.fit(y=as.matrix(data)[,c(whence, whither)],
            ##        x=cbind(1, as.matrix(data[,nodes])))
            ## 
            ## lm.fit(y=cor(data[,c(whence, whither)]),
            ##        x=cor(data[,nodes]))
            length <- min(length(resid.whither), length(resid.whence))

            ## We might be able to fuck with `method' and `exact' here
            ## to avoid the NA p-value problem.
            p.value <- cor.test(sample(resid.whence, length),
                                sample(resid.whither, length))$p.value

            ## What are we saying here? I.e. should we delete the edge
            ## or not?
            if (is.na(p.value)) {
              warning('p.value is NA: breaking.', immediate.=TRUE)
              break
            }

            if (!is.na(p.value) && p.value < 0.05) {
              graph[[whence]] <<- setdiff(graph[[whence]], whither)
              graph[[whither]] <<- setdiff(graph[[whither]], whence)
              break
            }
          }
        }
      }
    }
  })

  if (there.exist.adjacent.nodes.of.sufficient.cardinality)
    prune.conditional.independencies(graph, data, cardinality + 1)
}

as.influence <- function(x, ...)
  UseMethod('as.influence')

as.influence.graph <- function(graph, data, evidence=NULL) {
  proposals <-
    Reduce(paste,
           Map(function(node)
               sprintf('(propose! \'%s)', node),
               ls(graph)))
  correlation <- cor(data, use='pairwise.complete.obs')
  explanations <- NULL
  contradictions <- NULL
  for.each.adjacent.pair(graph, function(graph, whence, whither) {
    ## Prune the reverse link, while we're at it.
    graph[[whither]] <<- setdiff(graph[[whither]],
                                 whence)
    if (!is.na(correlation[whence, whither])) {
      if (correlation[whence, whither] >= 0)
        explanations <<-
          paste(sprintf('(explain! \'(%s) \'%s)',
                        whence,
                        whither),
                explanations)
      else
        contradictions <<-
          paste(sprintf('(contradict! \'(%s) \'%s)',
                        whence,
                        whither),
                contradictions)
    }
  })
  evidence <-
    Reduce(paste,
           Map(function(evidence)
               sprintf('(evidence! \'(%s))', evidence),
               evidence))
  paste(proposals,
        explanations,
        contradictions,
        evidence)
}

pipe.to.influence <- function(graph, data, output, evidence=NULL) {
  influence <- pipe(sprintf('influence %s', output))
  cat(as.influence(graph, data, evidence),
      file=influence)
  close(influence)
}
