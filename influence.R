library(functional)
library(Combinations)
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

has.adjacency <- function(graph, node)
  length(graph[[node]]) > 0 || node %in% Reduce(c, graph)

## With checks for continued adjacency, since the graph may be
## mutating from under our feet.
for.each.adjacent.pair <- function(graph, f) {
  for (whence in ls(graph))
    for (whither in graph[[whence]])
      if (is.adjacent(graph, whence, whither))
        f(graph, whence, whither)
}

prune.conditional.independencies <- function(graph,
                                             data,
                                             alpha=0.05,
                                             cardinality=0) {
  ## Replace this with a callCC?
  there.exist.adjacent.nodes.of.sufficient.cardinality <- FALSE

  for.each.adjacent.pair(graph, function(graph, whence, whither) {
    callCC(function(k) {
      debug(cardinality, whence, whither)
      ## Spirtes:
      ## 
      ## nodes <- setdiff(intersect(union(graph[[whence]], graph[[whither]]),
      ##                            nodes.from(graph, whence, whither)),
      ##                  c(whence, whither))
      ##
      ## Kalisch:
      ## 
      nodes <- setdiff(union(graph[[whence]], graph[[whither]]),
                       c(whence, whither))

      ## We might have to sample this fucking space and resort to
      ## combinadics, after all.
      ##
      ## k(NULL) on lm-failure is particularly pessimistic; we can
      ## probably afford to do some sampling here, since some
      ## lm-failures still result in pruning.
      if (length(nodes) >= cardinality) {
        there.exist.adjacent.nodes.of.sufficient.cardinality <<- TRUE
        for (m in cardinality:length(nodes)) {
          if (m == 0) {
            p.value <-
              tryCatch(cor.test(data[,whence], data[,whither])$p.value,
                       error=function(e) {
                         warning(sprintf('cor.test failed: %s',
                                         conditionMessage(e)),
                                 immediate.=TRUE)
                         NA
                       })
            ## This is questionable, isn't it: pruning on NA?
            if (is.na(p.value) || p.value < alpha) {
              debug(p.value)
              graph[[whence]] <- setdiff(graph[[whence]], whither)
              graph[[whither]] <- setdiff(graph[[whither]], whence)
              k(NULL)
            }
          } else {
            debug(m, length(nodes))
            combinations(length(nodes), m, function(x) {
              subset <- nodes[as.logical(x)]
              resid.whence <-
                tryCatch(resid(lm(with(data,
                                       as.formula(sprintf("%s ~ %s",
                                                          whence,
                                                          paste(subset, collapse='+')))))),
                         error=function(e) {
                           warning(sprintf('lm failed whence (%s): %s',
                                           whence,
                                           conditionMessage(e)),
                                   immediate.=TRUE)
                           NA
                         });
              if (is.na(resid.whence))
                k(NULL)

              resid.whither <-
                tryCatch(resid(lm(with(data,
                                       as.formula(sprintf("%s ~ %s",
                                                          whither,
                                                          paste(subset, collapse='+')))))),
                         error=function(e) {
                           warning(sprintf('lm failed whither (%s): %s',
                                           whither,
                                           conditionMessage(e)),
                                   immediate.=TRUE)
                           NA
                         });
              if (is.na(resid.whither))
                k(NULL)
              
              length <- min(length(resid.whither), length(resid.whence))

              p.value <-
                tryCatch(cor.test(sample(resid.whence, length),
                                  sample(resid.whither, length))$p.value,
                         error=function(e) {
                           warning(sprintf('cor.test failed: %s',
                                           conditionMessage(e)),
                                   immediate.=TRUE)
                           NA
                         });

              ## Vide supra.
              if (is.na(p.value) || p.value < alpha) {
                debug(p.value, subset)
                graph[[whence]] <<- setdiff(graph[[whence]], whither)
                graph[[whither]] <<- setdiff(graph[[whither]], whence)
                k(NULL)
              }
            })
          }
        }
      }
    })})
  
  if (there.exist.adjacent.nodes.of.sufficient.cardinality)
    prune.conditional.independencies(graph, data, alpha, cardinality + 1)
}
                         
as.influence <- function(x, ...)
  UseMethod('as.influence')

as.influence.graph <- function(graph, data, evidence=NULL) {
  nodes <- union(Filter(Curry(has.adjacency, graph=graph),
                        ls(graph)),
                 evidence)
  proposals <-
    Reduce(paste,
           Map(function(node)
               sprintf('(propose! \'%s)', node),
               nodes))
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
  influence <-
    pipe(sprintf('influence %s | dot -Tpng -o %s.png', output, output))
  cat(as.influence(graph, data, evidence),
      file=influence)
  close(influence)
}

run.influence.experiment <-
  function(experiment, data, evidence, alpha=0.05) {
    graph <- make.complete.undirected.graph(names(data))
    prune.conditional.independencies(graph, data, alpha)
    pipe.to.influence(graph, data, experiment, evidence)
  }
