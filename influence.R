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

prune.conditional.independencies <- function(graph, data, cardinality=0) {
  there.exist.adjacent.nodes.of.sufficient.cardinality <- FALSE

  for.each.adjacent.pair(graph, function(graph, whence, whither) {
    callCC(function(k) {
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
      ## debug(1)
      there.exist.adjacent.nodes.of.sufficient.cardinality <<- TRUE
      for (m in cardinality:length(nodes)) {
        ## debug(2)
        if (m == 0) {
          ## debug(2.5)
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
          ## debug(2.9)
          ## debug(nodes, m)
          ## Aha: this becomes prohibitively expensive; can we stream
          ## this?
          ## debug(combn(nodes, m, FUN=function(shit) NULL, simplify=FALSE))
          ## Need some kind of combinatorial for-each that discards
          ## its value.
          ##
          ## Let this be a lesson, though: don't gather where we're
          ## only shooting for side-effect.
          ##
          ## Combinadics:
          ## http://www.markmfredrickson.com/thoughts/2010-08-06-combinadics-in-r.html
          ## http://stats.stackexchange.com/questions/1286/how-can-i-obtain-some-of-all-possible-combinations-in-r
          ## http://compprog.wordpress.com/2007/10/17/generating-combinations-1/
          ## https://stat.ethz.ch/pipermail/r-help/2006-October/115012.html
          ## http://www.omegahat.org/Combinations/
          debug('YYYYYYYYYY', nodes, m)
          ## callCC(function(k)
                 combinations(length(nodes), m, function(x) {
          ## for (subset in combn(nodes, m)) {
            subset <- nodes[as.logical(x)]
            ## debug(subset)
            ## debug(subset)
            ## debug(3)
            resid.whence <-
              tryCatch(resid(lm(with(data,
                                     as.formula(sprintf("%s ~ %s",
                                                        whence,
                                                        paste(subset, collapse='+')))))),
                       error=function(e) {
                         ## warning(sprintf('lm failed on whence: %s; breaking.',
                         ##                 conditionMessage(e)),
                         ##         immediate.=TRUE)
                         NA
                       });
            if (is.na(resid.whence)) {
              ## debug('harro')
              ## graph[[whence]] <<- setdiff(graph[[whence]], whither)
              ## graph[[whither]] <<- setdiff(graph[[whither]], whence)
              k(NULL)
              return()
            }
            
            ## debug(4)
            resid.whither <-
              tryCatch(resid(lm(with(data,
                                     as.formula(sprintf("%s ~ %s",
                                                        whither,
                                                        paste(subset, collapse='+')))))),
                       error=function(e) {
                         ## warning(sprintf('lm failed on whither: %s; breaking.',
                         ##                 conditionMessage(e)),
                         ##         immediate.=TRUE)
                         NA
                       });
            if (is.na(resid.whither)) {
              ## debug('harro')
              ## graph[[whence]] <<- setdiff(graph[[whence]], whither)
              ## graph[[whither]] <<- setdiff(graph[[whither]], whence)
              k(NULL)
              return()
            }
            
            ## debug(5)
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
            ##
            ## We have another problem, though, of insufficient finite
            ## observations roughly 10 mins in.
            p.value <-
              tryCatch(cor.test(sample(resid.whence, length),
                                sample(resid.whither, length))$p.value,
                       error=function(e) {
                         warning(sprintf('cor.test failed: %s; breaking.',
                                         conditionMessage(e)),
                                 immediate.=TRUE)
                         NA
                       });

            ## What are we saying here? I.e. should we delete the edge
            ## or not?
            if (is.na(p.value)) {
              ## warning('p.value is NA: breaking.', immediate.=TRUE)
              ## graph[[whence]] <<- setdiff(graph[[whence]], whither)
              ## graph[[whither]] <<- setdiff(graph[[whither]], whence)
              k(NULL)
            } else if (p.value < 0.05) {
              debug('XXXXXXXXXXXXX', whence, whither)
              graph[[whence]] <<- setdiff(graph[[whence]], whither)
              graph[[whither]] <<- setdiff(graph[[whither]], whence)
              ## XXX: need to break out of combinations here!
              k(NULL)
            }
          })
          ## )
        }
      }
    }
  })})
    if (there.exist.adjacent.nodes.of.sufficient.cardinality)
      prune.conditional.independencies(graph, data, cardinality + 1)
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
      ## nodes <- setdiff(intersect(union(graph[[whence]], graph[[whither]]),
      ##                            nodes.from(graph, whence, whither)),
      ##                  c(whence, whither))
      nodes <- setdiff(union(graph[[whence]], graph[[whither]]),
                       c(whence, whither))
      ## Damn: we need to recalculate nodes after every change in the
      ## graph. We do that, don't we (cf. callCC)?
      ##
      ## The other problem is that, after deleting an edge,
      ## for.each.adjacent.pair should be adjusted; how to prevent it
      ## from starting from scratch, though?
      ##
      ## That's solved, at least, by the is.adjacent check in
      ## for.each.adjacent.pair (whew!).
      ##
      ## We might have to sample this fucking space and resort to
      ## combinadics, after all.
      ##
      ## Or, as a heuristic, can we specify some maximum
      ## subset-cardinality?
      ##
      ## Why not give the choice between
      ## sampling/max-subset-cardinality? They're both a form of
      ## sampling, aren't they?
      ##
      ## Memoization?
      ##
      ## k(NULL) on lm-failure is particularly pessimistic; we can
      ## probably afford to do some sampling here, since some
      ## lm-failures still result in pruning.
      ##
      ## Would we could fucking prune the lm-failures.
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
            if (!is.na(p.value) && p.value < alpha) {
              debug(p.value)
              graph[[whence]] <- setdiff(graph[[whence]], whither)
              graph[[whither]] <- setdiff(graph[[whither]], whence)
              k(NULL)
            }
          } else {
            debug(m, length(nodes))
            combinations(length(nodes), m, function(x) {
              subset <- nodes[as.logical(x)]
              ## debug(subset, vec.length=20)
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
                ## return()
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
                ## return()
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

              if (!is.na(p.value) && p.value < alpha) {
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
  influence <- pipe(sprintf('influence %s', output))
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
