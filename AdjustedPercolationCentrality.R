library(sna)
library(network)
library(diffcor)
library(ggplot2)
library(dplyr)

Node = setRefClass("Node", fields = list(id = "numeric", nbs = "list", tinf = "numeric"), 
                    methods = list(addNeighbour = function(neigh) {
                      nbs <<- append(nbs, neigh)
                    }))

# Queue element class
Element = setRefClass("Element", fields = list(node = "numeric", depth = "numeric"))


# Bisection search to add element to sorted list
search <- function(queue, elem, start, end) {
  if (length(queue) == 0) {
    queue = list(elem)
    return(queue)
  }
  if (start >= end - 1) {
    if (elem$depth >= queue[[start]]$depth) {
      queue = append(queue, elem, after = start)
      return(queue)
    }
    queue = c(elem, queue)
    return(queue)
  }
  mid = floor((end - start)/2 + start)
  if (elem$depth == queue[[mid]]$depth) {
    queue = append(queue, elem, after = mid)
    return(queue)
  }
  else if (queue[[mid]]$depth < elem$depth) {
    return(search(queue, elem, mid, end))
  }
  else if (queue[[mid]]$depth > elem$depth) {
    return(search(queue, elem, start, mid))
  }
  return(queue)
}

AdjustedPercolationCentrality <- function(adjMat, Infd) {
  
  # Infd - List of infected nodes
  # adjMat - Adjacency Matrix
  # target - Node to calculate centrality for
  
  nodes <- vector(mode = "list", length = nrow(adjMat))
  
  for (i in 1:nrow(adjMat)) {
    nodes[[i]] <- Node(id = i, nbs = list(), tinf = 0)
  }
  
  for (i in 1:length(nodes)) {
    for (j in 1:length(nodes)) {
      if (adjMat[i, j] == 1 && i != j) {
        person = nodes[[i]]
        person$addNeighbour(j)
      }
    }
  }
  
  max_len = length(nodes)
  
  dist <- matrix(max_len, nrow = length(nodes), ncol = length(nodes))
  
  numPaths <- diag(length(nodes)) # Identity matrix
  
  ids <- 1:length(nodes)
  
  n_reachable = vector(mode = "numeric", length = length(nodes)) # Number of nodes reachable from each node
  
  for (id in ids) {
    queue = list()
    
    queue = append(queue, Element(node = id, depth = 0))
    
    while (!(length(queue) == 0)) {
      
      # Retrieve lowest depth element and remove from queue
      elem = queue[[1]]
      queue = queue[-1]
      node = nodes[[elem$node]]
      
      dist[id, node$id] = elem$depth
      
      # Skip infected nodes (except the starting nodes)
      if (node$id %in% Infd && elem$depth > 0) {
        next
      }
      
      for (ident in node$nbs) {
        nb = nodes[[ident]]
        
        if (dist[id, nb$id] == max_len) {
          # If neighbour is not visited, add neighbour to queue
          dist[id, nb$id] = elem$depth + 1
          queue = search(queue, Element(node = nb$id, depth = elem$depth + 1), 1, length(queue) + 1)
        }
        
        else if ((dist[id, nb$id] == elem$depth - 1 && !(nb$id %in% Infd)) || nb$id == id) {
          # Update number of shortest paths from target to node
          numPaths[id, node$id] = numPaths[id, node$id] + numPaths[id, nb$id]
        }
      }
      # Add proportion of shortest (unpercolated) paths between id and node which pass through target
      n_reachable[id] = n_reachable[id] + 1
    }
    
  }
  
  pbc <- vector(mode = "numeric", length = length(nodes)) # Percolation betweenness centrality - continuously updated throughout simulation
  
  n_unperc <- 0 # Stores number of pairs of nodes reachable by unpercolated paths
  
  for (i in Infd) {
    n_unperc = n_unperc + (n_reachable[i] - 1)
    for (j in ids[-Infd]) {
      for (v in ids[-c(Infd, j)]) {
        if (dist[i, j] == dist[i, v] + dist[v, j] && dist[i, j] != max_len) {
          pbc[v] = pbc[v] + (numPaths[i, v]*numPaths[v, j])/numPaths[i, j]
        }
      }
    }
  }
  
  pbc/n_unperc # Return final result
}


PercolationCentrality <- function(adjMat, Infd) {
  
  # Infd - List of infected nodes
  # adjMat - Adjacency Matrix
  # target - Node to calculate centrality for
  
  nodes <- vector(mode = "list", length = nrow(adjMat))
  
  for (i in 1:nrow(adjMat)) {
    nodes[[i]] <- Node(id = i, nbs = list(), tinf = 0)
  }
  
  for (i in 1:length(nodes)) {
    for (j in 1:length(nodes)) {
      if (adjMat[i, j] == 1 && i != j) {
        person = nodes[[i]]
        person$addNeighbour(j)
      }
    }
  }
  
  max_len = length(nodes)
  
  dist <- matrix(max_len, nrow = length(nodes), ncol = length(nodes)) # Shortest path distances
  
  numPaths <- diag(length(nodes)) # Identity matrix
  
  queue = list()
  
  ids <- 1:length(nodes)
  
  n_reachable = vector(mode = "numeric", length = length(nodes))
  
  for (id in ids) {
    queue = list()
    
    queue = append(queue, Element(node = id, depth = 0))
    
    while (!(length(queue) == 0)) {
      
      # Retrieve lowest depth element and remove from queue
      elem = queue[[1]]
      queue = queue[-1]
      node = nodes[[elem$node]]
      
      if (dist[id, node$id] != max_len) {
        next
      }
      
      dist[id, node$id] = elem$depth
      
      
      for (ident in node$nbs) {
        nb = nodes[[ident]]
        if (dist[id, nb$id] == max_len) {
          # If neighbour is not visited, add neighbour to queue
          queue = search(queue, Element(node = nb$id, depth = elem$depth + 1), 1, length(queue) + 1)
        }
        else if (dist[id, nb$id] == elem$depth - 1) {
          # Update number of shortest paths from target to node
          numPaths[id, node$id] = numPaths[id, node$id] + numPaths[id, nb$id]
        }
      }
      
      # Add proportion of shortest (unpercolated) paths between id and node which pass through target
      n_reachable[id] = n_reachable[id] + 1
      
    }
    
  }
  
  pbc <- vector(mode = "numeric", length = length(nodes)) # Percolation betweenness centrality - continuously updated throughout simulation
  
  n_unperc <- 0 # Stores number of pairs of nodes reachable by unpercolated paths
  
  for (i in Infd) {
    n_unperc = n_unperc + (n_reachable[i] - 2)
    for (j in ids[-Infd]) {
      for (v in ids[-c(Infd, j)]) {
        if (dist[i, j] == dist[i, v] + dist[v, j]) {
          # Iteratively calculate percolation centrality for each node
          pbc[v] = pbc[v] + (numPaths[i, v]*numPaths[v, j])/numPaths[i, j]
        }
      }
    }
  }
  
  pbc/n_unperc # Return final result
}


# Run simulation and calculate the reproductive number
R0 <- function(adjMat, Infd, end, rate) {
  
  nodes <- vector(mode = "list", length = nrow(adjMat))
  
  init_len <- length(Infd)
  
  # Construct adjacency list format from adjacency matrix
  for (i in 1:nrow(adjMat)) {
    nodes[[i]] <- Node(id = i, nbs = list(), tinf = end + 1)
  }
  
  for (i in 1:length(nodes)) {
    for (j in 1:length(nodes)) {
      if (adjMat[i, j] == 1) {
        person = nodes[[i]]
        person$addNeighbour(j)
      }
    }
  }
  
  queue = list()
  
  # Add initially infected nodes to queue
  for (i in Infd) {
    queue = search(queue, Element(node = i, depth = 0), 1, length(queue) + 1)
  }
  
  n_inf = 0
  
  # Simulate epidemic
  # At each step, remove the earliest infection and transmit to all neighbours
  while (!(length(queue) == 0)) {
    elem = queue[[1]]
    queue = queue[-1]
    node = nodes[[elem$node]]
    time = elem$depth
    
    if (node$tinf < end + 1) {
      next # Node already processed
    }
    
    if (time >= end) {
      break # Simulation is over
    }
    
    # Set infection time and add node to infected list
    node$tinf = time
    Infd = c(Infd, node$id)
    
    for (ident in node$nbs) {
      nb = nodes[[ident]]
      
      t <- rexp(1, rate)
      
      if (time + t <= end) {
        n_inf = n_inf + 1
      }
      
      if (nb$tinf < time + t) {
        next
      }
      
      queue = search(queue, Element(node = ident, depth = time + t), 1, length(queue) + 1)
    }
  }
  
  # Return number of new cases
  c(length(Infd) - init_len, n_inf)
  
  
}

set.seed(-54224523)

matrices <- list()

# Randomly generate graphs
for (prob in seq(0.4, 0.5, length.out = 3)) {
  graph <- rgraph(40, 1, mode = "graph", tprob = prob)
  while (!is.connected(graph)) {
    graph <- rgraph(40, 1, mode = "graph", tprob = prob)
  }
  matrices <- append(matrices, list(graph))
}

ids <- 1:40

infd_nodes = sample(ids, 4, replace = FALSE)

reproductiveDiff = matrix(0, nrow = length(matrices), ncol = 40)
percolationCentrality = matrix(0, nrow = length(matrices), ncol = 40)
adjustedPercolationCentrality = matrix(0, nrow = length(matrices), ncol = 40)
times_inf = matrix(0, nrow = 3, ncol = 40)

ind = 0

for (mat in matrices) {
  
  ind = ind + 1
    
  percolationCentrality[ind, ] = PercolationCentrality(mat, infd_nodes)
    
  adjustedPercolationCentrality[ind, ] = AdjustedPercolationCentrality(mat, infd_nodes)
  
  r = 0
  
  # Simulate average reproductive numbers for simulation including all nodes
  for (index in 1:100) {
    r = r + R0(mat, infd_nodes, 100, 1/50)[2]
  }
  
  reproductiveNum = r/100
  
  for (i in ids[-infd_nodes]) {
    
    minor = mat[-i, -i]
    
    r = 0
    
    new_Infd = ifelse(infd_nodes > i, infd_nodes - 1, infd_nodes)
    
    # Simulate average reproductive numbers when not including node i
    
    for (index in 1:100) {
      r = r + R0(minor, new_Infd, 100, 1/50)[2]
    }
    
    reproductiveDiff[ind, i] = reproductiveNum - r/100
  }
}

corrRaw = c(0, 0, 0)
corrAdj = c(0, 0, 0)
z = c(0, 0, 0)
p = c(0, 0, 0)


for (i in 1:3) {
  corrRaw[i] = cor(percolationCentrality[i, -infd_nodes], reproductiveDiff[i, -infd_nodes], method = "spearman")
  corrAdj[i] = cor(adjustedPercolationCentrality[i, -infd_nodes], reproductiveDiff[i, -infd_nodes], method = "spearman")
  corrBoth = cor(adjustedPercolationCentrality[i, -infd_nodes], percolationCentrality[i, -infd_nodes])
  z[i] = diffcor.dep(corrAdj[i], corrRaw[i], corrBoth, 36, alternative = "one.sided")$z # Calculate test statistic using fisher's transformation
  p[i] = diffcor.dep(corrAdj[i], corrRaw[i], corrBoth, 36, alternative = "one.sided")$p
}

print(corrAdj)

print(corrRaw)

print(z)

print(p)




# mat = c(1, 1, 0, 1, 1, 1, 0, 1, 1)
# 
# adj = matrix(mat, nrow = 3, ncol = 3, byrow = TRUE)
# 
# Infd = 1
# 
# print(PercolationCentrality(adj, Infd))

