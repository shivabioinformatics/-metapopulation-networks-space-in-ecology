# Lab06 


# Part 1 

N0 = c(10, 20, 30, 40, 50)
growth = 1.10 
N1 = N0 * growth
N1

 
interaction_matrix = matrix(1:25, nrow=5)
interaction_matrix

row.names(interaction_matrix) = c("A", "B", "C", "D", "E")
interaction_matrix

colnames(interaction_matrix) = c("A", "B", "C", "D", "E")
interaction_matrix

identity_matrix = diag(1,5,5)
identity_matrix

new_matrix = identity_matrix * interaction_matrix
new_matrix

metapopulation_disersal = new_matrix * N1
metapopulation_disersal

#part 2 

metapop_Network <- matrix(
  c(0,1,0,1,0,
    1,0,1,0,1,
    0,1,0,1,0,
    1,0,1,0,1,
    0,1,0,1,0),
  nrow=5,ncol=5,byrow=TRUE)
metapop_Network

row.names(metapop_Network) = c("A", "B", "C", "D", "E")
colnames(metapop_Network) = c("A", "B", "C", "D", "E")
metapop_Network

metapop0 = c(100, 120, 110, 125, 150)
metapop_after_growth = metapop0 * 1.20
metapop_after_growth
metapop_after_growth_dispersal <- metapop_after_growth * metapop_Network
metapop_after_growth_dispersal

# part c 

N0 = abs(round(matrix(rnorm(10), nrow = 1), 2))
N0
metapopby10 = N0*100
metapopby10


adjacency_matrix10 <- matrix(c(rep(c(0,1), 5), rep(c(1,0), 5)), nrow=10, ncol=10, byrow=TRUE)
adjacency_matrix10



# Set initial values
steps = 100
current_step = 1

# Pre-allocate N1 matrix for all steps
num_subpopulations = length(metapopby10)
N1 = matrix(0, nrow = steps + 1, ncol = num_subpopulations)
N1[1, ] = num_subpopulations # Set initial population values

# Dispersal rate
dispersal_rate = 0.1 # 10%
r = 1.1

# Loop for 100 time steps
while (current_step <= steps) {
  # Growth within a time step
  N1[current_step + 1, ] <- N1[current_step, ] * r # Assuming r is defined
  # a) Emigration
  emigration = rowSums(N1[current_step, ] * dispersal_rate * adjacency_matrix10) # proportion of individuals dispersing
  # b) Immigration
  immigration = colSums(N1[current_step, ] * dispersal_rate * adjacency_matrix10)
  # c) Final step after dispersal
  N1[current_step + 1, ] <- N1[current_step + 1, ] - emigration + immigration
  # Move to the next time step
  current_step = current_step + 1
}

# View results
N1

install.packages("igraph")
library(igraph)

links = (N1)
links = as.matrix(links)
net <- graph.adjacency(N1, mode ="directed", weighted = NULL)
plot(net)

  
#plot
plot_metapop <- plot(N1[, 1], xlab = "Time", ylab = "Metapopulation Growth", main = "Metapopulation Growth Over time", type = "l")






