#Representation of Spatial Games and Corresponding Fitness Landscape

#Neighbors function checks neighbors of each element in matrix, as fitness is dependent on number of C and D (potentially) calls fitness functions
neighbors = function(pop, i, pop_matrix){

  # Need to use coordinates provided by which(pop1 == i) to sum neighbors
  location = which(pop_matrix == i,arr.ind = T) #Last argument returns matrix indices; if not T, 1D index returned
  
  # using the location provided by the index, computes the number of cooperators
  #Special cases for total cooperators are listed first: rows + corners
  if (i %in% pop_matrix[1,1] == T){
    #considering the top left corner
    cooperators = sum(pop[location[1]:(location[1]+1),location[2]:(location[2]+1)])-(pop[location[1],location[2]])   
  } else if (i %in% pop_matrix[nrow(pop_matrix),1] == T){
    # considering the bottom left corner 
    cooperators = sum(pop[(location[1]-1):(location[1]),location[2]:(location[2]+1)])-(pop[location[1],location[2]]) 
  } else if (i %in% pop_matrix[1,nrow(pop_matrix)] == T){
    # considering the top right corner
    cooperators = sum(pop[(location[1]):(location[1]+1),(location[2]-1):(location[2])])-(pop[location[1],location[2]])     
  }  else if (i %in% pop_matrix[nrow(pop_matrix),nrow(pop_matrix)] == T){
    # considering the bottom right corner
    cooperators = sum(pop[(location[1]-1):(location[1]),(location[2]-1):(location[2])])-(pop[location[1],location[2]]) 
  } else if (i %in% pop_matrix[1,2:(ncol(pop_matrix)-1)] == T){ 
    #considering top row. Important not to include corners
    cooperators = sum(pop[location[1]:(location[1]+1),(location[2]-1):(location[2]+1)])-(pop[location[1],location[2]])
  } else if (i %in% pop_matrix[2:(nrow(pop_matrix)-1),1] == T){
    #considering leftmost column
    cooperators = sum(pop[(location[1]-1):(location[1]+1),(location[2]):(location[2]+1)])-(pop[location[1],location[2]])
  } else if (i %in% pop_matrix[nrow(pop_matrix),2:(ncol(pop_matrix)-1)] == T){
    # considering bottom row
    cooperators = sum(pop[(location[1]-1):(location[1]),(location[2]-1):(location[2]+1)])-(pop[location[1],location[2]])
  } else if (i %in% pop_matrix[2:(nrow(pop_matrix)-1),ncol(pop_matrix)] == T){
    # considering rightmost row
    cooperators = sum(pop[(location[1]-1):(location[1]+1),(location[2]-1):(location[2])])-(pop[location[1],location[2]])
  } else {
    # And the center values: 
    cooperators = sum(pop[(location[1]-1):(location[1]+1),(location[2]-1):(location[2]+1)])-(pop[location[1],location[2]]) 
  }

  #Set up special cases for edges:
  edges = c(pop_matrix[1,2:(ncol(pop_matrix)-1)],pop_matrix[2:(nrow(pop_matrix)-1),1],
            pop_matrix[2:(nrow(pop_matrix)-1), ncol(pop_matrix)], pop_matrix[nrow(pop_matrix),2:(ncol(pop_matrix)-1)])
  
  #Special cases for defectors: edges and vertices:
  
  if (i==pop_matrix[1,1] | i == pop_matrix[1,nrow(pop_matrix)] | i == pop_matrix[nrow(pop_matrix),1] | i == pop_matrix[nrow(pop_matrix),nrow(pop_matrix)]){
    defectors = 3 - cooperators 
  } else if ( i %in% edges){
    defectors = 5 - cooperators
  } else {
    defectors = 8 - cooperators
  }
  return(data.frame(cooperators,defectors))
}

# Calculates fitness of center individual based upon neighbors
fitness = function(reward, strategy, pop_matrix, pop, i){
  
  # Find location of current index
  location = which(pop_matrix == i,arr.ind = T)
  
  # Compute fitness of that individual, depending on strategy 
  if (pop[location[1],location[2]] == 1){
    #calculate fitness of cooperator
    fit_value = reward[1,1]*strategy[1,i] + reward[1,2]*strategy[2,i] # reward a * number of cooperators, reward b * number of defectors  
  } else {
    fitness_defectors = reward[2,1]*strategy[1,i] + reward[2,2]*strategy[2,1] # reward a * number of cooperators, reward b * number of defectors 
  }
}

# Update the strategy of each individual in the population based on fitness of neighbors
update = function(fit_values, pop_index, i){
  location = which(pop_index == i,arr.ind = T)
  # Set individual to strategy of highest fitness
  if (i %in% pop_index[1,1] == T){
    #considering the top left corner
    max_fit = max(fit_values[location[1]:(location[1]+1),location[2]:(location[2]+1)]) # Find the max fitness in neighborhood
    updated_strategy = which(fit_values[location[1]:(location[1]+1),location[2]:(location[2]+1)] == max_fit,arr.ind = T) 
    # Find the location of the fitness out of the given neighbors
    # Above line returns location relative only to the subset of fit_values passed to which(), not whole fit_value population
    pop_subset = pop_index[(location[1]):(location[1]+1),location[2]:(location[2]+1)] 
    # Necessary to create a subset in order to find corresponding fitness
  } else if (i %in% pop_index[nrow(pop_index),1] == T){
    # considering the bottom left corner
    max_fit = max(fit_values[(location[1]-1):(location[1]),location[2]:(location[2]+1)])
    updated_strategy = which((fit_values[(location[1]-1):(location[1]),location[2]:(location[2]+1)]) == max_fit,arr.ind = T)
    pop_subset = pop_index[(location[1]-1):(location[1]),location[2]:(location[2]+1)]
  } else if (i %in% pop_index[1,nrow(pop_index)] == T){
    # considering the top right corner
    max_fit = max(fit_values[(location[1]):(location[1]+1),(location[2]-1):(location[2])])
    updated_strategy = which((fit_values[(location[1]):(location[1]+1),(location[2]-1):(location[2])]) == max_fit,arr.ind = T)
    pop_subset = pop_index[(location[1]):(location[1]+1),(location[2]-1):(location[2])]
  }  else if (i %in% pop_index[nrow(pop_index),nrow(pop_index)] == T){
    # considering the bottom right corner
    max_fit = max(fit_values[(location[1]-1):(location[1]),(location[2]-1):(location[2])])
    updated_strategy = which((fit_values[(location[1]-1):(location[1]),(location[2]-1):(location[2])]) == max_fit,arr.ind = T)
    pop_subset = pop_index[(location[1]-1):(location[1]),(location[2]-1):(location[2])]
  } else if (i %in% pop_index[1,2:(ncol(pop_index)-1)] == T){
    #considering top row. Important not to include corners
    max_fit = max(fit_values[location[1]:(location[1]+1),(location[2]-1):(location[2]+1)])
    updated_strategy = which((fit_values[location[1]:(location[1]+1),(location[2]-1):(location[2]+1)]) == max_fit,arr.ind = T)
    pop_subset = pop_index[location[1]:(location[1]+1),(location[2]-1):(location[2]+1)]
  } else if (i %in% pop_index[2:(nrow(pop_index)-1),1] == T){
    #considering leftmost column
    max_fit = max(fit_values[(location[1]-1):(location[1]+1),(location[2]):(location[2]+1)])
    updated_strategy = which((fit_values[(location[1]-1):(location[1]+1),(location[2]):(location[2]+1)]) == max_fit,arr.ind = T)
    pop_subset = pop_index[(location[1]-1):(location[1]+1),(location[2]):(location[2]+1)]
  } else if (i %in% pop_index[nrow(pop_index),2:(ncol(pop_index)-1)] == T){
    # considering bottom row
    max_fit = max(fit_values[(location[1]-1):(location[1]),(location[2]-1):(location[2]+1)])
    updated_strategy = which((fit_values[(location[1]-1):(location[1]),(location[2]-1):(location[2]+1)]) == max_fit,arr.ind = T)
    pop_subset = pop_index[(location[1]-1):(location[1]),(location[2]-1):(location[2]+1)]
  } else if (i %in% pop_index[2:(nrow(pop_index)-1),ncol(pop_index)] == T){
    # considering rightmost row
    max_fit = max(fit_values[(location[1]-1):(location[1]+1),(location[2]-1):(location[2])])
    updated_strategy = which((fit_values[(location[1]-1):(location[1]+1),(location[2]-1):(location[2])]) == max_fit,arr.ind = T)
    pop_subset = pop_index[(location[1]-1):(location[1]+1),(location[2]-1):(location[2])]
    
  } else {
    # And the center values:
    max_fit = max(fit_values[(location[1]-1):(location[1]+1),(location[2]-1):(location[2]+1)])
    updated_strategy = which((fit_values[(location[1]-1):(location[1]+1),(location[2]-1):(location[2]+1)]) == max_fit,arr.ind = T)
    pop_subset = pop_index[(location[1]-1):(location[1]+1),(location[2]-1):(location[2]+1)]
  }
  # Code below finds the location of highest fitness. 
  # Randomly choose fitness from best neighboring strategy if multiple individuals (including oneself) are the same
  if (nrow(updated_strategy) > 1){
    updated_strategy = updated_strategy[sample(nrow(updated_strategy),1),]  
    # including individuals current strategy
    individual_i = pop_subset[updated_strategy[1],updated_strategy[2]]
  } else {
    individual_i = pop_subset[updated_strategy[1],updated_strategy[2]]
  }
  location_i = which(pop_index == individual_i,arr.ind = T) # Finds the location at which the individual with highest fitness lives
  population.temporary[location[1],location[2]] <<- population[location_i[1],location_i[2]] # Updates the strategy of the individual under consideration
}

# Begin the Games!
play = function(){
  
  #Compute the number cooperators and defectors that surround each individual in the population
  neighbors.list = sapply(seq_along(population), FUN = neighbors, pop = population, pop_matrix = pop_matrix) # use the first argument as the index. Pass index to nieghbors function
  neighbors.list = as.numeric(neighbors.list) # Conerting to numeric because sapply() returning list type matrix.
  #Would be prudent to next time use vapply to specify type and length 
  neighbors.list =  matrix(neighbors.list,2,dim(population)[1]^2) 
  
  # Proceed to compute fitness of every individual
  population_fitness = sapply(seq_along(population), FUN = fitness, pop = population, pop_matrix = pop_matrix, reward = payoff_matrix, strategy = neighbors.list)
  fitness_matrix = matrix(population_fitness, dim(population)[1], dim(population)[1], byrow = T) # Convert to numeric. Necessary because of sapply() use

  # Update the strategy of each individual. Returns all strategies in vector. Unnecessary to return all strategies but inherent in vapply
  C_D = vapply(seq_along(population), FUN = update, FUN.VALUE = numeric(1), fit_values = fitness_matrix, pop_index = pop_matrix)
  # C_D returns numeric vector of all individuals of population. Honestly not sure why
  
  graph()
  
  population <<- population.temporary # Update whole population. 
  # Population updates simultaneously, as opposed to individual, continuous updates
  
  #image(population,axes=F,col = c("white","black"),xlab="",ylab="",add = F)#plots final population
  
  return(C_D)
}

initiate_conditions = function(){ # Establish the conditions for the initial population
  
  # Set all as global variables
  payoff_matrix <<- matrix(c(1,1.63,0,0),2,2)
  
  population <<- matrix(rep(1,10000),100,100)
  #population <<- matrix(rep(0,10000),100,100)
  # Set temporary population that will allow simultaneous update
  population.temporary <<-  matrix(population, nrow = nrow(population), ncol = ncol(population), byrow = F)
  # population won't point to population.temporary
  
  pop_matrix <<-  matrix(seq_along(population),nrow(population),ncol(population),byrow = T)  # Necessary for referencing later on
  
  # Single Defector
  population [49:51,49:51] <<- 0
  # Island of cooperators
  #population [49:51,49:51] <<-  1
  #Walkers
  
  # population[5:6,50:53] <<- 1
  # population[3:4,53] <<- 1
  # population[94:95,50:53] <<- 1
  # population[96:97,50] <<- 1
  
  # Set Graph and Call graph()
  
  par(mfrow = c(3,3))
  #image(population,axes=F,col = c("white","black"),xlab="",ylab="",add = F) # Black and white version
  
  # Lines below allow for control of color
  # image() chooses lowest value in matrix to be color of the first element indicated in col argument 
  ramp <<- colorRampPalette(c('white', 'black', 'yellow',"green"))
  color <<- ramp(4)
  image(population,axes=F,col = c(color[1],color[2]),xlab="",ylab="") # figure out color shit!
}

graph = function(){
  
  # Set matrix to be plotted 
  population.graph = matrix(population.temporary, nrow = nrow(population.temporary), ncol = ncol(population.temporary))
  # Compute the locations of evolved strategies
  
  # Where are the elements of population.temporary that are different than past population
  evolved_1 = which(!population == population.temporary & population == 1) # When population == 1, there was a change from cooperator to defector
  evolved_0 = which(!population == population.temporary & population == 0) # Defector to cooperator
  population.graph[evolved_1] = 2 # Can set elements of population as if it were vector
  population.graph[evolved_0] = 3
  image(population.graph,axes=F,col = c(color[1],color[2],color[3],color[4]),xlab="",ylab="")
}

graph_line = function(strategy){
  cooperators = vapply(strategy, FUN = sum, FUN.VALUE = numeric(1))
  defectors = 10000 - cooperators
  
  data <- data.frame(x = 1:games,cooperators,defectors)
  
  attach(data)
  require(ggplot2)
  
  ggplot(data, aes(x)) +                    
    geom_point(aes(y=cooperators), colour="red") +  
    geom_point(aes(y=defectors), colour="green") 
  
  # Regression: 
  # https://stackoverflow.com/questions/11949331/adding-a-3rd-order-polynomial-and-its-equation-to-a-ggplot-in-r
  #model_poly = lm(cooperators~poly(x,4,raw=T),data=data)#creates fourth degree polynomial, raw=T gives nonorthogonal polynomials???
  # 
  #print(coef(summary(model_poly)))# gives coefficients of model with some nice statistics, as opposed to coef ithout summary. 
  #print(coef(model_poly)) # Check the coefficients of the function
  # 
  # limit_length = range(x)
  # length_seq = seq(from=limit_length[1],to=limit_length[2])
  # predict = predict(model_poly,newdata=list(x=x), se.fit=T) #with list you can create headings accessible by $
  # # se.fit makes sure the stuff that we are going to need to call
  # 
  # plot(x,cooperators,xlim=limit_length,cex=.5, main = "Polynomial Regression")#cex sets scalin of plot dots
  # lines(length_seq,predict$fit,lwd=.5,col=2)#lty sets line type
  
}

initiate_conditions()
games = 20
strategy = replicate(games,list(play())) # Strategy returns list of the population demographics during each game


graph_line(strategy)

payoff_matrix
# Green: cooperators turned defectors
# Yellow: defectors that have cooperated
# How is script producing symmetry if some randomness is involved when fitnesses are the same???
# Because the only individuals that have the same fitness also have the same strategy

