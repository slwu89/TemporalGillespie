#generate the network medium the simulation will be run on
library(igraph)
erdos <- erdos.renyi.game(n=100,p=0.04,directed=FALSE,loops=FALSE)
erdos_edge <- as_edgelist(erdos)
erdos_edge <- erdos_edge[order(erdos_edge[,1]),]

#number of unique nodes from edgelist
num_unique <- function(edge){
  return(length(unique(as.vector(erdos_edge))))
}

#initalize the contactList of node tuples
init_contactList <- function(edge,root){
  
  contactList <- vector(mode="list",length=nrow(edge))
  
  for(i in 1:nrow(edge)){
    contactList[[i]]$i <- edge[i,1]
    contactList[[i]]$j <- edge[i,2]
    contactList[[i]]$i_state <- "s"
    contactList[[i]]$j_state <- "s"
    if(edge[i,1] == root){
      contactList[[i]]$i_state <- "i"
    }
    if(edge[i,2] == root){
      contactList[[i]]$j_state <- "i"
    }
  }
  
  return(contactList)
}

#take a contactList and return a list of SI tuples
generate_si_list <- function(contactList){
  
  si_list <- list()
  
  j <- 1 #iterator
  for(i in 1:length(contactList)){
    
    if(contactList[[i]]$i_state != "i" & contactList[[i]]$j_state != "i"){
      next()
    } else {
      si_list[[j]] <- contactList[[i]]
      j = j + 1
    }
    
  }
  
  return(si_list)
}


#main simulation function
sir_homogeneous <- function(edge, root , beta, mu, t_end){
  
  #number of nodes / states
  n_nodes = num_unique(edge)
  n_inf = 1L
  n_rec = 0L
  n_sus = n_nodes - n_inf
  
  #draw initial tau
  tau = rexp(1)
  
  #initialize contactList
  contactList <- init_contactList(edge,root)
  
  #initialize parameters for main loop
  time = 0.0 #time
  iter = 0L #iterator
  Mu = mu
  Beta = 0.0 #cumulative infection rate
  Lambda = 0.0 #cumulative transition rate
  r_transitionType = 0.0 #random variable for choosing which type of transition happens
  xi = 0.0 #fraction of time step left before transition
  m = 0L #transition process number
  contactList_t <- contactList #state of node tuples at iteration t
  contactList_t_i <- sapply(contactList_t,function(x){x$i})
  contactList_t_j <- sapply(contactList_t,function(x){x$j})
  
  for(t in 1:t_end){
    
    si_list <- generate_si_list(contactList_t)
    
    n_si = length(si_list)
    Beta = beta*n_si #cumulative infection rate
    Lambda = mu + Beta #cumulative transition rate
    
    #check if a transition takes place
    if(tau >= Lambda){ #no transition takes place
      
      tau = tau - Lambda
      
    } else { #at least one transition takes place
      
      xi = 1.0 #remaining fraction of time-step (overshoot)
      
      #sampling step
      while(tau < xi * Lambda){ #repeat if next tau is smaller than ~ Lambda-tau
        
        xi = xi - tau/Lambda #fraction of time-step left after transition
        r_transitionType = Lambda * runif(1) #random variable for weighted sampling of transitions
        if(r_transitionType < Beta){ #S to I
          
          m = sample(n_si,1) #transition m
          #update newly infected nodes
          new_inf = which(contactList_t_j == si_list[[m]]$j)
          for(i in 1:length(new_inf)){
            contactList_t[[new_inf[i]]]$j_state = "i"
          }
          
          n_inf = n_inf + 1
          Mu = Mu + mu
          #line 310 in C++
        }
        
      }
      
    }
    
  }
  
}



