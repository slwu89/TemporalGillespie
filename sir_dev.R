#generate the network medium the simulation will be run on
library(igraph)
n_nodes <- 100
erdos <- erdos.renyi.game(n=n_nodes,p=0.04,directed=FALSE,loops=FALSE)
erdos_edge <- as_edgelist(erdos)
erdos_edge <- erdos_edge[order(erdos_edge[,1]),]

beta <- 3/5 # R0 / infectious duration
mu <- 1/5 # 1 / infectious duration

set.seed(1)
tmp <- sir_homogeneous(n_nodes,erdos_edge,5,beta,mu,20)

sir_homogeneous <- function(n_nodes, edge, root, beta, mu, t_end, info=TRUE){
  
  output <- vector(mode="list",length=t_end) #OUTPUT LIST
  
  # n_nodes <- length(unique(as.vector(edge)))
  
  x <- rep("s",n_nodes) #set node states to S 
  x[root] <- "i" #set state of root node to I
  
  m_I <- root #list of infected nodes
  N_I <- 1 #number of infected nodes
  N_R <- 0 #number of recovered nodes
  Mu <- mu #cumulative recovery rate
  tau <- rexp(n=1,rate=1) #draw tau Exp(1)
  
  #run through the time-steps:
  for(t in 1:t_end){
    
    #print simulation diagnostics
    if(info){
      print(paste0("current t: ",t,", number infected: ",N_I,", number recovered: ",N_R,", number susceptible: ",n_nodes-N_I-N_R))
    }
    
    #update list of possible S to I transitions
    m_SI <- NULL #S nodes in contact with I nodes
    for(k in 1:nrow(edge)){
      i <- edge[k,1]
      j <- edge[k,2]
      if(x[i] == "s" & x[j] == "i"){
        m_SI <- c(m_SI,i)
      } else if(x[i] == "i" & x[j] == "s"){
        m_SI <- c(m_SI,j)
      } else {
        next()
      }
    } #end for
    
    M_SI <- length(m_SI)
    Beta <- beta*M_SI #cumulative infection rate
    Lambda <- Mu + Beta #cumulative transition rate
    
    #check if a transition takes place
    if(tau >= Lambda){ #no transition
      tau = tau - Lambda
    } else { #at least one transition
      xi = 1.0 #remaining fraction of time-step
      while(tau < xi*Lambda){
        xi <- xi - tau/Lambda #fraction of time-step left after transition
        z <- runif(n=1,min=0,max=Lambda-.Machine$double.eps)
        if(z < Beta){ #S to I transition
          if(length(m_SI)==1){ #drawn m at random from m_SI
            m <- m_SI
          } else {
            m <- sample(x=m_SI,size=1) 
          }
          x[m] <- "i"
          m_I <- c(m_I,m)
          N_I <- N_I + 1
          Mu <- Mu + mu
        } else { #I to R transition
          if(length(m_I)==1){ #draw m at random from m_I
            m <- m_I
          } else {
            m <- sample(x=m_I,size=1) 
          }
          x[m] <- "r"
          m_I <- m_I[-which(m_I==m)] #remove m from m_I
          N_I <- N_I - 1
          N_R <- N_R + 1
          Mu <- Mu - mu
        } #end if
        
        #update list of S to I transitions and rates
        m_SI <- NULL #S nodes in contact with I nodes
        for(k in 1:nrow(edge)){
          i <- edge[k,1]
          j <- edge[k,2]
          if(x[i] == "s" & x[j] == "i"){
            m_SI <- c(m_SI,i)
          } else if(x[i] == "i" & x[j] == "s"){
            m_SI <- c(m_SI,j)
          } else {
            next()
          }
        } #end for

        M_SI <- length(m_SI)
        Beta <- beta*M_SI #cumulative infection rate
        Lambda <- Mu + Beta #cumulate transition rate
        
        tau = rexp(n=1,rate=1) #draw new tau
        
        #DEBUG
        if(n_nodes-N_I-N_R < 0){
          browser()
        }
        
      } #end while
    } #end if
    
    #write out desired quantities
    output[[t]]$x <- x
    output[[t]]$m_SI <- m_SI
    output[[t]]$m_I <- m_I
    output[[t]]$N_I <- N_I
    output[[t]]$N_R <- N_R
    
    if(N_I==0){
      break()
    }
    
  } #end for
  
  return(Filter(Negate(is.null),output))
}




# #number of unique nodes from edgelist
# num_unique <- function(edge){
#   return(length(unique(as.vector(edge))))
# }
# 
# #initalize the contactList of node tuples
# init_contactList <- function(edge,root){
#   
#   contactList <- vector(mode="list",length=nrow(edge))
#   
#   for(i in 1:nrow(edge)){
#     contactList[[i]]$i <- edge[i,1]
#     contactList[[i]]$j <- edge[i,2]
#     contactList[[i]]$i_state <- "s"
#     contactList[[i]]$j_state <- "s"
#     if(edge[i,1] == root){
#       contactList[[i]]$i_state <- "i"
#     }
#     if(edge[i,2] == root){
#       contactList[[i]]$j_state <- "i"
#     }
#   }
#   
#   return(contactList)
# }
# 
# #take a contactList and return a list of SI tuples
# generate_si_list <- function(contactList){
#   
#   si_list <- list()
#   
#   j <- 1 #iterator
#   for(i in 1:length(contactList)){
#     
#     if(contactList[[i]]$i_state != "i" & contactList[[i]]$j_state != "i"){
#       next()
#     } else {
#       si_list[[j]] <- contactList[[i]]
#       j = j + 1
#     }
#     
#   }
#   
#   return(si_list)
# }