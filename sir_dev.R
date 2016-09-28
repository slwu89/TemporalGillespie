#Gillespie SSA to run SIR model on static network (simulation of time-homogeneous CTMC on static network medium)
sir_homogeneous <- function(n_nodes, edge, root, beta, mu, t_end, info=TRUE, detail=FALSE){
  
  output <- vector(mode="list",length=t_end)
  
  #return vector of node states after each transition
  if(detail){
    delta_t <- 0.0
    detail_out <- vector(mode="list",length=5e3L)
    detail_i <- 1L
  }  

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
        
        #return detailed output
        if(detail){
          if(detail_i > length(detail_out)){ #expand list
            detail_out <- c(detail_out,vector(mode="list",length=100))
          }
          detail_out[[detail_i]]$x <- x
          detail_out[[detail_i]]$time <- t + (tau / Lambda)
          detail_i <- detail_i + 1L
        }
        
        M_SI <- length(m_SI)
        Beta <- beta*M_SI #cumulative infection rate
        Lambda <- Mu + Beta #cumulative transition rate
        
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
  
  #return output
  if(detail){
    return(list(output=Filter(Negate(is.null),output),
                detail_out=Filter(Negate(is.null),detail_out)))
  } else {
    return(Filter(Negate(is.null),output))
  }
}


#generate the network medium the simulation will be run on
library(igraph)
n_nodes <- 200
erdos <- erdos.renyi.game(n=n_nodes,p=0.025,directed=FALSE,loops=FALSE)
erdos_edge <- as_edgelist(erdos)
erdos_edge <- erdos_edge[order(erdos_edge[,1]),]

beta <- 1.015/5 # R0 / infectious duration
mu <- 1/5 # 1 / infectious duration
set.seed(1)
sir_out <- sir_homogeneous(n_nodes=n_nodes,edge=erdos_edge,root=5,beta=beta,mu=mu,
                           t_end=50,info=TRUE,detail=TRUE)

sir_out_dat <- matrix(NA,nrow=length(sir_out$output),ncol=3)
sir_out_dat[,1] <- sapply(sir_out$output,function(x){n_nodes - x$N_I - x$N_R}) #S
sir_out_dat[,2] <- sapply(sir_out$output,function(x){x$N_I}) #I
sir_out_dat[,3] <- sapply(sir_out$output,function(x){x$N_R}) #R

get_state <- function(x){
  tab <- table(x)
  if(!"s" %in% names(tab)){
    tab[["s"]] <- 0
  }
  if(!"i" %in% names(tab)){
    tab[["i"]] <- 0
  }
  if(!"r" %in% names(tab)){
    tab[["r"]] <- 0
  }
  return(tab[c("s","i","r")])
}
sir_out_detail <- t(sapply(sir_out$detail_out,function(x){
  get_state(x$x)
}))
sir_out_detail_time <- sapply(sir_out$detail_out,function(x){x$time})

par(mfrow=c(1,2))
matplot(sir_out_dat,type="l",ylab="",main="Gillespie SSA on Static Network")
matplot(sir_out_detail_time,sir_out_detail,type="l",xlab="",ylab="",main="Gillespie SSA on Static Network (Detailed Output)")
par(mfrow=c(1,1))