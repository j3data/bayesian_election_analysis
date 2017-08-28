library(data.table)
library(ggplot2)

##################
###    ETL    ####
##################

# Import Data
githubURL_2016 <- 'https://cdn.rawgit.com/j3data/bayesian_election_analysis/master/election_result_2016.csv'
election_data_2016<-data.table(read.csv(githubURL_2016))

# Explore Data
head(election_data_2016)


# Plot GOP proportion
hist(election_data_2016$gop_pct_2012, breaks=seq(0,1,by=.05),main="2012 GOP Proportion by State",xlab="Percent of Two-Party Vote")

qplot(election_data_2016$gop_pct_2012, geom="histogram", bins=20,xlim=c(0,1), 
      main = "Histogram of 2012 GOP Proportion by State", xlab="Percent of Two-Party Vote",
      fill=I("grey"), col=I("black") )

# Calculate change in GOP proportion between 2012 and 2016 for all 56 electoral groups
delta <- election_data_2016$gop_pct_2016 - election_data_2016$prior    
n     <- nrow(election_data_2016)

# Plot change in GOP proportion by 2012 proportion
plot(election_data_2016$gop_pct_2012,election_data_2016$delta, xlab="2012 GOP Proportion", ylab="2016 Election Proportion Delta")
abline(h = 0, col = "red")

qplot(gop_pct_2012, delta, data = election_data_2016, xlim=c(0,1), 
      main = 'Plot of Δ by Proportion Voting GOP in 2012', xlab="2012 GOP Proportion", 
      ylab="Δ in GOP Proportion, 2012 to 2016") +
  geom_hline(data=election_data_2016, aes(yintercept=0), 
           linetype="dashed", size=1, colour="red")

# Calculate average delta, where average is the average of previously known deltas. updated for each new known state.
delta_avg <- double(n)               
for (i in 1:n){
  delta_avg[i] <- mean(delta[1:i])   
}
plot(1:n,delta_avg)                  # plot of average delta over time

qplot(1:n, delta_avg, data = election_data_2016,
      main = 'Plot of Δ for each new State Call', xlab="Number of States Called", 
      ylab="Δ")


######
## Electoral Votes over time
######

# Calculate running total of electoral votes
electoral_votes = double(n)  # running total of electoral votes
for (i in 1:n){
  if (i==1){electoral_votes[i] = election_data_2016$votes[i] * election_data_2016$trump_win[i]}
  else {electoral_votes[i] = electoral_votes[(i-1)] + election_data_2016$votes[i] * election_data_2016$trump_win[i]}
}
electoral_votes

# Plot electoral votes over time
plot(1:n,electoral_votes)       # plot of electoral_votes over time
abline(h = 270, col = "red")

qplot(1:n, electoral_votes, data = election_data_2016,
      main = 'GOP Electoral Votes after each Reported State', 
      xlab="Number of States Called", ylab="GOP Electoral Votes") +
  geom_hline(data=election_data_2016, aes(yintercept=270), 
             linetype="dashed", size=1, colour="red")


# Calculate expected electoral votes from unkown states
# t=1 is the time at which one state has reported results

#################
# Predict votes #
#################

# Calculate updated posterior for each new known state. poster mean = prior mean plus average delta. 
# prior plus average for all 56 electoral groups at time t=1 to 56 returned results

######
## Demo Gibbs Sampler
## State = 1 (Vermont)
######

# Calculate updated posterior for each new known state. poster mean = prior mean plus average delta. 
# prior plus average for all 56 electoral groups at time t=1 to 56 returned results

nsteps=1000
set.seed(0)
state=1
p=0
d=0
pmat=matrix(double(nsteps*n),nsteps,n)                 # prior plus average for all 56 electoral groups at time j=1 to 56 returned results
dmat=matrix(double(nsteps*n),nsteps,n)                 # prior plus average for all 56 electoral groups at time j=1 to 56 returned results
for (step in 1:nsteps){                                # repeat simulation nsteps times
  for (t in 1:56){                                     # one run for each new updated 2016 results
      d=rnorm(1,delta_avg[t],.10)
      p=rnorm(1,election_data_2016$prior[state]+d,.10)
      pmat[step,t]=p
      dmat[step,t]=d
  }
}

dim(pmat)

# Confirm the markov chain is mixing nicely
plot(pmat[,1],type='l')
acf(pmat[,1])


######
## Gibbs Sampler function
######

# Calculate updated posterior for each new known state. poster mean = prior mean plus average delta. 
# prior plus average for all 56 electoral groups at time t=1 to 56 returned results

votesamp=function(state,t){
  nsteps=1000
  set.seed(0)
  p=0
  d=0
  pmat=matrix(double(nsteps*1),nsteps,1)                 # prior plus average for all 56 electoral groups at time j=1 to 56 returned results
  dmat=matrix(double(nsteps*1),nsteps,1)                 # prior plus average for all 56 electoral groups at time j=1 to 56 returned results
  for (step in 1:nsteps){                                # repeat simulation nsteps times                                    # one run for each new updated 2016 results
      d=rnorm(1,delta_avg[t],.10)
      p=rnorm(1,election_data_2016$prior[state]+d,.001)
      pmat[step]=p
      dmat[step]=d
  }
  return(pmat)
}

# 1000 simulations for state=1 (vermont) at time=1 (one state reporting)
VT=votesamp(1,1)

# Based on this simulation, probability of GOP win
sum(VT[,1]>0.5)/nrow(VT)
# [1] 0.056



######
## Expected Votes
######

pred_thresh = .50     # require at least this much of a chance of GOP winning the state

projected_votes = double(n)
for (t in 1:n){               # t's increase over time
  for (i in (t+1):n){             # i's corresponde to states/ electoral groups
    if (i<=n){
      pmat_state <- votesamp(i,t)
      if (sum(pmat_state[,1]>0.5)/nrow(pmat_state)>pred_thresh){          # % of sim GOP p_i's > 0.5
        projected_votes[t] = projected_votes[t] + election_data_2016$votes[i]   # won if 2.5 mat > 0.5
      }
    }
  }
}
projected_votes  # projected values decrease as they are replaced by actual results

# Add projected votes to votes from called states
votes_proj = projected_votes + electoral_votes

# Plot credible vote count
plot(1:n,votes_proj, xlab = "Number of States Reporting", ylab="Projected Votes", 
     main=paste(pred_thresh*100,"% Win Threshold",sep = ""))       # plot of electoral_votes over time
abline(h = 270, col = "red")


qplot(1:n, votes_proj,
      main = "Expected GOP Electoral Votes", 
      xlab="Number of States Reporting", ylab="GOP Electoral Votes") +
  geom_hline(data=election_data_2016, aes(yintercept=270), 
             linetype="dashed", size=1, colour="red")



######
## Credible Intervals
######

# Alternatively, create predictions based upon credible intervals. 

# credible interval confidence
pct_conf <- .333                  # Change % confidence here

# credible interval votes
projected_votes = double(n)
for (t in 1:n){               # t's increase over time
  for (i in (t+1):n){             # i's corresponde to states/ electoral groups
    if (i<=n){
      pmat_state <- votesamp(i,t)
      if (quantile(pmat_state,(1-pct_conf)/2)>.5){
        projected_votes[t] = projected_votes[t] + election_data_2016$votes[i]   # won if 2.5 mat > 0.5
      }
    }
  }
}
projected_votes  # projected values decrease as they are replaced by actual results

# Calculate electoral votes plus project values for remaining states
votes_cred = projected_votes + electoral_votes

# Plot credible vote count
plot(1:n,votes_cred, ylab=paste("votes_",pct_conf*100,"pct_cred",sep = ""))       # plot of electoral_votes over time
abline(h = 270, col = "red")
