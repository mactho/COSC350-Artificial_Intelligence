# To run this program type "source("THOMAS_MACNEIL.R") in R and ensure
# the file is in R's current working directory

Colours <- c("Yellow","Blue","Red","Green","White")
Nationality <- c("Norweigan","Danish","British","German","Swedish")
Smokes <- c("Dunhill","Blends","Pallmall","Prince","Bluemasters")
Drinks <- c("Tea","Beer","Water","Coffee","Milk")
Pet <- c("Horse","Cats","Dogs","Birds","Fish")

substituteStrings <- function(mat){
	answer=matrix(mat,5,5,dimnames = list(c("Colour","Nationality","Smokes","Drinks","Pets"),c("House1","House2","House3","House4","House5")))
	for( i in 1:nrow(mat)){
		for( j in 1:ncol(mat)){
			if(i == 1){
				answer[i,j]=Colours[mat[i,j]]
			}
			if(i == 2){
				answer[i,j]=Nationality[mat[i,j]]
			}
			if(i==3){
				answer[i,j]=Smokes[mat[i,j]]
			}
			if(i==4){
				answer[i,j]=Drinks[mat[i,j]]
			}
			if(i==5){
				answer[i,j]=Pet[mat[i,j]]
			}
		}
	}
	return(answer)
}

fitnessTest <- function(solutionMatrix){
	
	# The Brit lives in the red house
	c1 = abs(which(solutionMatrix[2,]==3)-which(solutionMatrix[1,]==3))
	# The Swede keeps dogs as pets
	c2 = abs(which(solutionMatrix[2,]==5)-which(solutionMatrix[5,]==3))
	# The Dane drinks tea
	c3 = abs(which(solutionMatrix[2,]==2)-which(solutionMatrix[4,]==1))
	# The green house is on the immediate left of the white house
	c4 = abs(which(solutionMatrix[1,]==4)-(which(solutionMatrix[1,]==5)-1))
	# The green houses owner drinks coffee
	c5 = abs(which(solutionMatrix[1,]==4)-which(solutionMatrix[4,]==4))
	# The owner who smokes pallmalls rears birds
	c6 = abs(which(solutionMatrix[3,]==3)-which(solutionMatrix[5,]==4))
	# The owner of the yellow house smokes Dunhill
	c7 = abs(which(solutionMatrix[1,]==1)-which(solutionMatrix[3,]==1))
	# The owner of the centre house drinks milk
	c8 = abs((which(solutionMatrix[4,]==5))-3)
	# The Norwegian lives in the first house
	c9 = abs((which(solutionMatrix[2,]==1))-1)
	# The owner who smokes Blends lives next to the one that keeps cats
	c10 = abs(1 - abs(which(solutionMatrix[3,]==2)-which(solutionMatrix[5,]==2)))
	# The owner who keeps the horse lives next to the one who smokes dunhill
	c11 = abs(1 - abs(which(solutionMatrix[5,]==1)-which(solutionMatrix[3,]==1)))
	# The owner who smokes bluemasters drinks beer
	c12 = abs(which(solutionMatrix[3,]==5)-which(solutionMatrix[4,]==2))
	# The German smokes Prince
	c13 = abs(which(solutionMatrix[2,]==4)-which(solutionMatrix[3,]==4))
	# The Norwegian lives next to the blue house
	c14 = abs(1 - abs(which(solutionMatrix[2,]==1)-which(solutionMatrix[1,]==2)))
	# The owner who smokes Blends lives next to the one who drinks water
	c15 = abs(1 - abs(which(solutionMatrix[3,]==2)-which(solutionMatrix[4,]==3)))
	
	solution=cbind(c1,c2,c3,c4*10,c5*10,c6,c7,c8*10,c9*10,c10*10,c11*10,c12,c13,c14*10,c15*10)
	
	return(sum(solution))
}

permutations <- function(n){
    if(n==1){
        return(matrix(1))
    } else {
        sp <- permutations(n-1)
        p <- nrow(sp)
        A <- matrix(nrow=n*p,ncol=n)
        for(i in 1:n){
            A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
        }
        return(A)
    }
}

getSolution <- function(n){
	sol=c()
	for(i in 1:length(n)){
		sol=c(sol,perms[n[i],])
	}
	return(matrix(sol,5,5,byrow=TRUE))
}

cat("\n\nAuthored by Thomas Macneil\n220120443\nProgramming assignment\nCOSC350 Artificial Intelligence\nTrimester 2 2017")
cat("\n\nWe use differential evolution to solve the problem known as Einstein's riddle.  Differential evolution uitilizes the formula 'c-F*(b-a)' to mutate a challenger solution where c,b and a are solutions selected from the population or parent solutions.  If the new challenger solution is better than the current best, then the challenger becomes the new best solution\n\n") 
cat("Setting up functions and initializing variables...\n")

popsize=10
parameters=5
generations=800000
checkForLocal=30000
CR=0.4
F=0.8
iterations = 0
begin=Sys.time()
fit = numeric(popsize)
previousFit = fit;

cat("Creating permutations of 1 through 5\n")
perms = permutations(5)

cat("We are using an initial population size of",popsize,", a probability of crossover rate of",CR," and a mutation rate of",F,"and will run for",generations,"generations\n\n")

pop = matrix(round(runif(popsize * parameters,1,120)),popsize,parameters)
for(i in 1:nrow(pop)){
	fit[i]=fitnessTest(getSolution(pop[i,]))
}
previousFit = fit;
cat("The starting fitness of population is:",fit,"\n")

# plot fitness evolution
plot(0,min(fit),pch=20,col="red",xlim=c(0,generations),ylim=c(max(fit),0),xlab="generation",ylab="fitness")
abline(h=0,col="red")

cat("Running Differential Evolution process...\n\n")

for (i in 1:generations)
#while(min(fit)[1]!=0)
{
	if(iterations %% 4 == 0){
		F=2.0
	} else {
		F=0.8
	}
	
	# Uncomment if you want to refresh the population when it becomes stagnant after some iterations
	if(iterations %% checkForLocal == 0 && iterations != 0){
		cat("\nProgress update:\nAt iteration",iterations,", the best fitness so far is:",min(fit),"\n")
		cat("Fitness of population is: ",fit,"\n")
		cat("The last time we checked it was:",previousFit,"\n")
		if(identical(fit, previousFit) && min(fit) != 0){
			cat("We seem to have hit a local minimum so we are shaking things up in the population...\n")
			for(j in 1:nrow(pop)){
				if(min(fit)!=j){
					pop[j,] = round(runif(5,1,120))
				}
			}
			#pop = matrix(round(runif(popsize * parameters,1,120)),popsize,parameters)
			for(j in 1:nrow(pop)){
				fit[j]=fitnessTest(getSolution(pop[j,]))
			}
			cat("Fitness of population is: ",fit,"\n")
		} else {
			previousFit = fit
		}
	}
		
#	if(iterations %% 40000 == 0){
#		cat("Progress update:\nAt iteration",iterations,", the best fitness so far is: ",min(fit),"\n")
#		cat("Fitness of population: ",fit,"\n")
#		cat("previousFitness was:",previousFit,"\n\n")
#	}
	
	index=sample(popsize,4)
	crtf=CR>runif(parameters)
	crtf=which(crtf==T)
	challenger=pop[index[1],]
	challenger[crtf]=abs(round(pop[index[2],crtf]-F*(pop[index[3],crtf]-pop[index[4],crtf])))
	for(j in 1:length(challenger)){
		if(challenger[j]>120){
			challenger[j]=sample(1:120,1)
		}
	}
	challengerFitness=fitnessTest(getSolution(challenger))	
	if(challengerFitness<fit[index[1]]){
		pop[index[1],]=challenger
		fit[index[1]]=challengerFitness
		points(i,challengerFitness,pch=10,col="blue")
	}
	if( min(fit) == 0 ){
		break
	}
	iterations = iterations + 1
}
index=which(fit==min(fit))[1]

# Formatting the final matrix
finalAnswer = matrix(getSolution(pop[index,]),5,5)
finalAnswer = substituteStrings(finalAnswer)
cat("\nFinished...It took the current run of our DE algorithm",Sys.time()-begin,"seconds to find a solution to a problem with over 24 billion possible combinations\n\n")
cat("Our final matrix looks like: \n\n")
print(finalAnswer)
cat("\nThe final solution has a fitness of",min(fit),"And we find that the fish is in house number",which(finalAnswer[5,]=="Fish"))
