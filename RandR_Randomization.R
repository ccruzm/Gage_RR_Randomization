# Makes replicates from a small vector input to a 
# larger vector output

#--------------------------------------------------
# Prompt for replications, operators, and parts numbers.
# This is used in the for loop(s) in order to know
# the total iterations.
numPart <- readline(prompt="Enter number of parts: ")
numOper <- readline(prompt="Enter number of operators: ")
numRep <- readline(prompt="Enter number of replications: ")
#--------------------------------------------------

#--------------------------------------------------
# This section of code is used to generate a
# randomized vector of parts.

partsRandomized=0 # Used to initialize vector

# Generates a vector of "numPart" parts randomized for
# each "numOper" operator with "numRep" repetition.
for(i in 1:(as.numeric(numRep)*as.numeric(numOper)))
{
   partsRandomized <- c(partsRandomized,
			sample.int(as.numeric(numPart),
			as.numeric(numPart),
			replace=F))
}

# This function splices the first zero term used
# to initialize function
partsRandomized=partsRandomized[2:length(partsRandomized)]
#--------------------------------------------------

#--------------------------------------------------
# This section of code is used to generate a
# vector of operators per repetition

operOrganized=0 # Used to initialize vector

# Initial loop to iterate over number of
# Operators and Repetitions
for(i in 1:(as.numeric(numRep)*as.numeric(numOper)))
{
   # Loop to iterate over number of Parts
   for(j in 1:as.numeric(numPart))
   {
      # Used for number less than Operators
      if(i<=as.numeric(numOper))
      {
         operOrganized <- c(operOrganized, i)
      }
      # Used for number more than Operators
      # except last which modulus is zero
      else if((i%%as.numeric(numOper))%%i!=0)
      {
         operOrganized <- c(operOrganized,
			    (i%%as.numeric(numOper))%%i)
      }
      # Used for number more than Operators
      # in which modulus is zero
      else
      {
         operOrganized <- c(operOrganized,
			   (i%%as.numeric(numOper))%%i+
			   as.numeric(numOper))
      }
   }
}

# This function splices the first zero term used
# to initialize function
operOrganized=operOrganized[2:length(operOrganized)]
#--------------------------------------------------

#--------------------------------------------------
# This section of code is used to generate a
# vector of repetitions.

repOrganized=0 # Used to initialize vector

# Initial loop to iterate over number of
# Operators and Repetitions
for(i in 1:as.numeric(numRep))
{
   # Loop to iterate over number of Parts
   for(j in 1:(as.numeric(numPart)*as.numeric(numOper)))
   {
      repOrganized <- c(repOrganized, i)
   }
}

# This function splices the first zero term used
# to initialize function
repOrganized=repOrganized[2:length(repOrganized)]
#--------------------------------------------------

#--------------------------------------------------
# This code compiles the all vectors into a data
# frame for the xlsx file and provides column names.
df <- data.frame(repOrganized,
		 operOrganized,
		 partsRandomized)
colnames(df) <- c("Replications","Operators","Parts")
#--------------------------------------------------

#--------------------------------------------------
##################################################
# install package of library ONLY once		 #
# command: install.packages("rJava")		 #
# Require Java Jdk; separate installation	 #
# command: install.packages("xlsx") 		 #
##################################################

# Used to create "xlsx" file
require(xlsx)

# Sets directory where file is going to be written
# using a browser.
fileToChoose <- choose.dir(getwd(),
		"Choose a suitable folder")

# Writes data to "Randomized_RR.xlsx" file and file
# is placed in "fileToChoose" location.
write.xlsx(df,file.path(fileToChoose,"Randomized_RR.xlsx"))
#--------------------------------------------------