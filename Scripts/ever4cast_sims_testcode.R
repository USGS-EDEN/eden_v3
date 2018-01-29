## This code runs along ever4cast_sims.R so as to be able to run just one 
# simulation and a few days at a time (for testing & experimenting)

output_folder <- "./Output/testing_"


sims <- sims[1]
sim_names <- sim_names[1]


sim_list[[1]] <- sim_list[[1]][Date <= 20180103, ]
