

n_iter <- 50 # Number of iterations of the loop

# Initializes the progress bar
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

for(i in 1:n_iter) {
  
  #---------------------
  # Code to be executed
  #---------------------
  
  Sys.sleep(0.1) # Remove this line and add your code
  
  #---------------------
  
  # Sets the progress bar to the current state
  setTxtProgressBar(pb, i)

}

close(pb) # Close the connection