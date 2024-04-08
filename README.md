# HPC-SLURM
Hi! 

During my physics PhD, I was forced to build a cluster without help. I had no prior knowledge of this kind of task so I had to make a lot of effort to make this work. This repository is a collection of all the scripts that I had to make to set up a Beowulf-type cluster. It has everything to go from nothing to a functional cluster intended to calculate simulations of quantum chemistry, although some solutions may not be optimal. My main intent is to leave it as a reference for me in the future. However, if you are in the same hole I was, feel free to use everything in here. Some specifics:

1. Made for Ubuntu 20.04
2. Uses slurm as job submission
3. Has a script for user creation
4. Gaussian 16 bash script configuration for multiple users

Here, I present two approaches to setting up the cluster: the manual and the automatic.
1. The manual consists of a hands-on set of tasks done through a terminal. You will have to type everything. It is highly educational but takes a lot of time and scales badly with the number of nodes.
2. The automatic is fast but is not so educational. If you need to adjust the scripts, understanding the manual way facilitates the process.
