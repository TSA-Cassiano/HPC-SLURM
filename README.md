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

The manual steps can be found in here, while explanation regarding the automatic approach is here. Go to one of these links after setting up the physical arragment of the cluster explained below.

Regardless of the approach, you need to physically set up the cluster. The structure requires at least the following equipment:

1. 1 PC to be the master
2. At least 1 PC to be the slave
3. 1 switch
4. 1 additional Ethernet card

The master will have two connections: the intranet (a local net connecting the nodes with the master) and the internet (master -> world). The additional card is to allow the additional connection. To set up the cluster structure do:
1. Install the additional card in the master
2. Connect a Ethernet cable from one of the master's spots to the switch and another to the internet provider.
3. Connect a Ethernet cable from the switch to the slave.



