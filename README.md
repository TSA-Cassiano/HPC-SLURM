# HPC-SLURM
Hi! 

During my physics PhD, I was forced to build a cluster without help. I had no prior knowledge of this kind of task so I had to make a lot of effort to make this work. This repository is a collection of all the scripts that I had to make to set up a Beowulf-type cluster. It has everything to go from nothing to a functional cluster intended to calculate simulations of quantum chemistry, although some solutions may not be optimal. My main intent is to leave it as a reference for me in the future. However, if you are in the same hole I was, feel free to use everything in here. Some specifics:

1. Made for Ubuntu 20.04
2. Uses slurm as job submission
3. Has a script for user creation
4. Gaussian 16 bash script configuration for multiple users

In this repo, I present two approaches to setting up the cluster: the [manual](https://github.com/TSA-Cassiano/HPC-SLURM/wiki/Manual-setup) and the [automatic](https://github.com/TSA-Cassiano/HPC-SLURM/wiki/Automatic-setup).
1. The [manual](https://github.com/TSA-Cassiano/HPC-SLURM/wiki/Manual-setup) consists of a hands-on set of tasks done through a terminal. You will have to type everything. It is highly educational but takes a lot of time and scales badly with the number of nodes.
2. The [automatic](https://github.com/TSA-Cassiano/HPC-SLURM/wiki/Automatic-setup) is fast but is not so educational. If you need to adjust the scripts, understanding the manual way facilitates the process.

To proceed, go to the [wiki page](https://github.com/TSA-Cassiano/HPC-SLURM/wiki), where the physical setup is provided.


I would not be able to do this repository without the great content from the following references:

https://dannylinuxnoob.blogspot.com/2017/08/building-home-hpc-computer-cluster.html

https://nekodaemon.com/2022/09/02/Slurm-Quick-Installation-for-Cluster-on-Ubuntu-20-04/

https://www.blasbenito.com/post/01_home_cluster/

https://www.youtube.com/watch?v=gvR1eQyxS9I

https://drtailor.medium.com/how-to-setup-slurm-on-ubuntu-20-04-for-single-node-work-scheduling-6cc909574365

https://h3abionet.org/images/Technical_guides/L2_02_Basic_HPC_Cluster_Setup_Howto_Guide.pdf
