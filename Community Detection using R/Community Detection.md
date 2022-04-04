# Community Detection by applying various algorithms using R

In this lab, we use the Primary school data from the study by Stehle et al. (2011), entitled “High-Resolution Measurements of Face-to-Face Contact Patterns in a Primary School.” The full reference is below. 
We show the network graph plots showing community structure, using the Fast Greedy, Walktrap, Spinglass and Label Propagation and Girvan Newman algorithms in igraph. 
Based on the analysis we discuss the benefits and drawbacks of using the Girvan-Newman method for community detection with this dataset. When dealing with networks with a large number of nodes and data, the algorithm is inefficient. Because communities in vast and complicated networks are difficult to detect, Girvan Newman is not recommended for very large data sets. The Girvan Newman algorithm is an excellent fit for this dataset since it finds interactions between students and professors from various classes. Furthermore, displaying the transmission of infections between communities as well as the often visiting boundaries that connect them. 

## References 
Kolaczyk, Eric D., and Gábor Csárdi. Statistical analysis of network data with R. Vol. 65. Springer, 2014. (KC) 

Stehle ́ J, Voirin N, Barrat A, Cattuto C, Isella L, et al. (2011) High-Resolution Measurements of Face-to-Face Contact Patterns in a Primary School. PLoS ONE 6(8): e23176. doi:10.1371/journal.pone.0023176 
