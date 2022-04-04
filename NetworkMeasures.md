# Social-Network-Analytics-using-R
# Network Measures using R

We try to analyze whether the given graph is multigraph or not. We transform the network to a simple graph with the simplify() function, which combines the edge weights into a sum of existing weights between each pair of nodes. Then, it constructs an alternative weighting scheme, by taking the inverse of the logarithm of the default weight. Later, we generate an induced subgraph of the network.

We are using Merger_Net dataset for our analysis.
