# LSEQ

Haskell implementation of a CRDT[1]-based array with an underlying exponential tree (first described in [2]) and the allocation strategy LSEQ[3]. CRDT-based arrays can be used in collaborative editing systems to allow conflict free automatic merges of conflicting concurrent edits.[4]

The LSEQ algorithm is an optimization of the Logoot[5] data structure. A Logoot document will grow exponentially in size for certain operations (front and end editing). LSEQ uses randomized base doubling to optimize the allocation of identifiers. This results in near-optimal linear growth of document size for large documents (more than 1k insertions) when using LSEQ. The random base doubling approach of LSEQ uses the statistical property described by the central limit theorem (CLT) which implies that the sum of cumulative random events will converge towards a normal distribution. This is in contrast with a round-robin or non-random base doubling approach which does not share this beneficial statistical property.

## Warning

This project is a work in progress and mainly for research purposes. It is not production ready and no guarantees are made by the author. See the LICENSE for a more detailed warranty disclaimer.

## References

- [1] Shapiro, M., Preguiça, N., Baquero, C., Zawirski, M., ”A comprehensive study of Convergent and Commutative Replicated Data Types”, *Research Report*, 2011.
- [2] A. Andersson and M. Thorup. "Dynamic ordered sets with exponential search trees." *J. ACM*, 54(3), June 2007.
- [3] Nédelec B., Molli, P., Mostefaoui, A., Desmontils, E., ”LSEQ: an Adaptive Structure for Sequences in Distributed Collaborative Editing”, *DocEng '13 Proceedings of the 2013 ACM symposium on Document engineering*, ACM, New York 2013, pp. 37-46.
- [4] N. Preguic ̧a, J. M. Marquès, M. Shapiro, and M. Letia. "A commutative replicated data type for cooperative editing. *Distributed Computing Systems, 2009. ICDCS’09. 29th IEEE International Conference*,  IEEE 2009, pp. 395–403.
- [5] S. Weiss, P. Urso, and P. Molli. "Logoot: a scalable optimistic replication algorithm for collaborative editing on p2p networks." *Distributed Computing Systems, 2009. ICDCS’09. 29th IEEE International Conference*, IEEE 2009, pp. 404–412.
