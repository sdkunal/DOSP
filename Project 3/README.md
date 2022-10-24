# Project 3 - Chord Protocol

## Group Members

<ul>
  <li>Kunal Dudhe</li>
  <li>Aakash Naik</li>
</ul>

## Description

This project aims to implement the Chord protocol through a simulator based on actors written in Erlang.
We have implemented the Chord protocol successfully and implemented the node removal strategy as well.
The node removal strategy is not running synchronously; instead, the function runs when we stop searching for a particular key.

### Implementation Details

<ul>
  <li>Compile project3.erl: <b>c(project3).</b></li>
  <li>Run project3.erl by running a function named “start.”
    The function takes two arguments; the first is the number of nodes in the application, and the second is the maximum number of requests an actor can send.</li>
  <li>For running the Chord Protocol: <b>project3:start(NumNodes, NumRequests).</b></li>
  <li>For best results, we can choose the number of nodes to be a power of two so that the average number of hops is comparable with the logarithmic value of the number of nodes.
</li>
  <li>NumRequests gives an upper bound on how many search requests a node can make in the network.
    For experiment purposes, we chose a value > 2*log N.
    Once a node makes the maximum number of requests, the node no longer searches for a key; it stops its operation.</li>
</ul>

### Chord Protocol

We implemented Chord by generating several nodes and encrypting their PIDs (SHA-1 encryption) which acted as nodes in the network.
The SHA-1 encrypted PIDs were converted to strings.
The gaps between nodes were introduced naturally by the random nature of the encryption strategy of SHA-1.
This lets us create a ring by arranging the nodes chronologically with naturally generated gaps between two nodes.<br>

A successor node was determined based on the positioning of the keys.
To find the successors, we maintained a finger table for each node.
Each node had at most log N neighbors.<br>

We generated a key to be searched randomly and encrypted with SHA-1.
This random generation placed the key between 2 nodes on the Chord and gave a good sense of randomness to the key search in a network.<br>

We fired up the network by starting to look for the key from every node and hopped to a node in the finger table when we did not get a match.
If a node had m neighbors, then the finger table values were calculated by adding 2<sup>0</sup> to 2<sup>m - 1</sup> to the current node’s value.
After iterating through all the values in the finger table, if the protocol could not find the key, the last node in the finger table was used to start the search for the key once again.
This process repeated until the key was found, after which the number of hops for that node was used to calculate the average number of hops.<br>

#### Delete and add nodes to the network:

We deleted a node from the network and assigned all of its load to the successor node.
The reassignment was done between two key searches, and the finger table values were adjusted accordingly.<br>

Adding a node to the network was also done identically, and we could distribute the load of the next successor node accordingly.

### Results

We tested our network successfully up to 2048 nodes, and the average hop count was near log N in most cases.
The case where we got an average hop count greater than log N was the one in which the number of neighbors was not equal to log N.<br>

The number of requests was chosen to a maximum of 22 requests/node.
For values lesser than log N, the average number of hops increased considerably as fewer nodes could find the designated key.
This was due to many nodes being unable to use their entire finger table for look-up as they had already served as an intermediate node for a prior node.<br>
