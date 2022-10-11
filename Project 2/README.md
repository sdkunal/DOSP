# Project 2 - Gossip Algorithm

## Group Members

<ul>
  <li>Kunal Dudhe</li>
  <li>Aakash Naik</li>
</ul>

## Description

The aim of this project is to simulate the convergence of gossip type algorithms through a simulator based on actors written in Erlang.<br>
We have implemented two algorithms namely Gossip algorithm and Push-Sum algorithm for sum computation.<br>
We implemented 4 different network topologies for the dissemination of gossip and measured the performance of different topologies against each other.<br>
The 4 network topologies we build are: <br>
<ul>
  <li> Full Network </li>
  <li> Line </li>
  <li> 2D Grid </li>
  <li> Imperfect 3D Grid </li>
</ul>

### Implementation Details

<ul>
  <li>Compile project2.erl <b>c(server).</b></li>
  <li>Run project2.erl by running separate functions created for each network topology</li>
   <li>For running the Gossip algorithm  
      <ul>
        <li> Full Network - server:startFull(“Gossip”,NumNodes)</li>
        <li> Line - server:startLine(“Gossip”,NumNodes)</li>
        <li> 2D Grid - server:start2D(“Gossip”,NumNodes,NumCols)</li>
        <li> Imperfect 3D Grid - server:start3D(“Gossip”,NumNodes,NumCols)</li>
      </ul>
  </li>
  
  <li>For running the push-sum algorithm 
      <ul>
        <li> Full Network - server:startFull(“PushSum”,NumNodes)</li>
        <li> Line - server:startLine(“PushSum”,NumNodes)</li>
        <li> 2D Grid - server:start2D(“PushSum”,NumNodes,NumCols)</li>
        <li> Imperfect 3D Grid - server:start3D(“PushSum”,NumNodes,NumCols)</li>
      </ul>
  </li>
  
</ul>

### Results

#### Gossip Algorithm

We implemented the Gossip algorithm using 4 network topologies. We tested the network with 20000 nodes.
The Full network topology gave the worst results, and the system crashed after 700 nodes. Below is the graph showing the exponential running time of the full network topology.

<img width="555" alt="Screen Shot 2022-10-10 at 11 24 40 PM" src="https://user-images.githubusercontent.com/89472838/194993801-cfc6d0ea-a54b-4ce5-b771-033ea315a193.png">

Line topology and 2D grid topology converged almost simultaneously, and we could test them for 20000 nodes. Line performed slightly better for a higher number of nodes. 
We could test the Imperfect 3D grid topology for 8000 nodes. The system crashed for 10000 nodes. The time increased exponentially when the number of nodes was increased from 4000 to 8000.

<img width="590" alt="Screen Shot 2022-10-10 at 11 27 45 PM" src="https://user-images.githubusercontent.com/89472838/194993892-bcbe8973-f5ea-44fb-9d48-d71854a7d842.png">

Our experiment concludes that line topology converged the fastest for the Gossip algorithm.
Here’s an example of the gossip algorithm converging for the full network topology with 10 nodes.

<img width="638" alt="Screen Shot 2022-10-10 at 11 41 40 PM" src="https://user-images.githubusercontent.com/89472838/194994008-36cc3995-3cd0-47c2-a62c-fad778344544.png">

#### Push-Sum Algorithm

We implemented the Push-Sum algorithm using 4 network topologies. We tested the network with 49 nodes.
Line topology gave the worst results. 
Imperfect 3D grid topology performed almost as well as line and started diverging after we tested it for 36 nodes.
The 2D grid topology converged after a long time, but it took less time than the line topology.

<img width="564" alt="Screen Shot 2022-10-10 at 11 32 41 PM" src="https://user-images.githubusercontent.com/89472838/194993929-d56c249a-ab1b-4671-93a0-8f484aabc386.png">

Here’s an example of the push-sum algorithm converging for the full network topology with 10 nodes.

<img width="640" alt="Screen Shot 2022-10-10 at 11 42 00 PM" src="https://user-images.githubusercontent.com/89472838/194994042-e3f4db79-f855-4b8a-a7e7-9c739a73cbeb.png">
