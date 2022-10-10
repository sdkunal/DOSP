# COP 5615: Distributed Operating System Principles Project 2

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
  <li>Compile client.erl <b>c(client).</b> and server.erl <b>c(server).</b></li>
  <li>Run server.erl by running separate functions created for each network topology
      <ul>
        <li> Full Network - server:startFull(NumNodes)</li>
        <li> Line - server:startLine(NumNodes)</li>
        <li> 2D Grid - server:start2D(NumNodes,NumCols)</li>
        <li> Imperfect 3D Grid - server:start3D(NumNodes,NumCols)</li>
      </ul>
  </li> 
</ul>
