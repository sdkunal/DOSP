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
   <li>For running gossip algorithm  
      <ul>
        <li> Full Network - server:startFull(“Gossip”,NumNodes)</li>
        <li> Line - server:startLine(“Gossip”,NumNodes)</li>
        <li> 2D Grid - server:start2D(“Gossip”,NumNodes,NumCols)</li>
        <li> Imperfect 3D Grid - server:start3D(“Gossip”,NumNodes,NumCols)</li>
      </ul>
  </li>
  <li>For running push-sum algorithm  
      <ul>
        <li> Full Network - server:startFull(“PushSum”,NumNodes)</li>
        <li> Line - server:startLine(“PushSum”,NumNodes)</li>
        <li> 2D Grid - server:start2D(“PushSum”,NumNodes,NumCols)</li>
        <li> Imperfect 3D Grid - server:start3D(“PushSum”,NumNodes,NumCols)</li>
      </ul>
  </li>
  
</ul>
