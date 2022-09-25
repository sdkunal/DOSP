# COP 5615: Distributed Operating System Principles Project 1

## Group Members

<ul>
  <li>Kunal Dudhe</li>
  <li>Aakash Naik</li>
</ul>

## Description

The aim of this project is to simulate the working of a cryptographic hash function.<br>
This function compares if the hashed value of a randomly generated alphanumeric string contains the required number of leading zeros.<br>
To simulate the working of a distributed system, we used Erlang's actor model where the actors act as workers by communicating between themselves.<br>

## Implementation Details

<ul>
  <li>Compile client.erl <b>c(client)</b> and server.erl <b>c(server)</b></li>
  <li>Run server.erl by passing it the number of zeros <b>server:start(numberZeros)</b></li>
</ul>

## Results

### Size Of Work Unit

Process of execution:
<ul>
  <li>The server starts an actor which starts mining coins by sending the number of hashed strings to be computed to the client.</li>
  <li>The worker actor computes the number of hashes and sends the result back to the server which, in turn, prints the hash results on the server console.</li>
  <li>During this entire process the worker actors stay in communication with the server's actor.</li>
</ul>

We computed SHA256 hash for 6,000,000 strings each on the client and the server.<br>
Both client and server are 8 core machines; the server spawned 8 actors while the client spawned 4 actors.<br>
The coin mining was equally divided between all the workers.<br>
Each actor on the client was working on 1,500,000 strings while each actor on the server worked on 750,000 strings.<br>
The timer was started as soon as the server started and it ended when the last actor finished mining coins.<br>

### Searching For A String With 4 Leading Zeros

The result obtained when the number of leading zeros to be compared is 4 is as follows:

<img width="1084" alt="Result For Input 4" src="https://user-images.githubusercontent.com/89472838/192120508-089875e9-632b-4057-a131-cf57a034f277.png">

The number of strings to be generated was 12,000,000.<br>
The string was of a static length, appended after the Gatorlink ID.<br>
The format of the string provided to the hash function was: <gatorlink_id>:<random_string_of_length_six>.<br>
The master spawned six actors that acted as workers to calculate the SHA256 of the strings.<br>
Actors returned the strings that matched with the required number of leading zeros to the server, where they were printed.<br>
Print format: <number_of_zeros> <random_string> <sha256_encrypted_string> <worker_pid>.<br>

### Running Time Comparison

<strong>Single actor implementation:</strong><br>
Number of string: 12,000,000<br>
Number of zeros: 4<br>
String length: 6<br>

<img width="963" alt="Result For Input 4 (Single Thread)" src="https://user-images.githubusercontent.com/89472838/192121282-0e140c4a-62c1-46cf-af70-d4c11341bbc2.png">

Total time = End Time - Start Time<br>
Total Time = 67787615 microsceonds = 67.787615 seconds<br>

<strong>Multi-actor implementation:</strong><br>
Number of string: 12,000,000<br>
Number of actors spawned: 6<br>
Number of zeros: 4<br>
String length: 6<br>

<img width="947" alt="Result For Input 4 (Multi-thread)" src="https://user-images.githubusercontent.com/89472838/192121611-88a02850-d875-48fb-8b09-90fcfb5cd74f.png">

Total time = End Time - Start Time<br>
Total Time = 16035612 microsceonds = 16.035612 seconds<br>

Ratio of CPU time to Real time = $\frac{Total time taken by multi-actor implementation}{Total time taken by single actor implementation}$<br>
Ratio of CPU time to Real time = $\frac{67.787615}{16.035612}$<br>
Ratio of CPU time to Real time = 4.227317<br>

This shows that when 6 actors are spawned as workers, it trumps the single actor implemenetation in terms of time by a factor of 4.<br>

### Coin With Most Leading Zeros

The coin with the most leading zeros we managed to find was 7.<br>
The server spawned 8 actors to find the hashed value with most leading zeros.<br>

<img width="927" alt="Max Number of Zeros Found" src="https://user-images.githubusercontent.com/89472838/192122190-83c2422f-4b1a-4e7d-98ad-5328afaae182.png">

### Maximum Number Of Working Machines

The server and the client were two different machines.<br>
The server spawned workers to perform the calculations on the client.<br>
