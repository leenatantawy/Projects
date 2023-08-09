"""
Teleporters

Incoming Dean of the College, Melina Hale, has decided that winters have gotten too long and by next winter, the beloved tunnels connecting the entire campus should be accessible once again. Unfortunately, all the old tunnels running under the quad have become unusable and new ones need to be built. Of course, this should be kept economical and financially responsible. An engineering firm has provided all possible tunnels that could be dug, and of course the price tag for each of them. In a wild revelation, Fermilab announced that they now have a working teleporter, but it only works for short distances. The engineering firm has provided estimates for installing these teleporters in various building around campus (but not all). The dean wants all buildings to be connected by at least one path, but also for the entire project to cost as low as possible. To that end, you need to find the best set of tunnels and (maybe) teleporters to connect the entire campus for the next winter.

Input: In the first line there are three integers $N$, $K$, $M$, which correspond to the number of buildings, the number of buildings where a teleporter can be installed and the number of possible tunnels between buildings.
In the next $K$ lines there are two integers, $i$ and $B[i]$, meaning that a teleporter can be installed in building $i$ with cost $B[i]$.
Finally, the next $M$ lines contain three integers $i$, $j$, $c[i, j]$, which denote that there is a proposed tunnel between buildings $i$ and $j$ with cost $c[i, j]$.

Output: One line with a single integer, the minimum cost for the entire network.
"""

from collections import defaultdict
import heapq

def creategraph(N, teleporters, edges):
    keys = list(edges)
    # since buildings are not necessarily named 1,2,3 by index get the keys of each building
    new_node = N+1
    for i in range(len(teleporters)):
        for j in keys:
            if j == teleporters[i][0]:
                edges[j].append((new_node, teleporters[i][1]))
                #if they are the same then append a connecting edge to the imaginary telepprter node with the same cost as the teleporter 
    edges[N+1] = teleporters
    return edges

def prims(N, edges):
    H = []
    keys = list(edges)
    heapq.heappush(H, (0, keys[0]))
    pushed = set()
    #^^ for decrease key
    costList = [100000] * (N+1)
    #set all costs to big number
    costList[keys[0]] = 0
    costList[0] = 0
    #set cost of first building to 0
    keys = list(edges)
    #append the first building with cost 0
    reached = set()
    while H:
        cost, nodename = heapq.heappop(H)
        reached.add(nodename)
        # this is delete min, will give the minimum cost of all edges
        if nodename in pushed:
            #for decrease key
            continue
        else:
            pushed.add(nodename)
            for i in range(0, (len(edges[nodename]))):
                # for i in range of all edges of the new minimum edge look at all of its edges
                edge_node = edges[nodename][i][0]
                edge_cost = edges[nodename][i][1]
                if edge_node in reached:
                    continue
                else:
                #record building number
                    if costList[edge_node] > edge_cost and edge_node!=keys[0]:
                        #if the cost of this edge has not been recorded then set this to the cost of the edge
                        costList[edge_node] = edge_cost
                        heapq.heappush(H, (costList[edge_node], edge_node))
                        # for j in range(len(reached)):
                            #     if reached[j]['nodename'] == edge_node:
                            #         if reached[j]['cost'] > new_cost:
                            #             reached[j]['cost'] = new_cost
                            #         exists = True
                            # if exists == False:
                            #     reached.append({'nodename': edge_node, 'cost': new_cost})
    return costList

def solve(N, K, M,teleporters, edges):
    cost_norm = prims(N, edges)
    costnorm = sum(cost_norm)

    edges_tele = creategraph(N, teleporters, edges)

    cost_tele = prims(N+1, edges_tele)

    costtele = sum(cost_tele)


    if costtele >= costnorm:
        cost = costnorm
    else:
        cost = costtele
    
    return cost


def read_input():
    N, K, M = [int(i) for i in input().split()]
    teleporters = [[int(i) for i in input().split()] for _ in range(K)]
    edges = defaultdict(list)
    for i in range(M):
        u, v, c = [int(i) for i in input().split()]
        edges[u].append((v, c))
        edges[v].append((u, c))
    return N, K, M, teleporters, edges


def main():
    N, K, M, teleporters, edges = read_input()
    cost = solve(N, K, M, teleporters, edges)
    print(cost)


if __name__ == '__main__':
    main()
