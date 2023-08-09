#!/usr/bin/env pypy3

from collections import defaultdict
import heapq
def creategraph(N,M, batteries, dists):
    graph = []

    for i in range(N):
        nodes = []
        main = {}

        #create base node for each N, this contains battery distance and speed of each node
        main = {'nodename': i, 'distance': batteries[i][0], 'speed': batteries[i][1]}
        nodes.append(main)
        for k in range(M):
            if dists[k][0] == i+1:
                edges = {}

                #create adjacent edges to each base node and the dge length of each edge
                edges = {'nodename' : dists[k][1] -1, 'edgelength' : dists[k][2]}
                nodes.append(edges)
        graph.append(nodes)

    return graph

def dijkstras(reachedgraph, N):
    H = []
    #create a heap that contains all edges of the start node
    heapq.heappush(H, (0, reachedgraph[0][0]['nodename']))
    #set all distances to infinity
    timeList = [10000000] * N
    #set start node distance to 0
    timeList[0] = 0
    pushed = set()

    while H:
        #pop the minimum value in the heap (smallest time from base node)
        time, nodename = heapq.heappop(H)
        if nodename in pushed:
            continue
        else:
            pushed.add(nodename)
        #go through the edges of this value
            for i in range(1, len(reachedgraph[nodename])):
                #get each edge's time and nodename
                edge_time = reachedgraph[nodename][i]['time']
                edge_node = reachedgraph[nodename][i]['nodename']
                #if the edges time is set to infinity, set edge's time to its new time
                if timeList[edge_node] > time + edge_time:
                    timeList[edge_node] = time + edge_time
                    #push new edge to heap so we can search its edges
                    heapq.heappush(H, (timeList[edge_node], edge_node))

    return timeList[N-1]



def reachable(N, graph, start):
    
    H = []
    #create a new heap that prioritizes all edges in base node's heap
    heapq.heappush(H, (0, graph[start][0]['nodename']))
    #keep track of what nodes have already been pushed (replaces decrease key)
    pushed = set()
    #set all distances to infinity
    distanceList = [100000] * N
    #set start node distance to 0
    distanceList[start] = 0
    
    reached = []
    reached.append({'nodename': start, 'time':0})
    speed = graph[start][0]['speed']


    while H:
        distance, nodename = heapq.heappop(H) # this is delete min
        #gets the speed of the battery to divide it by the distances and add the values back to the heap as the time it takes to reach the node
        #if the node has already been pushed continue and if not add it
        if nodename in pushed:
            continue
        else:
            pushed.add(nodename)
            #checks if the distance of the start node has been passed if it has then we stop dijkstra's for that iteration
            # for each edge in the start
            for i in range(1, len(graph[nodename])):
                edge_dist = graph[nodename][i]['edgelength']
                edge_node = graph[nodename][i]['nodename']


                #check if distance has been set
                if distanceList[edge_node] > distance + edge_dist:
                    #set distance to the distance of the previous plus the distance of the edge which was 
                    distanceList[edge_node] = distance + edge_dist
                    if (distanceList[edge_node]) <= (graph[start][0]['distance']):
                        #if the distance to this edge is less than the start nodes available distance then add to heap
                        #BELOW IS THE INDIVIDUAL START NODES HEAD
                        new_dist = distanceList[edge_node]/speed
                        heapq.heappush(H, (distanceList[edge_node], edge_node))
                        exists = False
                        for j in range(len(reached)):
                            if reached[j]['nodename'] == graph[nodename][i]['nodename']:
                                if reached[j]['time'] > new_dist:
                                    reached[j]['time'] = new_dist
                                exists = True
                        if exists == False:
                            reached.append({'nodename': graph[nodename][i]['nodename'], 'time': new_dist})
                        #if not then set the allowed distance passed variable to true
    return reached

def solve(N, M, batteries, dists):
#NEED TO ITERATE THROUGH THE GRAPH AND ADD REACHABLE HEAPS
    
    graph = creategraph(N, M, batteries, dists)
    reachedgraph = []
    nodes = []
    for i in range(len(graph)):
        reached = reachable(N, graph, i)
        reachedgraph.append(reached)

    time = dijkstras(reachedgraph, N)
    return time
    #timegraph = dijkstras(reachedgraph)

def read_input():
    N, M = [int(i) for i in input().split()]
    batteries = [[int(i) for i in input().split()] for _ in range(N)]
    dists = [[int(i) for i in input().split()] for _ in range(M)]
    
    return N, M, batteries, dists



def main():
    N, M, batteries, dists = read_input()

    t = solve(N, M, batteries, dists)
    print(f'{t:.6f}')


if __name__ == '__main__':
    main()
