import networkx as nx

def make_edges():
    with open("day_25.txt", "rt") as f:
        rows = f.readlines()

    edges = []

    for row in rows:
        splitted = row.strip().split(':')
        left = splitted[0].strip()
        nodes = splitted[1].strip().replace('\n','').split(' ')

        for node in nodes:
            edges.append((left, node.strip()))
    return edges

def calculate_centralities(edges):
    G = nx.Graph()
    G.add_edges_from(edges)
    betweenness_centrality = nx.edge_betweenness_centrality(G)

    res = [(edge,c) for edge,c in betweenness_centrality.items()]
    sorted_res = sorted(res, key=lambda x: x[1])
    return sorted_res

def calculate_nodes(edges):
    nodes = set()
    for (x,y) in edges:
        nodes.add(x)
        nodes.add(y)
    return nodes

def remove_edges(edges, centralities):
    for (edge, _) in centralities[-3:]:
        if edge in edges:
            edges.remove(edge)
        else:
            edges.remove((edge[1], edge[0]))
    return edges

def initialize(nodes):
    return {n:n for n in nodes}

def find(node, parents):
    if node == parents[node]:
        return node
    else:
        return find(parents[node], parents)

def union(node1, node2, parents):
    parent1 = find(node1, parents)
    parent2 = find(node2, parents)
    parents[parent1] = parent2

def disjoint_set(nodes, edges):
    parents = initialize(nodes)
    for (node1, node2) in edges:
        union(node1, node2, parents)

    sets = dict()
    for node in parents:
        parent = find(node, parents)
        if parent in sets:
            sets[parent] += 1
        else:
            sets[parent] = 1
    print(sets)
    return sets

edges = make_edges()
nodes = calculate_nodes(edges)
centralities = calculate_centralities(edges)
fixed_edges = remove_edges(edges, centralities)

res = [x for x in disjoint_set(nodes, fixed_edges).values()]

total = res[0] * res[1]
print(total)
