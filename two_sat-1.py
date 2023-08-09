from __future__ import annotations

from functools import cached_property
from typing import NamedTuple, Tuple, Hashable, List 

import networkx as nx


class Literal(NamedTuple):
    variable: str
    negated: bool

    @classmethod
    def from_str(cls, data: str) -> Literal:
        data = data.strip()
        if data[0] == "~":
            return cls(variable=data[1:], negated=True)
        else:
            return cls(variable=data, negated=False)

    @property
    def negation(self) -> Literal:
        return Literal(variable=self.variable, negated=not self.negated)

    def __str__(self) -> str:
        return f"~{self.variable}" if self.negated else self.variable


Clause = Tuple[Literal, Literal]


class TwoSatInstance:
    """Class representing a 2SAT instance in conjunctive normal form.

    Examples
    --------
    >>> data = "(a | b) & (a | ~b) & (~a | b) & (~a | ~b)"
    >>> instance = TwoSatInstance(data)
    >>> print(instance)
    (a | b) & (a | ~b) & (~a | b) & (~a | ~b)
    >>> str(instance) == data
    True
    """
    clauses: list[Clause]

    def __init__(self, formula: str):
        clause_strings = formula.split("&")
        clauses: list[Clause] = []
        for clause_str in clause_strings:
            try:
                literal1_str, literal2_str = clause_str.strip("( )\n").split("|")
            except ValueError:
                raise ValueError(f"Clause {clause_str!r} does not contain 2 literals")
            literal1 = Literal.from_str(literal1_str)
            literal2 = Literal.from_str(literal2_str)
            clauses.append((literal1, literal2))

        self.clauses = clauses

    @property
    def variables(self) -> set[str]:
        return {literal.variable for clause in self.clauses for literal in clause}

    def check_assignment(self, assignment: dict[str, bool]) -> bool:
        """Checks whether the given assignment satisfies this TwoSatInstance."""
        return all(
            assignment[first.variable] ^ first.negated
            or assignment[second.variable] ^ second.negated
            for first, second in self.clauses
        )

    @cached_property
    def implication_graph(self) -> nx.DiGraph:
        """Builds the implication graph for this TwoSatInstance."""
        G = nx.DiGraph()
        for lit1, lit2 in self.clauses:
            G.add_edge(lit1.negation, lit2)
            G.add_edge(lit2.negation, lit1)

        return G

    @cached_property
    def is_satisfiable(self) -> bool:
        """Determines if this TwoSatInstance is satisfiable.

        Examples
        --------
        >>> formula = "(a | b) & (a | ~b) & (~a | b) & (~a | ~b)"
        >>> instance = TwoSatInstance(formula)
        >>> instance.is_satisfiable
        False
        >>> formula = "(a | ~b) & (~a | ~c) & (a | b) & (~c | d) & (~a | d)"
        >>> instance = TwoSatInstance(formula)
        >>> instance.is_satisfiable
        True
        """
        G = self.implication_graph
        
        sccs,scc_map = kosaraju(G)

        for v in scc_map:
            if v[0] != "~":
                if scc_map[v] == scc_map[v.negation]:
                    return False
        return True

    def compute_assignment(self) -> dict[str, bool]:
        """Computes an assignment which satisfies this TwoSatInstance.

        Raises
        ------
        RuntimeError
            If this TwoSatInstance is unsatisfiable.

        Examples
        --------
        >>> formula = "(a | b) & (a | ~b) & (~a | b) & (~a | ~b)"
        >>> instance = TwoSatInstance(formula)
        >>> instance.compute_assignment()
        Traceback (most recent call last):
         ...
        RuntimeError: Cannot compute assignment for unsatisfiable TwoSatInstance!
        >>> formula = "(a | ~b) & (~a | ~c) & (a | b) & (~c | d) & (~a | d)"
        >>> instance = TwoSatInstance(formula)
        >>> assignment = instance.compute_assignment()
        >>> # There are multiple possible satisfying assignments
        >>> assert assignment == {'a': True, 'b': False, 'c': False, 'd': True} or assignment == {'a': True, 'b': True, 'c': False, 'd': True}
        """
        if self.is_satisfiable == False: 
            raise RuntimeError("Cannot compute assignment for unsatisfiable TwoSatInstance!")
        
        G = self.implication_graph
        sccs, scc_map = kosaraju(G)
        assign = {}
        
        
        #greater than negation is true
        for var in scc_map:
            if str(var)[0] != "~":
                assign[var] = scc_map[var] > scc_map[var.negation]
        return assign
        

    def solve(self) -> dict[str, bool] | None:
        """Solves this TwoSatInstance. Returns the assignment, or None if unsatisfiable.

        Examples
        --------
        >>> formula = "(a | b) & (a | ~b) & (~a | b) & (~a | ~b)"
        >>> instance = TwoSatInstance(formula)
        >>> instance.solve() is None
        True

        >>> formula = "(a | ~b) & (~a | ~c) & (a | b) & (~c | d) & (~a | d)"
        >>> instance = TwoSatInstance(formula)
        >>> instance.check_assignment(instance.solve())
        True
        """
        if not self.is_satisfiable:
            return None
        return self.compute_assignment()

    def __str__(self) -> str:
        return "(" + ") & (".join(map(lambda clause: f"{clause[0]} | {clause[1]}", self.clauses)) + ")"

def dfs(adj_list, v, visited, postorder):
    """ Recursively visits all the children of v """
    stack = [v]
    while stack:
        v = stack.pop()

        if visited[v] == False:
            visited[v] = True
            # pop top of stack
            # set visited[v] to True
            for u in adj_list[v]:
                if visited[u] == False:
                    stack.append(u)
                    # add u to stack
            postorder.append(v)

def explore(adj_list, v, visited, scc):
    stack = [v]
    while stack:
        v = stack.pop()
        if visited[v] == False:
            scc.append(v)
            visited[v] = True
            # pop top of stack
            # set visited[v] to True
            for u in adj_list[v]:
                if visited[u] == False:
                    stack.append(u)
                    # add u to stack
    return scc

def kosaraju(graph: nx.DiGraph) -> list[list[Hashable]]:
    """Computes the strongly connected components of a directed graph using Kosaraju's algorithm.

    Parameters
    ----------
    graph : nx.DiGraph
        A directed graph

    Returns
    -------
    sccs : list[list[Hashable]]
        A list of lists, where each list is a strongly connected component in the input graph.

    Examples
    --------
    >>> graph = nx.DiGraph()
    >>> graph.add_edges_from([(1, 2), (2, 3), (3, 4), (4, 3), (2, 5), (5, 1), (5, 6), (6, 7), (7, 6), (3, 7), (4, 8), (8, 4)])
    >>> sccs = kosaraju(graph)
    >>> sorted([sorted(scc) for scc in sccs])
    [[1, 2, 5], [3, 4, 8], [6, 7]]
    """
    visited = {}
    postorder = []
    for j in graph:
        visited[j] = False

    rev_G = graph.reverse()

    for v in rev_G:
        dfs(graph, v, visited, postorder)
    count = 0
    sccs = {}
    scc_map = {}
    for j in graph:
        visited[j] = False
    while postorder:
        vertex = postorder.pop()
        if not visited[vertex]:
            scc = []
            scc = explore(rev_G, vertex, visited, scc)
            sccs[vertex] = scc
            for v in scc:
                scc_map[v] = count
            count+=1

    return sccs, scc_map


if __name__ == "__main__":
    # formula = "(a | ~b) & (~a | ~c) & (a | b) & (~c | d) & (~a | d)"
    # instance = TwoSatInstance(formula)
    # print(instance)
    # instance.is_satisfiable
    s = input()
    inst = TwoSatInstance(s)
    assignment = inst.solve()
    if assignment is None:
        print("Not Satisfiable")
    else:
        for literal, value in sorted(assignment.items()):
            print(f'{literal} {value}')

