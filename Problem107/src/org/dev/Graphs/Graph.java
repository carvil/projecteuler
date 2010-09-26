package org.dev.Graphs;


public class Graph {

	public int nvertices;
	public int[][] edges;
	
	public Graph(int[][] matrix) {
		this.edges = matrix;
		this.nvertices = matrix.length;
	}

	public int getNvertices() {
		return nvertices;
	}

	public void setNvertices(int nvertices) {
		this.nvertices = nvertices;
	}

	public int[][] getEdges() {
		return edges;
	}

	public void setEdges(int[][] edges) {
		this.edges = edges;
	}
	
	
}
