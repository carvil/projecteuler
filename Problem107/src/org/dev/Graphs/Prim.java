package org.dev.Graphs;

public class Prim {

	public Graph g;
	public int start;
	
	private final int MAXINT = Integer.MAX_VALUE;  
	private int[] distance;
	private int[] parent;
	
	public Prim(Graph g, int start) {
		super();
		this.g = g;
		this.start = start;
		this.distance = new int[this.g.getNvertices()];
		this.parent = new int[this.g.getNvertices()];
	}
	
	public int prim() {
		
		//Variables for loops and saving information along the way
		int i, v, weight, dist;
		//Total amount saved by creating a MST
		int save = 0;
		//Number of vertices
		int nVertices = this.g.getNvertices();
		//Current elements in the final tree
		boolean[] intree = new boolean[nVertices];
		
		//Initialization of the arrays
		for (i=0; i < nVertices; i++) {
			intree[i] = false;
			this.distance[i] = this.MAXINT;
			this.parent[i] = -1;
		}
		
		//Starting point
		this.distance[this.start] = 0;
		v = this.start;
		
		//Loop through the vertices
		while(intree[v] == false) {
			intree[v] = true;
			
			// Search all adjacent nodes and update distances
			for(int j=0; j<nVertices; j++) {
				weight = this.g.edges[v][j];
				if (weight != 0) {
					if((this.distance[j] > weight) && 
					   intree[j] == false) {
						if (this.distance[j] != this.MAXINT)
							save += this.distance[j];
						this.distance[j] = weight;
						this.parent[j] = v;
					}
					else if (intree[j] == false)
						save += weight;
				}
			}
			
			//Find out next vertice to explore
			v = 0;
			dist = this.MAXINT;
			for(i=0; i< nVertices; i++) {
				if ((intree[i] == false) && 
					 (dist > this.distance[i])) {
					dist = this.distance[i];
					v = i;
				}
			}
		}
		
		//Return the total saved
		return save;
	}
}
