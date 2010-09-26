package org.dev.Graphs;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class Main {

	public static void main(String[] args) throws IOException {
		
		File f = new File(args[0]);
		BufferedReader br = new BufferedReader( new FileReader(f));
		int[][] m = null;
		int size, line = 0;
		boolean b = true;
		String strLine = "";
		String[] weights = null;
		
		while( (strLine = br.readLine()) != null) {
			
			//replace - for zeros
			strLine = strLine.replaceAll("-", "0");
			//split teh string whenever there is a comma
			weights = strLine.split(",");
			
			//If it is the first time, initialize the matrix
			if(b) {
				size = weights.length;
				m = new int[size][size];
				b = false;
			}
			
			//parse the integers into the matrix
			for(int i = 0; i < weights.length; i++) {
				m[line][i] = Integer.parseInt(weights[i]);
			}
			line++;
		}
		
		//Create an instance of a graph
		Graph g = new Graph(m);
		//Create an instance of Prim
		Prim p = new Prim(g,0);
		//Execute the prime algorithm, which in my case
		//returns the total saved
		int res = p.prim();
		
		System.out.println(res);
	}

}
