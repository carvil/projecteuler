o = open("triangle.hs","a")
i = open("triangle.txt")
for line in open("triangle.txt"):
   line = line.replace(" ",",")
   line = line.strip()
   line = " [" + line + "],\n"
   o.write(line) 
o.close()

