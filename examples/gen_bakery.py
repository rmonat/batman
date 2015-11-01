n = 6
batman = True

choosing = ["choosing" + str(x) for x in xrange(0, n)]
num = ["num" + str(x) for x in xrange(0, n)]

vardecl = "var " + ''.join([(x + ":bool, ") for x in choosing]) + ''.join([(x + ":int, ") for x in num]) + "x:int;\n"
init = "initial " + ''.join([x + " == false and " for x in choosing]) + ''.join([x + " == 0 and " for x in num]) + "x == 0;\n\n"

print vardecl
print init

for i in xrange(0, n):
    thread = "thread t%d:\nbegin\n" % i
    thread += "\tchoosing%d = true;\n" % i
    thread += "\tnum%d = 1 + %s;\n" % (i, ' + '.join(num))
    thread += "\tchoosing%d = false;\n\n" % i

    for j in xrange(0, n):
        if (j != i):
            thread += "\twhile (choosing%d) do skip; done;\n" % j
            thread += "%s\tif (num%d > 0 and (num%d < num%d or (num%d == num%d and %d < %d))) then\n" % ("@" if batman else "", j, j, i, j, i, j, i)
            thread += "\t\twhile (num%d > 0) do skip; done;\n" % j
            thread += "\tendif;\n\n"

    thread += "\tx = %d;\n" % (2*i+1)
    thread += "\tx = x + 1;\n\n"

    thread += "\tnum%d = 0;\n" % i
    thread += "end\n\n"
    print thread
    

    
    
