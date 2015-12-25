import sys

if __name__ == "__main__":
    try:
        n = int(sys.argv[1])
    except IndexError:
        n = 2
        
    batman = True

    choosing = ["choosing" + str(x) for x in xrange(0, n)]
    num = ["num" + str(x) for x in xrange(0, n)]

    vardecl = "var " + ''.join([(x + ":bool, ") for x in choosing]) + ''.join([(x + ":int, ") for x in num]) + "x:int;\n"
    init = "initial " + ''.join([x + " == false and " for x in choosing]) + ''.join([x + " == 0 and " for x in num]) + "x == 0;\n\n"

    print vardecl
    print init

    for i in xrange(0, n):
        thread = "thread t%d:\nbegin\n" % i
        thread += "\twhile (true) do\n"
        thread += "\t\tchoosing%d = true;\n" % i
        thread += "\t\tnum%d = 1 + %s;\n" % (i, ' + '.join(num))
        thread += "\t\tchoosing%d = false;\n\n" % i

        for j in xrange(0, n):
            if (j != i):
                #            thread += "\t\twhile (choosing%d) do skip; done;\n" % j
                thread += "\t\tassume(not (choosing%d));\n" % j            
                #            thread += "%s\t\tif (num%d > 0 and (num%d < num%d or (num%d == num%d and %d < %d))) then\n" % ("@" if batman and ((j == n-1) or (i == n-1 and j == n-2)) else "", j, j, i, j, i, j, i)
                thread += "%s\t\tassume(not (num%d > 0 and (num%d < num%d or (num%d == num%d and %d < %d))));\n" % ("@" if batman and ((j == n-1) or (i == n-1 and j == n-2)) else "", j, j, i, j, i, j, i)
                #            thread += "\t\t\twhile (num%d > 0) do skip; done;\n" % j
                #            thread += "\t\t\tassume(not (num%d > 0));\n" % j
                #            thread += "\t\tendif;\n\n"

        thread += "\t\tx = %d;\n" % (2*i+1)
        thread += "\t\tx = x + 1;\n\n"

        thread += "\t\tnum%d = 0;\n" % i
        thread += "\tdone;\n"
        thread += "end\n\n"
        print thread
    

    
    
