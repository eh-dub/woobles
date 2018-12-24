from subprocess import call
# call(["ls", "-l"])

for x in range(1, 13):
    call(["convert", "-delay", "1x1000", "./out/latest/"+ str(x) +"/*.png", "./out/_out" + str(x) + ".gif"])