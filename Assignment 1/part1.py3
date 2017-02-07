
debuging = True


def debug(s):
    if debuging is True:
        print(s)

def debugz():
    if(debuging is True):
        print("\n")


def maketable(s1, s2):
    d = dict()
    for i1, i2 in zip(s1, s2):
        d[i1] = i2

    return d


def trans(ttable, s):
    string = ""
    for x in s:
        q = ttable.get(x, x)
    string += q

    return string


def histo(string):
    d = {}
    for letter in string:
        if letter not in d.keys():
            d[letter] = 0
        d[letter] +=1

    debug(d)
    sortedhisto = sorted(d.items(), key=lambda kv: (-kv[1], kv[0]), reverse = False)	


    return sortedhisto




def testhisto():
    s1 = "abcdefg"
    s2 = "aabbccddeeffgg"
    s3 = "ggfxaba"
    s4 = "Define a function testhisto() that tests your histo function, returning True if the code passes your tests, and False if the tests fail. Hint: By now you should be familiar with using dictionaries and should be able to write a function that builds the histogram in a single pass over the input string."

    debug(histo(s1))	
    debug(histo(s2))
    debug(histo(s3))
    debug(histo(s4))

def digraphs(s):
    dig = {}
    for x in range(len(s) -1):
        currentdig = '/%s%s/' % (s[x], s[x+1])
        #currentdig = s[x] + s[x+1]
        if currentdig not in dig.keys():
            dig[currentdig] = 0
        dig[currentdig] += 1

    sortedDig = sorted(dig.items(), key=lambda kv: (kv[1], kv[0]))
    return sortedDig





debug(digraphs("A digraph is a pair of characters that occur adjacent to one another in a text"))