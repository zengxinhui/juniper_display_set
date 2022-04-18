import re
import sys

# /\*.*?\*/ matches annotate
# #[^\n]+\n matches ignored comment
# [^\n;{}]+ matches any line
# ;|\{|} matches ; or { or }

def process(data):
    lconfig = []
    lannotations = []
    annotation = ''
    ltags = []
    temp = ''

    for elem in re.findall(r"/\*.*?\*/|#[^\n]+\n|[^\n;{}]+|;|\{|}", data):
        elem = elem.strip()

        if elem == '' or elem[0] == '#':
            continue
        elif elem[0:2] == '/*':
            lannotations.append("top")
            lannotations.append("edit " + " ".join(ltags))
            annotation = '"' + elem[3:-3] + '"'
        elif elem[0] == '{':
            ltags.append(temp)
            temp = ''
        elif elem[0] == '}':
            ltags.pop()
        elif elem[0] == ';':
            lconfig.append("set "        + " ".join(ltags) + " " + temp)
            temp = ''
        elif elem[0:9] == 'inactive:':
            lconfig.append("deactivate " + " ".join(ltags) + " " + elem[10:])
            temp = elem[10:]
        elif elem[0:8] == 'protect:':
            lconfig.append("protect "    + " ".join(ltags) + " " + elem[9:])
            temp = elem[9:]
        else:
            if annotation:
                lannotations.append("annotate " + elem + " " + annotation)
                annotation = ''
            if temp:
                temp = temp + " " + elem
            else:
                temp = elem

    for a in lconfig:
        print(a)
    for a in lannotations:
        print(a)

if len(sys.argv) != 2:
    print("Usage: python conv.py <filename>")
else:
    process(open(sys.argv[1], 'r').read()
