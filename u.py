#!/usr/bin/python
# -*- coding: utf-8 -*-

# Welcome to ⊔ [SquareCup]! See https://github/pelinquin/u
#-----------------------------------------------------------------------------
#  © Copyright 2011 Rockwell Collins, Inc 
#    This file is part of ⊔ [SquareCup].
#
#    ⊔ [SquareCup] is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    ⊔ [SquareCup] is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with ⊔ [SquareCup].  If not, see <http://www.gnu.org/licenses/>.
#-----------------------------------------------------------------------------

# The following code pass pychecker 
# Warning ! There is a small bug in Emacs editor default font: switch ⊔ 'squarecup' (2293) and ⊓ 'squarecap' (2294) char !   

r""" Model Based Engineering widelly use two dimensions graph-based diagrams. Because these diagrams represent viewpoints of a system, including dataflow, workflow and software architecture, they are the building blocks artifacts for specification, modeling and simulation activities. In particular code generation from this high level representation is mandatory.
The data format for these diagrams may be graphical; bitmap or vectorial, unfortunatelly mixing rendering/layout data with semantics.
\textsc{xml} based formats are also used like \textsc{xmi} for \textsc{mof/uml}. However, those formats suffering of several drawbacks like unreadability, unsusefull verbosity and not well adapted structure for representing graphs. \textsc{hutl} and \textsc{json} are not used.
The \emph{Graphviz} \textsc{dot} language or the simple \textsc{yuml} syntax have nice features but lacks to provide native typing and nesting. Our proposal in this paper is a typed graph dedicated language called \usgl{}, provinding the very minimal syntax for graphic and code generation. The language is mainly defined by one given Regular Expression. We present a non formal interpretation of the language, with examples from various models like \textsc{uml}, \emph{SysML}, \emph{Marte}, \textsc{aadl}, \emph{Xcos}, \emph{Kaos}, Entity-Relation Graph, Tree Diagram, Network graph, Flowchart, Petri-Net, State Machine, Markov Chain, Behavior Tree,...
This language is a universal representation for input of multi-model code generator tools. We provide as a proof of principe some simple example of code generation for C, Ada, Python, Java, Ocaml, Ruby and Scala Coding Languages. Generation can produce textual representation for \textsc{aadl},\textsc{sdl} and Lustre (Scade) and rely on their own chain to generate code.
Graphic generation is also supported for Tikz to documents and \textsc{svg} for web viewer/editor. 
Actually, we are introducing the concept of \emph{differential dual editing}; where textual and graphical editing are well supported by our language. All source code for a prototype parser and code generators, document generator in \pyt{} is attached to this \textsc{pdf} file.
"""
__author__  = 'Laurent Fournier'
__email__   = 'lfournie@rockwellcollins.com'
__title__   = 'The Universal Short Graph Language'
__version__ = '0.1a'
__license__ = 'GPLv3'

import os,sys,re,hashlib,shutil,subprocess,urllib
#import unicodedata

__RE_U__ = r'''                # RegExp with 10 groups
   (?:                                # Token is NODE:
    (?=(?:[^\W\d_]|:[^\W\d_]|[\"\'])) #  check not empty 
    (?:([^\W\d_]\w*)|)                #  Name      G1
    (?:[\"\']([^\"]*)[\"\'])?         #  Label     G2
    (?:\.(\w+))?                      #  Port      G3
    (?::([^\W\d_]\w*)|)               #  Type      G4
    (?:\(([^\)]*)\))?                 #  Arguments G5
   )|(?:                              # Or EDGE:
    ([\-<>])                          #  Head      G6
    (?:\"([^\"]*)\")?                 #  Label     G7
    (?:([^\W\d_]\w*)|)                #  Type      G8
    #([^\W\d_a-zA-Z])?                #  Type      G8
    (?:\(([^\)]*)\))?                 #  Arguments G9
    ([\-<>])                          #  Tail      G10
   )
'''

__RE_FILTER__ = [(r'(?m)\#.*$',''),            # remove comments
                 (r'\s*$',''),                 # right strip 
                 (r'^\s*',''),                 # left strip
                 (r'\s*[\}\]]\s*', '\'], \''), # add quote and open bracket
                 (r'\s*[\{\[]\s*', '\', [\''), # add quote and closing bracket
                 (r'^(.*)$', '[\'\\1\']'),     # add start and end line brackets
                 (r'\'\',\s*',''),             # remove left empty elements
                 (r',\s*\'\'','')]             # remove right empty elements

_SVGNS    = 'xmlns="http://www.w3.org/2000/svg"'
_XLINKNS  = 'xmlns:xlink="http://www.w3.org/1999/xlink"'

__OUT_LANG__ = {'c'     :'c',
                'python':'py',
                'ada'   :'adb',
                'scala' :'scl',
                'java'  :'java',
                'ruby'  :'rb',
                'ocaml' :'ml',
                'lua'   :'lua',
                'tikz'  :'tex',
                'svg'   :'svg',
                'aadl'  :'adl',
                'sdl'   :'sdl',
                'lustre':'lst'}

__IN_MODEL__ = ['UML','SysML','AADL-Graph','Marte','Xcos','Kaos','Entity-Relation-Graph','Tree-Diagram',
                'Network-Graph','Flowchart','Petri-net','State-Machine','Markov-Chain','Behavior-Tree'] 

__CODE_GEN_SET__ = {
    ('Simple','#simple dot diagram\nA->B'):
        ({'A': (), 'B': ()}, [('A', '->', 'B')]),
    #('Class','A"my_classA":class -I> B"my_classB":class'):    
    #    ({'A': ('my_classA', 'class'), 'B': ('my_classB', 'class')}, [('A', '->', 'B', None, 'I')]),
    #('unicode',u'A您'):    
    #    ({'A您': ()}, []),
    #('Class',u'您 A'):    
    #    ({u'您': (), 'A': ()}, []),
    }

__AST_SET__ = [
    'A',
    '"L"',
    ':T',
    '{a}',
    'A"L"',
    'A:T',
    'A{a}',
    '"L":T',
    '"L"{a}',
    ':T{a}',
    'A"L":T',
    'A"L"{a}',
    '"L":T{a}',
    'A:T{a}',
    'A"L":T{}',
    'A"L":T{}(arg)',
    'A(arg1,arg2)',
    '"L"(arg)',
    ':T(arg)',
    'Aaa',
    'A1',
    ' A',
    'A ',
    ' A ',
    '\n\nA\n\n',
    'éàùç',
    '您您好',
    '"This is a long label" ',
    '"Label with \"escape\" substring" ',
    '"Multi\nlines\nlabel" ',
    #' """This is "a" \'very\' \nlong label""" ',
    #' \'Simple quote\' ',
    'A:Type1 B:Type2',
    'A B',
    'A B C',
    'A B C D',
    'A"label1" A"label2"',
    '"label":T1 "label":T2',
    'A"label" B"label"',
    'A{a} B{b1 b2} C{c1 c2 c3}',
    'A{a} B {b} {c}',
    'A{a} B[b]',
    ':T1{a b} :T2{c d}',
    'A{ B{c b} C{e f} }',
    'A{B{C{c}}}',
    '"x-y"',
    '"x+y"',
    '"x*y"',
    '"x/y"',
    '"x.y"',
    '"x,y"',
    '"x%y"',
    '"x^y"',
    '"x=y"',
    '"x:y"',
    '"x&y"',
    '"x|y"',
    '"x>y"',
    '"x<y"',
    '#comment\nA',
    ' #comment1\nA\n #comment2',
    'A->B',
    'A->B->C',
    'A--B', 
    'C->D',
    'E>-F',
    'G>>H',
    'I<>J',
    'K<-L',
    'M-<N',
    'O<<P', 
    'Q><R',
    'A ->B',
    'A-> B',
    'A -> B',
    'A->A',
    'A -"label"- B',
    'A -Type- B',
    'A -(arg)- B',
    'A -"label"Type- B',
    'A -Type(arg)- B',
    'A -"label"(arg)- B',
    'A -"label"Type(arg)- B',
    'A -(arg1,arg2,arg3)- B',
    'A{a1 a2} -> B{b1 b2}',
    '{a1 a2} -> B{b1 b2}',
    'A{a1 a2} -> {b1 b2}',
    '{a1 a2} -> {b1 b2}',
    'A{a1 -> a2} B{b1 -> b2}',
    'A{a1 -> a2} -> B{b1 -> b2}',
    'A.1 -> B.2',
    'A.por1 -> B.por2',
    'A:T1 B:T2 A.1->B.2',
    'A.1->B.2 A:T1 B:T2']
    
def find_id(x):
    "find Node id "
    if x[0]: # name
        return x[0]
    else:
        if x[1]: # label
            return re.sub(r'\W','',x[1])
        else: # type
            return '__%s'%x[3]

class u:
    """ This is the base class for ⊔ 
    One can customize that class by adding/modifying the mapping structure (self.m)
    or by overloading a gen_x() method
    """

    def __init__(self):
        "define type mapping"
        self.m = {}
        for l in __OUT_LANG__:
            self.m[l] = ({'':[]},{'':[]})
            self.m['svg'] = ({'':('fill:black;','filter:url(#.shadow);fill-opacity:.1;',4,4,('p1','p2')),
                              'T':('fill:red;','fill:blue;fill-opacity:.6;',8,18,('p1','p2','p3','p4')),
                              'O':('fill:blue;','filter:url(#.shadow);fill-opacity:.1;',30,30,('in1','in2','out1','out2')),
                              'C':('fill:blue;','filter:url(#.shadow);fill-opacity:.1;',
                                   30,60,('pin1','pin2','pin3','pin4','pin5','pin6','pin7','pin8',
                                          'pin9','pin10','pin11','pin12','pin13','pin14','pin15','pin16')),
                              'D':('fill:blue;','filter:url(#.shadow);fill-opacity:.1;',
                                   20,40,('pin1','pin2','pin3','pin4','pin5','pin6'))},
                             {'' : 'stroke:black; stroke-width:1; fill:none; marker-end:url(#.arrow);',
                              'I': 'stroke:green; stroke-width:2; fill:none; marker-end:url(#.arrow);',
                              'L': 'stroke:red; stroke-width:3; fill:none; marker-end:url(#.arrow);'})
            self.m['tikz'] = ({'':('rectangle,draw=black!40,fill=gray!10',['p1','p2']),
                               'T':('circle,drop shadow,draw=green!40,fill=gray!20',['in1','in2','out1','out2']), 
                               'O':('rectangle,drop shadow,rounded corners=3pt,draw=red!40,fill=blue!25',[])
                               },
                              {'':'->,>=latex',
                               'I':'->,>=open diamond',
                               'L':'->,>=triangle 60'
                               })
            self.m['c'] = ({'':[]},{'':[]})
            
    def parse1(self,x):
        r""" test of doctest !
        parse('A"my_classA":class -类> B"my_classB":class')    # does not support well unicode!
        ({'A': ('my_classA', 'class'), 'B': ('my_classB', 'class')}, [('A', '->', 'B', None, '类')])
        parse('A -> B')    
        ({'A': (), 'B': ()}, [('A', '->', 'B')])
        """
        Nodes,Edges,kids,nid,oid,npo,opo,nid,moid,c = {},[],{},None,None,'','',[],[],[]
        if type(x).__name__ == 'str': 
            x = eval(reduce(lambda y,k: re.sub(k[0],k[1],y),__RE_FILTER__,x))
        for s in x:
            if type(s).__name__ == 'list':
                n,k,e = self.parse1(s)
                mnid = k.keys()
                if nid:
                    mnid = [nid]
                if moid and c:
                    for i in moid:
                        for j in mnid:
                            Edges.append(strip3((i,c[0]+c[4],j,c[1],c[2],c[3])))
                    c = []
                moid = mnid
                if oid and Nodes.has_key(oid):
                    t = list(Nodes[oid])
                    t.append(k.keys())
                    Nodes[oid] = tuple(t)
                oid = nid = None
                Nodes.update(n)
                Edges += e
            else:
                nodes = {}
                for m in re.compile(__RE_U__,re.U|re.X).finditer(s):
                    a = map(lambda k:m.group(k),range(1,11))
                    if a[5] and a[9]: # this is an edge
                        c = a[5:10]
                    else: # this is a node
                        nid,npo = find_id(a[:5]),'.%s'%a[2] if a[2] else ''
                        if not nodes.has_key(nid):
                            nodes[nid] = strip3(tuple(a[1:2]+a[3:5]))
                        if c and oid and nid:
                            Edges.append(strip3((oid+opo,c[0]+c[4],nid+npo,c[1],c[2],c[3])))
                            oid,opo,c = None,'',[]
                        oid,opo = nid,npo
                Nodes.update(nodes)
                kids.update(nodes)
        return (Nodes,kids,Edges)

    def parse(self,x):
        "Use two functions for return type consistency"
        n,k,e = self.parse1(x)
        return n,e
        
    def gen_c(self,ast):
        "/* Generated from ⊔ AST: */\n"
        o,m = '/* %s */\n/* %s */\n'%ast,self.m['c']
        o += '/* Types parameters: %s %s */'%m
        Nodes,Edges = ast
        for x in Nodes:
            if Nodes[x]:
                if len(Nodes[x]) == 2 and Nodes[x][1] == 'class':
                    o += '\n/* Class: %s */\n'%Nodes[x][0]
                    o += 'typedef struct %s {\n'%x
                    o += '  int a;\n'
                    o += '} %s;\n'%x
        return self.gen_c.__doc__ + o + '\n/* %s Nodes %s Edges */\n'%(len(Nodes),len(Edges))

    def gen_python(self,ast):
        """#!/usr/bin/python\n# -*- coding: utf-8 -*-\n# Generated from ⊔ AST:\n"""
        o,m = '# %s\n# %s\n'%ast,self.m['python']
        o += '# Types parameters: %s %s\n'%m
        return self.gen_python.__doc__ + o + '\n# end file\n'

    def gen_ada(self,ast):
        "-- Generated from ⊔ AST:\n"
        o,m = '-- %s\n-- %s\n\n'%ast,self.m['ada']
        o += '-- Types parameters: %s %s\n'%m
        o += 'with Ada.Text_IO;\n\n'
        o += 'procedure Hello is\nbegin\n\tAda.Text_IO.Put_Line("Hi!");\nend Hello;\n\n'
        return self.gen_ada.__doc__ + o + '\n-- end file\n'

    def gen_scala(self,ast):
        "// Generated from ⊔ AST:\n"
        o,m = '// %s\n// %s\n'%ast,self.m['scala']
        o += '// Types parameters: %s %s\n'%m
        return self.gen_scala.__doc__ + o + '\n// end file\n'

    def gen_java(self,ast):
        "// Generated from ⊔ AST:\n"
        o,m = '// %s\n// %s\n'%ast,self.m['java']
        o += '// Types parameters: %s %s\n'%m
        return self.gen_java.__doc__ + o + '\n// end file\n'

    def gen_ruby(self,ast):
        "# Generated from ⊔ AST:\n"
        o,m = '# %s\n# %s\n'%ast,self.m['ruby']
        o += '# Types parameters: %s %s\n'%m
        return self.gen_ruby.__doc__ + o + '\n# end file\n'

    def gen_ocaml(self,ast):
        "(* Generated from ⊔ AST: *)\n"
        o,m = '(* %s *)\n(* %s *)\n'%ast,self.m['ocaml']
        o += '(* Types parameters: %s %s)*\n'%m
        return self.gen_ocaml.__doc__ + o + '\n(* end file *)\n'

    def gen_lua(self,ast):
        "-- Generated from ⊔ AST:\n"
        o,m = '-- %s\n-- %s\n'%ast,self.m['lua']
        o += '-- Types parameters: %s %s\n'%m
        return self.gen_lua.__doc__ + o + '\n-- end file\n'

    def gen_tikz(self,ast,standalone=True):
        "% Generated from ⊔ AST:\n"
        o,m = '',self.m['tikz']
        if standalone:
            o += r'\documentclass[a4paper]{article} \usepackage{tikz}' + '\n'
            o += r'\begin{document}' + '\n'
        pos = layout(ast[0],ast[1])
        o += '%% %s\n%% %s\n'%ast 
        Nodes,Edges = ast 
        m = self.m['tikz']
        o += gen_tikz_header(m,gettypes(ast)) + r'\begin{tikzpicture}[auto,node distance=15mm,semithick]'+ '\n'
        for n in pos:
            #name = n.encode('utf-8')
            name = n
            label = name 
            shape = 'node_' if (len(Nodes[n])<2 or not Nodes.has_key(n)) else 'node_%s'%Nodes[n][1]
            tt = m[0][''][1] if (len(Nodes[n])<2 or not Nodes.has_key(n) or not m[0].has_key(Nodes[n][1])) else m[0][Nodes[n][1]][1]
            (x,y) = (pos[n][0]/25,pos[n][1]/25)
            port_layout = [['west'],['west','east'],[192,12,-12],[168,192,12,-12],['cool']]
            o += r'\node[%s](%s) at (%0.3f,%0.3f) {%s};'%(shape,name,x,y,label) + '\n'
            if tt and len(tt) < 5:
                rep = port_layout[len(tt)-1]
                for i in range(len(rep)):
                    o += r'\draw (%s.%s) node{\tiny{%s}};'%(n,rep[i],tt[i]) + '\n'
        for e in Edges:
            boucle = '[bend left]'
            typ = 'edge_' if len(e)<5 else 'edge_%s'%e[4]
            label = '' if len(e)<4 else 'node{%s}'%e[3] 
            o += r'\draw[%s](%s) to%s %s(%s);'%(typ,e[0],boucle,label,e[2]) + '\n'
        o += r'\end{tikzpicture}'+ '\n'
        if standalone:
            o +=  r'\end{document}'
        return self.gen_tikz.__doc__  + o + '\n% end file\n'

    def gen_svg(self,ast):
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!-- Generated from ⊔ AST: -->\n"
        m = self.m['svg']
        pos,ratio = layout(ast[0],ast[1],'LR'),4
        Nodes,Edges = ast
        o = '<!-- doubledash replaced by double underscore\n' + re.sub(r'\-\-','__','%s\n%s'%ast) + '\n-->\n' 
        o += '<svg %s>\n'%_SVGNS
        o += gen_svg_header(m,gettypes(ast))
        o += '<title id=".title">⊔: %s</title>\n'%__title__
        #o += '<link %s rel="shortcut icon" href="./logo16.png"/>\n'%(_XHTMLNS,pfx)
        #o += '<script %s type="text/ecmascript" xlink:href="%s/%s"></script>\n'%(_XLINKNS,pfx,__JS__)
        o += '<!-- This requires some Javascript because Text bounding-box computation is only available client-side! -->\n'
        o += '<!-- Any way to have bounding-box computation server side would help me so I can remove Javascript! -->\n'
        o += include_js()
        o += '<g id=".nodes">\n'
        for n in pos:
            #label = n.encode('utf-8')
            label = n
            if ast[0][n]:
                if ast[0][n][0]:
                    #label = ast[0][n][0].encode('utf-8')
                    label = ast[0][n][0]
            style = 'node_' if (len(Nodes[n])<2 or not Nodes.has_key(n)) else 'node_%s'%Nodes[n][1]
            t = '' if not (Nodes.has_key(n) and (len(Nodes[n])>1) and m and m[0].has_key(Nodes[n][1])) else Nodes[n][1]
            o += '<g id="%s" class="%s" mx="%s" my="%s">'%(n,style,m[0][t][2],m[0][t][3])
            o += '<rect rx="5"/><text x="%s" y="%s">%s</text><g>'%(pos[n][0]*ratio,pos[n][1]*ratio,label)
            ports = m[0][t][4]
            delta = 200.0/len(ports)
            d = delta/2.0 - 100
            for p in ports:
                o += '<rect class="port" width="6" height="6" pos="%s"/><text class="tiny">%s</text>'%(d,p)
                d += delta
            o += '</g></g>\n'
        o += '</g>\n<g id=".connectors" >\n'
        for e in Edges:
            typ = 'edge_' if len(e)<5 else 'edge_%s'%e[4]
            o += '<g class="%s" n1="%s" n2="%s"><path/></g>\n'%(typ,e[0],e[2])
        o += '</g>\n'
        return self.gen_svg.__doc__ + o + '\n</svg>\n<!-- end file -->\n'

    def gen_aadl(self,ast):
        "-- Generated from ⊔ AST:\n"
        o,m = '-- %s\n-- %s\n'%ast,self.m['aadl']
        o += '-- Types parameters: %s %s\n'%m
        return self.gen_aadl.__doc__ + o + '\n-- end file\n'

    def gen_sdl(self,ast):
        "# Generated from ⊔ AST:\n"
        o,m = '# %s\n# %s\n'%ast,self.m['sdl']
        o += '# Types parameters: %s %s\n'%m
        return self.gen_sdl.__doc__ + o + '\n# end file\n'

    def gen_lustre(self,ast):
        "-- Generated from ⊔ AST:\n"
        o,m = '-- %s\n-- %s\n'%ast,self.m['lustre']
        o += '-- Types parameters: %s %s\n'%m
        return self.gen_lustre.__doc__ + o + '\n-- end file\n'


def gen_readme():
    """Welcome to the ⊔ [SquareCup] Language Project !\n==========================================\n
⊔ is a proposal for a 'Universal Short Graph Language'\n\n
This is a 'just one file' [Open-source](https://github.com/pelinquin/u/blob/master/COPYING) project, easy to use, easy to share!\n\n
All is included or generated from the [u.py](https://github.com/pelinquin/u/blob/master/u.py) file.\n\n
Launch that Python file to pass tests and to generate formated documentation.\n\n
You can use it as a Python module and overload functions.\n\n
Or use it as a Web service...for [instance](https://193.84.73.209/u?about).  
For your convenience, the [u.pdf](https://github.com/pelinquin/u/blob/master/u.pdf?raw=true) file is also commited.\n\nEnjoy!
"""
    digest = hashlib.sha1(open(__file__).read()).hexdigest()
    o = '[u.py](https://github.com/pelinquin/u/blob/master/u.py) SHA1 digest start with %s...\n\n'%digest[:5]
    open('README.md','w').write(o + gen_readme.__doc__)

def gen_apache_conf():
    """# Apache config file in WSGI mod
# This file is generated. Do not edit by hands!
# Place this file in '/etc/apache2/conf.d' directory
# and restart Apache: '/etc/init.d/apache2 restart'"""
    prg,path = os.path.basename(sys.argv[0])[:-3],os.path.abspath(sys.argv[0])
    open('%s.conf'%prg,'w').write('%s\n\nWSGIScriptAlias /%s %s\n'%(gen_apache_conf.__doc__,prg,path))

def tex_section():
    r"""\section{Parsing}
   Comments (\texttt{\#...} ) are first removed from \usgl{} code before parsing.
The Parser resulting structure is not exactly the regular expression group. Nodes and egdes are tuple: $$ \begin{array}{cc} Node:& (Name,Type,Label,Arguments,Children) \\ Edge:& (Arrow,Type,Label,Arguments,sourceNodes,sourcePorts,destNodes,destPorts)\end{array}$$ The parser replaces the raw block ($G6$) by the references of inner nodes. $G2$ is not stored in the node but in the edge using it. ($G5$) and ($G10$) arguments strings are precomputed as arrays. For edges, $G7$ and $G11$ are concatenated."""
    o = tex_section.__doc__
    o += insert_code(r'__RE_U') 
    return o

def tex_header():
    r"""%% This file is generated. Do not edit by hands!
    \documentclass[a4paper,10pt]{article}
    \usepackage[margin=2cm]{geometry}
    \usepackage[utf8]{inputenc}
    \usepackage{url}
    \usepackage{draftwatermark}
    \usepackage{listings}
    \usepackage{lmodern}
    \usepackage{color}
    \usepackage{lettrine}
    \usepackage{embedfile}
    \usepackage{graphicx}
    \usepackage{tikz}
    \usepackage{longtable}
    \usepackage{array}
    \newcommand{\pyt}{\emph{Python}}
    \newcommand{\pdf}{\textsc{pdf}}
    \newcommand{\usgl}{\textsc{usgl}}
    \begin{document}
    \lstset{language=Python,breaklines=true}
    """
    o = tex_header.__doc__
    o += r'\title{\bf $\sqcup:$ %s} \author{%s -- \url{%s} \\ \tiny{version: %s}} \maketitle'%(__title__,__author__,__email__,__version__)
    o += r'\embedfile[filespec=%s]{%s}'%(os.path.basename(sys.argv[0]),os.path.abspath(sys.argv[0]))
    return o + '\n'

def biblio():
    r"""\bibitem{lamport} Leslie Lamport \emph{\LaTeX: A Document Preparation System}. Addison Wesley, Massachusetts, 2nd Edition, 1994.
    \bibitem{tikz} XXX \emph{TikZ}.
    \bibitem{xmi} OMG \emph{XMI}.
    \bibitem{xmi} OMG \emph{MOF}.
    \bibitem{uml} G. Booch, J. Rumbaugh, and I. Jacobson \emph{The Unified Modelling Language User Guide}. Addison-Wesley,1999.
    \bibitem{hutn} OMG \emph{Human Usable Textual Notation}. 2005.
    \bibitem{dot} Emden R. Gansner. \emph{The DOT language}. 2002.
    \bibitem{yuml} Tobin Harris \emph{yUML} yuml.me 2002.
    \bibitem{svg} XXX \emph{SVG}.
    \bibitem{tgf} XXX \emph{TGF}.
    \bibitem{gml} XXX \emph{graphML}.
    \bibitem{gxl} XXX \emph{GXL}.
    \bibitem{aadl} XXX \emph{AADL}.
    \bibitem{lustre} XXX \emph{Lustre}.
    \bibitem{sdl} XXX \emph{SDL}.
"""
    return r'\begin{thebibliography}{99}' + biblio.__doc__ + r'\end{thebibliography}'

def insert_code(pat):
    " ....in LaTeX "
    o,d = r'\lstset{basicstyle=\ttfamily ,numbers=left, numberstyle=\tiny, stepnumber=5, numbersep=5pt}',False
    o += r'\begin{lstlisting}[texcl]' + '\n'
    for l in open(__file__).readlines():
        if re.match(r'(if|def|class|__)',l): d = False
        if re.match(pat,l): d = True
        if d: o += l
    return o + r'\end{lstlisting}' + '\n'

def gen_doc(): 
    r"""\section{A Short Language for Graphs}
    A graph is represented as a couple $(V,E)$ of arrays of Nodes/Vertices and Edges/Arcs. Each edge references source node(s) and destination node(s). More exactly the connexion point of a node is a \emph{port}). Both edges and nodes are objects having an attribute list defined in a type (a class). A node can be nested, thus including another graphs. The curly brackets are delimiters for nesting.
A particular attribute is named \emph{label} and delimited with quotes (simple, double or string of three double quotes for multilines labels). This label is a free string displayed on the node/edge. \\

For a given edge of type $T$, there is nine differents links represented as:

\begin{tabular}{lc}
 undirected& \texttt{-T-} \\
 simple right& \texttt{-T>} \\
 simple left& \texttt{<T-} \\
 bidirectional& \texttt{<T>} \\
 full right& \texttt{>T>} \\
 full left& \texttt{<T<} \\
 opposite& \texttt{>T<} \\
 source right& \texttt{-T<} \\
 source left& \texttt{>T-} \\
\end{tabular} \\

Nodes have a name identifier that is a regular word of the \emph{unicode} encoding. There is no need to give an identifier for edges since they are defined by a couple of (source,destination) nodes ports.
Naming nodes allows to reference them easilly all node links without repeating all the nodes attributes.
The edge/node type can also be named or implicit and it can support some constructor parameters inside parentheses and comma separator. Edge type name can be just one unicode character long to make the link not too long between the head and the tail using characters $[-<>]$.
It is also a good practice to use foreign languages characters to reference types so they can be distinguished from node's name (instances). 
Nodes have ports so edges can reference them directly. This is important for nodes types where the connected point of the node and edge has semantic meaning, because these ports are named or because ports position is important (top, bottom, right,left).\\
The \textsc{usgl} language does not include any geometric properties for the graph layout. The shape/color of each nodes/edge type is defined elsewhere. Also the graph layout may be defined by set of algorithms for improving rendering and limiting edge crossing or set manually but all geometric data is never usefull for our language. 
We argue that our small language is complete enought for Model to Code generation.  
\section{Ready for code generation}
To generate code from an \usgl{} intance one must first define a semantic mapping between the node/edge type features and coding constructions. This mapping can be based on graphical patterns related to code patterns.
For instance, we must state that the \texttt{-I>} relation represents inheritance for a \textsc{uml} class diagram.
\section{Type checking}
Introduce here how to constraint nodes type with edge type...Do we need an OCL like language for that? Can we propose a language for pre/post conditions and invariant checking? 
\section{Differential dual editing}
The basic idea is that the editors tools enable both graphical and textual editing at the very same time with minimum toogleling between the graphic and the textual mode. Graphic updates are made by difference. Exactly as a text editor insert ou remove a character in a text flow without processing all the document, the corresponding modified graphic nodes or edges are updated without processing all other nodes, edges. A background task may compute the best layout for the graph at a some change level. Because \usgl{} has a very simple syntax close to the graphic, all atomic operation has a direct equivalence in text mode or in graphic mode.
There is no need to store or share the image dump of the graph except for printing. The \usgl{} file can be shared between modelers, editor tools and code generators and remaining short, human readable and meaningfull for rendering a graph.
\\
    """
    o = tex_header() + r'\begin{abstract}' + __doc__ + r'\end{abstract}' + '\n' + gen_doc.__doc__
    o += tex_section()
    o += r'\section{The Test Set}' + '\n' + insert_data(__CODE_GEN_SET__) 
    o += biblio() + '\n'
    o += r'\begin{flushright}\emph{The end of the document}\end{flushright}' +'\n' + r'\end{document}'+'\n' 
    src = os.path.basename(sys.argv[0])
    open(src[:-2] + 'tex','w').write(o)
    gen_pdf(src[:-2])

def gen_pdf(src):
    tex = src + 'tex'
    if subprocess.Popen(('which','pdflatex'),stdout=subprocess.PIPE).communicate()[0]:
        subprocess.Popen(('cd /tmp; pdflatex -interaction=batchmode %s 1>/dev/null'%os.path.abspath(tex)), shell=True).communicate()
        shutil.move('/tmp/%spdf'%src,'%spdf'%src) 
    else:
        sys.stderr.write('pdflatex not installed!\n')

def tex2pdf(o):
    src = 'tikzfile' # better use tmpfile module
    open('/tmp/%s.tex'%src,'w').write(o)
    subprocess.Popen(('cd /tmp; pdflatex -interaction=batchmode %s.tex 1>/dev/null'%src), shell=True).communicate()
    return open('/tmp/%s.pdf'%src).read()

def insert_data(h):
    r"""The test set is an array of cases. Cases have a name, an \usgl{} code sample and the expected \pyt{} structure as a $(V,E)$ couple of nodes and edges. The \usgl{} parser use that test set to check against expected data. From the Nodes and Edges arrays, one can either generate vector graphics (\textsc{TikZ} in this table but could also be \textsc{svg} on a web server) or generate source code. Each node and edge type define a particular node/edge shape for graphic or a particular construction in a programming language. Generating graphic code requests also a layout algorithm for placing nodes and edges. Because layout never carry graph semantics, we can run an automatic algorithm with simple lisibility criteria like balacing nodes on the page or avoiding edge crossing.    
\\ """
    o,num = insert_data.__doc__ + '\n',0
    o += r'\lstset{language=Python,breaklines=true}'+ '\n' + gen_tikz_header()
    o += r'\begin{longtable}{|l|l|p{7cm}|l|} ' + '\n' + r'\hline'+ '\n'
    o += r'\bf{Case} & \bf{Test name} & \bf{U code} & \bf{TikZ generated diagram} \\ \hline'+ '\n'
    for l in h:
        num += 1
        o += r'\vspace{0pt}$%02d$ &'%num + '\n'
        o += r'%s'%re.sub('_','\_',l[0]) + '& \n'
        #o += r'\begin{lstlisting}[texcl] ' + '\n' + l[1].encode('utf-8') + '\n' + r'\end{lstlisting}' + ' & \n'
        o += r'\begin{lstlisting}[texcl] ' + '\n' + l[1] + '\n' + r'\end{lstlisting}' + ' & \n'
        uobj = u()
        o += uobj.gen_tikz(h[l],False) + r'\\ \hline' + '\n'
    return o + r'\end{longtable}' + '\n'

def reg(value):
    " function attribute is a way to access matching group in one line test "
    reg.v = value
    return value

######### WEB APPLICATION ###########

def application(environ,start_response):
    """<title>⊔</title><style>h1,p,li,b{font-family:helvetica neue,helvetica,arial,sans-serif;} a{text-decoration:none;}</style>
<h1><a href="https://github.com/pelinquin/u" style="font-size:64pt;color:DodgerBlue;" title="SquareCup">⊔</a> Web service</h1>
<p>Any URL argument (after '?') is interpreted as a valid ⊔ string and the default output is the AST (a Python data structure).
Example: [<a href="u?A->B">/u?A->B</a>]</p>
<p>If a language name is given first, then the output is the generated code for this language. Example: [<a href="u?ada&A->B">/u?ada&A->B</a>]</p>
<p>With no other argument than a language name (no '&'), a local file browser is provided to select an input ⊔ file for upload.
Example: [<a href="u?tikz">/u?tikz</a>]</p>
<p>If the '_' character is given before the language name, then the output is the interpretation of the generated language;</p>
   <li>For <b>svg</b> the graphics is rendered within the browser. Example: [<a href="u?_svg&A->B">/u?_svg&A->B</a>]</li>
   <li>For <b>tikz</b>, the pdf reader is called for rendering the graphics. Example: [<a href="u?_tikz&A->B">/u?_tikz&A->B</a>]</li>
<p>Special keywords:</p>
   <li><i>pdf</i> or <i>paper</i> returns the generated paper on ⊔ in pdf format: [<a href="u?pdf">/u?pdf</a>]</li>
   <li><i>update</i> [<a href="u?update">/u?update</a>] is used to update the web application with the last release from 
[<a href="https://github.com/pelinquin/u">https://github.com/pelinquin/u</a>]</li>
   <li><i>help</i>,<i>about</i> or <i>usage</i> displays this page.</li>
<p>If no argument is given, the output [<a href="u">/u</a>] is the Python source code for reading or downloading.
</p><p>Supported output languages are:</p><b>"""
    s,mime,o,uobj = urllib.unquote(environ['QUERY_STRING']),'text/plain;charset=UTF-8','Error!',u()
    if reg(re.match(r'\s*(update$|about$|help$|usage$|pdf$|paper$|)(?:(_?)(%s|raw|ast)(?:&(.*)|)|(.*))\s*$'%'|'.join(__OUT_LANG__),s,re.I)):
        form,action,under,lang,args = False,reg.v.group(1),reg.v.group(2),reg.v.group(3),reg.v.group(5) if reg.v.group(5) else reg.v.group(4)
        if lang: lang = lang.lower()
        if (action,under,lang,args) == ('',None,None,None):
            start_response('200 OK',[('Content-type',mime),('Content-Disposition','filename=u.py')])
            return [(open(environ['SCRIPT_FILENAME']).read())] 
        elif action and action.lower() in ('about','help','usage'):
            mime,o = 'text/html;charset=UTF-8','<html><title>⊔ v%s</title>'%__version__ + application.__doc__ + ', '.join(__OUT_LANG__) + '</b>\n'
            o += '<p>Supported Input Modeling Formalism are:</p><b>' + ', '.join(__IN_MODEL__) + '</b></html>\n'
        elif action and action.lower() in ('paper','pdf'):
            o = open('%s/u.pdf'%os.path.dirname(environ['SCRIPT_FILENAME'])).read()
            mime = 'application/pdf'
        elif action and action.lower() == 'update':
            if environ['SERVER_NAME'] != 'pelinquin': # update not possible from RCF network
                cmd = 'cd %s/..; rm -rf u; git clone git://github.com/pelinquin/u.git; cd u'%os.path.dirname(environ['SCRIPT_FILENAME'])
            else:
                cmd = 'ls %s'%__file__
            res = subprocess.Popen((cmd), shell=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE).communicate()
            o = res[1] if res[1] else '%s server Updated!'%environ['SERVER_NAME']
        elif args == None:
            if environ['REQUEST_METHOD'].lower() == 'post':
                raw = environ['wsgi.input'].read(int(environ.get('CONTENT_LENGTH','0')))
                ast = uobj.parse('\n'.join(raw.split('\n')[4:-2]))
                if lang in ('ast','raw'):
                    o = '%s %s'%ast
                else: 
                    o = eval('uobj.gen_%s(ast)'%lang) #o = eval('uobj.gen_%s(ast)'%lang).encode('utf-8')
            else:
                mime,form,o = 'text/html',True, '<form method=post enctype=multipart/form-data><input type=file name=a onchange="submit();"/></form>'
        elif lang in (None, 'ast','raw'):
            o = '# Python ⊔ AST\n\n%s %s'%uobj.parse(args)
        else:
            ast = uobj.parse(args)
            o = eval('uobj.gen_%s(ast)'%lang) #o = eval('uobj.gen_%s(ast)'%lang).encode('utf-8')
        if (under == '_') and not form:
            if (lang == 'tikz'):
                o = tex2pdf(o)
                mime = 'application/pdf'
            elif lang == 'svg':
                mime = 'application/xhtml+xml'
            #elif lang in ('c','ada'):
            #    mime = 'application/octet-stream'
    header = [('Content-type',mime)]
    if lang:
        ext = 'u' if lang in (None, 'ast','raw') else __OUT_LANG__[lang]
        if under == '_' and lang in ('c','ada'):
            header.append(('Content-Disposition','attachment; filename=a.out'))
        else:
            header.append( ('Content-Disposition','filename=file.%s'%ext))
    start_response('200 OK',header)
    return [(o)] 

def strip3(z):
    return z[:-3] if not (z[-1] or z[-2] or z[-3]) else z[:-2] if not (z[-1] or z[-2]) else z[:-1] if not z[-1] else z
    
######### UTILITIES ###########

def layout(nodes,edges,rankdir='TB'):
    "computes layout for graphics (tikz and svg) generation"
    bbx,bby,pos,d = None,None,{},'digraph G { rankdir=%s '%rankdir
    for n in nodes:
        label = n if not n[0] else n[0] #label = n.encode('utf-8') if not n[0] else n[0]
        d+= ' %s[label="%s"];'%(n,label) #d+= ' %s[label="%s"];'%(n.encode('utf-8'),label)
    for e in edges:
        d+= ' %s->%s'%(e[0],e[2]) #d+= ' %s->%s'%(e[0].encode('utf-8'),e[2].encode('utf-8'))
    p = subprocess.Popen(['dot'], stdout=subprocess.PIPE, stdin=subprocess.PIPE)
    for l in p.communicate(input=d + '}')[0].split('\n'):
        if reg(re.search('bb="0,0,(\d+),(\d+)"',l)):
            bbx,bby = float(reg.v.group(1)),float(reg.v.group(2))
        elif reg(re.search('^\s*(\w+)\s*\[label=[^,]*, pos="(\d+),(\d+)"',l)) and bbx and bby:
            pos[reg.v.group(1)] = (int(reg.v.group(2))*100/bbx,int(reg.v.group(3))*100/bby)
    return pos

def gen_tikz_header(m=[],(ln,le)=({},{})):
    r"""\usetikzlibrary{shapes,fit,arrows,shadows,backgrounds}
"""
    #\tikzset{node_O/.style = {draw,circle,inner sep=10pt,path pp={ \draw[black] (path pp bounding box.south) -- (path pp bounding box.north) (path pp bounding box.west) -- (path pp bounding box.east);}}} 
    o = gen_tikz_header.__doc__
    if m:
        for n in m[0]:
            if ln.has_key(n):
                o += r'\tikzstyle{node_%s} = [%s]'%(n,m[0][n][0]) + '\n'
        for e in m[1]:
            if le.has_key(e):
                o += r'\tikzstyle{edge_%s} = [%s]'%(e,m[1][e]) + '\n'
    return o + '\n'

def include_js():
    r"""
if (typeof($)=='undefined') { function $(id) { return document.getElementById(id.replace(/^#/,'')); } }
var nodeBox  = [];

function nodes_path(b1,b2) {
  var x1 = b1[0].x + b1[0].width/2; var y1 = b1[0].y + b1[0].height/2;
  var x2 = b2[0].x + b2[0].width/2; var y2 = b2[0].y + b2[0].height/2;
  var h1 = 1 + b1[0].height/2 + b1[2]; var l1 = 1 + b1[0].width/2 + b1[1];
  if (x1 == x2) {
    if (y1<y2) { y1 += h1;
    } else { y1 -= h1; }
  } else if (y1 == y2) {
    if (x1<x2) { x1 += l1;
    } else { x1 -= l1; }
  } else {
    var Q = x1-x2; var R = y1-y2; var P = Q/R;
    if (Math.abs(P) < l1/h1) {
      if (R<0) { y1 += h1; x1 += h1*P;
      } else { y1 -= h1; x1 -= h1*P; }
    } else {
      if (Q<0) { x1 += l1; y1 += l1/P;
      } else { x1 -= l1; y1 -= l1/P; }
    }
  }
  var h2 = 1 + b2[0].height/2 + b2[2]; var l2 = 1 + b2[0].width/2 + b2[1];
  if (x2 == x1) {
    if (y2<y1) { y2 += h2;
    } else { y2 -= h2; }
  } else if (y2 == y1) {
    if (x2<x1) { x2 += l2;
    } else { x2 -= l2; }
  } else {
    var Q = x2-x1; var R = y2-y1; var P = Q/R;
    if (Math.abs(P) < l2/h2) {
      if (R<0) { y2 += h2; x2 += h2*P;
      } else { y2 -= h2; x2 -= h2*P; }
    } else {
      if (Q<0) { x2 += l2; y2 += l2/P;
      } else { x2 -= l2; y2 -= l2/P; }
    }
  }
  return ('M'+x2+','+y2+'L'+x1+','+y1);
}
    window.onload = function () { 
      var t = $('.nodes').childNodes;
      for (var n = 0; n < t.length; n++) {
        if (t[n].nodeName == 'g') { 
          nodeBox[t[n].id] = [t[n].firstChild.nextSibling.getBBox(),parseInt(t[n].getAttribute('mx')),parseInt(t[n].getAttribute('my'))]; 
        } 
      }
      for (var n = 0; n < t.length; n++) {
        if (t[n].nodeName == 'g') {
          var mx = nodeBox[t[n].id][1];
          var my = nodeBox[t[n].id][2];
          var b = nodeBox[t[n].id][0];
          var shape = t[n].firstChild;
          shape.setAttribute('x',b.x-mx);
          shape.setAttribute('y',b.y-my);
          shape.setAttribute('width',b.width+2*mx);
          shape.setAttribute('height',b.height+2*my);
          var ports = shape.nextSibling.nextSibling.childNodes;
          var x = 0; var y = 0; 
          var pos = 0;
          for (var i = 0; i < ports.length; i++) {
            if (ports[i].nodeName == 'rect') { 
              var pos = parseInt(ports[i].getAttribute('pos'));
              if (pos<0) { x=b.x-mx-6; y=b.y-my-3+(pos+100)*(b.height+2*my)/100;
              } else { x=b.x+b.width+mx; y=b.y-my-3+(100-pos)*(b.height+2*my)/100; }
              ports[i].setAttribute('x',x); ports[i].setAttribute('y',y);
            } 
            if (ports[i].nodeName == 'text') { 
              ports[i].setAttribute('dominant-baseline','middle');
              if (pos<0) { x=b.x-mx+1; y=b.y-my+(pos+100)*(b.height+2*my)/100;
              } else {
                ports[i].setAttribute('text-anchor','end');
                x=b.x+b.width+mx-1; y=b.y-my+(100-pos)*(b.height+2*my)/100;
              }
              ports[i].setAttribute('x',x); ports[i].setAttribute('y',y);
            } 
          }
        }
      }
      var t = $('.connectors').childNodes;
      for ( var n = 0; n < t.length; n++ ) {
        if (t[n].nodeName == 'g') { 
          t[n].firstChild.setAttribute('d',nodes_path(nodeBox[t[n].getAttribute('n2')],nodeBox[t[n].getAttribute('n1')]));
        }
      }
    }"""
    o = '<script %s type="text/ecmascript">\n/*<![CDATA[*//*---->*/\n'%_XLINKNS
    return o + include_js.__doc__  + '\n/*--*//*]]>*/</script>\n'

def svg_defs():
    """ """
    o = '<defs>'
    o += '<marker id=".arrow" viewBox="0 0 500 500" refX="80" refY="50" markerUnits="strokeWidth" orient="auto" markerWidth="40" markerHeight="30"><polyline points="0,0 100,50 0,100 10,50" fill="#555"/></marker>'
    o += '<radialGradient id=".grad" cx="0%" cy="0%" r="90%"><stop offset="0%" stop-color="#FFF"/><stop offset="100%" stop-color="#DDD" class="end"/></radialGradient>'
    o += '<filter id=".shadow" filterUnits="userSpaceOnUse"><feGaussianBlur in="SourceAlpha" result="blur" stdDeviation="2"/><feOffset dy="3" dx="2" in="blur" result="offsetBlur"/><feMerge><feMergeNode in="offsetBlur"/><feMergeNode in="SourceGraphic"/></feMerge></filter>'
    return o + '</defs>\n'

def gen_svg_header(m,(ln,le)):
    ""
    o = '<style type="text/css">\n'
    o += 'text.tiny { font-size: 4pt; fill:DarkSlateGray; }\n'
    o += 'rect.port { stroke-width:0; fill:lightblue; }\n'
    for n in m[0]:
        if ln.has_key(n):
            o += 'g.node_%s > text { %s }\ng.node_%s > rect { %s }\n'%(n,m[0][n][0],n,m[0][n][1]) 
    for e in m[1]:
        if le.has_key(e):
            o += 'g.edge_%s path { %s }\n'%(e,m[1][e]) 
    return o + '</style>\n' + svg_defs() + '\n'

def gettypes(ast):
    ""
    nl,el = {'':True},{'':True}
    Nodes,Edges = ast
    for n in Nodes:
        if len(Nodes[n]) > 1:
            nl[Nodes[n][1]] = True 
    for e in Edges:
        if len(e) > 4:
            el[e[4]] = True 
    return nl,el

################# MAIN ################

def code_gen_test(ref=False):
    """ These test are published in the PDF paper and generates coding files for all output languages
    For non regression on AST, better see doctest in the header of the parse function
    ref=True  -> build the reference files
    ref=False -> compare computed files with reference files
    """
    uobj = u()
    for case in __CODE_GEN_SET__:
        ast = uobj.parse(case[1])
        if ast != __CODE_GEN_SET__[case]: 
            print '|%s|\n%s\n%s'%(case[0],ast,__CODE_GEN_SET__[case])
        assert ast == __CODE_GEN_SET__[case] 
        for l in __OUT_LANG__:
            if not os.path.isdir(l): os.mkdir(l)   
            refname,cmpname = '%s/%s_ref.%s'%(l,case[0],__OUT_LANG__[l]),'%s/%s.%s'%(l,case[0],__OUT_LANG__[l])
            if ref:
                open(refname,'w').write(eval('uobj.gen_%s(ast)'%l)) 
                #open(refname,'w').write(eval('uobj.gen_%s(ast)'%l).encode('utf-8'))
            else:
                open(cmpname,'w').write(eval('uobj.gen_%s(ast)'%l))
                #open(cmpname,'w').write(eval('uobj.gen_%s(ast)'%l).encode('utf-8'))    
                r = subprocess.Popen(('diff',refname,cmpname),stdout=subprocess.PIPE).communicate()[0].strip()
                if not re.match('^\s*$',r): print refname,r
                assert re.match('^\s*$',r)

def ast_test(ref=False):
    ""
    n,h,uobj = 0,' {\n',u()
    for i in __AST_SET__:
        n +=1
        i = re.sub(r'\n','\\\\n',i)
        h += '\t# %02d\n\t\'%s\':\n\t\t%s,\n'%(n,i,uobj.parse(i))
    h += '\n}'
    try:
        eval(h)
    except:
        print 'error in ast!'
    reff = 'ref.txt'
    if ref:
        open(reff,'w').write(h)   
    else:
        if os.path.isfile(reff):
            content = open(reff).read()
            assert content == h

if __name__ == '__main__':
    "Run the module or use it as WSGI application with an Apache server"
    try:
        subprocess.Popen(['dot'], stdout=subprocess.PIPE,stdin=subprocess.PIPE).communicate(input='digraph G { A->B }')
    except:
        print '...Error: please install \'graphviz\' package'
        sys.exit()

    import doctest
    doctest.testmod()
    code_gen_test(True)
    ast_test(True)
    gen_apache_conf()
    gen_doc()
    gen_readme()

    # debug zone!
    #uobj = u()
    #print uobj.parse('A->B')
    #import antigravity

    #s = u' AA ⊔A A⊔ C您'
    #for m in re.compile(r'\s*(\w+)\s*',re.U).finditer(s):
    #    print m.groups()
    #ast = parse('A:O B:T C-I>D')
    #gen_svg(ast)

# the end
