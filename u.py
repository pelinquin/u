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
# Warning ! There is a small bug in Emacs editor default font: switch ⊔ 'squarecap' (U+2293) and ⊓ 'squarecup' (U+2294) char !   

r""" Model Based Engineering widelly use two dimensions graph-based diagrams. Because these diagrams represent viewpoints of a system, including dataflow, workflow and software architecture, they are the building blocks artifacts for specification, modeling and simulation activities. In particular code generation from this high level representation is mandatory.
The data format for these diagrams may be graphical; bitmap or vectorial, unfortunatelly mixing rendering/layout data with semantics.
\textsc{xml} based formats are also used like \textsc{xmi} for \textsc{mof/uml}. However, those formats suffering of several drawbacks like unreadability, unsusefull verbosity and not well adapted structure for representing graphs. \textsc{hutl} and \textsc{json} are not used.
The \emph{Graphviz} \textsc{dot} language or the simple \textsc{yuml} syntax have nice features but lacks to provide native typing and nesting. Our proposal in this paper is a typed graph dedicated language called \usgl{}, provinding the very minimal syntax for graphic and code generation. The language is mainly defined by one given Regular Expression. We present a non formal interpretation of the language, with examples from various models like \textsc{uml}, \emph{SysML}, \emph{Marte}, \textsc{aadl}, \emph{Xcos}, \emph{Kaos}, Entity-Relation Graph, Tree Diagram, Network graph, Flowchart, Petri-Net, State Machine, Markov Chain, Behavior Tree, Flow-based programming diagram,...
This language is a universal representation for input of multi-model code generator tools. We provide as a proof of principe some simple example of code generation for C, Ada, Python, Java, Ocaml, Ruby and Scala Coding Languages. Generation can produce textual representation for \textsc{aadl},\textsc{sdl} and Lustre (Scade) and rely on their own chain to generate code.
Graphic generation is also supported for Tikz to documents and \textsc{svg} for web viewer/editor. 
Actually, we are introducing the concept of \emph{differential dual editing}; where textual and graphical editing are well supported by our language. All source code for a prototype parser and code generators, document generator in \pyt{} is attached to this \textsc{pdf} file.
"""
__author__  = 'Laurent Fournier'
__email__   = 'lfournie@rockwellcollins.com'
__title__   = 'The Universal Short Graph Language'
__version__ = '0.1a'
__license__ = 'GPLv3'
__url__     = 'github/pelinquin/u'

import os,sys,re,hashlib,shutil,subprocess,urllib,datetime,httplib

#__RE_LABEL__ = r'''
#   (
#    "{3}.*?"{3}|             # triple double quote
#    '{3}.*?'{3}|             # triple simple quote
#    "(?:(?:[^"\\]|\\.)*)"|   # double quote with escape
#    '(?:(?:[^'\\]|\\.)*)'    # simple quote with escape
#   )'''
#__RE_LABEL__ = r'''("{3}.*?"{3}|'{3}.*?'{3}|"(?:(?:[^"\\]|\\.)*)"|'(?:(?:[^'\\]|\\.)*)')'''

__RE_U__ = r'''                # RegExp with 10 groups
   (?:                                # Token is NODE:
    (?=(?:[^\W\d_]|:[^\W\d_]|[\"\'])) #  check not empty 
    (?:([^\W\d_]\w*)|)                #  Name      G1
    (?:[\"\']([^\"]*)[\"\'])?         #  Label     G2
    (?:\.(\w+))?                      #  Port      G3
    (?::([^\W\d_]\w*)|)               #  Type      G4
    (?:\(([^\)]*)\))?                 #  Arguments G5
   )|(?:                              # Or EDGE:
    ([\-=<>])                         #  Head      G6
    (?:\"([^\"]*)\")?                 #  Label     G7
    (?:(\w)|)                         #  Type      G8
    #(?:([^\W\d_]\w*)|)                #  Type      G8
    #([^\W\d_a-zA-Z])?                 #  Type      G8
    (?:\(([^\)]*)\))?                 #  Arguments G9
    ([\-=<>])                         #  Tail      G10
   )
'''
 
__RE_FILTER__ = [(r'(?m)\#.*$',''),            # remove comments
                 (r'(?m)\s*$',''),             # right strip 
                 (r'(?m)^\s*',''),             # left strip
                 (r'\n',' '),                  # on one line
                 (r'\s*[\}\]]\s*', '\'], \''), # add quote and open bracket
                 (r'\s*[\{\[]\s*', '\', [\''), # add quote and closing bracket
                 (r'^(.*)$', '[\'\\1\']'),     # add start and end line brackets
                 (r'\'\',\s*',''),             # remove left empty elements
                 (r',\s*\'\'','')]             # remove right empty elements

_XHTMLNS  = 'xmlns="http://www.w3.org/1999/xhtml"'
_SVGNS    = 'xmlns="http://www.w3.org/2000/svg"'
_XLINKNS  = 'xmlns:xlink="http://www.w3.org/1999/xlink"'

__OUT_LANG__ = {'c'      :['c'   ,('/*'  ,'*/' ,'')],
                'python' :['py'  ,('#'   ,''   ,'#!/usr/bin/python\n# -*- coding: utf-8 -*-\n')],
                'ada'    :['adb' ,('--'  ,''   ,'')],
                'scala'  :['scl' ,('--'  ,''   ,'')],
                'java'   :['java',('//'  ,''   ,'')],
                'ruby'   :['rb'  ,('#'   ,''   ,'')],
                'ocaml'  :['ml'  ,('(*'  ,'*)' ,'')],
                'haskell':['hs'  ,('{-'  ,'-}' ,'')],
                'lua'    :['lua' ,('--'  ,''   ,'')],
                'tikz'   :['tex' ,('%'   ,''   ,'')],
                'svg'    :['svg' ,('<!--','-->','<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n')],
                'aadl'   :['adl' ,('--'  ,''   ,'')],
                'sdl'    :['sdl' ,('--'  ,''   ,'')],
                'lustre' :['lst' ,('--'  ,''   ,'')],
                'vhdl'   :['hdl' ,('--'  ,''   ,'')],
                'systemc':['sc'  ,('//'  ,''   ,'')]}

__IN_MODEL__ = ['UML','SysML','AADL-Graph','Marte','PSL','Xcos','Kaos','Entity-Relation-Graph','Tree-Diagram',
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
    ('OnlyId'                 ,'A'),
    ('OnlyLabel'              ,'"L"'),
    ('OnlyType'               ,':T'),
    ('OnlyChild'              ,'{a}'),
    ('Id+Label'               ,'A"L"'),
    ('Id+Type'                ,'A:T'),
    ('Id+Child'               ,'A{a}'),
    ('Label+Type'             ,'"L":T'),
    ('Label+Child'            ,'"L"{a}'),
    ('Type+Child'             ,':T{a}'),
    ('Id+Label+Type'          ,'A"L":T'),
    ('Id+Label+Child'         ,'A"L"{a}'),
    ('Label+Type+Child'       ,'"L":T{a}'),
    ('Id+Type+Child'          ,'A:T{a}'),
    ('Id+Label+Type+Child'    ,'A"L":T{}'),
    ('Id+Label+Type+Arg+Child','A"L":T(arg){}'),
    ('Id with two args'       ,'A(arg1,arg2)'),
    ('Label+argument'         ,'"L"(arg)'),
    ('Type+argument'          ,':T(arg)'),
    ('Word Id'                ,'Aaa'),
    ('Not first digit'        ,'A1'),
    ('white space'            ,' A'),
    ('end white space'        ,'A '),
    ('both white spaces'      ,' A '),
    ('several lines'          ,'\n\nA\n\n'),
    ('latin1 char'            ,'éàùç'),
    ('unicode char'           ,'您您好'),
    ('long label'             ,'"This is a long label" '),
    ('label with quotes'      ,'"Label with \"escape\" substring" '),
    (''                       ,'"Multi\nlines\nlabel" '),
    #(''                       ,' """This is "a" \'very\' \nlong label""" '),
    #(''                       ,' \'Simple quote\' '),
    (''                       ,'A:Type1 B:Type2'),
    ('2 nodes'                ,'A B'),
    ('3 nodes'                ,'A B C'),
    ('4 nodes'                ,'A B C D'),
    ('overload label'         ,'A"label1" A"label2"'),
    (''                       ,'A A"label"'),
    (''                       ,'A"label" A'),
    (''                       ,'A:T A"label"'),
    (''                       ,'A"label" A:T'),
    (''                       ,'A{a} A"label"'),
    (''                       ,'A"label" A{a}'),
    (''                       ,'A(x) A"label"'),
    (''                       ,'A"label" A(x)'),
    (''                       ,'A A"label1" A"label2"(x)'),
    (''                       ,'A"label2"(x) A"label" A'),
    (''                       ,'"label":T1 "label":T2'),
    (''                       ,'A"label" B"label"'),
    (''                       ,'A{a} B{b1 b2} C{c1 c2 c3}'),
    (''                       ,'A{a} B {b} {c}'),
    (''                       ,'A{a} B[b]'),
    (''                       ,':T1{a b} :T2{c d}'),
    (''                       ,'A{ B{c b} C{e f} }'),
    (''                       ,'A{B{C{c}}}'),
    (''                       ,'"x-y"'),
    (''                       ,'"x+y"'),
    (''                       ,'"x*y"'),
    (''                       ,'"x/y"'),
    (''                       ,'"x.y"'),
    (''                       ,'"x,y"'),
    (''                       ,'"x%y"'),
    (''                       ,'"x^y"'),
    (''                       ,'"x=y"'),
    (''                       ,'"x:y"'),
    (''                       ,'"x&y"'),
    (''                       ,'"x|y"'),
    (''                       ,'"x>y"'),
    (''                       ,'"x<y"'),
    (''                       ,'#comment\nA'),
    (''                       ,' #comment1\nA\n #comment2'),
    (''                       ,'A->B'),
    (''                       ,'A->B->C'),
    ('1'                      ,'A>>B'), 
    ('2'                      ,'A><B'),
    ('3'                      ,'A>-B'),
    ('4'                      ,'A>=B'),
    ('5'                      ,'A<>B'),
    ('6'                      ,'A<<B'),
    ('7'                      ,'A<-B'),
    ('8'                      ,'A<=B'),
    ('9'                      ,'A->B'),
    ('10'                     ,'A-<B'),
    ('11'                     ,'A--B'), 
    ('12'                     ,'A-=B'),
    ('13'                     ,'A=>B'), 
    ('14'                     ,'A=<B'),
    ('15'                     ,'A=-B'),
    ('16'                     ,'A==B'),
    ('spaceBeforeEdge'        ,'A ->B'),
    (''                       ,'A-> B'),
    (''                       ,'A -> B'),
    (''                       ,'A->A'),
    (''                       ,'A -"label"- B'),
    (''                       ,'A -Type- B'),
    (''                       ,'A -(arg)- B'),
    (''                       ,'A -"label"Type- B'),
    (''                       ,'A -Type(arg)- B'),
    (''                       ,'A -"label"(arg)- B'),
    (''                       ,'A -"label"Type(arg)- B'),
    (''                       ,'A -(arg1,arg2,arg3)- B'),
    (''                       ,'A{a1 a2} -> B{b1 b2}'),
    (''                       ,'{a1 a2} -> B{b1 b2}'),
    (''                       ,'A{a1 a2} -> {b1 b2}'),
    (''                       ,'{a1 a2} -> {b1 b2}'),
    (''                       ,'A{a1 -> a2} B{b1 -> b2}'),
    (''                       ,'A{a1 -> a2} -> B{b1 -> b2}'),
    (''                       ,'A.1 -> B.2'),
    (''                       ,'A.por1 -> B.por2'),
    (''                       ,'A:T1 B:T2 A.1->B.2'),
    (' '                      ,'A.1->B.2 A:T1 B:T2'),
    ('Double definition'      ,'A{a} A{b}'),
]

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
                          'R':('fill:red;font-family:helvetica,arial,sans-serif;}','fill:none;stroke:black;stroke-width:1;',0,0,()),
                          'O':('fill:blue;','filter:url(#.shadow);fill-opacity:.1;',30,30,('in1','in2','out1','out2')),
                          'C':('fill:blue;','filter:url(#.shadow);fill-opacity:.1;',
                               30,60,('pin1','pin2','pin3','pin4','pin5','pin6','pin7','pin8',
                                      'pin9','pin10','pin11','pin12','pin13','pin14','pin15','pin16')),
                          'D':('fill:blue;','filter:url(#.shadow);fill-opacity:.1;',
                               20,40,('pin1','pin2','pin3','pin4','pin5','pin6'))},
                         {'' : 'stroke:black; stroke-width:1; fill:none; marker-end:url(#.arrow);',
                          'I': 'stroke:green; stroke-width:2; fill:none; marker-end:url(#.arrow);',
                          'L': 'stroke:red; stroke-width:3; fill:none; marker-end:url(#.arrow);'})
        self.m['tikz'] = ({'':('circle,drop shadow,draw=green!40,fill=gray!20',()),
                           'S':('rectangle,draw=black!40,fill=gray!10',('p1','p2')),
                           'T':('circle,drop shadow,draw=green!40,fill=gray!20',('in1','in2','out1','out2')), 
                           'O':('rectangle,drop shadow,rounded corners=3pt,draw=red!40,fill=blue!25',()),
                           },
                          {'':'--',
                           'I':'->,>=open diamond',
                           'L':'->,>=triangle 60',
                           'droit':'--',
                           'simple':'->,>=latex',
                           'S':'->,>=latex',
                           })
        self.m['c'] = ({'C':['class',],
                        'c':['class',]}
                       ,{'':[]})
            
    def setType(self,lang,t,tab,isnode=True):
        ""
        if isnode:
            self.m[lang][0][t] = tab
        else:
            self.m[lang][1][t] = tab

    def merge(self,a,b):
        ""
        r = list(b)
        for i in range(len(b)):
            if (b[i] in (None,[])) and len(a)>i and (a[i] not in (None,[])):
                r[i] = a[i]
        if len(a) > len(b):
            r += a[len(b)-len(a):]
        return tuple(r)

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
                    if t:
                        if t[0]:
                            t[0] += k.keys()
                        else:
                            t[0] = k.keys()
                    else:
                        t.append(k.keys())                        
                    Nodes[oid] = tuple(t)
                    #print oid, Nodes[oid]
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
                        #if a[1] != None:
                        #    l = a[1] 
                        #a[1] = [re.sub(r'\\','',l[1:-1] if not l[:3] in ('"""',"'''") else l[3:-3])]
                        #a[1] = l[1:-1] 
                        nid,npo = find_id(a[:5]),'.%s'%a[2] if a[2] else ''                        
                        attr = strip4(tuple([[]] + a[1:2] + a[3:5]))
                        if Nodes.has_key(nid):
                            nodes[nid] = self.merge(Nodes[nid],attr)
                        if nodes.has_key(nid):
                            nodes[nid] = self.merge(nodes[nid],attr)
                        else:
                            nodes[nid] = attr
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

    def hf(self,appli):
        "Add header and footer to generated code"
        lang = re.sub('gen_','',appli.__name__)
        com = __OUT_LANG__[lang][1]
        (sc,ec,head) = com
        def app(ast):
            Nodes,Edges = ast
            d = '%s'%datetime.datetime.now()
            o = '%s%s ⊔ Generated Code [%s] %s\n'%(head,sc,d[:19],ec)
            o += '%s CPU Times:  %s   %s\n'%(sc,os.times()[:-1],ec)            
            o += '%s ******** Do not edit by hand! ******** %s\n'%(sc,ec)
            digest = hashlib.sha1(open(__file__).read()).hexdigest()
            o += '%s SHA1: %s %s\n'%(sc,digest[:-8],ec)
            o += '%s Forge:  https://github.com/pelinquin/u %s\n'%(sc,ec)
            o += '%s © Copyright 2011 Rockwell Collins, Inc %s\n'%(sc,ec)
            o += '%s ** GNU General Public License  (v3) ** %s\n'%(sc,ec)
            dast = '%s %s'%ast
            if re.search(r'\-{2}',dast):
                o += '\n%s Doubledash replaced by double underscore !%s\n'%(sc,ec)        
                dast = re.sub(r'\-\-','__','%s'%dast)
            o += '\n%s AST = %s %s\n'%(sc,dast,ec)
            a = appli(ast)
            o += '\n%s Types parameters: %s %s\n'%(sc,self.m[lang],ec) + a
            return o + '\n%s %s Nodes %s Edges %s Lines | The end of file %s'%(sc,len(Nodes),len(Edges),len(a.split('\n'))+13,ec)
        return app

    def gen_c(self,ast):
        "/* C */\n"
        m,classT,mainT = self.m['c'],[],[]
        for t in m[0]:
            if len(m[0][t])>0:
                if m[0][t][0] == 'class':
                    classT.append(t)
        o = '\n'
        o += '/* %s */'%classT
        Nodes,Edges = ast
        for x in Nodes:
            if Nodes[x]:
                if len(Nodes[x]) == 2:
                    if Nodes[x][1] in classT:
                        o += '\n/* Class: %s */\n'%Nodes[x][0]
                        o += 'typedef struct %s {\n'%x
                        o += '  int a;\n'
                        o += '} %s;\n'%x
                    elif Nodes[x][1] in mainT:
                        o += '\nint main(void) {\n'
                        o += '  return(0); \n}\n'
        return self.gen_c.__doc__ + o

    def gen_python(self,ast):
        "## Python 2.7"
        o = '\n\n'
        Nodes,Edges = ast
        for x in Nodes:
            if Nodes[x]:
                if len(Nodes[x]) == 2:
                    if Nodes[x][1] == 'class':
                        o += '\nclass %s:\n'%x
                        o += '\t""" %s """\n'%Nodes[x][0]
                        o += '\ta=0\n'
                    elif Nodes[x][1] == 'main':
                        o += '\nif __name__ == \'__main__\': \n'
                        o += '\tprint \'yes\'\n'
        return self.gen_python.__doc__ + o 

    def gen_ada(self,ast):
        """-- ADA 95 with Ravenscar profile
pragma Profile (Ravenscar);
"""
        m = self.m['ada']
        o = 'with Ada.Text_IO;\n\n'
        o += 'procedure Hello is\nbegin\n\tAda.Text_IO.Put_Line("Hi!");\nend Hello;\n\n'
        return self.gen_ada.__doc__ + o 

    def gen_scala(self,ast):
        "// Scala\n"
        o,m = '',self.m['scala']
        return self.gen_scala.__doc__ + o 

    def gen_java(self,ast):
        "// Java\n"
        o,m = '',self.m['java']
        return self.gen_java.__doc__ + o 

    def gen_ruby(self,ast):
        "# Generated from ⊔ AST:\n"
        o,m = '',self.m['ruby']
        return self.gen_ruby.__doc__ + o 

    def gen_ocaml(self,ast):
        "(* Objective Caml *)\n"
        o,m = '',self.m['ocaml']
        return self.gen_ocaml.__doc__ + o 
    
    def gen_haskell(self,ast):
        "{- Haskell -}\n"
        o,m = '',self.m['haskell']
        return self.gen_ocaml.__doc__ + o 

    def gen_lua(self,ast):
        "-- Lua \n"
        o,m = '',self.m['lua']
        return self.gen_lua.__doc__ + o 

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
            shape = 'node_' if (len(Nodes[n])<3 or not Nodes.has_key(n)) else 'node_%s'%Nodes[n][2]
            (x,y) = (pos[n][0]/25,pos[n][1]/25)
            o += r'\node[%s](%s) at (%0.3f,%0.3f) {%s};'%(shape,name,x,y,label) + '\n'
            # ports
            tt = m[0][''][1] if (len(Nodes[n])<3 or not Nodes.has_key(n) or not m[0].has_key(Nodes[n][2])) else m[0][Nodes[n][2]][1]
            if tt:
                delta,p = 360/len(tt),-180
            for i in tt:
                o += r'\draw (%s.%s) node{\tiny{%s}};'%(n,p,i) + '\n'
                p += delta
        for e in Edges:
            boucle = '[bend left]'
            typ = 'edge_' if len(e)<5 or e[4] == None else 'edge_%s'%e[4]
            label = '' if len(e)<4 or e[3] == None else 'node{%s}'%e[3] 
            o += r'\draw[%s](%s) to%s %s(%s);'%(typ,e[0],boucle,label,e[2]) + '\n'
        o += r'\end{tikzpicture}'+ '\n'
        if standalone:
            o +=  r'\end{document}'
        return self.gen_tikz.__doc__  + o 

    def gen_svg(self,ast,with_js=False):
        """<!-- the \'with_js\' boolean defines if Javascript is requested or not -->\n"""
        m = self.m['svg']
        pos,ratio = layout(ast[0],ast[1],'LR'),4
        Nodes,Edges = ast
        o = '<svg %s>\n'%_SVGNS
        o += '<title id=".title">%s</title>\n'%__title__
        o += get_favicon()
        o += '<path id="logo" stroke-width="5" fill="none" stroke="Dodgerblue" onclick="window.open(\'http://%s\');" title="⊔ [http://%s]" opacity=".02" d="M10,10L10,35L30,35L30,10"/>\n'%(__url__,__url__)
        o += gen_svg_header(m,gettypes(ast))
        if with_js:
            o += include_js()
        o += '<g id=".nodes">\n'
        Ports,Nodebox,Nodeports = {},{},{}
        for n in pos:
            #label = n.encode('utf-8')
            label = n
            if Nodes[n]:
                if Nodes[n][1]:
                    #label = ast[0][n][1].encode('utf-8')
                    label = Nodes[n][1]
            style = 'node_' if (len(Nodes[n])<3 or not Nodes.has_key(n)) else 'node_%s'%Nodes[n][2]
            t = '' if not (Nodes.has_key(n) and (len(Nodes[n])>2) and m and m[0].has_key(Nodes[n][2])) else Nodes[n][2]
            mx,my = m[0][t][2],m[0][t][3]
            o += '<g id="%s" class="%s" mx="%s" my="%s">'%(n,style,mx,my)
            #label = '<tspan>%s</tspan><tspan x="%s" dy="1em">%s</tspan><tspan x="%s" dy="1em">%s</tspan>'%(n,pos[n][0]*ratio,n,pos[n][0]*ratio,n)
            x,y,w,h = getbbox(label,pos[n][0]*ratio,pos[n][1]*ratio)
            Nodebox[n],Nodeports[n] = (x-mx,y-my,w+2*mx,h+2*my),[]
            label = '<tspan>%s</tspan>'%label
            o += '<rect rx="4" x="%s" y="%s" width="%s" height="%s"/>'%Nodebox[n]
            o += '<text class="node" x="%s" y="%s">%s</text>'%(pos[n][0]*ratio,pos[n][1]*ratio,label)
            o += '<g>' 
            ports = m[0][t][4]
            Ports[n] = ports
            if ports:
                delta = 200.0/len(ports)
                d = delta/2.0 - 100
                for p in ports:
                    b = Nodebox[n]
                    if d<0:
                        anchor,x,y = 'start',b[0]+1, b[1] + (d+100)*b[3]/100 
                        rx = x-7
                        Nodeports[n].append((x-6,y))
                    else:
                        anchor,x,y = 'end',b[0] + b[2]-1, b[1] + (100-d)*b[3]/100
                        rx = x+1
                        Nodeports[n].append((x+6,y))
                    o += '<rect class="port" x="%s" y="%s" width="6" height="6" pos="%s"/>'%(rx,y-3,d)
                    o += '<text class="tiny" x="%s" y="%s" dominant-baseline="middle" text-anchor="%s">%s</text>'%(x,y,anchor,p)
                    d += delta
            #print Nodeports
            o += '</g>'
            #if not with_js:
            #    o += '<rect style="stroke:red;stroke-width:1;fill:none;" x="%s" y="%s" width="%s" height="%s"/>'%Nodebox[n]
            o += '</g>\n'
        o += '</g>\n<g id=".connectors" >\n'
        ne = 0
        for e in Edges:
            ne += 1
            if e and type(e[-1]).__name__ ==  'list':
                print 'pb!'
            edge_label = e[3] if len(e)>3 else ''
            typ = 'edge_' if len(e)<5 else 'edge_%s'%e[4]
            n1,n2,p1,p2 = e[0],e[2],'',''
            ep1,ep2 = '',''
            if re.search(r'\.',e[0]):
                [n1,p1] = e[0].split('.')
                if re.match(r'^\d+$',p1):
                    ep1 = int(p1)
                    p1 = ' p1="%s"'%int(p1)
                elif p1 in Ports[n1]:
                    ep1 = Ports[n1].index(p1)
                    p1 = ' p1="%s"'%Ports[n1].index(p1)
                else:
                    p1 = ''
            if re.search(r'\.',e[2]):
                [n2,p2] = e[2].split('.') 
                if re.match(r'^\d+$',p2):
                    ep2 = int(p2)
                    p2 = ' p2="%s"'%int(p2)
                elif p2 in Ports[n2]:
                    ep2 = Ports[n2].index(p2)
                    p2 = ' p2="%s"'%Ports[n2].index(p2)
                else:
                    p2 = ''
            if ep1 != '' and ep2 != '' and ep1<len(Nodeports[n1]) and ep2<len(Nodeports[n2]):
                d = nodes_path2(Nodeports[n1][ep1],Nodeports[n2][ep2],Nodebox.values())
            elif ep1 != '' and ep1<len(Nodeports[n1]):
                d = nodes_path1(Nodebox[n2],Nodeports[n1][ep1],False)
            elif ep2 != '' and ep2<len(Nodeports[n2]):
                d = nodes_path1(Nodebox[n1],Nodeports[n2][ep2],True)
            else:
                d = nodes_path(Nodebox[n1],Nodebox[n2])
            mx,my = (Nodebox[n1][0] + Nodebox[n2][0])/2,(Nodebox[n1][1] + Nodebox[n2][1])/2
            
            o += '<g class="%s" n1="%s" n2="%s"%s%s><path id="e_%s" d="%s"/>'%(typ,n1,n2,p1,p2,ne,d)
            if edge_label:
                o += '<text><textPath %s xlink:href="#e_%s" startOffset="50%%">%s</textPath></text>'%(_XLINKNS,ne,edge_label)
            o += '</g>\n'
        o += '</g>\n'
        

        return self.gen_svg.__doc__ + o + '\n</svg>'

    def gen_aadl(self,ast):
        "-- AADL\n"
        o,m = '',self.m['aadl']
        return self.gen_aadl.__doc__ + o 

    def gen_sdl(self,ast):
        "# SDL\n"
        o,m = '',self.m['sdl']
        if m[0].has_key('I'):
            o += '%s'%m[0]['I']
        return self.gen_sdl.__doc__ 

    def gen_lustre(self,ast):
        "-- Lustre\n"
        o,m = '',self.m['lustre']
        return self.gen_lustre.__doc__ + o 

    def gen_vhdl(self,ast):
        "-- VHDL\n"
        o,m = '',self.m['vhdl']
        return self.gen_vhdl.__doc__ + o 

    def gen_systemc(self,ast):
        "/* SystemC */\n"
        o,m = '',self.m['systemc']
        return self.gen_systemc.__doc__ + o 

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
    digest = hashlib.sha1(open(__file__).read()).hexdigest()
    prg,path = os.path.basename(sys.argv[0])[:-3],os.path.abspath(sys.argv[0]) 
    o = '# SHA1:%s\n\n'%digest
    o += 'WSGIScriptAlias /%s %s\n'%(prg,path)
    o += 'WSGIScriptAlias /⊔ %s\n'%path
    o += 'AliasMatch /fonts/([^\.]*\.otf) %s/fonts/$1\n'%os.path.dirname(path)
    open('%s.conf'%prg,'w').write('%s\n'%gen_apache_conf.__doc__+o)

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
    \usepackage{embedfile}
    \usepackage{graphicx}
    \usepackage{tikz}
    \usepackage{longtable}
    \usepackage{array}
    \newcommand{\pyt}{\emph{Python}}
    \newcommand{\pdf}{\textsc{pdf}}
    \begin{document}
    \lstset{language=Python,breaklines=true}
    """ 
    digest = hashlib.sha1(open(__file__).read()).hexdigest()
    o = tex_header.__doc__
    o += r'\title{\bf $\sqcup$: %s} \author{%s -- \url{%s} \\ '%(__title__,__author__,__email__) + '\n'
    o += r'\tiny{version: %s [\texttt{%s}]\footnote{the first five hexadecimal bytes of the \textsc{sha1} digest of \texttt{u.py} source file. Please compare it with the one published \url{https://%s} to get the last release.}}}'%(__version__,digest[:5],__url__) + '\n'
    o += r'\maketitle' + '\n'
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

def get_favicon():
    d = '%s'%datetime.datetime.now() # this is ack to change favicon in the cache
    code = '<svg xmlns="http://www.w3.org/2000/svg" n="%s"><path stroke-width="2.5" fill="none" stroke="Dodgerblue" d="M3,1 L3,14 L13,14 L13,1"/><path d="M33,18 L27,21 L33,24Z" fill="white"/></svg>'%d
    data = code.encode('base64').replace('\n','')
    return '<link %s rel="shortcut icon" type="image/svg+xml" href="data:image/svg+xml;base64,%s"/>\n'%(_XHTMLNS,data)

def application(environ,start_response):
    """<title>⊔</title><style>h1,h6,p,li,b{font-family:helvetica neue,helvetica,arial,sans-serif;} a{text-decoration:none;} h6{text-align:right;}</style>
<h1><a href="https://github.com/pelinquin/u" style="font-size:64pt;color:DodgerBlue;" title="SquareCup">⊔</a> Web service</h1>
<p>The Web service root name (after domain and server name in the URL) is: ⊔ [<a href="u?about">/u...</a>] or [<a href="⊔?about">/⊔...</a>] (U+2294).</p>
<p>Any URL argument (after '?') shall be a valid ⊔ string and the default output is the AST (a Python data structure).
Example: [<a href="u?A->B">/u?A->B</a>]</p>
<p>If a language name is given first, then the output is the generated code for this language. Example: [<a href="u?ada&A->B">/u?ada&A->B</a>]</p>
<p>With no other argument than a language name (no '&'), a local file browser is provided to select an input ⊔ file for upload.
Example: [<a href="u?tikz">/u?tikz</a>]</p>
<p>If the '_' character is given before the language name, then the output is the interpretation of the generated language;</p>
   <li>For <b>svg</b> the graphics is rendered within the browser. Example: [<a href="u?_svg&A->B">/u?_svg&A->B</a>]</li>
   <li>For <b>tikz</b>, the pdf reader is called for rendering the graphics. Example: [<a href="u?_tikz&A->B">/u?_tikz&A->B</a>]</li>
<p>Special keywords:</p>
   <li><i>pdf</i> or <i>paper</i> returns the generated paper on ⊔ in pdf format: [<a href="u?pdf">/u?pdf</a>]</li>
   <li><i>beamer</i> returns the generated beamer slides on ⊔ in pdf format: [<a href="u?beamer">/u?beamer</a>]</li>
   <li><i>update</i> [<a href="u?update">/u?update</a>] is used to update the web application with the last release from 
[<a href="https://github.com/pelinquin/u">https://github.com/pelinquin/u</a>]</li>
   <li><i>help</i>,<i>about</i> or <i>usage</i> displays this page.</li>
<p>If no argument is given, the output [<a href="u">/u</a>] is the Python source code for reading or for <a href="u"><b>download</b></a>.
</p><p>Supported output languages are:</p><b>"""
    s,mime,o,uobj = urllib.unquote(environ['QUERY_STRING']),'text/plain;charset=UTF-8','Error!',u()
    if reg(re.match(r'\s*(update$|about$|help$|usage$|pdf$|paper|beamer$|)(?:(_?)(%s|raw|ast)(?:&(.*)|)|(.*))\s*$'%'|'.join(__OUT_LANG__),s,re.I)):
        form,action,under,lang,args = False,reg.v.group(1),reg.v.group(2),reg.v.group(3),reg.v.group(5) if reg.v.group(5) else reg.v.group(4)
        if lang: lang = lang.lower()
        if (action,under,lang,args) == ('',None,None,None):
            start_response('200 OK',[('Content-type',mime),('Content-Disposition','filename=u.py')])
            return [(open(environ['SCRIPT_FILENAME']).read())] 
        elif action and action.lower() in ('about','help','usage'):
            mime,o = 'text/html;charset=UTF-8','<html><title>v%s</title>%s'%(__version__,get_favicon())
            o += application.__doc__ + ', '.join(__OUT_LANG__) + '</b>\n'
            o += '<p>Supported Input Modeling Formalism are:</p><b>' + ', '.join(__IN_MODEL__) + ',...</b>\n'
            digest = hashlib.sha1(open(__file__).read()).hexdigest()
            o += '<h6>Digest: %s</h6></html>'%digest[:5]
        elif action and action.lower() in ('paper','pdf'):
            o,mime = open('%s/u.pdf'%os.path.dirname(environ['SCRIPT_FILENAME'])).read(),'application/pdf'
        elif action and action.lower() == 'beamer':
            o,mime = open('%s/beamer_u.pdf'%os.path.dirname(environ['SCRIPT_FILENAME'])).read(),'application/pdf'
        elif action and action.lower() == 'update':
            if environ['SERVER_NAME'] != 'pelinquin': # update not possible from RCF network
                cmd = 'cd %s/..; rm -rf u; git clone git://github.com/pelinquin/u.git; cd u'%os.path.dirname(environ['SCRIPT_FILENAME'])
            else:
                cmd = 'ls %s'%__file__
            res = subprocess.Popen((cmd), shell=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE).communicate()
            o,mime = '<html><p>','text/html'
            o += res[1] if res[1] else '%s server Updated!'%environ['SERVER_NAME']
            o += '</p><a href="u?about">...go to main page</a>'
            digest = hashlib.sha1(open(__file__).read()).hexdigest()
            o += '<h6>Digest: %s</h6></html>'%digest[:5]
        elif args == None:
            if environ['REQUEST_METHOD'].lower() == 'post':
                raw = environ['wsgi.input'].read(int(environ.get('CONTENT_LENGTH','0')))
                ast = uobj.parse('\n'.join(raw.split('\n')[4:-2]))
                if lang in ('ast','raw'):
                    o = '%s %s'%ast
                else: 
                    o = eval('uobj.hf(uobj.gen_%s)(ast)'%lang) #o = ....encode('utf-8')
            else:
                mime,form,o = 'text/html',True, '<form method=post enctype=multipart/form-data><input type=file name=a onchange="submit();"/>'
                #o += '<input type=button value=test onclick="submit();"/>'
                o += '</form>'
        elif lang in (None, 'ast','raw'):
            o = '# ⊔ Python Abstract Syntax Structure:\n\n%s %s'%uobj.parse(args)
        else:
            ast = uobj.parse(args)
            o = eval('uobj.hf(uobj.gen_%s)(ast)'%lang) #o = eval('uobj.gen_%s(ast)'%lang).encode('utf-8')
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
        ext = 'u' if lang in (None, 'ast','raw') else __OUT_LANG__[lang][0]
        if under == '_' and lang in ('c','ada'):
            header.append(('Content-Disposition','attachment; filename=a.out'))
        else:
            header.append( ('Content-Disposition','filename=file.%s'%ext))
    start_response('200 OK',header)
    return [(o)] 

def strip3(z):
    return z[:-3] if not (z[-1] or z[-2] or z[-3]) else z[:-2] if not (z[-1] or z[-2]) else z[:-1] if not z[-1] else z

def strip4(z):
    return z[:-4] if not (z[-1] or z[-2] or z[-3] or z[-4]) else z[:-3] if not (z[-1] or z[-2] or z[-3]) else z[:-2] if not (z[-1] or z[-2]) else z[:-1] if not z[-1] else z
    
######### UTILITIES ###########

def layout(nodes,edges,rankdir='TB'):
    "computes layout for graphics (tikz and svg) generation"
    bbx,bby,pos,d = None,None,{},'digraph G { rankdir=%s '%rankdir
    for n in nodes:
        label = n if not n[0] else n[0] #label = n.encode('utf-8') if not n[0] else n[0]
        d+= ' %s[label="%s"];'%(n,label) #d+= ' %s[label="%s"];'%(n.encode('utf-8'),label)
    for e in edges:
        n1,n2 = re.sub(r'\..+$','',e[0]),re.sub(r'\..+$','',e[2])
        d+= ' %s->%s'%(n1,n2) #d+= ' %s->%s'%(e[0].encode('utf-8'),e[2].encode('utf-8'))
    p = subprocess.Popen(['dot'], stdout=subprocess.PIPE, stdin=subprocess.PIPE)
    for l in p.communicate(input=d + '}')[0].split('\n'):
        if reg(re.search('bb="0,0,([\.\d]+),([\.\d]+)"',l)):
            bbx,bby = float(reg.v.group(1)),float(reg.v.group(2))
        elif reg(re.search('^\s*(\w+)\s*\[label=[^,]*, pos="([\.\d]+),([\.\d]+)"',l)) and bbx and bby:
            pos[reg.v.group(1)] = (float(reg.v.group(2))*100/bbx,float(reg.v.group(3))*100/bby)
    return pos

def gen_tikz_header(m=[],(ln,le)=({},{})):
    r"""\usetikzlibrary{shapes,fit,arrows,shadows,backgrounds}"""
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

def getbbox(text,x,y):
    ""
    ajust = {'i':.5,'l':.5,'w':1.35}
    h,l = 18,.0
    for c in text:
        if ajust.has_key(c):
            l += ajust[c]
        else:
            l += 1.
    return x,y-h+3,l*10,h+1

def nodes_path2(p1,p2,nodes=[]):
    ""
    m = 20
    x1,y1,x2,y2 = p1[0],p1[1],p2[0],p2[1]
    o = 'M%s,%s'%(x1,y1)
    if abs(x2-x1) > abs(y2-y1):
        o += 'L%s,%s'%((x2+x1)//2,y1)
        o += 'L%s,%s'%((x2+x1)//2,y2)
        for n in nodes:
            if n[1] < y2 and y2 < n[1] + n[3]:
                if x2>x1:
                    if n[0] > x1 and n[0]< x2:
                        if y1 != y2:
                            if ((n[1]-y2)//(y1-y2))<0:
                                o += 'L%s,%s'%((x2+x1)//2,n[1])
                                o += 'L%s,%s'%(x2,n[1])
                            else:
                                o += 'L%s,%s'%((x2+x1)//2,(n[1]+n[3]))
                                o += 'L%s,%s'%(x2,(n[1]+n[3]))
                        else:
                            o += 'L%s,%s'%((x2+x1)//2,(n[1]+n[3]))
                            o += 'L%s,%s'%(x2,(n[1]+n[3]))
    else:        
        o += 'L%s,%s'%(x1,(y2+y1)//2)
        o += 'L%s,%s'%(x2,(y2+y1)//2)
    o += 'L%s,%s'%(x2,y2)
    return o

def nodes_path2_old(p1,p2,nodes=[]):
    ""
    m = 20
    x1,y1,x2,y2 = p1[0],p1[1],p2[0],p2[1]
    o = 'M%s,%s'%(x1,y1)
    o += 'L%s,%s'%(x1,y1)
    if abs(x2-x1) > abs(y2-y1):
        for n in nodes:
            if n[1] < y1 and n[1] + n[3] > y1:
                if n[0] > x1:
                    o += 'L%s,%s'%(n[0],y1)
        o += 'L%s,%s'%(x2,y1)
    else:
        
        o += 'L%s,%s'%(x1,y2)
    o += 'L%s,%s'%(x2,y2)
    o += 'L%s,%s'%(x2,y2)
    return o

def nodes_path_old(b1,b2):
    ""
    m = 20
    x1,y1,x2,y2 = b1[0] + b1[2]/2, b1[1] + b1[3]/2,b2[0] + b2[2]/2, b2[1] + b2[3]/2
    o = 'M%s,%s'%(x1,y1)
    o += 'L%s,%s'%(x1+m,y1)
    if abs(x2-x1) > abs(y2-y1):
        o += 'L%s,%s'%(x2-m,y1)
    else:
        o += 'L%s,%s'%(x1,y2)
    o += 'L%s,%s'%(x2-m,y2)
    o += 'L%s,%s'%(x2,y2)
    return o

def nodes_path1(b,p,way):
    ""
    x1,y1,x2,y2 = b[0] + b[2]/2, b[1] + b[3]/2, p[0],p[1] 
    h1,l1 = 1 + b[3]/2, 1 + b[2]/2
    if x1 == x2:
        if y1<y2:  
            y1 += h1
        else: 
            y1 -= h1
    elif y1 == y2:
        if x1<x2: 
            x1 += l1
        else: 
            x1 -= l1
    else:
        Q,R = x1-x2,y1-y2
        P = Q/R
        if abs(P) < l1/h1:
            if R<0: 
                y1 += h1 
                x1 += h1*P
            else: 
                y1 -= h1
                x1 -= h1*P
        else:
            if Q<0:
                x1 += l1
                y1 += l1/P
            else:
                x1 -= l1
                y1 -= l1/P
    if way:
        d = 'M%s,%sL%s,%s'%(x1,y1,x2,y2)
    else:
        d = 'M%s,%sL%s,%s'%(x2,y2,x1,y1)
    return d

def nodes_path(b1,b2):
    ""
    x1,y1,x2,y2 = b1[0] + b1[2]/2, b1[1] + b1[3]/2, b2[0] + b2[2]/2, b2[1] + b2[3]/2
    h1,l1,h2,l2 = 1 + b1[3]/2, 1 + b1[2]/2, 1 + b2[3]/2, 1 + b2[2]/2
    if x1 == x2:
        if y1<y2:  
            y1 += h1
            y2 -= h2
        else: 
            y1 -= h1
            y2 += h2
    elif y1 == y2:
        if x1<x2: 
            x1 += l1
            x2 -= l2
        else: 
            x1 -= l1
            x2 += l2
    else:
        Q,R = x1-x2,y1-y2
        P = Q/R
        if abs(P) < l1/h1:
            if R<0: 
                y1 += h1 
                x1 += h1*P
            else: 
                y1 -= h1
                x1 -= h1*P
        else:
            if Q<0:
                x1 += l1
                y1 += l1/P
            else:
                x1 -= l1
                y1 -= l1/P
        if abs(P) < l2/h2:
            if R>0: 
                y2 += h2 
                x2 += h2*P
            else: 
                y2 -= h2
                x2 -= h2*P
        else:
            if Q>0:
                x2 += l2
                y2 += l2/P
            else:
                x2 -= l2
                y2 -= l2/P
    return 'M%s,%sL%s,%s'%(x1,y1,x2,y2)

def include_js():
    r"""
if (typeof($)=='undefined') { function $(id) { return document.getElementById(id.replace(/^#/,'')); } }
var nodeBox   = [];
var nodePorts = [];

function nodes_path1(x1,y1,b2,way) {
  var x2 = b2.x + b2.width/2; var y2 = b2.y + b2.height/2;
  var h2 = 1 + b2.height/2; var l2 = 1 + b2.width/2;
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
  if (way == true) {
   d = x1+','+y1+'L'+x2+','+y2;
  } else {
   d = x2+','+y2+'L'+x1+','+y1;
  }
  return ('M'+d);
}

function nodes_path2(x1,y1,x2,y2) {
  var m = 20;
  var o = 'M'+x2+','+y2;
  o += 'L' + (x2+m) + ',' + y2;
  if (Math.abs(x2-x1)>Math.abs(y2-y1)) {
    o += 'L' + (x1-m) + ',' + y2;
  } else {
    o += 'L' + x2 + ',' + y1;
  }
  o += 'L' + (x1-m) + ',' + y1;
  o += 'L' + x1 + ',' + y1;
  return (o);
}

function nodes_path(b1,b2) {
  var x1 = b1.x + b1.width/2; var y1 = b1.y + b1.height/2;
  var x2 = b2.x + b2.width/2; var y2 = b2.y + b2.height/2;
  var h1 = 1 + b1.height/2; var l1 = 1 + b1.width/2;
  var h2 = 1 + b2.height/2; var l2 = 1 + b2.width/2;
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
          var mx = parseInt(t[n].getAttribute('mx')); var my = parseInt(t[n].getAttribute('my'));
          var b = t[n].firstChild.nextSibling.getBBox();
          b.x -= mx; b.y -= my; b.width += 2*mx; b.height += 2*my;
          nodeBox[t[n].id] = b;
          nodePorts[t[n].id] = [];
        } 
      }
      for (var n = 0; n < t.length; n++) {
        if (t[n].nodeName == 'g') {
          var b = nodeBox[t[n].id];
          var shape = t[n].firstChild;
          shape.setAttribute('x',b.x);
          shape.setAttribute('y',b.y);
          shape.setAttribute('width',b.width);
          shape.setAttribute('height',b.height);
          var ports = shape.nextSibling.nextSibling.childNodes;
          var x = 0; var y = 0; 
          var pos = 0;
          for (var i = 0; i < ports.length; i++) {
            if (ports[i].nodeName == 'rect') { 
              var pos = parseInt(ports[i].getAttribute('pos'));
              if (pos<0) { x=b.x-6; y=b.y-3+(pos+100)*b.height/100;
              } else { x=b.x+b.width; y=b.y-3+(100-pos)*b.height/100; }
              ports[i].setAttribute('x',x); ports[i].setAttribute('y',y);
            } 
            if (ports[i].nodeName == 'text') { 
              ports[i].setAttribute('dominant-baseline','middle');
              if (pos<0) { 
                x=b.x+1; y=b.y+(pos+100)*b.height/100;
                nodePorts[t[n].id].push([x-6,y]);
              } else {
                ports[i].setAttribute('text-anchor','end');
                x=b.x+b.width-1; y=b.y+(100-pos)*b.height/100;
                nodePorts[t[n].id].push([x+6,y]);
              }
              ports[i].setAttribute('x',x); ports[i].setAttribute('y',y);
            } 
          }
        }
      }
      var t = $('.connectors').childNodes;
      for ( var n = 0; n < t.length; n++ ) {
        if (t[n].nodeName == 'g') { 
          var d = '';
          if (t[n].hasAttribute('p1')) { 
            var tg1 =  nodePorts[t[n].getAttribute('n1')][t[n].getAttribute('p1')];
            if (t[n].hasAttribute('p2')) { 
              var tg2 =  nodePorts[t[n].getAttribute('n2')][t[n].getAttribute('p2')];
              d = nodes_path2(tg2[0],tg2[1],tg1[0],tg1[1]);
            } else {
              d = nodes_path1(tg1[0],tg1[1],nodeBox[t[n].getAttribute('n2')],true);
            }
          } else {
            if (t[n].hasAttribute('p2')) { 
              var tg2 =  nodePorts[t[n].getAttribute('n2')][t[n].getAttribute('p2')];
              d = nodes_path1(tg2[0],tg2[1],nodeBox[t[n].getAttribute('n1')],false);
            } else {
              d = nodes_path(nodeBox[t[n].getAttribute('n2')],nodeBox[t[n].getAttribute('n1')]);
            }
          }
          t[n].firstChild.setAttribute('d',d);
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

    o += '@font-face { font-family: Graublau; src: url(\'./fonts/GraublauWeb.otf\') format("opentype"); }'
    o += '@font-face { font-family: vag; src: url(\'./fonts/VAG-HandWritten.otf\') format("opentype"); }'
    o += 'text {font-family:vag,helvetica neue,helvetica,arial,sans-serif;}'
    o += 'text.tiny { font-family:helvetica neue,helvetica,arial,sans-serif;font-size: 4pt; fill:DarkSlateGray; }\n'
    o += 'text.node { font-size: 1em; } text:hover { font-weight:bold;} rect.port { stroke-width:0; fill:lightblue; }\n'
    o += 'path:hover, rect:hover { opacity:0.5; cursor:crosshair;}\n'
    o += 'path#logo:hover { opacity:1;}\n'
    for n in m[0]:
        if ln.has_key(n):
            o += 'g.node_%s > text { %s } g.node_%s > rect { %s }\n'%(n,m[0][n][0],n,m[0][n][1]) 
    for e in m[1]:
        if le.has_key(e):
            o += 'g.edge_%s path { %s }\n'%(e,m[1][e]) 
    return o + '</style>\n' + svg_defs() + '\n'

def gettypes(ast):
    ""
    nl,el = {'':True},{'':True}
    Nodes,Edges = ast
    for n in Nodes:
        if len(Nodes[n]) > 2:
            nl[Nodes[n][2]] = True 
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
            refname,cmpname = '%s/%s_ref.%s'%(l,case[0],__OUT_LANG__[l][0]),'%s/%s.%s'%(l,case[0],__OUT_LANG__[l][0])
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
    n,h,uobj = 0,'{\n',u()
    for j,i in __AST_SET__:
        n +=1
        i = re.sub(r'\n','\\\\n',i)
        h += '\t# %03d: %s\n\t\'%s\':\n\t%s,\n\n'%(n,j,i,uobj.parse(i))
    h += '}\n'
    try:
        eval(h)
    except:
        print 'error in parsing !'
    reff = 'ref.txt'
    if ref:
        if os.path.isfile(reff):
            shutil.move(reff,'old_'+reff)
        open(reff,'w').write(h)   
    else:
        if os.path.isfile(reff):
            content = open(reff).read()
            assert content == h

class beamer:
    r"""%% This is generated, do not edit by hands!
\documentclass{beamer}
\usepackage{beamerthemeshadow}
\usepackage{draftwatermark}
\usepackage{listings}
\usepackage{embedfile}
\usepackage{graphicx}
\usepackage{tikz}
"""
    def __init__(self,title,author,email,dat,logo=None):
        r"""\begin{document} 
\frame{\titlepage} \section{\textsc{Draft}} 
"""
        self.src = os.path.basename(sys.argv[0])
        self.tex = beamer.__doc__ + '\n'
        self.tex += r'\embedfile[filespec=%s]{%s}'%(self.src,os.path.abspath(self.src))
        self.tex += r'\title{%s}'%title + '\n'
        self.tex += r'\author{%s\inst{*}}\institute{*%s}'%(author,email) + '\n' + r'\date{%s}'%dat + '\n'
        if os.path.isfile(os.path.abspath(logo)):
            self.tex += r'\pgfdeclareimage[height=.6cm]{logo}{%s}'%os.path.abspath(logo) + '\n' + r'\logo{\pgfuseimage{logo}}' + '\n\n'
        self.tex += beamer.__init__.__doc__ + '\n'

    def gen_tex(self):
        r"""\end{document}"""
        self.tex += beamer.gen_tex.__doc__ + '\n'
        open('beamer_%stex'%self.src[:-2],'w').write(self.tex)

    def frame(self,title,content):
        ""
        self.tex += r'\frame{\frametitle{%s}'%title + '\n'
        self.tex += content + '\n'
        self.tex += '}\n'

    def frame2(self,title,c1,c2):
        ""
        self.tex += r'\frame{\frametitle{%s}'%title + '\n'
        self.tex += r'\begin{columns}[c]' + '\n'
        self.tex += r'\column{1.5in}' + '\n' + c1 + '\n'
        self.tex += r'\column{1.5in}' + '\n' + c2 + '\n'
        self.tex += r'\end{columns}' + '\n'
        self.tex += '}\n'

    def itemize(self,title,tab):
        ""
        self.tex += r'\frame{\frametitle{%s}'%title + '\n'
        self.tex += reduce(lambda y,k: y+r'\item %s'%k+ '\n',tab,r'\begin{itemize}') + r'\end{itemize}' + '}\n'

    def itemize_graph(self,title,tab,g):
        ""
        self.tex += r'\frame{\frametitle{%s}'%title + '\n'
        self.tex += r'\begin{columns}[c]' + '\n'
        self.tex += r'\column{1.5in}' + '\n'
        self.tex += reduce(lambda y,k: y+r'\item %s'%k+ '\n',tab,r'\begin{itemize}') + r'\end{itemize}'
        self.tex += r'\column{1.5in}' + '\n' + g + '\n'
        self.tex += r'\end{columns}' + '\n'
        self.tex += '}\n'

    def gen_pdf(self):
        ""
        self.gen_tex()
        tex = 'beamer_%stex'%self.src[:-2]
        pdf = 'beamer_%spdf'%self.src[:-2]
        if subprocess.Popen(('which','pdflatex'),stdout=subprocess.PIPE).communicate()[0]:
            subprocess.Popen(('cd /tmp; pdflatex -interaction=batchmode %s 1>/dev/null'%os.path.abspath(tex)), shell=True).communicate()
            shutil.move('/tmp/%s'%pdf,pdf) 
        else:
            sys.stderr.write('pdflatex not installed!\n')

def gen_beamer():
    ""
    uobj = u()
    slides = beamer(r'The $\sqcup$ Language',__author__,__email__,'December $7^{th}$ 2011','rcf.png')
    slides.frame(r'What $\sqcup$ is?', r""" 
The $\sqcup$ language is a {\bf Universal Graph Language};\\
\begin{itemize}
\item Symbol: $\bigsqcup$ \\
\end{itemize} 
\begin{itemize}
\item Name: ``square cup''
\item Universality (close to ``u'')
\item Unicode character $\sqcup$: (U+2294)
\end{itemize} 
\begin{itemize}
\item License: \textsc{gpl} v3
\end{itemize} 
""")
    slides.frame('Graph', r"""A {\bf graph}: $G = (V,E) $ \\
where: \\ $$V=\{v_i\} \: \text{and} \: E=\{e_{k}\}$$ ...a set of nodes (vertices) and a set of arcs (edges) between nodes.\\
$$e_{k} = (\{ v_{i_p}\},\{v_{j_q} \})$$ Edge links some origin nodes set to some destination nodes set.\\
$p$ and $q$ are ports references.\\
Rmq: $|i|>1$ or $|j|>1$ for multi-links.\\ 
Some attributes list is attached to each node $v_i$ and each edge $e_k $.""")
    slides.itemize_graph(r'$\sqcup$ \textsc{thonus} features', (r'\textsc{T}-yped',
                                                                r'\textsc{H}-ierachical',
                                                                r'\textsc{O}-nline',
                                                                r'\textsc{N}-eutral',
                                                                r'\textsc{U}-nicode',
                                                                r'\textsc{S}-hort'),
                         uobj.gen_tikz(uobj.parse('T--H T--O T--N T--U T--U T--S H--O H--N H--U H--S O--N O--U O--S N--U N--S U--S'),False)
                         )    
    slides.frame('$\sqcup$ at a glance',r"""\begin{tabular}{ll}
\texttt{Hello -> "World!"} & Hello World! \\
\texttt{A B foo bar}  & Some Nodes \\
\texttt{A->B C<-D -- E} & Nodes with links \\
\texttt{A\{B C \{D\}\}} & Node Composition \\
\texttt{A->\{B C\}} & Mulilinks \\
\texttt{A"This is a node label"} & Labels \\
\texttt{A:T B:Class} & Typed nodes \\
\texttt{A(2,4) B(f 6 8)} & Arguments \\
\texttt{A <Y> B} & Typed arc \\
\texttt{A"name":T(arg) A->B A<-C} & Id reuse \\
\texttt{A.pin2 -> B.5} & Indexed and named ports \\
\end{tabular} 
""")
    slides.frame('The big picture', r"""\usetikzlibrary{shapes.multipart} 
\tikzstyle{node} = [rectangle,rounded corners=3pt,drop shadow,draw=gray!40,fill=gray!20]
\tikzstyle{model} = [rectangle,rounded corners=2pt,drop shadow,draw=gray!40,fill=brown!30]
\tikzstyle{tool} = [rectangle,rounded corners=5pt,drop shadow,draw=gray!40,fill=red!30]
\tikzstyle{graph} = [ellipse,drop shadow,draw=gray!40,fill=green!20]
\tikzstyle{lang} = [ellipse,drop shadow,draw=gray!40,fill=blue!20]
\tikzstyle{edge} = [->,>=latex]
\tikzstyle{dbl} = [->,>=latex,dashed]
\tikzstyle{def} = [->>,dotted]
%\begin{tikzpicture}[auto,node distance=15mm,semithick]
\begin{tikzpicture}[every text node part/.style={align=left}]
\node[node](A) at(1,4){$\sqcup$ concrete \\ syntax \\ string};
\node[node](B) at(5,4){$\sqcup$ abstract \\ syntax \\ structure};
\node[tool](J) at(5,1){$\sqcup$ type \\ checker};
\node[graph](C) at(7.5,6){SVG};
\node[graph](D) at(8.5,5){Tikz};
\node[lang](E) at(9.5,3){Ada};
\node[lang](F)at(10,2){Ocaml};
\node[lang](G)at(9,1){AADL};
\node[lang](H)at(8,0){XXXX};
\node[model](I) at(1,1){UML \\ Simulink \\ KAOS \\...};
\draw[edge](A)to node[auto]{$\sqcup$ parser}(B);
\draw[edge](B)to node[sloped,above]{web. gen.}(C);
\draw[edge](B)to node[sloped,above]{doc. gen.}(D);
\draw[edge](B)to node[sloped,above]{code gen.}(E);
\draw[edge](B)to (F);
\draw[edge](B)to node[sloped,above]{model gen.}(G);
\draw[edge](B)to node[sloped,above]{generator}(H);
\draw[def](I)to node[auto]{use}(A);
\draw[dbl](J)to (B);
\end{tikzpicture}
""")




    slides.frame('The main principles', r"""\begin{block}{Structure}
$\sqcup$ only manage the structure of the graph, not the semantics.\\
$\sqcup$ parser builds an Abstract Syntax Tree (a Python data Structure) \\ Types libraries are doing the real job.
\end{block}
\begin{block}{Rendering}
Graphics rendering is just a matter of code generation. \\ Customize the generator to style your graphs.
\end{block} 
\begin{block}{Pipes}
To generate code, $\sqcup$ uses UNIX like piped small tools on the graph Abstract Syntax Tree.
\end{block} 
""")
    slides.frame('Syntax building blocks',r"""\begin{itemize}
\item for Nodes:
\begin{itemize}
  \item \texttt{ID}: an unicode word to identify the node
  \item \texttt{port}: a named or indexed port (type compatible)
  \item \texttt{label}: a string on possibly several lines separator is simple quote, double quote or triple quotes 
  \item \texttt{type}: Type name available in the node types library 
  \item \texttt{args}: arguments list
\end{itemize} 
\item for Edges:
\begin{itemize}
  \item \texttt{(<>-=)}: Arrow head
  \item \texttt{label}: a string on possibly several lines separator is simple quote, double quote or triple quotes 
  \item \texttt{type}: Type name available in the edge types library 
  \item \texttt{args}: Edge Arguments
  \item \texttt{(<>-=)}: Arrow tail
\end{itemize} 
\item for blocks:
\begin{itemize}
  \item \texttt{\{...\}}: delimiters
\end{itemize} 
\end{itemize} 
""")
    slides.itemize('From the Dot (Graphviz) Language',(r'Dot\footnote{AT\&T Bell Laboratories} is not typed'
                                                       ,'Dot composition (cluster) is not generic'
                                                       ,'Dot ports are not (well) implemented'
                                                       ,r'Dot is not minimal (\texttt{A->B} raises syntax error)'
                                                       ,'Dot mixes structure and layout'
                                                       ,'Limited Dot layout algorithms (nodes place + arc path)'))
    slides.itemize(r'From the XML format',(r'XML is for XHTML what $\sqcup$ is for (UML,Simulink,...)'
                                           ,'XML is basically suited for trees not graphs'
                                           ,'XML has a lot of glue characters'
                                           ,'XML does not enforce id on each elements'
                                           ,'XML use Xlink,Xpath for referencing'
                                           ,'XML raises attribute versus elements dilemma' 
                                           ,'XML is unreadable in practice'
                                           ,'Transformations are complex (XSLT)'
                                           ,'Type checking using DTD,XSL,RelaxNG'))
    slides.frame('$\sqcup$ Types',r"""\begin{itemize}
\item User defines is own types library for:
  \begin{itemize}
  \item Used Nodes \item Used Edges
  \end{itemize}
\end{itemize} 
The types library:
\begin{itemize}
\item defines the semantics of the input formalism (UML,Scade,...)\\
\item maps to output patterns (Ada,SVG,...)\\
\item defines a {\bf Domain Specific Language}
\item customize graphic output
\end{itemize} 
Two different nodes types may rendered with different shapes/decorations in SVG but may maps to the same class construction for Python generation.
""")
    slides.frame('Semi-Formal and Formal',r"""
A semi-formal node is a typed node with informal (english) sentence in its label.\\
A formal node is a types node with all attributes valid stream from formal languages.\\
The label may be used to embedd procedure, function, class definition on several lines. \\
The arguments may be used call, customize or instantiate. \\
The type definition may include default code.
""")
    slides.frame('Overload nodes rules',r"""
\begin{block}{rules:}
Node Definition and Node Usage are identical!\\
Node,Edges accumulates properties:
\end{block}
\begin{tabular}{lcl}
\texttt{A"hello" A:T}        &$\equiv$&  \texttt{A"hello":T}\\
\texttt{A"hello" A->B}       &$\equiv$&  \texttt{A"hello"->B}\\
\texttt{A->B A"hello"}       &$\equiv$&  \texttt{A"hello" A->B}\\
\texttt{A B C A}             &$\equiv$&  \texttt{C A B}\\
\texttt{A"label1" A"label2"} &$\equiv$&  \texttt{A"label2"}\\
\texttt{A(arg1) A(arg2)}     &$\equiv$&  \texttt{A(arg2)}\\
\texttt{A:T1 A:T2}           &$\equiv$&  \texttt{A:T2}\\
\texttt{A\{A\}}              &$\equiv$& \texttt{A}  \\
\end{tabular}""")
    slides.frame('Edge rules',r"""
\begin{block}{rule:}
Edges have no ids!
\end{block}
\begin{tabular}{lcl}
\texttt{A->B A->B}           &$\neq$&  \texttt{A->B}\\
\texttt{A -x> B A -y> B}     &$\neq$&  \texttt{A -y> B A -y> B}\\
\texttt{A -(1)> B A -(2)> B} &$\equiv$&  \texttt{A -(2)> B A -(1)> B}\\
\texttt{A -(1)> B A -(2)> B} &$\neq$&  \texttt{A -(2)> B A -(2)> B}\\
\end{tabular}\\
16 possible arrow types for each edge type \texttt{X}:
\begin{tabular}{|c|c|c|c|}
\hline \texttt{-X>} & \texttt{=X>} & \texttt{>X>} & \texttt{<X>}  \\
\hline \texttt{-X<} & \texttt{=X<} & \texttt{>X<} & \texttt{<X<}  \\ 
\hline \texttt{-X-} & \texttt{=X-} & \texttt{>X-} & \texttt{<X-}  \\ 
\hline \texttt{-X=} & \texttt{=X=} & \texttt{>X=} & \texttt{<X=}  \\ \hline
\end{tabular}""")
    slides.itemize('Candidate model formalisms',__IN_MODEL__)
    slides.itemize('Expected code generation',__OUT_LANG__)
    slides.frame('Graphic generation',r"""
\begin{itemize}
\item SVG for Web publishing
\item Tikz for \TeX{} and \textsc{pdf} exporting
\end{itemize}
Layout (nodes placement and edge path) does not carry semantics\\
Do not let end-user define it, let advanced algorithms do the layout with goals:
\begin{itemize}
\item Balance nodes in the canvas
\item Minimize edge crossing
\item Find best path for edges
\item Follow graphic design rules 
\item Follow Typographic rules
\end{itemize}
The same graph may have several styles (Themes) \\
\TeX{}principle: nice graphic output is a requirement !
""")
    slides.frame(r'Needs',r"""\begin{itemize}
\item A theoretical support
\item A constraint definition language (Real,OCL,...)
\item A better types definition (currently dictionnary of properties)
\item A support for many code generators
\item An embedded and large test set
\item Plugins for formal model checkers and theorem provers.
\end{itemize}
""")
    slides.frame('Next about $\sqcup{}$!',r"""\begin{block}{All is on the forge:} \url{https://%s} \end{block} 
\begin{block}{Source code:} See PDF attached file:u.py \\ and generate this beamer  \end{block}"""%__url__)
    slides.gen_pdf()

def post(server, service, content):
    B,CRLF = '----------ThIs_Is_tHe_bouNdaRY_$','\r\n'
    body = CRLF.join(['--%s'%B,'Content-Disposition: form-data; name=""; filename=""','Content-Type: text/html','',content,'--%s--'%B,''])
    h = httplib.HTTP(server)
    h.putrequest('POST', service)
    h.putheader('content-type', 'multipart/form-data; boundary=%s'%B)
    h.putheader('content-length', str(len(body)))
    h.endheaders()
    h.send(body)
    h.getreply()
    return h.file.read()

if __name__ == '__main__':
    "Run the module or use it as WSGI application with an Apache server"
    try:
        subprocess.Popen(['dot'], stdout=subprocess.PIPE,stdin=subprocess.PIPE).communicate(input='digraph G {A->B}')
    except:
        print '...Error: please install \'graphviz\' package !'
        sys.exit()

    import doctest
    doctest.testmod()
    #code_gen_test(True)
    ast_test(True)
    gen_apache_conf()
    gen_doc()
    gen_beamer()
    gen_readme()

    ########## debug zone! ##############
    # test multilinks
    x = 'H->T->O->U->S->H->O->S'
    uobj = u()
    #print uobj.gen_svg(uobj.parse(x))


    # 1 - test unicode
    #s = u' AA ⊔A A⊔ C您'
    #for m in re.compile(r'\s*(\w+)\s*',re.U).finditer(s):
    #    print m.groups()
    
    # 1 - test getopt 
    import getopt
    opts, args = getopt.getopt(sys.argv[1:],'hf:',['help','format'])
    o = 'raw'
    for r in opts:
        if r[0] in ('-h','--help'):
            print help('u')
        elif r[0] in ('-f','--format'):
            o = r[1]
    for arg in args:
        if os.path.isfile(arg):
            server = '127.0.0.1' # use '193.84.73.209'
            print post(server, '/u?%s'%o, open(arg).read())

    # 4 - test quotes
    s = '\'A \\\'B\\\'C\' "D\\"E\\"F" """ foo """ bar """ zzz """ \'\'\' foo \'\'\' bar \'\'\' zzz \'\'\'  '
    for m in re.compile(r"""(
"{3}.*?"{3}|            # triple double quote
'{3}.*?'{3}|            # triple simple quote
"(?:(?:[^"\\]|\\.)*)"|      # double quote
'(?:(?:[^'\\]|\\.)*)'       # simple quote
)""",re.U|re.X).finditer(s): 
        a = m.group(1) 
        b = a[1:-1] if not  a[:3] in ('"""',"'''") else a[3:-3]
        tab = [re.sub(r'\\','',b)]

    for m in re.compile(r"""
"{3}(.*?)"{3}|            # triple double quote
'{3}(.*?)'{3}|            # triple simple quote
"((?:[^"\\]|\\.)*)"|      # double quote
'((?:[^'\\]|\\.)*)'       # simple quote
""",re.U|re.X).finditer(s): 
        pass

# the end
