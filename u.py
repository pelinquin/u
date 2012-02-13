#!/usr/bin/python
# -*- coding: utf-8 -*-

# Welcome to ⊔ [SquareCup]! See https://github/pelinquin/u
#-----------------------------------------------------------------------------
#  © Copyright 2012 Rockwell Collins, Inc 
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
# Warning ! There is a small bug in Emacs editor default font: swap ⊔ 'squarecap' (U+2293) and ⊓ 'squarecup' (U+2294) char!   

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
__version__ = '0.2'
__license__ = 'GPLv3'
__url__     = 'github/pelinquin/u'

import os,sys,re,hashlib,shutil,subprocess,urllib,datetime,httplib,base64

__digest__ = base64.urlsafe_b64encode(hashlib.sha1(open(__file__).read()).digest())[:5]

# Rmq: Those following global data may be stored in a database (sql or berkeley)
#      allowing to customize the code generators

__separator__ = r'[|\'`";,!~^@*+/$]' # 14 chars
__delimiter__ = r'%s(?:%s%s)?'%(__separator__,__separator__,__separator__) # one or three chars
__RE_U__ = r'''     # RegExp (nodes and edges)
   (?:              # NODE:  
    (?=[^\s<\-=>])  # Not empty token 
    (?:(\w+)|)      # Name      
    (?::(\w)|)      # Type pre  
    ((%s)(.+?)\4|\(([^)]+)\)|) # Content
    (\w|)           # Type post 
    (?:\.(\w+|\*)|) # Port      
   |                # or EDGE:  
    ([<\-=>])       # Head      
    (\w|)           # Type pre  
    ((%s)(.+?)\12|\(([^)]+)\)|) # Content
    (\w|)           # Type post
    ([<\-=>])       # Tail
)'''%(__delimiter__,__delimiter__)

__RE_FILTER__ = [
    (r'(?m)\#.*$',''),            # remove comments
    (r'\n',r'\\n'),
    (r'(?m)\s*$',''),             # right strip 
    (r'(?m)^\s*',''),             # left strip
    (r'\s*[\}\]]\s*', '\'], \''), # add quote and open bracket
    (r'\s*[\{\[]\s*', '\', [\''), # add quote and closing bracket
    (r'^(.*)$', '[\'\\1\']'),     # add start and end line brackets
    (r'\'\',\s*',''),             # remove left empty elements
    (r',\s*\'\'','')]             # remove right empty elements

__RE_FILTERu__ = [
    (r'(?m)\#.*$',''),            # remove comments
    (r'\n',r'\\n'),
    (r'(?m)\s*$',''),              # right strip 
    (r'(?m)^\s*',''),              # left strip
    (r'\s*[\}\]]\s*', '\'], u\''), # add quote and open bracket
    (r'\s*[\{\[]\s*', '\', [u\''), # add quote and closing bracket
    (r'^(.*)$', '[u\'\\1\']'),     # add start and end line brackets
    (r'\'\',\s*',''),              # remove left empty elements
    (r',\s*\'\'','')]              # remove right empty elements
# TBC: factorize Regexp for ascii and unicode

_XHTMLNS  = 'xmlns="http://www.w3.org/1999/xhtml"'
_SVGNS    = 'xmlns="http://www.w3.org/2000/svg"'
_XLINKNS  = 'xmlns:xlink="http://www.w3.org/1999/xlink"'

__OUT_LANG__ = {'c'          :['c'   ,('/*'  ,'*/' ,'')],
                'objectivec' :['m'   ,('/*'  ,'*/' ,'')],
                'python'     :['py'  ,('#'   ,''   ,'#!/usr/bin/python\n# -*- coding: utf-8 -*-\n')],
                'ada'        :['adb' ,('--'  ,''   ,'')],
                'scala'      :['scl' ,('--'  ,''   ,'')],
                'java'       :['java',('//'  ,''   ,'')],
                'ruby'       :['rb'  ,('#'   ,''   ,'')],
                'ocaml'      :['ml'  ,('(*'  ,'*)' ,'')],
                'haskell'    :['hs'  ,('{-'  ,'-}' ,'')],
                'lua'        :['lua' ,('--'  ,''   ,'')],
                'tikz'       :['tex' ,('%'   ,''   ,'')],
                'svg'        :['svg' ,('<!--','-->','<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n')],
                'aadl'       :['adl' ,('--'  ,''   ,'')],
                'sdl'        :['sdl' ,('--'  ,''   ,'')],
                'lustre'     :['lst' ,('--'  ,''   ,'')],
                'vhdl'       :['hdl' ,('--'  ,''   ,'')],
                'systemc'    :['sc'  ,('//'  ,''   ,'')]}
 
__DATA_ports__ = {
    'T': ('i','o'),
    'O': ('in1','in2','out1','out2'),
    'C': ('p1','p2','p3','p4','p5','p6','p7','p8','p9','p10','p11','p12','p13','p14','p15','p16'),
    'D': ('pin1','pin2','pin3','pin4','pin5','pin6'),
    'x': ('pin1','pin2','pin3','pin4','pin5','pin6'),
    }

__DATA_svg__ = ({
        '':  ('node','fill:black;','rect|-5,40,5','fill-opacity:.1;', 10, 10),
        'Z': ('node','fill:black;','path|-4,0,0','stroke:green;stroke-width:1;fill:none;', 2, 2),
        'E': ('node','fill:black;','ellipse','stroke:green;stroke-width:1;fill:gray;', 20, 20),
        'F': ('node','fill:black;','rect','filter:url(#.shadow);stroke:green;stroke-width:1;fill:gray;fill-opacity:.1;', 5, 50),
        'm': ('node','fill:black;','rect','filter:url(#.shadow);fill-opacity:.1;', 4, 4),
        'T': ('node','fill:red;','rect','fill:blue;fill-opacity:.6;', 8, 18),
        'R': ('node','fill:red;font-family:helvetica,arial,sans-serif;','rect','fill:none;stroke:black;stroke-width:1;',0,0),
        'O': ('node','fill:blue;','rect','filter:url(#.shadow);fill-opacity:.1;', 30, 30),
        'C': ('class','fill:blue;','rect','filter:url(#.shadow);fill-opacity:.1;', 30, 60),
        'c': ('class','fill:blue;','rect|0,4,4','fill-opacity:.1;stroke:gray;stroke-width:.5;', 2, 0),
        'D': ('node','fill:blue;','rect','filter:url(#.shadow);fill-opacity:.1;', 10, 50),
        'x': ('node','fill:blue;','rect','fill:blue;fill-opacity:.2;', 10, 50),
        'd': ('node','fill:blue;','rect','filter:url(#.shadow);fill-opacity:.1;', 10, 50),
        'p': ('place','fill:black;','circle','fill:green;fill-opacity:.3;', 10, 2),
        't': ('transition','fill:white;','rect','fill:black;fill-opacity:.8;', 2, 40),
        },{
        '' : 'stroke:black; stroke-width:1.5; fill:none; marker-end:url(#.arrow);',
        'r': 'stroke:black; stroke-width:1.5; fill:none; marker-start:url(#.r_arrow);',
        'I': 'stroke:green; stroke-width:2; fill:none; marker-end:url(#.arrow);',
        'L': 'stroke:red; stroke-width:3; fill:none; marker-end:url(#.arrow);',
        })


__DATA_tikz__ = ({
        '':  ('node','circle,drop shadow,draw=green!40,fill=gray!20'),
        'S': ('component','rectangle,draw=black!40,fill=gray!10'),
        'T': ('component','circle,drop shadow,draw=green!40,fill=gray!20'), 
        'O': ('node','rectangle,drop shadow,rounded corners=3pt,draw=red!40,fill=blue!25'),
        't': ('node','rectangle,rounded corners=5pt,drop shadow,draw=gray!40,fill=red!30'),
        'n': ('node','rectangle,rounded corners=3pt,drop shadow,draw=gray!40,fill=gray!20'),
        'm': ('node','rectangle,rounded corners=2pt,drop shadow,draw=gray!40,fill=brown!30'),
        'g': ('node','ellipse,drop shadow,draw=gray!40,fill=green!20'),
        'l': ('node','ellipse,drop shadow,draw=gray!40,fill=blue!20'),
        'a': ('node','ellipse,drop shadow,draw=gray!40,fill=blue!20'),
        'b': ('node','rectangle,drop shadow,draw=gray!40,fill=blue!20'),
        'c': ('node','diamond,drop shadow,draw=gray!40,fill=blue!20'),
        'd': ('node','regular polygon,regular polygon sides=5,drop shadow,draw=gray!40,fill=blue!20'),
        'e': ('node','regular polygon,regular polygon sides=6,drop shadow,draw=gray!40,fill=blue!20'),
        'f': ('node','regular polygon,regular polygon sides=8,drop shadow,draw=gray!40,fill=blue!20'),
        },{
        '':  '--',
        'I': '->,>=open diamond',
        'L': '->,>=triangle 60',
        'r': '--',
        's': '->,>=latex',
        'S': '->,>=latex',
        'e': '->,>=latex',
        'l': '->,>=latex,dashed',
        'd': '->>,dotted',
        })

__DATA_c__ = ({
        'C': ('class',),
        'c': ('class',),
        'H': ('class',),
        'h': ('class',),
        'm': ('main',),
        'e': ('extern',),
        'E': ('extern',),
        },{
        '':(),
        })

__DATA_python__ = ({
        'C': ('class',),
        'c': ('class',),
        },{
        '':(),
        })

__DATA_objectivec__ = ({'': (),},{'':(),})
__DATA_ada__        = ({'': (),},{'':(),})
__DATA_scala__      = ({'': (),},{'':(),})
__DATA_java__       = ({'': (),},{'':(),})
__DATA_ruby__       = ({'': (),},{'':(),})
__DATA_ocaml__      = ({'': (),},{'':(),})
__DATA_haskell__    = ({'': (),},{'':(),})
__DATA_lua__        = ({'': (),},{'':(),})
__DATA_aadl__       = ({'': (),},{'':(),})
__DATA_sdl__        = ({'': (),},{'':(),})
__DATA_lustre__     = ({'': (),},{'':(),})
__DATA_vhdl__       = ({'': (),},{'':(),})
__DATA_systemc__    = ({'': (),},{'':(),})

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
    ('OnlyContent'            ,'"L"'),
    ('OnlyType'               ,':T'),
    ('OnlyChild'              ,'{a}'),
    ('Id+Content'             ,'A"L"'),
    ('Id+Type'                ,'A:T'),
    ('Id+Child'               ,'A{a}'),
    ('Content+Type'           ,'"L"T'),
    ('Type+Content'           ,':T"L"'),
    ('Content+Child'          ,'"L"{a}'),
    ('Type+Child'             ,':T{a}'),
    ('Id+Content+Type'        ,'A"L"T'),
    ('Id+Type+Content'        ,'A:T"L"'),
    ('Id+Content+Child'       ,'A"L"{a}'),
    ('Content+Type+Child'     ,'"L"T{a}'),
    ('Type+Content+Child'     ,':T"L"{a}'),
    ('Id+Type+Child'          ,'A:T{a}'),
    ('Id+Content+Type+Child'  ,'A"L"T{}'),
    ('Id+Type+Content+Child'  ,'A:T"L"{}'),
    #("Delimiter:'"            ,"Z'content'"),
    ('Delimiter:"'            ,'B"content"'),
    ('Delimiter:`'            ,'C`content`'),
    ('Delimiter:;'            ,'D;content;'),
    ('Delimiter:,'            ,'E,content,'),
    ('Delimiter:!'            ,'F!content!'),
    ('Delimiter:~'            ,'G~content~'),
    ('Delimiter:^'            ,'H^content^'),
    ('Delimiter:@'            ,'I@content@'),
    ('Delimiter:*'            ,'J*content*'),
    ('Delimiter:+'            ,'K+content+'),
    ('Delimiter:/'            ,'L/content/'),
    ('Delimiter:$'            ,'M$content$'),
    ('Delimiter:()'           ,'N(content)'),
    ('Word Id'                ,'Aaa'),
    ('Not first digit'        ,'A1'),
    ('First digit'            ,'1A'),
    ('white space'            ,' A'),
    ('end white space'        ,'A '),
    ('both white spaces'      ,' A '),
    ('several lines'          ,r'\n\nA\n\n'),
    ('latin1 char'            ,'éàùç'),
    #('Unicode char'           ,u'A:您'),
    #('Unicode char'           ,u'B:好'),
    ('Long content'           ,'"This is a long content" '),
    ('Multilines'             ,r'"Multi\nlines\ncontent" '),
    ('Types'                  ,'A:T B:U'),
    ('2 nodes'                ,'A B'),
    ('3 nodes'                ,'A B C'),
    ('4 nodes'                ,'A B C D'),
    ('Overload content'       ,'A"content1" A"content2"'),
    ('Accumulation1'          ,'A A"content"'),
    ('Accumulation2'          ,'A"content" A'),
    ('TypeContentAcc'         ,'A:T A"content"'),
    ('ContentTypeAcc'         ,'A"content" A:T'),
    ('ChildContentAcc'        ,'A{a} A"content"'),
    ('ContentChildAcc'        ,'A"content" A{a}'),
    (''                       ,'A A"content1" A"content2"(x)'),
    (''                       ,'A"content2"(x) A"content" A'),
    (''                       ,'"content"T1 "content"T2'),
    (''                       ,'A"content" B"content"'),
    (''                       ,'A{a} B{b1 b2} C{c1 c2 c3}'),
    (''                       ,'A{a} B {b} {c}'),
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
    (''                       ,r'#comment\nA'),
    (''                       ,r' #comment1\nA\n #comment2'),
    ('Chain2'                 ,'A->B'),
    ('Chain3'                 ,'A->B->C'),
    ('Link1'                  ,'A>>B'), 
    ('Link2'                  ,'A><B'),
    ('Link3'                  ,'A>-B'),
    ('Link4'                  ,'A>=B'),
    ('Link5'                  ,'A<>B'),
    ('Link6'                  ,'A<<B'),
    ('Link7'                  ,'A<-B'),
    ('Link8'                  ,'A<=B'),
    ('Link9'                  ,'A->B'),
    ('Link10'                 ,'A-<B'),
    ('Link11'                 ,'A--B'), 
    ('Link12'                 ,'A-=B'),
    ('Link13'                 ,'A=>B'), 
    ('Link14'                 ,'A=<B'),
    ('Link15'                 ,'A=-B'),
    ('Link16'                 ,'A==B'),
    ('SpaceBeforeEdge'        ,'A ->B'),
    ('SpaceAfterEdge'         ,'A-> B'),
    ('Spaces'                 ,'A -> B'),
    ('Autoref'                ,'A->A'),
    (''                       ,'A -(content)- B'),
    (''                       ,'A -T- B'),
    (''                       ,'A -T(content)- B'),
    (''                       ,'A -(content)T- B'),
    (''                       ,'A{a1 a2} -> B{b1 b2}'),
    (''                       ,'{a1 a2} -> B{b1 b2}'),
    (''                       ,'A{a1 a2} -> {b1 b2}'),
    (''                       ,'{a1 a2} -> {b1 b2}'),
    (''                       ,'A{a1 -> a2} B{b1 -> b2}'),
    (''                       ,'A{a1 -> a2} -> B{b1 -> b2}'),
    (''                       ,'A.1 -> B.2'),
    (''                       ,'A.por1 -> B.por2'),
    (''                       ,'A:T B:U A.1->B.2'),
    (' '                      ,'A.1->B.2 A:T B:U'),
    ('Port0'                  ,'A.0->B.pin1->C.*->D'),
    ('Port1'                  ,'A:T.0->B:T.1'),
    ('Port2'                  ,'A:T.1->{B:T.0 C:T.0}'),
    ('Port3'                  ,'{A:T.1 B:T.0}->C:T.1'),
    ('Port4'                  ,'{A:T.1 B:T.0}->{C:T.1 D:T.0}'),
    ('Port5'                  ,'A:T.1->B:T.0->C:T.1'),
    ('Double definition'      ,'A{a} A{b}'),
]

def find_id(x):
    "find Node id "
    if x[0]: # name
        return x[0]
    else:
        if x[2]: # content
            return re.sub(r'\W','',x[2])
        else: # type
            return '__%s'%x[1]

class u:
    """ This is the base class for ⊔ 
    One can customize that class by adding/modifying __DADA_xxx__ structure or by overloading a gen_xxx() method
    """

    def __init__(self):
        "Load types mapping"
        self.m = {}
        for l in __OUT_LANG__:
            self.m[l] = eval('__DATA_%s__'%l)
            
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

    def parse_raw(self,x):
        "returns 10uple (Name,Type,Content,Port,Head+Tail,Type,Content)"
        res = []
        for m in re.compile(__RE_U__,re.U|re.X|re.S).finditer(x):
            tn = m.group(7) if m.group(7) else m.group(2)
            te = m.group(15) if m.group(15) else m.group(10) if m.group(10) else None
            ln = m.group(5) if m.group(5) else m.group(6) if m.group(6) else None
            le = m.group(13) if m.group(13) else m.group(14) 
            arrow = m.group(9)+m.group(16) if m.group(9) and m.group(16) else None
            res.append((m.group(1),tn,ln,m.group(8),arrow,te,le))
        return res

    def addedge(self,c,x,y,po):
        """ """
        edges = []
        if c:
            for i in x:
                ti = map(lambda k:i+'.%s'%k,po[i]) if po.has_key(i) else [i] 
                for j in y:
                    tj = map(lambda k:j+'.%s'%k,po[j]) if po.has_key(j) else [j] 
                    for p in ti:
                        for q in tj:
                            edges.append(strip2((p,c[0],q,c[1],c[2])))
        return edges

    def parse(self,x):
        return self.parse1(x,{})

    def parse1(self,x,po):
        """ """
        nodes,edges,c,o,par = {},[],None,[],[]
        if type(x).__name__ == 'str':
            x = eval(reduce(lambda y,k: re.sub(k[0],k[1],y,re.S),__RE_FILTER__,x))
        elif type(x).__name__ == 'unicode':
            x = eval(reduce(lambda y,k: re.sub(k[0],k[1],y,re.S),__RE_FILTERu__,x))
        for t in x:
            if type(t).__name__ == 'list':
                n,e = self.parse1(t,po)
                for k in n:
                    nodes[k] = self.merge(nodes[k],n[k]) if nodes.has_key(k) else n[k]
                edges += e
                m = n.keys()
                if c == None and par:
                    nodes[o[0]] = self.merge(nodes[o[0]],[m]) if nodes.has_key(o[0]) else (m)
                else:
                    edges += self.addedge(c,o,m,po)
                    c = None
                    #c,po = None,{} # check if needed !
                if not par:
                    o = m
                par = []
            else:
                for a in self.parse_raw(t):
                    if a[4]: 
                        c,par = a[4:],[]
                    else:
                        nid,attr = find_id(a[:3]),strip3(([],a[1],a[2]))
                        nodes[nid] = self.merge(nodes[nid],attr) if nodes.has_key(nid) else attr
                        if a[3]:
                            t = nodes[nid][1] if nodes.has_key(nid) and len(nodes[nid]) > 1 and nodes[nid][1] != None else a[1]
                            if __DATA_ports__.has_key(t):
                                if (re.match(r'^\d+$',a[3]) and int(a[3]) < len(__DATA_ports__[t])) or  a[3] in __DATA_ports__[t]:
                                    po.setdefault(nid,[]).append(a[3])
                                elif a[3] == '*':
                                    po[nid] = __DATA_ports__[t]
                        if c:
                            edges += self.addedge(c,o,[nid],po)
                            #c,po = None,{}
                            c = None
                        o,par = [nid],[nid]
        return nodes,edges

    def hf(self,appli,host='127.0.0.1'):
        "Add header and footer to generated code"
        lang = re.sub('gen_','',appli.__name__)
        com = __OUT_LANG__[lang][1]
        (sc,ec,head) = com
        def app(ast):
            Nodes,Edges = ast
            d = '%s'%datetime.datetime.now()
            o = '%s%s ⊔ Generated Code [%s] %s\n'%(head,sc,d[:19],ec)
            tt = os.times()
            o += '%s CPU Times: %27s %s\n'%(sc, map(lambda k:round(k,1),tt[:-1]),ec) 
            o += '%s Host: %32s %s\n'%(sc,host,ec)            
            o += '%s ******** Do not edit by hand! ******** %s\n'%(sc,ec)
            o += '%s Base64 short sha1 digest: %12s %s\n'%(sc,__digest__,ec)
            o += '%s Forge:  https://github.com/pelinquin/u %s\n'%(sc,ec)
            o += '%s © Copyright 2012 Rockwell Collins, Inc %s\n'%(sc,ec)
            o += '%s ** GNU General Public License  (v3) ** %s\n'%(sc,ec)
            dast = '%s %s'%ast
            if re.search(r'\-{2}',dast):
                o += '\n%s Doubledash replaced by double underscore !%s\n'%(sc,ec)        
                dast = re.sub(r'\-\-','__','%s'%dast)
            o += '\n%s AST = %s %s\n'%(sc,dast,ec)
            #o += '%s Nodes = {%s\n'%(sc,ec)
            #for x in Nodes:
            #    o += '%s %10s: %60s, %s\n'%(sc,x,Nodes[x],ec)
            #o += '%s } %s\n'%(sc,ec)
            #o += '%s Edges = [%s\n'%(sc,ec)
            #for x in Edges:
            #    o += '%s %s, %s\n'%(sc,x,ec)
            #o += '%s}%s\n'%(sc,ec)
            a = appli(ast)
            nt,et = gettypes(ast)
            for n in nt:
                if n and __DATA_ports__.has_key(n):
                    o += '%s Node type:"%s" Ports: %s %s\n'%(sc,n,__DATA_ports__[n],ec)
            o += '\n%s Types parameters: %s %s\n'%(sc,self.m[lang],ec) + a
            return o + '\n%s %s Nodes %s Edges %s Lines | The end of file %s'%(sc,len(Nodes),len(Edges),len(a.split('\n'))+13,ec)
        return app

    def toposort(self,edges,classT):
        "returns topologic sort"
        #def tsort(d):
        #    while True:
        #        ordered = set(item for item, dep in d.items() if not dep)
        #        if not ordered: break
        #        yield ','.join(sorted(ordered))
        #        d = { item: (dep - ordered) for item,dep in d.items() if item not in ordered }
        #    if d: # cycle!
        #        yield 
        data = {}
        for e in edges:
            n1,n2 = re.sub(r'\..*$','',e[0]),re.sub(r'\..*$','',e[2])
            data.setdefault(n1,[]).append(n2)
            if not data.has_key(n2): data[n2] = []
        res = []#z for z in tsort({i:set(data[i]) for i in data})]
        res.reverse()
        return res

    def gen_raw(self,ast):
        ""
        return ''
        

    def gen_c(self,ast):
        "/* C default code generator */\n"
        m,classT,externT,allT,o,hasclass = self.m['c'],[],[],[],'',False
        for t in m[0]:
            if len(m[0][t])>0: 
                if m[0][t][0] == 'class':
                    classT.append(t)
                    allT.append(t)
                elif m[0][t][0] == 'extern':
                    externT.append(t)
                    allT.append(t)
        Nodes,Edges = ast
        for x in Nodes:
            if Nodes[x] and len(Nodes[x]) > 1 and Nodes[x][1] in classT:
                 hasclass = True 
        if hasclass:
            o += '#pragma once\n'
        seq,head,body = self.toposort(Edges,allT),'',''
        #o += '// %s \n'%seq
        for x in Nodes:
            if Nodes[x] and len(Nodes[x]) > 1 and Nodes[x][1] in externT:
                for i in Nodes[x][2].split('|') if len(Nodes[x])>2 else []:
                    body += 'extern void %s(void*);\n'%i
            if Nodes[x] and len(Nodes[x]) > 1 and Nodes[x][1] in classT:
                t = [] if len(Nodes[x]) < 3 else Nodes[x][2].split('|')
                if len(t) > 1:
                    body += '\n/* Node: %s */\n'%x
                    body += 'typedef struct %s {\n'%t[0]
                    for z in t[1].split(','):
                        ti = z.split()
                        body += '  %s;\n'%z if len(ti)>1 else '  int %s;\n'%z 
                        if len(ti)>1:
                            head += '#include "%s.h"\ntypedef struct %s %s;\n'%(ti[0],ti[0],ti[0])
                    body += '} struct_%s;\n'%t[0]
                if len(t) > 2:
                    body += '%s* %s_init(void);\n'%(x,x)
                    body += 'void %s_delete(%s *self);\n'%(x,x)
                    for z in t[2].split(','):
                        body += 'void %s(%s *self);\n'%(z,x) 
        if seq:     
            body += '#include "mid.h"\n\nvoid u_main(void) {\n'
            for x in seq:
                for l in x.split(','):
                    for i in Nodes[l][2].split('|') if len(Nodes[l])>2 else []:
                        body += '  %s;\n'%i
                        if not re.match(r'\s*(char|int|float|double|.*\(|.*=)',i):
                            head += '#include "%s.h"\n'%i.split()[0] 
            body += '}\n'
        return self.gen_c.__doc__ + o + head + body 

    def gen_python(self,ast):
        "## Python 2.7"
        m,classT,mainT,o = self.m['python'],[],[],'\n\n'
        for t in m[0]:
            if len(m[0][t])>0 and m[0][t][0] == 'class':
                classT.append(t)
        Nodes,Edges = ast
        for x in Nodes:
            if Nodes[x]:
                if len(Nodes[x]) > 1 and Nodes[x][1] in classT:
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

    def gen_objectivec(self,ast):
        "/* Objective-C*/\n"
        o,m = '',self.m['objectivec']
        return self.gen_objectivec.__doc__ + o 

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

    def gen_tikz(self,ast,standalone=True,rx=2,ry=2,ori='LR'):
        "% Generated from ⊔ AST:\n"
        o,m = '',self.m['tikz']
        if standalone:
            o += r'\documentclass[a4paper]{article} \usepackage{tikz}' + '\n'
            o += r'\begin{document}' + '\n'
        pos = layout(ast[0],ast[1],ori)
        Nodes,Edges = ast 
        m = self.m['tikz']
        #o += gen_tikz_header(m,gettypes(ast)) + r'\begin{tikzpicture}[auto,node distance=15mm,semithick]'+ '\n'
        o += gen_tikz_header(m,gettypes(ast)) + r'\begin{tikzpicture}[every text node part/.style={align=left}]'+ '\n'
        for n in pos:
            name = n
            label = name if (len(Nodes[n])<3 or not Nodes.has_key(n) or (Nodes[n][2] == None)) else Nodes[n][2]
            shape = 'node' if (len(Nodes[n])<2 or not Nodes.has_key(n)) else 'node%s'%Nodes[n][1]
            (x,y) = (pos[n][0]*rx/25,pos[n][1]*ry/25)
            o += '\\node[%s](%s) at (%0.3f,%0.3f) {%s};'%(shape,name,x,y,label) + '\n'
            # ports
            t = '' if (len(Nodes[n])<2 or not Nodes.has_key(n)) else Nodes[n][1]
            tt = __DATA_ports__[t] if __DATA_ports__.has_key(t) else [] 
            if tt:
                delta,p = 360/len(tt),-180
                for i in tt:
                    o += r'\draw (%s.%s) node{\tiny{%s}};'%(n,p,i) + '\n'
                    p += delta
        for e in Edges:
            #boucle = '[bend left]'
            boucle = '[loop right]' if e[0] == e[2] else ''  
            typ = 'edge' if len(e)<4 or e[3] == None else 'edge%s'%e[3]
            label = '' if len(e)<5 or e[4] == None else 'node[font=\\small,sloped,above]{%s}'%e[4] 
            label = re.sub('@',r'\\',label)
            o += r'\draw[%s](%s) to%s %s(%s);'%(typ,e[0],boucle,label,e[2]) + '\n'
        o += r'\end{tikzpicture}'+ '\n'
        if standalone:
            o +=  r'\end{document}'
        return self.gen_tikz.__doc__  + o 

    def gen_svg(self,ast,boxes={}):
        """ SVG with Javascript AJAX to get bounding boxes"""
        m,classT = self.m['svg'],[]
        for t in m[0]:
            if len(m[0][t])>0 and m[0][t][0] == 'class':
                classT.append(t) 
        Nodes,Edges = ast
        pos,ratio = layout(Nodes,Edges,'LR'),4
        o = '<svg %s>\n'%_SVGNS + gen_svg_header(m,gettypes(ast),True if boxes else False)
        if boxes: 
            o += '<title id=".title">%s</title>\n'%__title__ + get_favicon() + get_logo() 
            o += include_js_pan()
        else:
            o += include_js()
        o += '<g>\n' 
        o += '<g id=".nodes">\n' 
        portsPos,ports = {},{}
        for n in pos:
            t = '' if not (Nodes.has_key(n) and (len(Nodes[n])>1) and m[0].has_key(Nodes[n][1])) else Nodes[n][1]
            mx,my = m[0][t][4],m[0][t][5]
            style = 'node' if (not Nodes.has_key(n) or len(Nodes[n])<2) else 'node%s'%Nodes[n][1]
            o += '<g id="%s" class="%s"'%(n,style)
            if boxes.has_key(n):
                o += '>'
                a,sty = boxes[n],m[0][t][2].split('|')
                if len(sty)>1:
                    skewx,rx,ry = sty[1].split(',')
                else:
                    skewx,rx,ry = 0,0,0
                o += '<g transform="translate(%s,%s) skewX(%s)">'%(a[0]+a[2]/2,a[1]+a[3]/2,skewx)
                if m[0][t][2][0] == 'r':
                    o += '<rect x="%s" y="%s" width="%s" height="%s" rx="%s" ry="%s"/>'%(-a[2]/2,-a[3]/2,a[2],a[3],rx,ry)
                elif m[0][t][2][0] == 'p':
                    o += '<path d="M%s,%sL%s,%sL%s,%sL%s,%sZ"/>'%(-a[2]/2,-a[3]/2,a[2]/2,-a[3]/2,a[2]/2,a[3]/2,-a[2]/2,a[3]/2)
                elif m[0][t][2][0] == 'e':
                    o += '<ellipse rx="%s" ry="%s"/>'%(a[2]/2,a[3]/2)
                elif m[0][t][2][0] == 'c':
                    o += '<circle r="%s"/>'%(a[2]/2)
                o += '</g>'
            else:
                o += ' mx="%s" my="%s">'%(mx,my)
            disp,dy,x,y = '',0,pos[n][0]*ratio,pos[n][1]*ratio 
            na,nm = 0,0
            #o += '<circle r="2" cx="%s" cy="%s"/>'%(x,y)
            if len(Nodes[n]) > 1 and Nodes[n][1] in classT:
                args = [n] if (not Nodes.has_key(n) or len(Nodes[n])<3 or Nodes[n][2] == None) else Nodes[n][2].split('|')
                (na,nm) = (0,0) if len(args) < 2 else (len(args[1].split(',')),0) if len(args) == 2 else (len(args[1].split(',')),len(args[2].split(',')))
                disp,dy,x,y = '',0,pos[n][0]*ratio,pos[n][1]*ratio
                disp += '<tspan class="tiny" dominant-baseline="text-after-edge" x="%s">Class</tspan>'%x
                disp += '<tspan dx="10">%s</tspan>'%args[0]
                #print args
                if len(args)>1:
                    disp += '<tspan class="tiny" x="%s" dy="2em">Attributes</tspan>'%(x)
                    for l in args[1].split(','):
                        disp += '<tspan class="body" x="%s %s" dy="1em">+%s: Int</tspan>'%(x,x+10,l)  
                    if len(args)>2:
                        disp += '<tspan class="tiny" x="%s" dy="2em">Methods</tspan>'%(x) 
                        for l in args[2].split(','):
                            disp += '<tspan class="body" x="%s %s" dy="1em">+%s(): Int</tspan>'%(x,x+10,l) 
            else:
                args = [n] if (not Nodes.has_key(n) or len(Nodes[n])<3 or Nodes[n][2] == None) else Nodes[n][2].split('\n')
                for l in args:
                    disp += '<tspan x="%s" dy="%sem">%s</tspan>'%(x,dy,l) 
                    dy = 1
            o += '<text class="node" x="%s" y="%s">%s</text>'%(x,y,disp)
            if boxes.has_key(n):
                o += '<g>' 
                portsPos[n],ports[n] = [],__DATA_ports__[t] if __DATA_ports__.has_key(t) else []
                if ports[n]:
                    delta = 200.0/len(ports[n])
                    d = delta/2.0 - 100
                    for p in ports[n]:
                        b = boxes[n]
                        if d<0:
                            anchor,x,y = 'start',b[0]+1, b[1] + (d+100)*b[3]/100 
                            rx = x-7
                            portsPos[n].append((x-6,y))
                        else:
                            anchor,x,y = 'end',b[0] + b[2]-1, b[1] + (100-d)*b[3]/100
                            rx = x+1
                            portsPos[n].append((x+6,y))
                        o += '<rect class="port" x="%s" y="%s" width="6" height="6" pos="%s"/>'%(rx,y-3,d)
                        o += '<text class="tiny" x="%s" y="%s" dominant-baseline="middle" text-anchor="%s">%s</text>'%(x,y,anchor,p)
                        d += delta
                elif len(Nodes[n]) > 1 and Nodes[n][1] in classT:
                    H=[]
                    if na>0 and nm>0:
                        H.append(2*(boxes[n][3]-2*(8+my))/(2+na+nm))
                        H.append(8 + (1+.5*na)*H[0])
                    elif na>0:
                        H.append((boxes[n][3]-8-2*my)/(1+.5*na))
                    for h in H:
                        o += '<path class="sep" d="M%s,%sl%s,0"/>'%(boxes[n][0],boxes[n][1]+h+my+1,boxes[n][2])
                o += '</g>\n' 
            o += '</g>\n' 
        o += '</g>\n'
        if boxes: 
            o += self.gen_svg_connectors(Edges,boxes,portsPos,ports)
        return o + '\n</g></svg>'
        #return o + '\n</svg>'

    def gen_svg_connectors(self,edges,boxes,portsPos,ports): 
        o,ne = '<g id=".connectors" >\n',0
        for e in edges:
            n1,n2 = e[0],e[2]
            p1,p2,ep1,ep2 = '','','',''
            ne += 1
            label,typ = e[4] if len(e)>4 else '',e[3] if (len(e)>4 and e[3] != None) else ''
            if re.search(r'\.',n1):
                [n1,p1] = n1.split('.')
                if re.match(r'^\d+$',p1):
                    ep1,p1 = int(p1),' p1="%s"'%int(p1)
                elif p1 in ports[n1]:
                    ep1 = list(ports[n1]).index(p1)
                    p1 = ' p1="%s"'%ep1
            if re.search(r'\.',n2):
                [n2,p2] = n2.split('.')
                if re.match(r'^\d+$',p2):
                    ep2,p2 = int(p2),' p2="%s"'%int(p2)
                elif p2 in ports[n2]:
                    ep2 = list(ports[n2]).index(p2)
                    p2 = ' p2="%s"'%ep2
                else:
                    ep2,p2='',''
            if ep1 != '' and ep2 != '' and ep1<len(portsPos[n1]) and ep2<len(portsPos[n2]):
                d = nodes_path2o(portsPos[n1][ep1],portsPos[n2][ep2],boxes.values())
            elif ep1 != '' and ep1<len(portsPos[n1]):
                d = nodes_path1(boxes[n2],portsPos[n1][ep1],False)
            elif ep2 != '' and ep2<len(portsPos[n2]):
                d = nodes_path1(boxes[n1],portsPos[n2][ep2],True)
            else:
                d = nodes_path(boxes[n1],boxes[n2])
            o += '<g class="edge%s" n1="%s" n2="%s"%s%s><path id="e_%s" d="%s"/>'%(typ,n1,n2,p1,p2,ne,d)
            if label:
                o += '<text><textPath %s xlink:href="#e_%s" startOffset="50%%">%s</textPath></text>'%(_XLINKNS,ne,label) 
            args = label.split('|')
            if args:
                o += '<text class="tiny"><textPath %s xlink:href="#e_%s" startOffset="0%%">%s</textPath></text>'%(_XLINKNS,ne,args[0])
                if len(args) == 2:
                    o += '<text class="tiny"><textPath %s xlink:href="#e_%s" text-anchor="end" startOffset="100%%">%s</textPath></text>'%(_XLINKNS,ne,args[1])
            o += '</g>\n'
        return o + '</g>\n'

    def gen_aadl(self,ast):
        "-- AADL\n"
        o,m = '',self.m['aadl']
        Nodes,Edges = ast
        for n in Nodes:
            pass
        for e in Edges:
            pass
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
For your convenience, the [u.pdf](https://github.com/pelinquin/u/blob/master/u.pdf?raw=true) and [beamer_u.pdf](https://github.com/pelinquin/u/blob/master/beamer_u.pdf?raw=true) files are also commited.\n\nEnjoy!
"""
    o = '%s: [u.py](https://github.com/pelinquin/u/blob/master/u.py) base64 encoded sha1 short digest\n\n'%__digest__
    open('README.md','w').write(o + gen_readme.__doc__)

def gen_makefile():
    "# Simple Makefile example to use code generator remote call and separate code compilation\n# see https://github.com/pelinquin/u\n"
    open('a.u','w').write('# generated from u.py\n(a|z,b *bb)h')
    open('b.u','w').write('# generated from u.py\n(b|a *aa)h')
    open('c.u','w').write('A"a mya|b myb" -> B"mya.bb = &myb|mya.bb->aa = &mya|myb.aa->z = 4" ->{ C"mid_register(f1,(void *)&myb)" D"""mid_register(f2,(void *)"Hello")""" } "f1|f2"e')
    rules = {'all': 'exe|./exe',
             'exe': 'auto.o mid.o app.o|gcc -pthread auto.o mid.o app.o -o exe',
             'auto.o': 'auto.c|gcc -c auto.c',
             'app.o': 'app.c|gcc -c app.c',
             'mid.o': 'mid.c mid.h|gcc -c mid.c',
             'a.h': 'a.u|./u.py -fc a.u > a.h',
             'b.h': 'b.u|./u.py -fc b.u > b.h',
             'auto.c': 'c.u a.h b.h|./u.py -fc c.u > auto.c',
             'clean':'|rm -f *o exe auto.c a.h b.h'}
    o = '# Base64 encoded sha1 short digest: %s\n\n'%__digest__
    for r in rules:
        o += '%s: %s\n\t%s\n\n'%((r,)+tuple(rules[r].split('|')))
    open('makefile','w').write(gen_makefile.__doc__ + o)

def gen_apache_conf():
    """# Apache config file in WSGI mod
# This file is generated. Do not edit by hands!
# Place this file in '/etc/apache2/conf.d' directory
# and restart Apache: '/etc/init.d/apache2 restart'"""
    prg,path = os.path.basename(sys.argv[0])[:-3],os.path.abspath(sys.argv[0]) 
    o = '# Base64 encoded sha1 short digest:%s\n\n'%__digest__
    o += 'WSGIScriptAlias /%s %s\n'%(prg,path)
    o += 'WSGIScriptAlias /⊔ %s\n'%path
    o += 'AliasMatch /fonts/([^\.]*\.otf) %s/fonts/$1\n'%os.path.dirname(path)
    o += 'WSGIApplicationGroup %{GLOBAL}\n' # to avoid autoTSLkey apache error
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
    \pagestyle{myheadings}
    """ 
    o = tex_header.__doc__
    o += r'\markright{\tiny{\texttt{%s}}\hfill}'%__digest__ + '\n'
    o += r'\title{\bf $\sqcup$: %s} \author{%s -- \url{%s} \\ '%(__title__,__author__,__email__) + '\n'
    o += r'\tiny{version\footnote{the source file \texttt{u.py} for re-generating \LaTeX{} code and \textsc{pdf} file is attached to this \textsc{pdf} document.}: %s [\texttt{%s}]\footnote{the first five characters of the base64 encoding of the \textsc{sha1} digest of \texttt{u.py} source file. Please compare it with the one published at \url{https://%s} to check equality or to get the last release.}}}'%(__version__,__digest__,__url__) + '\n'
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

class article:
    r"""%% This is generated, do not edit by hands!
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
"""
    def __init__(self,title,author,email,dat=None,logo=None):
        r"""\begin{document}"""
        self.src = os.path.basename(sys.argv[0])
        self.tex = article.__doc__ + '\n'
        self.tex += r'\embedfile[filespec=%s]{%s}'%(self.src,os.path.abspath(self.src))
        self.tex += r'\title{%s}'%title + '\n'
        self.tex += r'\author{%s\inst{*}}\institute{*%s}'%(author,email) + '\n' 
        if dat:
            self.tex += r'\date{%s}'%dat + '\n'
        self.tex += article.__init__.__doc__ + r'\maketitle\section{Draft:\texttt{%s}}'%__digest__ + '\n'

    def gen_pdf(self):
        r"""\end{document}"""
        self.tex += article.gen_pdf.__doc__ + '\n'
        open('tmp_%stex'%self.src[:-2],'w').write(self.tex)
        tex,pdf = 'tmp_%stex'%self.src[:-2],'tmp_%spdf'%self.src[:-2]
        if subprocess.Popen(('which','pdflatex'),stdout=subprocess.PIPE).communicate()[0]:
            # don't understand why but needs to run twice pdflatex !
            lt = os.path.abspath(tex)
            subprocess.Popen(('cd /tmp; pdflatex -interaction=batchmode %s 1>/dev/null; pdflatex -interaction=batchmode %s 1>/dev/null'%(lt,lt)), shell=True).communicate()
            #shutil.move('/tmp/%s'%pdf,pdf) 
            open(pdf,'w').write(re.sub('(\/ID \[[^\]]+\]|\/CreationDate \([^\)]+\)|\/ModDate \([^\)]+\))','',open('/tmp/%s'%pdf).read()))
        else:
            sys.stderr.write('pdflatex not installed!\n')

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
    article(r'The $\sqcup$ Language',__author__,__email__,'December $7^{th}$ 2011','rcf.png').gen_pdf() 

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
        #shutil.move('/tmp/%spdf'%src,'%spdf'%src) 
        open('%spdf'%src,'w').write(re.sub('(\/ID \[[^\]]+\]|\/CreationDate \([^\)]+\)|\/ModDate \([^\)]+\))','',open('/tmp/%spdf'%src).read()))
        # remove those to get a deterministic pdf! "/CreationDate (D:2...) /ModDate (D:20...) /ID [<1...> <1...>]"
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

def get_logo():
    return '<path id="logo" stroke-width="5" fill="none" stroke="Dodgerblue" onclick="window.open(\'http://%s\');" title="⊔ [http://%s]" opacity=".02" d="M10,10L10,35L30,35L30,10"/>\n'%(__url__,__url__)

def get_param(environ):
    param = {}
    if environ['REQUEST_METHOD'] == 'POST':    
        for m in re.compile(r'([^&=]+)\s*=\s*([^&=]*)').finditer(environ['wsgi.input'].read(int(environ.get('CONTENT_LENGTH','0')))):
            param[m.group(1)] = m.group(2)
    return param

def get_editor(environ,args):
    ""
    o,content,gid = '<?xml version="1.0" encoding="UTF-8" ?>\n<html %s>\n'%_XHTMLNS,'',''
    o += '<title>⊔</title><style>textarea.editor{resize:none;width:100%; color:white;background-color:#444;}</style>\n'
    o += '<style>input:required:invalid, input:focus:invalid { background-image: url(\'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAeVJREFUeNqkU01oE1EQ/mazSTdRmqSxLVSJVKU9RYoHD8WfHr16kh5EFA8eSy6hXrwUPBSKZ6E9V1CU4tGf0DZWDEQrGkhprRDbCvlpavan3ezu+LLSUnADLZnHwHvzmJlvvpkhZkY7IqFNaTuAfPhhP/8Uo87SGSaDsP27hgYM/lUpy6lHdqsAtM+BPfvqKp3ufYKwcgmWCug6oKmrrG3PoaqngWjdd/922hOBs5C/jJA6x7AiUt8VYVUAVQXXShfIqCYRMZO8/N1N+B8H1sOUwivpSUSVCJ2MAjtVwBAIdv+AQkHQqbOgc+fBvorjyQENDcch16/BtkQdAlC4E6jrYHGgGU18Io3gmhzJuwub6/fQJYNi/YBpCifhbDaAPXFvCBVxXbvfbNGFeN8DkjogWAd8DljV3KRutcEAeHMN/HXZ4p9bhncJHCyhNx52R0Kv/XNuQvYBnM+CP7xddXL5KaJw0TMAF8qjnMvegeK/SLHubhpKDKIrJDlvXoMX3y9xcSMZyBQ+tpyk5hzsa2Ns7LGdfWdbL6fZvHn92d7dgROH/730YBLtiZmEdGPkFnhX4kxmjVe2xgPfCtrRd6GHRtEh9zsL8xVe+pwSzj+OtwvletZZ/wLeKD71L+ZeHHWZ/gowABkp7AwwnEjFAAAAAElFTkSuQmCC\'); background-position: right top; background-repeat: no-repeat; -moz-box-shadow: none; } input:required:valid { background-image: url(\'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAepJREFUeNrEk79PFEEUx9/uDDd7v/AAQQnEQokmJCRGwc7/QeM/YGVxsZJQYI/EhCChICYmUJigNBSGzobQaI5SaYRw6imne0d2D/bYmZ3dGd+YQKEHYiyc5GUyb3Y+77vfeWNpreFfhvXfAWAAJtbKi7dff1rWK9vPHx3mThP2Iaipk5EzTg8Qmru38H7izmkFHAF4WH1R52654PR0Oamzj2dKxYt/Bbg1OPZuY3d9aU82VGem/5LtnJscLxWzfzRxaWNqWJP0XUadIbSzu5DuvUJpzq7sfYBKsP1GJeLB+PWpt8cCXm4+2+zLXx4guKiLXWA2Nc5ChOuacMEPv20FkT+dIawyenVi5VcAbcigWzXLeNiDRCdwId0LFm5IUMBIBgrp8wOEsFlfeCGm23/zoBZWn9a4C314A1nCoM1OAVccuGyCkPs/P+pIdVIOkG9pIh6YlyqCrwhRKD3GygK9PUBImIQQxRi4b2O+JcCLg8+e8NZiLVEygwCrWpYF0jQJziYU/ho2TUuCPTn8hHcQNuZy1/94sAMOzQHDeqaij7Cd8Dt8CatGhX3iWxgtFW/m29pnUjR7TSQcRCIAVW1FSr6KAVYdi+5Pj8yunviYHq7f72po3Y9dbi7CxzDO1+duzCXH9cEPAQYAhJELY/AqBtwAAAAASUVORK5CYII=\'); background-position: right top; background-repeat: no-repeat; }</style>\n' + include_js_editor()
    #p = os.path.dirname(__file__)
    #if p not in sys.path:
    #    sys.path.append(p)
    #import gitu
    #mygit = gitu.gitu()
    #param = get_param(environ)
    #if param.has_key('content'):
    #    content = param['content']
    #if param.has_key('gid'):
    #    gid = param['gid']
    #    content = mygit.cat(gid)
    o += '<form method="post">'
    o += '<input pattern="\w{5,10}" title="git id" type="search" list="l" name="gid" value="%s" onchange="submit();" data-message="6 to 10 digits" required/><datalist id="l">'%gid
    #for i in mygit.list():
    #    if reg(re.search(r'^100644 blob ([0-9a-f]{40})\t(\w+)$',i)):
    #        o += '<option value="%s"></option>'%reg.v.group(2)
    o += '</datalist><input type="button" id=".save" name="s" disabled="true" value="Save" onclick="submit();"/>'
    o += '<textarea id=".editor" name="content" class="editor" spellcheck="false" rows="20">%s</textarea>'%content
    return o + '</form></html>'

def application(environ,start_response):
    """<title>⊔</title><style>h1,h2,h6,p,li,b,a,td{font-family:helvetica neue,helvetica,arial,sans-serif;} a{text-decoration:none;} 
table {border: 1px solid #666;width:100%;border-collapse:collapse;}
td,th {border: 1px solid #666;}
h6{position:absolute;top:0;right:10;} a.logo{font-size:100pt;color:DodgerBlue;line-height:80%}</style>
<h1><a href="https://github.com/pelinquin/u" class="logo" title="SquareCup">⊔</a> Web service</h1>
<h2>Using a Web browser</h2>
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
<p>If no argument is given, the output [<a href="u">/u</a>] is the Python source code for reading or for <a href="u"><b>download</b></a>.</p>
<h2>Using command line</h2>
<p>Usage: "./u.py -h&lt;host server&gt; -f&lt;language name&gt; &lt;⊔ input file&gt;" (default server is localhost).</p>
<p>Run such commands in local Makefile to manage independent module code generation/compilation/link.</p>
<h2>[Planned] Supported output languages</h2><b>"""
    s,mime,o,uobj,host = urllib.unquote(environ['QUERY_STRING']),'text/plain;charset=UTF-8','Error!',u(),environ['SERVER_NAME']
    if reg(re.match(r'\s*(update$|about$|help$|usage$|pdf$|paper$|beamer$|edit$|ace$|log$|)(?:(_?)(%s|raw|ast)(?:&(.*)|)|(.*))\s*$'%'|'.join(__OUT_LANG__),s,re.I)):
        form,action,under,lang,args = False,reg.v.group(1),reg.v.group(2),reg.v.group(3),reg.v.group(5) if reg.v.group(5) else reg.v.group(4)
        if lang: lang = lang.lower()
        if (action,under,lang,args) == ('',None,None,None):
            start_response('200 OK',[('Content-type',mime),('Content-Disposition','filename=u.py')])
            return [(open(environ['SCRIPT_FILENAME']).read())] 
        elif action and action.lower() in ('about','help','usage'):
            mime,o = 'text/html;charset=UTF-8','<html><title>Version:%s Digest:%s</title>%s'%(__version__,__digest__,get_favicon())
            #mime = 'application/xml;charset=UTF-8'
            o += application.__doc__ + ', '.join(__OUT_LANG__) + '</b>\n'
            o += '<h2>[Planned] Supported Input Modeling Formalisms</h2><b>' + ', '.join(__IN_MODEL__) + ',...</b>\n'
            tbl = {'A->B':'cas1','A->B->C':'cas2','A(Z|a,b|func)c':'cas3'}
            o += '<table><tr><td>Cases</td><td><b>⊔ code</b></td><td><b>Nodes</b></td><td><b>Edges</b></td><td>svg</td><td>tikz</td><td>c</td></tr>'
            for i in tbl:
                n,e = uobj.parse(i)
                o += '<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td><a href="u?_svg&%s" target="_blank">☑</a></td><td><a href="u?_tikz&%s">☑</a></td><td><a href="u?c&%s">☑</a></td></tr>'%(tbl[i],i,n,e,i,i,i)
            o += '</table>'
            o += '<h6 title="Base64 encoded short sha1 digest">%s</h6></html>'%__digest__
        elif action and action.lower() in ('paper','pdf'):
            o,mime = open('%s/u.pdf'%os.path.dirname(environ['SCRIPT_FILENAME'])).read(),'application/pdf'
        elif action and action.lower() == 'beamer':
            o,mime = open('%s/beamer_u.pdf'%os.path.dirname(environ['SCRIPT_FILENAME'])).read(),'application/pdf'
        elif action and action.lower() in ('edit','ace'):
            #o,mime = get_editor(environ,args),'application/xml;charset=UTF-8'
            o,mime = get_editor(environ,args),'text/html;charset=UTF-8'
        elif action and action.lower() == 'log':
            mime,lf = 'text/plain;charset=UTF-8','/var/log/apache2/error.log'
            #o = open(lf).read() if os.path.isfile(lf) else 'no log file'
            o = 'no log file'
        elif action and action.lower() == 'update':
            if environ['SERVER_NAME'] != 'pelinquin': # update not possible from RCF network because port 22 closed
                cmd = 'cd %s/..; rm -rf u; git clone git://github.com/pelinquin/u.git; cd u'%os.path.dirname(environ['SCRIPT_FILENAME'])
            else:
                cmd = 'ls %s'%__file__
            res = subprocess.Popen((cmd), shell=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE).communicate()
            o,mime = '<html><p>','text/html'
            o += res[1] if res[1] else '%s server Updated!'%environ['SERVER_NAME']
            o += '</p><a href="u?about">...go to main updated page</a></html>'
        elif args == None:
            if environ['REQUEST_METHOD'].lower() == 'post':
                raw = environ['wsgi.input'].read(int(environ.get('CONTENT_LENGTH','0')))
                ast = uobj.parse('\n'.join(raw.split('\n')[4:-2]))
                if lang in ('ast','raw'):
                    o = '%s %s'%ast
                else: 
                    o = eval('uobj.hf(uobj.gen_%s,\'%s\')(ast)'%(lang,host)) #....encode('utf-8')
            else:
                mime,form,o = 'text/html',True, '<form method=post enctype=multipart/form-data><input type=file name=a onchange="submit();"/>'
                #o += '<input type=button value=test onclick="submit();"/>'
                o += '</form>'
        elif lang in (None, 'ast','raw'):
            ast = uobj.parse(args)
            o = '# ⊔ Python Abstract Syntax Structure:\n\n%s %s'%uobj.parse(args)
            #o = '# ⊔ Python Abstract Syntax Structure:\n\n%s %s'%uobj.hf(uobj.gen_raw)(ast)
        else:
            ast = uobj.parse(args)
            if environ['REQUEST_METHOD'].lower() == 'post':
                raw = environ['wsgi.input'].read(int(environ.get('CONTENT_LENGTH','0')))
                o = eval('uobj.gen_%s(ast,%s)'%(lang,eval(urllib.unquote(re.sub(r'^\w=','',raw))))) 
            else:
                o = eval('uobj.hf(uobj.gen_%s,\'%s\')(ast)'%(lang,host)) #....encode('utf-8')
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
        elif under == '_' and lang in ('tikz',):
            header.append(('Content-Disposition','attachment; filename=out.pdf'))
        else:
            header.append( ('Content-Disposition','filename=file.%s'%ext))
    start_response('200 OK',header)
    return [(o)] 

def strip2(z):
    return z[:-2] if not (z[-1] or z[-2]) else z[:-1] if not z[-1] else z
    
def strip3(z):
    return z[:-3] if not (z[-1] or z[-2] or z[-3]) else z[:-2] if not (z[-1] or z[-2]) else z[:-1] if not z[-1] else z

######### UTILITIES ###########

def layout(nodes,edges,rankdir='TB'):
    "computes layout for graphics (tikz and svg) generation"
    bbx,bby,pos,d = None,None,{},'digraph G { rankdir=%s '%rankdir
    for n in nodes:
        label = n if (len(nodes[n])<3 or nodes[n][2] == None) else re.sub(r'\\','\\\\n',nodes[n][2])  
        label = re.sub(r'[\n|]','\\\\n',label) 
        label = re.sub(r'\$[^\$]+\$','_',label)
        label = re.sub(r',','_',label)
        d+= ' %s[label="%s"];'%(n,label) 
    for e in edges:
        n1,n2 = re.sub(r'\..+$','',e[0]),re.sub(r'\..+$','',e[2])
        label = '' if len(e)<4 or e[3] == None else '[label="%s"];'%e[3]
        label = re.sub(r'\$[^\$]+\$','_',label)
        d+= ' %s->%s %s'%(n1,n2,label) 
    #print d + '}' # for debug
    p = subprocess.Popen(['dot'], stdout=subprocess.PIPE, stdin=subprocess.PIPE)
    for l in p.communicate(input=d + '}')[0].split('\n'):
        #print l
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
                o += r'\tikzstyle{node%s} = [%s]'%(n,m[0][n][1]) + '\n'
        for e in m[1]:
            if le.has_key(e):
                o += r'\tikzstyle{edge%s} = [%s]'%(e,m[1][e]) + '\n'
    return o + '\n'

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

def nodes_path2o(p1,p2,nodes=[]):
    ""
    x1,y1,x2,y2 = p1[0],p1[1],p2[0],p2[1]
    o = 'M%s,%s'%(x1,y1)
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
function postURL(data) {
  var req = new XMLHttpRequest();
  req.onreadystatechange = function () {
    if (req.readyState == 4) {
      if (req.status == 200) { document.replaceChild(req.responseXML.documentElement,document.documentElement);} 
      else { alert('Error Post status:'+ req.status); }
    }
  }
  req.open('POST', window.location.href,true); req.send(JSON.stringify(data)); 
}
function submitURL(data) { 
  var f = document.createElement('form');
  f.setAttribute('method','post');
  h = document.createElement('input');
  h.setAttribute('type','hidden');
  h.setAttribute('name','a');
  h.setAttribute('value',JSON.stringify(data));
  f.appendChild(h);
  document.documentElement.appendChild(f);
  f.submit();
}
window.onload = function () { 
  var box = {};
  var t = document.getElementById('.nodes').childNodes;
  for (var n = 0; n < t.length; n++) {
    if (t[n].nodeName == 'g') {
      var b = t[n].firstChild.getBBox();
      var mx = parseInt(t[n].getAttribute('mx')); var my = parseInt(t[n].getAttribute('my'));
      box[t[n].id] = [b.x-mx,b.y-my,b.width+2*mx,b.height+2*my];
    }
  }
  if (Object.keys(box).length != 0) { 
    submitURL(box); //postURL(box);
  }
}"""
    o = '<script %s type="text/ecmascript">\n/*<![CDATA[*//*---->*/\n'%_XLINKNS
    return o + include_js.__doc__  + '\n/*--*//*]]>*/</script>\n'

def include_js_pan():
    r"""var pan = false, stO, stF;
document.documentElement.setAttributeNS(null, "onmouseup",   "hMouseUp(evt)");
document.documentElement.setAttributeNS(null, "onmousedown", "hMouseDown(evt)");
document.documentElement.setAttributeNS(null, "onmousemove", "hMouseMove(evt)");
function getP(evt) { var p = document.documentElement.createSVGPoint(); p.x = evt.clientX; p.y = evt.clientY; return p; }
function setCTM(ele,m) { ele.setAttribute("transform", "matrix(" + m.a + "," + m.b + "," + m.c + "," + m.d + "," + m.e + "," + m.f + ")"); }
function hMouseMove(evt) {
  if (pan) {
    var p = getP(evt).matrixTransform(stF);
    setCTM(document.getElementById('.nodes').parentNode, stF.inverse().translate(p.x-stO.x, p.y-stO.y));
  } 
}
function hMouseDown(evt) {
  pan = true;
  stF = document.getElementById('.nodes').parentNode.getCTM().inverse();
  stO = getP(evt).matrixTransform(stF);
}
function hMouseUp(evt) { pan = false;}"""
    o = '<script %s type="text/ecmascript">\n/*<![CDATA[*//*---->*/\n'%_XLINKNS
    return o + include_js_pan.__doc__  + '\n/*--*//*]]>*/</script>\n'

def include_js_editor():
    r"""
function postURL(data) {
  var req = new XMLHttpRequest();
  req.onreadystatechange = function () {
    if (req.readyState == 4) {
      if (req.status == 200) { document.replaceChild(req.responseXML.documentElement,document.documentElement);} 
      else { alert('Error Post status:'+ req.status); }
    }
  }
  req.open('POST', window.location.href,true); req.send(JSON.stringify(data)); 
}
if (typeof($)=='undefined') { 
  function $(id) { return document.getElementById(id.replace(/^#/,'')); } 
}
function change_editor(evt) {
  $('.save').removeAttribute('disabled');
}
window.onload = function () { 
   $('.editor').addEventListener('keyup', change_editor, false);
}"""
    o = '<script %s type="text/ecmascript">\n/*<![CDATA[*//*---->*/\n'%_XLINKNS
    return o + include_js_editor.__doc__  + '\n/*--*//*]]>*/</script>\n'

def svg_defs():
    """ """
    o = '<defs>'
    o += '<marker id=".arrow" viewBox="0 0 500 500" refX="80" refY="50" markerUnits="strokeWidth" orient="auto" markerWidth="40" markerHeight="30"><polyline points="0,0 100,50 0,100 20,50" fill="#555"/></marker>'
    o += '<marker id=".r_arrow" viewBox="0 0 500 500" refX="70" refY="50" markerUnits="strokeWidth" orient="auto" markerWidth="40" markerHeight="30"><polyline points="150,0 50,50 150,100 130,50" fill="#555"/></marker>'
    o += '<radialGradient id=".grad" cx="0%" cy="0%" r="90%"><stop offset="0%" stop-color="#FFF"/><stop offset="100%" stop-color="#DDD" class="end"/></radialGradient>'
    o += '<filter id=".shadow" filterUnits="userSpaceOnUse"><feGaussianBlur in="SourceAlpha" result="blur" stdDeviation="2"/><feOffset dy="3" dx="2" in="blur" result="offsetBlur"/><feMerge><feMergeNode in="offsetBlur"/><feMergeNode in="SourceGraphic"/></feMerge></filter>'
    return o + '</defs>\n'

def gen_svg_header(m,(ln,le),full=True):
    ""
    o = '<style type="text/css">\n'
    o += '@font-face { font-family: Graublau; src: url(\'./fonts/GraublauWeb.otf\') format("opentype");}\n'
    o += '@font-face { font-family: vag; src: url(\'./fonts/VAG-HandWritten.otf\') format("opentype");}\n'
    o += 'text {font-family:helvetica neue,helvetica,arial,sans-serif;}\n'
    o += 'text.tiny, tspan.tiny { font-family:helvetica neue,helvetica,arial,sans-serif;font-size: 4px; fill:DarkSlateGray;}\n'
    o += 'tspan.body { font-family:helvetica neue,helvetica,arial,sans-serif;font-size: .5em; fill:DarkSlateGray;}\n'
    o += 'text.node { font-size: 1em;}\n'
    if full:
        o += 'textPath {dominant-baseline:text-after-edge;}\n'
        #o += 'text:hover {stroke:gray;fill:none;} \n'
        o += 'rect.port { stroke-width:0; fill:lightblue;}\n'
        o += 'path:hover, rect:hover { opacity:0.5; cursor:crosshair;}\n'
        o += 'path#logo:hover { opacity:1;}\n'
    for n in m[0]:
        if ln.has_key(n):
            o += 'g.node%s > text { %s }\n'%(n,m[0][n][1]) 
            sty = m[0][n][2].split('|')[0]
            o += 'g.node%s > g > %s, path.sep { %s }\n'%(n,sty,m[0][n][3]) 
    for e in m[1]:
        if le.has_key(e):
            o += 'g.edge%s path { %s }\n'%(e,m[1][e]) 
    o += '</style>\n'
    if full:
        o += svg_defs()
    return o + '\n'

def gettypes(ast):
    ""
    nl,el = {'':True},{'':True}
    Nodes,Edges = ast
    for n in Nodes:
        if len(Nodes[n]) > 1:
            nl[Nodes[n][1]] = True 
    for e in Edges:
        if len(e) > 3:
            el[e[3]] = True 
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
        #i = re.sub(r'\n','\\\\n',i)
        h += '\t# %03d: %s\n\t\'%s\':\n\t%s,\n\n'%(n,j,i,uobj.parse(i))
    h += '}\n'
    try:
        eval(h)
    except:
        print 'error in parsing!'
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
    def __init__(self,title,author,email,dat=None,logo=None):
        r"""\begin{document} \frame{\titlepage}"""
        self.src = os.path.basename(sys.argv[0])
        self.tex = beamer.__doc__ + '\n'
        self.tex += r'\embedfile[filespec=%s]{%s}'%(self.src,os.path.abspath(self.src))
        self.tex += r'\title{%s}'%title + '\n'
        self.tex += r'\author{%s\inst{*}}\institute{*%s}'%(author,email) + '\n' 
        if dat:
            self.tex = r'\date{%s}'%dat + '\n'
        if os.path.isfile(os.path.abspath(logo)):
            self.tex += r'\pgfdeclareimage[height=.6cm]{logo}{%s}'%os.path.abspath(logo) + '\n' + r'\logo{\pgfuseimage{logo}}' + '\n\n'
        self.tex += beamer.__init__.__doc__ + r'\section{Draft:\texttt{%s}}'%__digest__ + '\n'

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
        
    def itemize2(self,title,tab1,tab2):
        ""
        self.tex += r'\frame{\frametitle{%s}'%title + '\n'
        self.tex += r'\begin{columns}[l]' + '\n'
        self.tex += r'\column{1.5in}' + '\n'
        self.tex += reduce(lambda y,k: y+r'\item %s'%k+ '\n',tab1,r'\begin{itemize}') + r'\end{itemize}'
        self.tex += r'\column{1.5in}' + '\n' 
        self.tex += reduce(lambda y,k: y+r'\item %s'%k+ '\n',tab2,r'\begin{itemize}') + r'\end{itemize}'
        self.tex += r'\end{columns}' + '\n'
        self.tex += '}\n'

    def gen_pdf(self):
        ""
        self.gen_tex()
        tex = 'beamer_%stex'%self.src[:-2]
        pdf = 'beamer_%spdf'%self.src[:-2]
        if subprocess.Popen(('which','pdflatex'),stdout=subprocess.PIPE).communicate()[0]:
            # don't understand why but needs to run twice pdflatex !
            lt = os.path.abspath(tex)
            subprocess.Popen(('cd /tmp; pdflatex -interaction=batchmode %s 1>/dev/null; pdflatex -interaction=batchmode %s 1>/dev/null'%(lt,lt)), shell=True).communicate()
            #shutil.move('/tmp/%s'%pdf,pdf) 
            open(pdf,'w').write(re.sub('(\/ID \[[^\]]+\]|\/CreationDate \([^\)]+\)|\/ModDate \([^\)]+\))','',open('/tmp/%s'%pdf).read()))
        else:
            sys.stderr.write('pdflatex not installed!\n')

def gen_beamer():
    ""
    uobj = u()
    #slides = beamer(r'The $\sqcup$ Language',__author__,__email__,'December $7^{th}$ 2011','rcf.png')
    slides = beamer(r'The $\sqcup$ Language',__author__,__email__,None,'rcf.png')
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
    slides.frame('Sparse Graph', r"""A {\bf graph}: $G = (V,E) $ \\
where: \\ $$V=\{v_i\} \; \text{and} \; E=\{e_{k}\}$$ ...a set of nodes (vertices) and a set of arcs (edges) between nodes.\\
$$e_{k} = (v_{i_p},v_{j_q})$$ Edge links an origin node to one destination node.\\
[$p$ and $q$ are ports references]\\
Dictionnary: some attributes as (key,value) pair is attached to each node $v_i$ and each edge $e_k $.""")
    slides.itemize_graph(r'$\sqcup$ \textsc{thonus} features', (r'\textsc{T}-yped',
                                                                r'\textsc{H}-ierachical',
                                                                r'\textsc{O}-nline',
                                                                r'\textsc{N}-eutral',
                                                                r'\textsc{U}-nicode',
                                                                r'\textsc{S}-hort'),
                         uobj.gen_tikz(uobj.parse('{T:a H:b O:c N:d U:e S:f}->{T H O N U S}'),False,1.6,1.6,'TB'))    
    slides.frame(r'$\sqcup$ at a glance',r"""\begin{tabular}{ll}
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
    slides.frame(r'The big picture (with $\sqcup$!)', uobj.gen_tikz(uobj.parse(r'A"$\sqcup$ concrete\\\\syntax\\\\string"n B" $\sqcup$ abstract\\\\syntax\\\\Python\\\\structure"n  A -e($\sqcup$ parser)> B B -e(Web)> "SVG"g B -e(doc.gen.)> "Tikz"g B -e(code gen.)> "Ada"l B -e> "Ocaml"l B -e(model gen.)> "AADL"l B -e(generator)> "XXXX"l e"UML\\\\Simulink\\\\KAOS\\\\..."m -d(use)> A "$\sqcup$ type\\\\checker"t -l> B "optimizer"t -l> A'),False,2.8,1.5))
    slides.frame(r'$\sqcup$ facts', r"""\begin{block}{Structure}
$\sqcup$ only manages the structure of the graph, not the semantics.\\
$\sqcup$ parser builds an Abstract Syntax Tree (a Python data Structure) \\ The types libraries are doing the real job!
\end{block}
\begin{block}{Rendering}
Graphics rendering is almost a matter of code generation. \\ Just customize the generator to style your graphs.
\end{block} 
\begin{block}{Pipes}
To generate code, $\sqcup$ uses UNIX like piped small tools on the graph Abstract Syntax Tree.
\end{block} 
""")
    slides.frame('Syntax building elements',r"""\begin{itemize}
\item Nodes:
\begin{itemize}
  \item \texttt{ID}: a unicode word to identify the node
  \item \texttt{type}: Type name available in the node types library 
  \item \texttt{content}: Free dictionnary of attributes, including label or arguments
  \item \texttt{port}: a named or indexed port (type compatible)
\end{itemize} 
\item Edges:
\begin{itemize}
  \item \texttt{(<>-=)}: Arrow head
  \item \texttt{type}: Type name available in the edge types library 
  \item \texttt{content}: Free dictionnary of attributes, including label or arguments
  \item \texttt{(<>-=)}: Arrow tail
\end{itemize} 
\item Blocks:
\begin{itemize}
  \item \texttt{\{...\}}: delimiters
\end{itemize} 
\end{itemize}""")
    slides.itemize('From the Dot (Graphviz) Language',(r'\textsc{Dot}\footnote{AT\&T Bell Laboratories} is not typed'
                                                       ,r'\textsc{Dot} composition (cluster) is not generic'
                                                       ,r'\textsc{Dot} ports are not (well) implemented'
                                                       ,r'\textsc{Dot} is not minimal (\texttt{"A->B"} raises a syntax error)'
                                                       ,r'\textsc{Dot} mixes structure and layout'
                                                       ,r'Limited \textsc{Dot} layout algorithms (nodes place + arc path)'))
    slides.itemize(r'From the XML format',(r'XML is for XHTML what $\sqcup$ is for Models (UML,Simulink,...)'
                                           ,'XML is basically suited for trees not graphs'
                                           ,'XML has a lot of glue characters'
                                           ,'XML does not enforce id on each elements'
                                           ,'XML use Xlink,Xpath for referencing'
                                           ,'XML raises {\it attributes} versus {\it elements} dilemma' 
                                           ,'XML is combersome to use'
                                           ,'XML is unreadable in practice'
                                           ,'Transformations are complex (XSLT)'
                                           ,'Type checking using DTD, XSD, RelaxNG'))
    slides.itemize(r'From yUML (yUML.me)',(r'yUML is an online service'
                                           ,'yUML rendering is nice'
                                           ,'yUML syntax is simple and readable'
                                           ,'yUML layout is automatic'
                                           ,'yUML is not open-source'
                                           ,'yUML is not typed (just themes)'
                                           ,'yUML is not hierarchical'
                                           ,'yUML generation is a little slow'
                                           ,'yUML is not oriented for code generation' 
                                           ,'yUML does not support SVG nor Tikz output'))
    slides.frame('$\sqcup$ Types',r"""\begin{itemize}
\item User defines is own types library for:
  \begin{itemize}
  \item Used Nodes \item Used Edges
  \end{itemize}
\end{itemize} 
A types library:
\begin{itemize}
\item defines the semantics of the input formalism (UML, Scade,...)\\
\item maps to output patterns (Ada, SVG,...)\\
\item defines a {\bf Domain Specific Language}
\item customize graphics output
\end{itemize} 
For instance, two different nodes types may be rendered with different shapes/decorations in SVG but may map to the same class construction for Scala generation.""")
    slides.frame('Semi-Formal and Formal',r"""
A {\bf semi-formal node} is a typed node with informal (english) sentence in its label.\\
A {\bf formal node} is a types node with all attributes valid stream from formal languages.\\
The label may be used to embedd procedure, function, class definition,..., on several lines. \\
The arguments may be used call, customize or instantiate. \\
The type definition may include default source code.""")
    slides.frame('Overload nodes rules',r"""
\begin{block}{rules:}
{\bf Node definition} and {\bf Node usage} are identical!\\
Node, Edges accumulates properties:
\end{block}
\begin{tabular}{lcl}
\texttt{A"hello" A:T}        &$\equiv$&  \texttt{A"hello":T}\\
\texttt{A"hello" A->B}       &$\equiv$&  \texttt{A"hello"->B}\\
\texttt{A->B A"hello"}       &$\equiv$&  \texttt{A"hello" A->B}\\
\texttt{A B C A}             &$\equiv$&  \texttt{C A B}\\
\texttt{A"label1" A"label2"} &$\equiv$&  \texttt{A"label2"}\\
\texttt{A(arg1) A(arg2)}     &$\equiv$&  \texttt{A(arg2)}\\
\texttt{A:T1 A:T2}           &$\equiv$&  \texttt{A:T2}\\
\texttt{A\{A\}}              &$\equiv$&  \texttt{A}  \\
\end{tabular}""")
    slides.frame('Edge rules',r"""
\begin{block}{rule:} Edge has no id! \end{block}
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
    slides.itemize2('Candidate model formalisms',__IN_MODEL__[:8],__IN_MODEL__[8:])
    slides.itemize2('Expected code generation',__OUT_LANG__.keys()[:9],__OUT_LANG__.keys()[9:])
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
The same graph may have several styles; themes \\
\TeX{} principle: nice graphic output is a requirement !""")
    slides.frame(r'Today needs',r"""\begin{itemize}
\item a theoretical support,
\item a constraint definition language (Real,OCL,...),
\item a better types definition (currently dictionnary of properties),
\item templates for many code generators,
\item an embedded and large test set,
\item plugins for formal model checkers and theorem provers.
\end{itemize}""")
    slides.frame('Next about $\sqcup{}$!',r'\begin{block}{All is on the forge:} \url{https://%s} \end{block} \begin{block}{Source code:} See PDF attached file: \texttt{u.py} \\ ...and generate this beamer. \end{block}'%__url__)
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
    "Command line usage"
    try:
        subprocess.Popen(['dot'], stdout=subprocess.PIPE,stdin=subprocess.PIPE).communicate(input='digraph G {A->B}')
    except:
        print '...Error: please install \'graphviz\' package !'
        sys.exit()

    #import doctest
    #doctest.testmod()
    #code_gen_test(True)

    import getopt
    opts, args = getopt.getopt(sys.argv[1:],'h:f:s',['host=','format=','stdin'])
    o,host = 'raw','127.0.0.1' # use '193.84.73.209'

    if not opts and not args:
        ast_test(True)
        gen_apache_conf()
        gen_doc()
        gen_beamer()
        gen_readme()
        gen_makefile()

    for r in opts:
        if r[0] in ('-h','--host'):
            host = r[1] 
        elif r[0] in ('-f','--format'):
            o = r[1]
        elif r[0] in ('-s','--stdin'):
            args.append(sys.stdin.read())
        else:
            print help('u') 
    for arg in args:
        data = open(arg).read() if os.path.isfile(arg) else arg
        print post(host, '/u?%s'%o, data)
    if not args:
        print 'Digest:%s'%__digest__

    ########## debug zone! ##############
    uobj = u()
    tab = ('A->B',
           'A{B C}->D{E F}',
           '{B C}->{E F}',
           'A->B->C',
           'A{a->b}->B{c->d} X->{u v} {t z}->Y',
           'a:T"L".1 -> {c.2{e} d:T.3 f:T"L"{g} }',
           'A->{B{C}}',
           '-> A {B} ->',
           u'A:您',
           u'A{B:您} C{D}',
           'A:c(a,b|f1,f2)',
           r'AA"hello\nworld"',
           'A"Hellowo|adssd|a,b"c')
    #print re.sub(r'\n',r'\\n',x,re.M,re.S)
    #x= '{A:O.1 C:O.4} -> {B:O.in2 B:O.in3 D}'
    #x= 'A:O.* -> B:O.*'
    #print x
    #print uobj.parse(x)

# the end
