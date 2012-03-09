#!/usr/bin/python3
# -*- coding: utf-8 -*-

# Welcome to ⊔ [SquareCup]!
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

# Warning! Small bug in Emacs editor default font: 
# swap ⊔ 'squarecap' (U+2293) and ⊓ 'squarecup' (U+2294) char!   

r"""
LaTeX doc here!
"""

__author__   = 'Laurent Fournier'
__email__    = 'lfournie@rockwellcollins.com'
__title__    = 'The Universal Short Graph Language'
__version__  = '0.3'
__license__  = 'GPLv3'
__url__      = 'github.com/pelinquin/u'
__git_base__ = '/u'

import sys, os, re, hashlib, subprocess, urllib, base64, random, functools, datetime

__separator__ = r'[|\'`";,!~^@*+/$]' # 14 chars
__delimiter__ = r'%s(?:%s%s)?' % (__separator__, __separator__, __separator__) # one or three chars
__RE_U__ = r'''     # RegExp 
   (?:              # Three basic tokens:
    ([{}\]\[])      # (1) Block 
   |
    (?:\#[^\n]*)    # or (2) Line comment
   |                # or (3) NODE:
    (?=[^\s<\-=>])  # Not empty token 
    (?:(\w+)|)      # Name      
    (?::(\w)|)      # Type pre  
    ((%s)(.+?)\5|\(([^)]+)\)|) # Content
    (\w|)           # Type post 
    (?:\.(\w+|\*)|) # Port      
   |                # or (4) EDGE:  
    ([<\-=>])       # Head      
    (\w|)           # Type pre  
    ((%s)(.+?)\13|\(([^)]+)\)|) # Content
    (\w|)           # Type post
    ([<\-=>])       # Tail
)''' % (__delimiter__, __delimiter__)

_XHTMLNS  = 'xmlns="http://www.w3.org/1999/xhtml"'
_SVGNS    = 'xmlns="http://www.w3.org/2000/svg"'
_XLINKNS  = 'xmlns:xlink="http://www.w3.org/1999/xlink"'

__OUT_LANG__ = {'c'          :['c',    ('/*', '*/', '')],
                'objectivec' :['m',    ('/*', '*/', '')],
                'python'     :['py',   ('#', '', '#!/usr/bin/python\n# -*- coding: utf-8 -*-\n')],
                'ada'        :['adb',  ('--', '', '')],
                'scala'      :['scl',  ('--', '', '')],
                'java'       :['java', ('//', '', '')],
                'ruby'       :['rb',   ('#', '', '')],
                'ocaml'      :['ml',   ('(*', '*)', '')],
                'haskell'    :['hs',   ('{-', '-}', '')],
                'lua'        :['lua',  ('--', '', '')],
                'tikz'       :['tex',  ('%', '', '')],
                'svg'        :['svg',  ('<!--', '-->', '<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n')],
                'aadl'       :['adl',  ('--', '', '')],
                'sdl'        :['sdl',  ('--', '', '')],
                'lustre'     :['lst',  ('--', '', '')],
                'vhdl'       :['hdl',  ('--', '', '')],
                'systemc'    :['sc',   ('//', '', '')]}
 
__DATA_ports__ = {
    None:('p1', 'p2', 'p3', 'p4'),
    'T': ('i', 'o'),
    'O': ('in1', 'in2', 'out1', 'out2'),
    'C': ('p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15', 'p16'),
    'D': ('pin1', 'pin2', 'pin3', 'pin4', 'pin5', 'pin6'),
    'x': ('pin1', 'pin2', 'pin3', 'pin4', 'pin5', 'pin6'),
    }

__DATA_svg__ = ({
        None:('node','fill:black;','rect|-5,40,5','fill-opacity:.1;', 10, 10),
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
        'C': ('class', ),
        'c': ('class', ),
        'H': ('class', ),
        'h': ('class', ),
        'm': ('main', ),
        'e': ('extern', ),
        'E': ('extern', ),
        },{
        None:(),
        })

__DATA_python__ = ({
        'C': ('class', ),
        'c': ('class', ),
        },{
        None:(),
        })

__DATA_objectivec__ = ({None: (), }, {None:(), })
__DATA_ada__        = ({None: (), }, {None:(), })
__DATA_scala__      = ({None: (), }, {None:(), })
__DATA_java__       = ({None: (), }, {None:(), })
__DATA_ruby__       = ({None: (), }, {None:(), })
__DATA_ocaml__      = ({None: (), }, {None:(), })
__DATA_haskell__    = ({None: (), }, {None:(), })
__DATA_lua__        = ({None: (), }, {None:(), })
__DATA_aadl__       = ({None: (), }, {None:(), })
__DATA_sdl__        = ({None: (), }, {None:(), })
__DATA_lustre__     = ({None: (), }, {None:(), })
__DATA_vhdl__       = ({None: (), }, {None:(), })
__DATA_systemc__    = ({None: (), }, {None:(), })

__IN_MODEL__ = ['UML', 'SysML', 'AADL-Graph', 'Marte', 'PSL', 'Xcos', 'Kaos', 'Entity-Relation-Graph', 'Tree-Diagram',
                'Network-Graph', 'Flowchart', 'Petri-net', 'State-Machine', 'Markov-Chain', 'Behavior-Tree'] 

__AST_SET__ = [
    ('OnlyId',                 'A'),
    ('OnlyContent',            '"L"'),
    ('OnlyType',               ':T'),
    ('OnlyChild',              '{a}'),
    ('Id+Content',             'A"L"'),
    ('Id+Type',                'A:T'),
    ('Id+Child',               'A{a}'),
    ('Content+Type',           '"L"T'),
    ('Type+Content',           ':T"L"'),
    ('Content+Child',          '"L"{a}'),
    ('Type+Child',             ':T{a}'),
    ('Id+Content+Type',        'A"L"T'),
    ('Id+Type+Content',        'A:T"L"'),
    ('Id+Content+Child',       'A"L"{a}'),
    ('Content+Type+Child',     '"L"T{a}'),
    ('Type+Content+Child',     ':T"L"{a}'),
    ('Id+Type+Child',          'A:T{a}'),
    ('Id+Content+Type+Child',  'A"LT{}'),
    ('Id+Type+Content+Child',  'A:T"L"{}'),
    ("Delimiter:'",            "Z'content'"),
    ('Delimiter:"',            'B"content"'),
    ('Delimiter:`',            'C`content`'),
    ('Delimiter:;',            'D;content;'),
    ('Delimiter:,',            'E,content,'),
    ('Delimiter:!',            'F!content!'),
    ('Delimiter:~',            'G~content~'),
    ('Delimiter:^',            'H^content^'),
    ('Delimiter:@',            'I@content@'),
    ('Delimiter:*',            'J*content*'),
    ('Delimiter:+',            'K+content+'),
    ('Delimiter:/',            'L/content/'),
    ('Delimiter:$',            'M$content$'),
    ('Delimiter:()',           'N(content)'),
    ('Word Id',                'Aaa'),
    ('Not first digit',        'A1'),
    ('First digit',            '1A'),
    ('white space',            ' A'),
    ('end white space',        'A '),
    ('both white spaces',      ' A '),
    ('several lines',         r'\n\nA\n\n'),
    ('latin1 char',            'éàùç'),
    ('Unicode char',           'A:您'),
    ('Unicode char',           'B:好'),
    ('Long content',           '"This is a long content" '),
    ('Multilines',            r'"Multi\nlines\ncontent" '),
    ('Types',                  'A:T B:U'),
    ('2 nodes',                'A B'),
    ('3 nodes',                'A B C'),
    ('4 nodes',                'A B C D'),
    ('Overload content',       'A"content1" A"content2"'),
    ('Accumulation1',          'A A"content"'),
    ('Accumulation2',          'A"content" A'),
    ('TypeContentAcc',         'A:T A"content"'),
    ('ContentTypeAcc',         'A"content" A:T'),
    ('ChildContentAcc',        'A{a} A"content"'),
    ('ContentChildAcc',        'A"content" A{a}'),
    ('cas01',                  'A A"content1" A"content2"(x)'),
    ('cas02',                  'A"content2"(x) A"content" A'),
    ('cas03',                  '"content"T1 "content"T2'),
    ('cas04',                  'A"content" B"content"'),
    ('cas05',                  'A{a} B{b1 b2} C{c1 c2 c3}'),
    ('cas06',                  'A{a} B {b} {c}'),
    ('cas07',                  ':T1{a b} :T2{c d}'),
    ('cas08',                  'A{ B{c b} C{e f} }'),
    ('cas09',                  'A{B{C{c}}}'),
    ('cas10',                  '"x-y"'),
    ('cas11',                  '"x+y"'),
    ('cas12',                  '"x*y"'),
    ('cas13',                  '"x/y"'),
    ('cas14',                  '"x.y"'),
    ('cas15',                  '"x,y"'),
    ('cas16',                  '"x%y"'),
    ('cas17',                  '"x^y"'),
    ('cas18',                  '"x=y"'),
    ('cas19',                  '"x:y"'),
    ('cas20',                  '"x&y"'),
    ('cas21',                  '"x|y"'),
    ('cas22',                  '"x>y"'),
    ('cas23',                  '"x<y"'),
    ('cas24',                  '#comment\nA'),
    ('cas25',                  '#comment1\nA\n #comment2'),
    ('Chain2',                 'A->B'),
    ('Chain3',                 'A->B->C'),
    ('Link1',                  'A>>B'), 
    ('Link2',                  'A><B'),
    ('Link3',                  'A>-B'),
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
    ('cas26'                  ,'A -(content)- B'),
    ('cas27'                  ,'A -T- B'),
    ('cas28'                  ,'A -T(content)- B'),
    ('cas29'                  ,'A -(content)T- B'),
    ('cas30'                  ,'A{a1 a2} -> B{b1 b2}'),
    ('cas31'                  ,'{a1 a2} -> B{b1 b2}'),
    ('cas32'                  ,'A{a1 a2} -> {b1 b2}'),
    ('cas33'                  ,'{a1 a2} -> {b1 b2}'),
    ('cas34'                  ,'A{a1 -> a2} B{b1 -> b2}'),
    ('cas35'                  ,'A{a1 -> a2} -> B{b1 -> b2}'),
    ('cas36'                  ,'A.1 -> B.2'),
    ('cas37'                  ,'A.por1 -> B.por2'),
    ('cas38'                  ,'A:T B:U A.1->B.2'),
    ('cas39'                  ,'A.1->B.2 A:T B:U'),
    ('Port0'                  ,'A.0->B.pin1->C.*->D'),
    ('Port1'                  ,'A:T.0->B:T.1'),
    ('Port2'                  ,'A:T.1->{B:T.0 C:T.0}'),
    ('Port3'                  ,'{A:T.1 B:T.0}->C:T.1'),
    ('Port4'                  ,'{A:T.1 B:T.0}->{C:T.1 D:T.0}'),
    ('Port5'                  ,'A:T.1->B:T.0->C:T.1'),
    ('Port*'                  ,'A:T.*->B:T.*'),
    ('PortOutOfrange'         ,'A:T.5->B:T.pin12'),
    ('Double definition'      ,'A{a} A{b}'),
    ('MainC','Code:C@@@#include <stddio.h>\nvoid main(void){\ninta;\n}\n@@@'),
]

# (0) Parser

def gettypes(ast):
    ""
    nl, el = {'':True}, {'':True}
    Nodes, Edges = ast
    for n in Nodes:
        if len(Nodes[n]) > 1:
            nl[Nodes[n][1]] = True 
    for e in Edges:
        if len(e) > 3:
            el[e[3]] = True 
    return nl, el

class u:
    """ This is the base class for ⊔ 
    One can customize that class by adding/modifying __DADA_xxx__ structure or by overloading a gen_xxx() method
    """

    def __init__(self):
        "Load types mapping"
        self.m = {}
        for l in __OUT_LANG__:
            self.m[l] = eval('__DATA_%s__' % l)

    def addedge(self, child, target, cli):
        "utils"
        Edges = []
        for c in child:
            tc = range(-int(c[1])) if c[1] and int(c[1]) < 0 else [c[1]]
            for i in target:
                ti = range(-int(i[1])) if i[1] and int(i[1]) < 0 else [i[1]]
                for p in tc:
                    for q in ti:
                        Edges.append(((c[0],p), cli[0], (i[0],q), cli[1], cli[2]))
        return Edges

    def parse(self, x):
        "kernel"
        Nodes, Edges = {}, []
        tgt, cht, op, orig, link, bing, state = [], [], False, [None,], False, False, 0
        cli = []
        for m in re.compile(__RE_U__, re.U|re.X|re.S).finditer(x):
            #print(m.groups())
            if m.group(1) in ('{','['):
                op, tgt = True, []
                if state == 1: 
                    state = 2
                orig.append(None)
            elif m.group(1) in ('}',']'):
                if orig:
                    orig.pop()
                op, state = False, 0
                if cht and bing: 
                    Edges += self.addedge(cht, tgt, cli)
                    bing = False
            elif m.group(10):
                link, bing, state, cht = True, True, 0, tgt
                cli = [m.group(10) + m.group(17), m.group(14), m.group(16) if m.group(16) else m.group(11)] 
            else:
                if not op: tgt = []
                typ, lab = m.group(8) if m.group(8) else m.group(3), m.group(6)
                nid = self.findid(m.group(2),typ,lab)
                if state != 2: 
                    tgt.append((nid, self.getport(typ,m.group(9))))
                parent = orig[-2] if state == 2 and len(orig)>1 else None
                Nodes[nid] = (parent, typ, lab)
                if state == 0: 
                    state = 1
                if not op and cht and bing: 
                    Edges += self.addedge(cht, tgt, cli)
                    bing, link = False, False
                if orig:
                    if link and orig[-1]:
                        Edges += self.addedge([(orig[-1], None)], [(nid, None)], cli)
                    link, orig[-1] = False, nid
        return Nodes, Edges

    def getport(self, typ, por):
        vpo = None
        if typ in __DATA_ports__:
            if por and re.match(r'^\d+$', por) and int(por) < len(__DATA_ports__[typ]): 
                vpo = int(por)
            elif por in __DATA_ports__[typ]:
                vpo = __DATA_ports__[typ].index(por)
            elif por == '*':
                vpo = - len(__DATA_ports__[typ])
        return vpo

    def findid(self, nid, typ, lab):
        "find best node id "
        return nid if nid else re.sub(r'\W','',lab) if lab else '__%s' % typ

    def layout(self, ast, rankdir = 'TB'):
        "dot"
        pos, d = {}, 'digraph G { rankdir=%s ' % rankdir
        Nodes, Edges = ast
        for n in Nodes:
            label = n
            d += ' %s[label="%s"];' % (n, label)
        for e in Edges:
            pass
        return pos

    def headfoot(self, appli, lang='python', host='127.0.0.1'):
        " Print header "
        def app(ast):
            (sc, ec, head) = __OUT_LANG__[lang][1]
            #head, sc, ec = '', '#', ''
            nodes, edges = ast
            digest = base64.urlsafe_b64encode(hashlib.sha1(open(__file__).read()).digest())[:5]
            d = '{}'.format(datetime.datetime.now())
            o = '{}{} ⊔ Generated Code [{}] {}\n'.format(head, sc, d[:19], ec)
            o += '{} ******** Do not edit by hand! ******** {}\n'.format(sc, ec)
            o += '{} Base64 short sha1 digest: {: >12} {}\n'.format(sc, digest, ec)
            o += '{} Host: {: >32} {}\n'.format(sc, host, ec) 
            o += '{} Forge:  https://{} {}\n'.format(sc, __url__, ec)
            o += '{} © Copyright 2012 Rockwell Collins, Inc {}\n'.format(sc, ec)
            o += '{} ** GNU General Public License  (v3) ** {}\n'.format(sc, ec) 
            o += '\n%s AST = %s %s\n\n'%(sc,re.sub(r'\-\-','__','%s %s'%ast), ec)
            nt, et = gettypes(ast)
            for n in nt:
                if n in __DATA_ports__:
                    o += '%s Node type:"%s" Ports: %s %s\n'%(sc, n, __DATA_ports__[n], ec)
            o += '\n'
            return o + appli(ast) + '\n{} {} Nodes {} Edges {: >30} {}'.format(sc, len(nodes), len(edges), 'the end of file.', ec)
        return app

    def gen_ast(self,ast):
        " ast "
        nodes, edges = ast
        o = '# ⊔ Python Abstract Syntax Structure:\n\n'
        o += 'Nodes = {\n'
        for n in nodes:
            o += ' \'{}\': {},\n'.format(n, nodes[n])
        o += '}\n\nEdges = [\n' 
        for e in edges:
            o += ' {},\n'.format(e)
        return o + ']\n'

    def gen_svg(self, ast):
        "svg"
        nodes, edges = ast
        o = '{}'.format(ast)
        pos = self.layout(ast)
        for n in nodes:
            o += ' \'{}\': {},\n'.format(n, nodes[n])
        for e in edges:
            o += ' {},\n'.format(e)
        return '<svg>{}</svg>\n'.format(o)

# (1) Doc generation 

class beamer:
    r"""%% This is generated, do not edit by hands!
\documentclass{beamer}
"""
    def __init__(self, title, author, email):
        r"""\begin{document} \frame{\titlepage}"""
        pkg = ('beamerthemeshadow', 'draftwatermark', 'listings', 'embedfile', 'graphicx', 'tikz')
        self.src = os.path.basename(sys.argv[0])
        self.tex = beamer.__doc__ + '\n'
        self.tex += r'\embedfile[filespec=%s]{%s}' % (self.src, os.path.abspath(self.src))
        self.tex += r'\title{%s}' % title + '\n'
        self.tex += r'\author{%s\inst{*}}\institute{*%s}' % (author, email) + '\n' 
        self.tex += beamer.__init__.__doc__ 
        for p in pkg:
            self.tex += r'\usepackage{%s}' % p + '\n'
        self.tex += r'\section{Draft:\texttt{%s}}' % __digest__ + '\n'

    def frame(self, title, content):
        "frame"
        self.tex += r'\frame{\frametitle{%s}' % title + '\n'
        self.tex += content + '\n'
        self.tex += '}\n'
    
    def gen_pdf(self):
        r"""\end{document}"""
        self.tex += beamer.gen_pdf.__doc__ + '\n'
        open('toto.tex','w').write(self.tex)
        subprocess.Popen(('cd /tmp; pdflatex -interaction=batchmode /home/laurent/u/toto.tex 1>/dev/null'), shell=True).communicate()
        #x = open('/tmp/toto.pdf','rb').read()
        #open('toto.pdf','w').write(open('/tmp/toto.pdf','rb').read())

def gen_doc():
    "gen_doc"
    pass

def gen_beamer():
    "gen_beamer"
    slides = beamer(r'The $\sqcup$ Language', __author__, __email__)
    slides.frame(r'What $\sqcup$ is?', r""" 
The $\sqcup$ language is a {\bf Universal Graph Language};\\
\begin{itemize}
\item Symbol: $\bigsqcup$ \\
\end{itemize} 
""")
    slides.gen_pdf()

# (2) Tests

def gen_test(cas=0):
    "gen_test"
    c, uobj = ('A', 'B', 'C', '{', '}', '->', ' '), u()
    if cas == 0:
        for i in range(10):
            x, l = '', 0
            for j in range(100):
                r, d = random.choice(c), True
                if r == '{':
                    l += 1
                elif r == '}':
                    if l > 0:
                        l -= 1
                    else:
                        d = False
                if d:
                    x += r
            if l > 0:
                x += '}'*l
            print ((i, x), uobj.parse(x))
    elif cas == 1:
        for i in range(10):
            x = functools.reduce(lambda x, i:x+random.choice(c), range(100), '')
            print ((i, x), uobj.parse(x))
    elif cas == 2:
        for x in __AST_SET__:
            print (x, uobj.parse(x[1]))

# (3) Git storage

def register(content=''):
    """ If the same content is requested, then id does not change """
    base = __BASE__
    if not os.path.isdir(base):
        os.mkdir(base)
    rev = dbhash.open('%s/rev.db' % base,'c')
    if rev.has_key(content):
        gid = rev[content]
    else:
        gid = create_id(rev)
        rev[content] = gid
    rev.close()    
    return gid

def print_db(db):
    """ """
    d = dbhash.open(db, 'r')
    for item in d.keys():
        print ('[%s] %s -> %s' % (db,item,d[item]))
    d.close()

def create_id(rev):
    """ Create a new diagram id"""
    rev['_'] = '%d' % (long(rev['_'])+1) if rev.has_key('_') else '0'
    return base64.urlsafe_b64encode(hashlib.sha1(rev['_']).digest())[:-18]

class gitu:
    """ All git methods share the same env """

    def __init__(self, user='anybody', ip='0.0.0.0'):
        """ create the GIT repository if needed"""
        if not os.path.isdir(__git_base__):
            os.mkdir(__git_base__)
        e = os.environ.copy()
        e['GIT_AUTHOR_NAME'],e['GIT_AUTHOR_EMAIL'] = user,ip
        e['GIT_COMMITTER_NAME'],e['GIT_COMMITTER_EMAIL'] = 'laurent','pelinquin@gmail.com'
        e['GIT_DIR'] = '%s/.git'%__git_base__
        self.e = e
        if not os.path.isdir(e['GIT_DIR']):
            subprocess.Popen(('git', 'init','-q'), env=e,close_fds=True).communicate()
            p = subprocess.Popen(('git', 'hash-object','-w','--stdin'), env=e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
            li = '100644 blob %s\tstart\n'%p.communicate(' \n')[0].strip()
            q = subprocess.Popen(('git', 'mktree'), env=e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
            r = subprocess.Popen(('git', 'commit-tree', q.communicate(li)[0].strip()), env=e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
            subprocess.Popen(('git', 'update-ref', 'refs/heads/master',r.communicate('start')[0].strip()), env=e, stdout=subprocess.PIPE).communicate()

    def save(self, key, c, state=''):
        """ """
        p = subprocess.Popen(('git', 'show', 'master^{tree}:'+key), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate('')
        p = subprocess.Popen(('git', 'ls-tree', 'master^{tree}'), env=self.e, stdout=subprocess.PIPE)
        liste = p.communicate()[0].strip()
        if err:
            liste += '\n100644 blob %s\t%s'%(self.sha(c),key) 
            self.commit (liste,key)
        else:
            if out[:-1] != c:
                self.commit(re.sub(r'(100644 blob) [0-9a-f]{40}(\t%s)'%key,'\\1 %s\\2'%self.sha(c),liste),key+'\n'+state)
        p = subprocess.Popen(('git', 'log','--pretty=format:%H','-1'), env=self.e, stdout=subprocess.PIPE)
        return p.communicate()[0][:15]

    def sha(self, content):
        """ """
        p = subprocess.Popen(('git', 'hash-object','-w','--stdin'), env=self.e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        return p.communicate(content+'\n')[0].strip()

    def commit(self, li, msg):
        """ """
        p = subprocess.Popen(('git', 'mktree'), env=self.e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        sha = p.communicate(li)[0].strip()
        p = subprocess.Popen(('git', 'show-ref','--hash','refs/heads/master'), env=self.e, stdout=subprocess.PIPE)
        parent = p.communicate()[0].strip()
        p = subprocess.Popen(('git', 'commit-tree', sha,'-p',parent), env=self.e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        mm = p.communicate(msg)[0].strip()
        p = subprocess.Popen(('git', 'update-ref', 'refs/heads/master',mm), env=self.e, stdout=subprocess.PIPE)

    def list(self):
        """ """
        p = subprocess.Popen(('git', 'ls-tree', 'master^{tree}'), env=self.e, stdout=subprocess.PIPE)
        liste = p.communicate()[0].strip()
        return liste.split('\n')

    def history(self, key=''):
        """ """
        if key:
            p = subprocess.Popen(('git', 'log', '--pretty=format:%H:%an:%ar:%s','--',key), env=self.e, stdout=subprocess.PIPE)
        else:
            p = subprocess.Popen(('git', 'log', '--pretty=format:%H:%an:%ar:%s'), env=self.e, stdout=subprocess.PIPE)
        liste = p.communicate()[0].strip()
        return liste.split('\n')

    def gethead(self, key):
        """ """
        p = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%H:%an:%ar:%at','--',key), env=self.e, stdout=subprocess.PIPE) # ar
        return p.communicate()[0].strip()
    
    def revision(self, key):
        """ """
        c = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%H','--', key), env=self.e, stdout=subprocess.PIPE)
        return c.communicate()[0][:15]

    def date(self, key):
        """ """
        c = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%ci','--', key), env=self.e, stdout=subprocess.PIPE)
        return c.communicate()[0][:-5]

    def cat(self, key):
        """ """
        p = subprocess.Popen(('git', 'show', 'master^{tree}:'+key), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate()
        return '"Not Found!"' if err else out[:-1]

    def cat_blob(self, key):
        """ """
        p = subprocess.Popen(('git', 'show', key), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate()
        return '' if err else out[:-1]

    def cat_revision(self, gid):
        """ """
        p = subprocess.Popen(('git', 'show', 'master^{tree}:%s'%gid), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        content, err = p.communicate()
        c = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%H', '--', gid), env=self.e, stdout=subprocess.PIPE)
        rev = c.communicate()[0]
        return ('','[Diagram Not Found cat_revision!]') if err else (rev[:15],content[:-1])

    def cat_getrev(self, rev):
        """ """
        c = subprocess.Popen(('git', 'log', '--pretty=oneline','-1',rev), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = c.communicate()
        idd, cont = ['', ''], '[Diagram Not Found!]'
        if not err:
            if out != '':
                idd = out.strip().split()
                p = subprocess.Popen(('git', 'show','%s:%s'%(rev,idd[1])), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                cont = p.communicate()[0][:-1]
        return idd[0][:15],idd[1],cont

    def cat_full(self, key, arev):
        """ """
        c = subprocess.Popen(('git', 'log', '--pretty=format:%H','-1',arev), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = c.communicate()
        rev, cont = '', '[Diagram Not Found!]'
        if not err:
            if out != '':
                rev = out.strip()
                p = subprocess.Popen(('git', 'show','%s:%s'%(rev,key)), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                cont = p.communicate()[0][:-1]
        return rev[:15], cont

    def cat_simple(self, key, rev):
        """ """
        p = subprocess.Popen(('git', 'show','%s:%s'%(rev,key)), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        return p.communicate()[0][:-1]

    def test(self, key, rev):
        """ """
        c = subprocess.Popen(('git', 'log', '%s:@%s'%(rev,key)), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        o, e = c.communicate()
        return False if e else True 
    
    def tag(self, name, rev):
        """ """
        c = subprocess.Popen(('git', 'tag', name, rev), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        o, e = c.communicate()
        return e if e else o

    def tag_list(self):
        """ """
        o, e = subprocess.Popen(('git', 'tag'), env=self.e, stdout=subprocess.PIPE).communicate()
        return o

# (4) Web application 

def reg(value):
    " function attribute is a way to access matching group in one line test "
    reg.v = value
    return value

def application(environ, start_response):
    """ WSGI Web application """
    s, mime, o, myu = urllib.unquote(environ['QUERY_STRING']), 'text/plain;charset=UTF-8', 'Error!', u()
    host = environ['SERVER_NAME']
    if reg(re.match(r'\s*(_?)(svg|)&?((\w{10})|(.*))\s*$', s, re.I)):
        lang, gid, arg = reg.v.group(2), reg.v.group(4), reg.v.group(5) 
    header = [('Content-type', mime)]
    start_response('200 OK', header)
    if lang:
        o = myu.headfoot(myu.gen_svg, lang, host)(myu.parse(arg))
    else:
        if gid:
            mygit = gitu()
            arg = mygit.cat(gid)
        o = myu.headfoot(myu.gen_ast, 'python', host)(myu.parse(arg))
    return [(o)] 

# (5) Command line

def post_old(server, service, content):
    B, CRLF = '----------ThIs_Is_tHe_bouNdaRY_$',  '\r\n'
    body = CRLF.join(['--%s' % B, 'Content-Disposition: form-data; name=""; filename=""', 'Content-Type: text/html', '', content,'--%s--' % B, ''])
    h = http.client.HTTPConnection(server)
    h.putrequest('POST', service)
    h.putheader('content-type', 'multipart/form-data; boundary=%s' % B)
    h.putheader('content-length', str(len(body)))
    h.endheaders()
    h.send(body)
    h.getreply()
    return h.file.read().encode('utf-8')

def post(server, service, content):
    B, CRLF = '----------ThIs_Is_tHe_bouNdaRY_$',  '\r\n'
    body = CRLF.join(['--%s' % B, 'Content-Disposition: form-data; name=""; filename=""', 'Content-Type: text/plain', '', content,'--%s--' % B, ''])
    h = http.client.HTTPConnection(server)
    headers = {'content-type': 'multipart/form-data; boundary=%s' % B, 'content-length': str(len(body))}
    h.request('POST', '', service, headers)
    print ('ici')
    r = h.getresponse()
    print (r.status)
    return r.read()

if __name__ == '__main__':
    " Command line"
    import getopt, http.client
    __digest__ = base64.urlsafe_b64encode(hashlib.sha1(open(__file__).read().encode('utf-8')).digest())[:5]
    opts, args = getopt.getopt(sys.argv[1:],'h:f:s',['host=','format=','stdin'])
    o,host = 'raw','127.0.0.1' # use '193.84.73.209 for testing'

    if not opts and not args:
        gen_test(2) 
        gen_doc()
        gen_beamer()
        print(__digest__)
    
    for r in opts:
        if r[0] in ('-h','--host'):
            host = r[1] 
        elif r[0] in ('-f','--format'):
            o = r[1]
        elif r[0] in ('-s','--stdin'):
            args.append(sys.stdin.read())
        else:
            print (help('u')) 
 
    for arg in args:
        data = open(arg).read() if os.path.isfile(arg) else arg
        #print (post(host, '/u?%s'%o, data))
        post('pelinquin','/u3?','A->B')
 
    ########## debug zone! ##############
    MYU = u()
    #print (MYU.gen_svg(MYU.parse('A.1->B.2')))
    
    
    
