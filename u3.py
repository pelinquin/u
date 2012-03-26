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

# SHORT TODO LIST:
# - nested dot layout
# - use python compiler module
# - svg with proportional police size
# - types data in Berkeley database
# - add block operator

r"""
Abstract LaTeX doc here!
"""

__author__   = 'Laurent Fournier'
__email__    = 'lfournie@rockwellcollins.com'
__title__    = 'The Universal Short Graph Language'
__version__  = '0.3'
__license__  = 'GPLv3'
__url__      = 'github.com/pelinquin/u'
__git_base__ = '/u'

import sys, os, re, hashlib, subprocess, urllib.parse, base64, random, functools, datetime, shutil, html, ast

__digest__ = base64.urlsafe_b64encode(hashlib.sha1(open(__file__, 'r', encoding='utf-8').read().encode('utf-8')).digest())[:5]

_XHTMLNS  = 'xmlns="http://www.w3.org/1999/xhtml"'
_SVGNS    = 'xmlns="http://www.w3.org/2000/svg"'
_XLINKNS  = 'xmlns:xlink="http://www.w3.org/1999/xlink"'

__separator__ = r'[\|\'`";,!~^@*+/$]' # 14 chars
__delimiter__ = r'%s(?:%s%s)?' % (__separator__, __separator__, __separator__) # one or three chars
__RE_U__ = r'''     # RegExp 
   (?:              # Three basic tokens:
    ([{}])          # (1) Block 
   |
    (?:\#[^\n]*)    # or (2) Line comment
   |                # or (3) NODE:
    (?=[^\s<\-=>])  # Not empty token 
    (?:(\w{1,10})|) # Name      
    (?::(\w)|)      # Type pre  
    ((%s)(.+?)\5|\[([^\]]+)\]|\(([^)]+)\)|) # Content
    (\w|)           # Type post 
    (?:\.(\w{1,20}|\*)|) # Port      
   |                # or (4) ARC:  
    ([<\-=>])       # Head      
    (?:(\w)|)       # Type pre  
    ((%s)(.+?)\14|\[([^\]]+)\]|\(([^)]+)\)|) # Content
    (\w|)           # Type post
    ([<\-=>])       # Tail
)''' % (__delimiter__, __delimiter__)

__ARC_T__  = ('--', '->', '-<', '-=', '=-', '=>', '=<', '==', '<-', '<>', '<<', '<=', '>-', '>>', '><', '>=')
__NODE_T__ = ('|',  '\'', '`',  '"',  ';',  ',',  '!',  '~',  '^',  '@',  '*',  '+',  '/',  '$',  '(',  '[' )
 
__ACTIONS__ = ('download', 'source', 'update', 'about', 'help', 'usage', 'pdf', 
               'paper', 'beamer', 'edit', 'ace', 'git', 'log', 'test', 'parse', 'unparse', 'random')

__OUT_LANG__ = {'c'          :['c',    ('/*', '*/', ''), 'gcc ansi pedantic'],
                'objectivec' :['m',    ('/*', '*/', ''), ''],
                'python'     :['py',   ('#', '', '#!/usr/bin/python3\n# -*- coding: utf-8 -*-\n'), ' python 3.2 '],
                'ada'        :['adb',  ('--', '', ''), 'Ada95 Ravenscar'],
                'scala'      :['scl',  ('--', '', ''), ''],
                'java'       :['java', ('//', '', ''), ''],
                'ruby'       :['rb',   ('#', '', ''), ''],
                'ocaml'      :['ml',   ('(*', '*)', ''), ''],
                'haskell'    :['hs',   ('{-', '-}', ''), ''],
                'lua'        :['lua',  ('--', '', ''), ''],
                'tikz'       :['tex',  ('%', '', ''), 'for LaTeX'],
                'svg'        :['svg',  ('<!--', '-->', '<?xml version="1.0" encoding="UTF-8"?>\n'), 'Mozilla  Webkit'],
                'aadl'       :['adl',  ('--', '', ''), 'AADL v2'],
                'sdl'        :['sdl',  ('--', '', ''), ''],
                'lustre'     :['lst',  ('--', '', ''), ''],
                'vhdl'       :['hdl',  ('--', '', ''), ''],
                'systemc'    :['sc',   ('//', '', ''), ''],
                'xml'        :['xml',  ('<!--', '-->', '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'), '⊔ serialization'],
}
 
__DATA_ports__ = {
    None:('p1', 'p2', 'p3', 'p4'),
    'T': ('i', 'o'),
    'O': ('in1', 'in2', 'out1', 'out2'),
    'V': ('in1', 'in2', 'out1', 'out2'),
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
        }, {
        None:'stroke:black; stroke-width:1.5; fill:none; marker-end:url(#.arrow);',
        'r': 'stroke:black; stroke-width:1.5; fill:none; marker-start:url(#.r_arrow);',
        'I': 'stroke:green; stroke-width:2; fill:none; marker-end:url(#.arrow);',
        'L': 'stroke:red; stroke-width:3; fill:none; marker-end:url(#.arrow);',
        })

__DATA_tikz__ = ({
        None:('node','circle,drop shadow,draw=green!40,fill=gray!20'),
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
        }, {
        None:'--',
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
        }, {
        None:(),
        })

__DATA_python__ = ({
        'C': ('class', ),
        'c': ('class', ),
        }, {
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
__DATA_xml__        = ({None: (), }, {None:(), })

__IN_MODEL__ = {
    'UML':                   """myclass:C"hello" 
""", 
    'SysML':                 'A->B', 
    'AADL-Graph':            'A->B', 
    'Marte':                 'A->B', 
    'PSL':                   'A->B', 
    'Xcos':                  'A->B', 
    'Kaos':                  'A->B', 
    'Entity-Relation-Graph': 'A->B', 
    'Tree-Diagram':          'A->B',
    'Network-Graph':         'A->B', 
    'Flowchart':             'A->B', 
    'Petri-net':             'A->B', 
    'State-Machine':         'A->B', 
    'Markov-Chain':          'A->B', 
    'Behavior-Tree':         'A->B',
} 

__AST_SET__ = [
    ('OptimizedUnparse',       'A"l1"->B"l2"'),
    ('Multidef',               'A"label" A:T A'),
    ('Nested',                 'A{B C{D E} F K{I}} G C{H}'),
    ('Cyclic',                 'A{B{A}}'),
    ('Parent0',                'A(label A)T {B(label B)U}'),
    ('Link0',                  'A->B'),
    ('Link1',                  'A->{B}->C->D{E}->F'),
    ('Link2',                  'A{B{C->D}->E{F->G}}->H{I{J->K}->L{M->N}}'),
    ('Parent1',                'A{B C} {L} D->{E} F{G H{I->J} K} }}}'),
    ('Parent2',                'A{B->C} D{{E}} F{G}{H}'),
    ('HelloWorld',             'Hello->World'),
    ('BasicComposition',       'Parent{Child1 Child2}'),
    ('BasicMultilinks',        'A->{B C}'),
    ('BasicPorts',             'A.1->B.p2'),
    ('BasicLabel',             'nodeA(This is a label for nodeA)'),
    ('BasicNodeType',          'nodeA:T'),
    ('BasicArcType',           'A<T>B'), 
    ('BasicFullLink',          'B[l1]T.0 -U"l2"> C:V(l3).2 '),
    ('BasicParentFullLink',    'A { B -> C } '),
    ('BasicYUMLstyle',         '(node1) -> [node2]'),
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
    ('Delimiter:|',            'A|content|'),
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
    ('Delimiter:[]',           'N[content]'),
    ('CustomDelimiter1',       'O*/@ content */@'),
    ('CustomDelimiter2',       'P*/* content */*'),
    ('CustomDelimiter3',       'Q/*/ content /*/'),
    ('CustomDelimiter4',       'R+++ content +++'),
    ('CustomDelimiter5',       'S**/ content **/'),
    ('WordId',                 'Aaa'),
    ('NotFirstDigit',          'A1'),
    ('FirstDigit',             '1A'),
    ('WhiteSpace',             ' A'),
    ('EndWhiteSpace',          'A '),
    ('BothWhiteSpaces',        ' A '),
    ('SeveralLines',           '\n\nA\n\n'),
    ('Latin1Char',             'éàùç'),
    ('UnicodeCharId',          '您'),
    ('UnicodeCharType',        'A:您'),
    ('UnicodeCharContent',     'B(好您)'),
    ('UnicodeCharLinkType',    'A-好>B'),
    ('Long content',           '"This is a long content" '),
    ('Multilines',             '"Multi\nlines\ncontent" '),
    ('Types',                  'A:T B:U'),
    ('2nodes',                 'A B'),
    ('3nodes',                 'A B C'),
    ('4nodes',                 'A B C D'),
    ('OverloadContent',        'A"content1" A"content2"'),
    ('Accumulation1',          'A A"content"'),
    ('Accumulation2',          'A"content" A'),
    ('TypeContentAcc',         'A:T A"content"'),
    ('ContentTypeAcc',         'A"content" A:T'),
    ('ChildContentAcc',        'A{a} A"content"'),
    ('ContentChildAcc',        'A"content" A{a}'),
    ('Case01',                 'A A"content1" A"content2"T'),
    ('Case02',                 'A"content2"T A"content" A'),
    ('Case03',                 '"content"T "content"U'),
    ('Case04',                 'A"content" B"content"'),
    ('Case05',                 'A{a} B{b1 b2} C{c1 c2 c3}'),
    ('Case06',                 'A{a} B {b} {c}'),
    ('Case07',                 ':T{a b} :U{c d}'),
    ('Case08',                 'A{ B{c b} C{e f} }'),
    ('Case09',                 'A{B{C{c}}}'),
    ('Case10',                 '"x-y"'),
    ('Case11',                 '"x+y"'),
    ('Case12',                 '"x*y"'),
    ('Case13',                 '"x/y"'),
    ('Case14',                 '"x.y"'),
    ('Case15',                 '"x,y"'),
    ('Case16',                 '"x%y"'),
    ('Case17',                 '"x^y"'),
    ('Case18',                 '"x=y"'),
    ('Case19',                 '"x:y"'),
    ('Case20',                 '"x&y"'),
    ('Case21',                 '"x|y"'),
    ('Case22',                 '"x>y"'),
    ('Case23',                 '"x<y"'),
    ('Case24',                 '#comment\nA'),
    ('Case25',                 '#comment1\nA\n #comment2'),
    ('Chain2',                 'A->B'),
    ('Chain3',                 'A->B->C'),
    ('Link1',                  'A>>B'), 
    ('Link2',                  'A><B'),
    ('Link3',                  'A>-B'),
    ('Link4',                  'A>=B'),
    ('Link5',                  'A<>B'),
    ('Link6',                  'A<<B'),
    ('Link7',                  'A<-B'),
    ('Link8',                  'A<=B'),
    ('Link9',                  'A->B'),
    ('Link10',                 'A-<B'),
    ('Link11',                 'A--B'), 
    ('Link12',                 'A-=B'),
    ('Link13',                 'A=>B'), 
    ('Link14',                 'A=<B'),
    ('Link15',                 'A=-B'),
    ('Link16',                 'A==B'),
    ('SpaceBeforeArc',        'A ->B'),
    ('SpaceAfterArc',         'A-> B'),
    ('Spaces',                 'A -> B'),
    ('Autoref',                'A->A'),
    ('Case26',                 'A -(content)- B'),
    ('Case27',                 'A -T- B'),
    ('Case28',                 'A -T(content)- B'),
    ('Case29',                 'A -(content)T- B'),
    ('Case30',                 'A{a1 a2} -> B{b1 b2}'),
    ('Case31',                 '{a1 a2} -> B{b1 b2}'),
    ('Case32',                 'A{a1 a2} -> {b1 b2}'),
    ('Case33',                 '{a1 a2} -> {b1 b2}'),
    ('Case34',                 'A{a1 -> a2} B{b1 -> b2}'),
    ('Case35',                 'A{a1 -> a2} -> B{b1 -> b2}'),
    ('Case36',                 'A.1 -> B.2'),
    ('Case37',                 'A.p1 -> B.p2'),
    ('Case38',                 'A:T B:U A.1->B.2'),
    ('Case39',                 'A.1->B.2 A:T B:U'),
    ('Port0',                  'A.0->B.pin1->C.*->D'),
    ('Port1',                  'A:T.0->B:T.1'),
    ('Port2',                  'A:T.1->{B:T.0 C:T.0}'),
    ('Port3',                  '{A:T.1 B:T.0}->C:T.1'),
    ('Port4',                  '{A:T.1 B:T.0}->{C:T.1 D:T.0}'),
    ('Port5',                  'A:T.1->B:T.0->C:T.1'),
    ('Port*',                  'A:T.*->B:T.*'),
    ('PortOutOfRange',         'A:T.5->B:T.pin12'),
    ('Doubledefinition',       'A{a} A{b}'),
    ('LinkBefore',             '->A{->B}'),
    ('LinkAfter',              '{A->}B->'),
    ('LinkOverload',           'A->--B'),
]

# (0) Parser

def gettypes(ast):
    "_"
    nl, el = {'':True}, {'':True}
    nodes, arcs = ast
    for n in nodes:
        if len(nodes[n]) > 1:
            nl[nodes[n][1]] = True 
    for e in arcs:
        if len(e) > 3:
            el[e[3]] = True 
    return nl, el

class u:
    """ This is the base class for ⊔ 
    One can customize that class by adding/modifying __DADA_xxx__ structure and by overloading a gen_xxx() method
    """
  
    def __init__(self):
        "Load types mapping"
        self.m = {}
        def gen_x(self, ast):
            return 'TODO!'
        for l in __OUT_LANG__:
            self.m[l] = eval('__DATA_%s__' % l)
            if not (hasattr(u, 'gen_%s' % l) and callable(getattr(u, 'gen_%s' % l))):
                gen_x.__name__ = 'gen_%s' % l
                setattr(u, gen_x.__name__, gen_x)

    def addarcs(self, child, target, cli):
        "not used!"
        arcs = []
        for c in child:
            tc = range(-int(c[1])) if c[1] and int(c[1]) < 0 else [c[1]]
            for i in target:
                ti = range(-int(i[1])) if i[1] and int(i[1]) < 0 else [i[1]]
                for p in tc:
                    for q in ti:
                        arcs.append(((c[0], p), (i[0], q)) + cli)
        return arcs

    def addarc(self, c, i, cli):
        "utils"
        arcs = []
        tc = range(-int(c[1])) if c[1] and int(c[1]) < 0 else [c[1]]
        ti = range(-int(i[1])) if i[1] and int(i[1]) < 0 else [i[1]]
        for p in tc:
            for q in ti:
                arcs.append(((c[0], p), (i[0], q)) + cli)
        return arcs

    def typeLabel(self, g, arc=True):
        "_"
        if arc:
            lab = g[16] if g[16] else g[15] if g[15] else g[14]
            typ = g[17] if g[17] else g[11]
            sep = g[13] if g[13] else g[12][0] + g[12][-1] if g[12] else None 
            res = (__ARC_T__.index(g[10]+g[18]), typ, sep, lab)
        else:
            typ = g[8] if g[8] else g[2]
            lab = g[7] if g[7] else g[6] if g[6] else g[5]
            sep = g[4] if g[4] else g[3][0] + g[3][-1] if g[3] else None 
            nid = g[1] if g[1] else re.sub(r'\W', '', '_%s' % lab.lower())[:9] if lab else '__%s' % typ
            res = (nid, typ, sep, lab)
        return res

    def merge_attr(self, nid, nodes, prt, typ, sep, lab):
        "_"
        if nid in nodes:
            tmp, i = list(nodes[nid]), 0
            for elt in (prt, typ, sep, lab):
                if elt != None:
                    tmp[i] = elt
                i += 1
            nodes[nid] = tuple(tmp)
        else:
            nodes[nid] = (prt, typ, sep, lab)    

    def parse(self, x):
        "kernel parser"
        nodes, arcs, = {}, []
        sak = [(None, None),] # for parent setting
        sgl, cli, stl = False, (), [[],] # for arc setting
        for m in re.compile(__RE_U__, re.U|re.X|re.S).finditer(x):
            if sak:
                if m.group(1) == '{': # open block
                    sak.append((None, None))
                    stl.append([])
                    sgl = False
                elif m.group(1) == '}': # close block
                    sak.pop()
                    stl.pop()
                    if sak: sak[-1] = (None, None)
                    sgl = False
                elif m.group(11): # link
                    sak[-1] = (None, None)
                    cli, sgl = self.typeLabel(m.groups()), True
                else: # node
                    (nid, typ, sep, lab) = self.typeLabel(m.groups(), False)
                    por, prt = self.getport(typ, m.group(10)), sak[-2][0] if len(sak)>1 else None
                    if not prt and len(stl)>1: stl[-2] = []
                    if sgl and stl[-1]: 
                        arcs += self.addarc(stl[-1][-1], (nid, por), cli) 
                    sak[-1], sgl = (nid, por), False
                    stl[-1].append((nid, por))
                    self.merge_attr(nid, nodes, prt, typ, sep, lab)
        return nodes, arcs

    def format_node(self, n, nod):
        "_"
        typ = ':%s' % nod[1] if nod[1] else ''
        [st1, st2] = nod[2] if nod[2] and len(nod[2]) == 2 else [nod[2], nod[2]]
        lab = '%s%s%s' % (st1, nod[3], st2) if nod[3] else ''
        return '{}{}{}'.format(n, typ, lab)

    def format_arc(self, e):
        "_"
        [st1, st2] = e[4] if e[4] and len(e[4]) == 2 else [e[4], e[4]]
        lab, typ = '%s%s%s' % (st1, e[5], st2) if e[5] else '', '%s' % e[3] if e[3] else ''
        return '{}{}'.format(typ, lab)

    def unparse(self, ast):
        " returns an optimized u string from an AST"
        nodes, arcs = ast
        count = {}
        for e in arcs:
            for x in (e[0][0], e[1][0]):
                if not nodes[x][0]:
                    count[x] = count[x] + 1 if x in count else 0
        o, tree = '', {}
        #o += 'count{}\n'.format(count)
        #o += 'nodes{}\n'.format(nodes)
        for n in nodes:
            if n not in count or count[n] > 0:
                prnt = nodes[n][0]
                if prnt:
                    if prnt in tree:
                        tree[prnt].append(n)
                    else:
                        tree[prnt] = [n]
                elif not n in tree:
                    tree[n] = [] 
        #o += 'tree{}\n'.format(tree)
        for n in tree:
            o += self.format_node(n, nodes[n])
            if tree[n]:
                o += '{'
                for c in tree[n]:
                    o += self.format_node(c, nodes[c]) + ' '
                o = o[:-1] + '}'
            o += ' '
        if tree:
            o += '\n'
        for e in arcs:
            (p0, p1, nr) = ('.%s'%e[0][1] if e[0][1] != None else '', '.%s' % e[1][1] if e[1][1] != None else '', [])
            for n in (e[0][0], e[1][0]):
                nr.append(self.format_node(n, nodes[n]) if n in count and count[n] == 0 else n)                     
            arrow = __ARC_T__[e[2]]
            o += '{}{} {}{}{} {}{} '.format(nr[0], p0, arrow[0], self.format_arc(e), arrow[1], nr[1], p1)
        if arcs:
            o += '\n'
        return o 

    def getport(self, typ, por):
        "_"
        vpo = None
        if typ in __DATA_ports__:
            if por and re.match(r'^\d+$', por) and int(por) < len(__DATA_ports__[typ]): 
                vpo = int(por)
            elif por in __DATA_ports__[typ]:
                vpo = __DATA_ports__[typ].index(por)
            elif por == '*':
                vpo = - len(__DATA_ports__[typ])
        return vpo

    def headfoot(self, appli, lang='python', host='127.0.0.1'):
        "Print header"
        def app(ast):
            (sc, ec, head) = __OUT_LANG__[lang][1]
            nodes, arcs = ast
            d = '{}'.format(datetime.datetime.now())
            o = '{}{} ⊔ Generated Code [{}] {}\n'.format(head, sc, d[:19], ec)
            o += '{} ******** Do not edit by hand! ******** {}\n'.format(sc, ec)
            o += '{} Base64 short sha1 digest: {: >12} {}\n'.format(sc, __digest__.decode('utf-8'), ec)
            o += '{} Host: {: >32} {}\n'.format(sc, host, ec) 
            o += '{} Forge:  https://{} {}\n'.format(sc, __url__, ec)
            o += '{} © Copyright 2012 Rockwell Collins, Inc {}\n'.format(sc, ec)
            o += '{} ** GNU General Public License  (v3) ** {}\n'.format(sc, ec) 
            o += '{} Output language: {} [{}] {}\n'.format(sc, lang, __OUT_LANG__[lang][2], ec)
            o += '\n%s AST = %s %s\n\n' % (sc, re.sub(r'\-\-', '__', '%s %s' % ast), ec)
            nt, et = gettypes(ast)
            for n in nt:
                if n in __DATA_ports__:
                    o += '%s Node type:"%s" Ports: %s %s\n' % (sc, n, __DATA_ports__[n], ec)
            o += '\n'
            for e in et:
                o += '' # language dependent!
                #o += '%s Arc type:"%s" %s\n' % (sc, 'tmp', ec)
            o += '%s Topologic sort:%s %s\n' % (sc, self.toposort(arcs), ec)
            return o + appli(ast) + '\n{} {} Nodes {} Arcs {: >30} {}'.format(sc, len(nodes), len(arcs), 'the end of file.', ec)
        return app

    def gen_ast(self, ast):
        " ast "
        nodes, arcs = ast
        o = '# ⊔ Python Abstract Syntax Structure:\n\n'
        o += 'Nodes = {\n'
        for n in nodes:
            o += ' \'{}\': {},\n'.format(n, nodes[n])
        o += '}\n\nArcs = [\n' 
        for e in arcs:
            o += ' {},\n'.format(e)
        return o + ']\n'

    def layout(self, ast, rankdir='TB'):
        "Computes 2D automatic layout for graphics (tikz and svg) generation"
        nodes, arcs = ast
        bbx, bby, pos, d = None, None, {}, 'digraph G { rankdir=%s ' % rankdir
        for n in nodes:
            d += ' %s[label="%s"];' % (n, n)
        for e in arcs:
            d += ' %s->%s %s' % (e[0][0], e[1][0], '') 
        d += '}' 
        #print (d) # for debug
        p = subprocess.Popen(('dot'), stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        for l in p.communicate(input=d.encode('utf-8'))[0].decode('utf-8').split('\n'):
            #print (l) for debug
            if reg(re.search('bb="0,0,([\.\d]+),([\.\d]+)"', l)):
                bbx, bby = float(reg.v.group(1)), float(reg.v.group(2))
            elif reg(re.search('^\s*(\w+)\s*\[label=[^,]*, pos="([\.\d]+),([\.\d]+)"', l)) and bbx and bby:
                pos[reg.v.group(1)] = (int(float(reg.v.group(2))*100/bbx), int(float(reg.v.group(3))*100/bby))
        return pos

    def include_js(self):
        r"""
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
  var t = document.documentElement.childNodes;
  for (var n = 0; n < t.length; n++) {
    if (t[n].nodeName == 'text') { 
      var b = t[n].getBBox();
      var tx = parseInt(t[n].getAttribute('x'));
      var ty = parseInt(t[n].getAttribute('y'));      
      box[t[n].id] = [b.x, b.y, b.width, b.height, tx, ty]; 
    }
  }
  if (Object.keys(box).length != 0) { submitURL(box); }
}"""
        o = '<script %s type="text/ecmascript">\n/*<![CDATA[*//*---->*/\n' % _XLINKNS
        return o + self.include_js.__doc__  + '\n/*--*//*]]>*/</script>\n'

    def include_js_zoom_pan(self):
        r"""var pan = false, stO, stF;document.documentElement.setAttributeNS(null,"onmouseup","hMouseUp(evt)"); document.documentElement.setAttributeNS(null,"onmousedown", "hMouseDown(evt)");document.documentElement.setAttributeNS(null,"onmousemove","hMouseMove(evt)");window.addEventListener('DOMMouseScroll', hMouseWheel, false); 
function getP(evt) { var p = document.documentElement.createSVGPoint(); p.x = evt.clientX; p.y = evt.clientY; return p; }
function setCTM(ele,m) { ele.setAttribute("transform", "matrix(" + m.a + "," + m.b + "," + m.c + "," + m.d + "," + m.e + "," + m.f + ")"); }
function hMouseMove(evt) { if (pan) { var p = getP(evt).matrixTransform(stF); setCTM(document.getElementById('.graph'), stF.inverse().translate(p.x-stO.x,p.y-stO.y)); }}
function hMouseDown(evt) { 
document.documentElement.setAttribute('cursor','-moz-grabbing'); // for FF only //document.documentElement.setAttribute('cursor','move');
pan = true; stF = document.getElementById('.graph').getCTM().inverse(); stO = getP(evt).matrixTransform(stF);}
function hMouseUp(evt) { pan = false; document.getElementById('zoom').firstChild.nodeValue = stO.x.toFixed(0) + ',' + stO.y.toFixed(0); document.documentElement.setAttribute('cursor','default');}
function hMouseWheel(evt) { var g = document.getElementById('.graph'); do_zoom(evt.detail,getP(evt));}
function zoom(delta) { var q = document.documentElement.createSVGPoint(); q.x = window.innerWidth/2; q.y = window.innerHeight/2; do_zoom(delta,q);}
function do_zoom(delta,q) {
  var g = document.getElementById('.graph'); 
  var p = q.matrixTransform(g.getCTM().inverse()); 
  var tg = document.getElementById('target');
  if (delta<0) { tg.firstChild.setAttribute('display','inline'); } else { tg.firstChild.nextSibling.setAttribute('display','inline'); }
  tg.setAttribute('transform','translate(' + q.x + ',' + q.y + ')');
  var k = document.documentElement.createSVGMatrix().translate(p.x,p.y).scale(1+delta/-90).translate(-p.x,-p.y); 
  setCTM(g, g.getCTM().multiply(k)); 
  if(typeof(stF) == "undefined") stF = g.getCTM().inverse(); 
  stF = stF.multiply(k.inverse()); 
  document.getElementById('zoom').firstChild.nodeValue = (100.0/stF.a).toFixed(0)+ '%';
  setTimeout('hidetarget()',400);
}
function hidetarget() {var tg = document.getElementById('target'); tg.firstChild.setAttribute('display','none'); tg.firstChild.nextSibling.setAttribute('display','none');}"""
        o = '<script %s type="text/ecmascript">\n/*<![CDATA[*//*---->*/\n' % _XLINKNS
        return o + self.include_js_zoom_pan.__doc__  + '\n/*--*//*]]>*/</script>\n'

    def user_interface(self):
        "_"
        o = '<text id="zoom"  x="99%" y="12" title="zoom factor">100%</text>\n'
        o += '<svg id="zoomin" title="zoom in" onclick="zoom(-10);" viewBox="0 0 20 20" y="18" width="99%" height="20" preserveAspectRatio="xMaxYMin meet"><rect height="100%" width="20" fill="lightgray" stroke-width="0"/><rect x="3" y="8" height="20%" width="14" fill="white"/><rect x="8" y="3" height="70%" width="4" fill="white"/></svg>'
        o += '<svg id="zoomout" title="zoom out" onclick="zoom(10);" viewBox="0 0 20 20" y="40" width="99%" height="20" preserveAspectRatio="xMaxYMin meet"><rect height="100%" width="20" fill="lightgray" stroke-width="0"/><rect x="3" y="8" height="20%" width="14" fill="white"/></svg>\n'
        o += '<g id="target" transform="translate(0,0)" fill="none" stroke-width="1" stroke="red"><path display="none" d="M-50,-40L-50,-50L-40,-50M40,-50L50,-50L50,-40M50,40L50,50L40,50M-40,50L-50,50L-50,40"/><path display="none" d="M-60,-50L-50,-50L-50,-60M50,-60L50,-50L60,-50M60,50L50,50L50,60M-50,60L-50,50L-60,50"/></g>\n'
        return o

    def gen_svg(self, ast, box={}):
        "svg"
        o = '<svg %s>\n' % _SVGNS + self.gen_svg_header(True if box else False)
        if box: 
            nodes, arcs = ast
            seq = self.toposort(arcs)            
            o += '<title id=".title">%s</title>\n' % __title__ + favicon() + logo(.02) + '\n' + self.include_js_zoom_pan() + self.user_interface()
            o += '<g id=".graph">\n'
            for n in box:
                t = nodes[n][1]
                mx, my = __DATA_svg__[0][t][4], __DATA_svg__[0][t][5]
                box[n][:4] = [sum(p) for p in zip(box[n][:4],(-mx, -my, 2*mx, 2*my))]
                o += ' <rect stroke="red" stroke-width="2" fill="none" x="%s" y="%s" width="%s" height="%s"/>\n' % tuple(box[n][:4])
                lab = nodes[n][3] if nodes[n][3] != None else n
                o += ' <text class="node" id="{}" x="{}" y="{}">{}</text>\n'.format(n, box[n][4], box[n][5], lab) 
            o += ' <g id=".arcs" >\n'
            for e in arcs:
                d = npath(box[e[0][0]], box[e[1][0]])
                o += '  <path d="{}"/>\n'.format(d)
            o += ' </g> <!--arcs -->\n'
            o += '</g> <!-- graph -->\n' 
        else:
            nodes, arcs = ast # temporary use pos instead!
            o += self.include_js()
            pos, ratio = self.layout(ast), 4
            for n in pos:
                x, y = pos[n][0]*ratio, pos[n][1]*ratio
                lab = nodes[n][3] if nodes[n][3] != None else n
                o += '<text id="{}" x="{}" y="{}">{}</text>\n'.format(n, x, y, lab) 
        return o + '</svg>\n'

    def toposort(self, arcs):
        "returns topologic sort"
        def tsort(d):
            while True:
                ordered = set(item for item, dep in d.items() if not dep)
                if not ordered: break
                yield ','.join(sorted(ordered))
                d = { item: (dep - ordered) for item, dep in d.items() if item not in ordered }
            if d: # cycle!
                yield 
        data = {}
        for e in arcs:
            n1, n2 = e[0][0], e[1][0]
            data.setdefault(n1, []).append(n2)
            if not n2 in data: data[n2] = []
        res = [z for z in tsort({i:set(data[i]) for i in data})]
        res.reverse()
        return res

    def gen_svg_header(self, full=False):
        "_"
        o = '<style type="text/css">\n'
        o += '@font-face{font-family:Graublau; src: url(\'./fonts/GraublauWeb.otf\') format("opentype");}\n'
        o += '@font-face{font-family:vag; src: url(\'./fonts/VAG-HandWritten.otf\') format("opentype");}\n'
        o += 'text{font-family:helvetica neue,helvetica,arial,sans-serif;}\n'
        o += 'text.tiny,tspan.tiny{font-family:helvetica neue,helvetica,arial,sans-serif;font-size: 4px;fill:DarkSlateGray;}\n'
        o += 'tspan.body{font-family:helvetica neue,helvetica,arial,sans-serif;font-size: .5em;fill:DarkSlateGray;}\n'
        o += 'text.node{font-size: 1em;}\ntext#zoom{font-size: .8em;fill:lightgray;text-anchor:end;}\n'
        if full:
            o += 'svg#zoomin,svg#zoomout{cursor:pointer;}\n'
            o += 'path{stroke:black;}\n'
            o += 'g#target path{stroke:red;}\n'
        o += '</style>'
        return o + '\n'

    def gen_c(self, ast):
        "c"
        nodes, arcs = ast
        o = ''
        pos = self.layout(ast)
        for n in nodes:
            o += ' \'{}\': {},\n'.format(n, nodes[n])
        for e in arcs:
            o += ' {},\n'.format(e)
        return o

    def gen_python(self, ast):
        "python"
        #nodes, arcs = ast
        o = ''
        #for n in nodes: o += ' \'{}\': {},\n'.format(n, nodes[n])
        #for e in arcs: o += ' {},\n'.format(e)
        return o 

    def gen_xml(self, ast):
        """u,node,arc{display:block;} node{color:blue;} arc{color:green;}
u:before{display:block;text-align:center;font-size:20pt;content: '⊔ XML serialization';}
u:after{display:block;font-size:8pt;text-align:right;content: '[' attr(digest) ']';}
node{display:block;font-size:10pt;} 
node:before{display:block;font-size:12pt;content: 'Node: 'attr(id)' Parent: ('attr(parent)') Type: ['attr(type)'] Separator:'attr(separator)} 
arc{display:block;font-size:10pt;} 
arc:before{display:block;font-size:12pt;content: 'Arc: 'attr(src)' port:('attr(src_port)') -> 'attr(dst)' port:('attr(dst_port)') Type: [' attr(type)']'}"""
        nodes, arcs = ast
        o = '<?xml-stylesheet type="text/css" href="data:text/css,{}"?>\n\n'.format(self.gen_xml.__doc__)
        o += '<u digest="{}">\n <nodes nb="{}">\n'.format(__digest__.decode('utf-8'), len(nodes))
        for n in nodes: 
            par = ' parent="{}"'.format(nodes[n][0]) if nodes[n][0] else ''
            typ = ' type="{}{}"'.format(nodes[n][1], html.escape(nodes[n][2]) if nodes[n][2] else '') if nodes[n][1] or nodes[n][2] else ''
            o += '  <node id="{}"{}{}'.format(n, par, typ)
            o += '>\n   {}\n  </node'.format(nodes[n][3]) if nodes[n][3] else '/'
            o += '>\n'
        o += ' </nodes>\n <arcs nb="%s">\n' % len(arcs)
        for e in arcs: 
            typ = ' type="{}{}{}"'.format(e[3] if e[3] else '', e[2], html.escape(e[4]) if e[4] else '') 
            psrc = ' src_port="{}"'.format(__DATA_ports__[e[3]][e[0][1]]) if e[0][1] != None else ''
            pdst = ' dst_port="{}"'.format(__DATA_ports__[e[3]][e[1][1]]) if e[1][1] != None else ''       
            o += '  <arc src="{}"{} dst="{}"{}{}'.format(e[0][0], psrc, e[1][0], pdst, typ)
            o += '>\n   {}\n  </arc'.format(html.escape(e[5])) if e[5] else '/' 
            o += '>\n'
        return o + ' </arcs>\n</u>\n' 

# utilities

def npath(b1, b2):
    "computes svg path for linking two boxes arcs"
    x1, y1, x2, y2 = b1[0] + b1[2]/2, b1[1] + b1[3]/2, b2[0] + b2[2]/2, b2[1] + b2[3]/2
    h1, l1, h2, l2 = 1 + b1[3]/2, 1 + b1[2]/2, 1 + b2[3]/2, 1 + b2[2]/2
    if x1 == x2:
        if y1 < y2:  
            y1 += h1
            y2 -= h2
        else: 
            y1 -= h1
            y2 += h2
    elif y1 == y2:
        if x1 < x2: 
            x1 += l1
            x2 -= l2
        else: 
            x1 -= l1
            x2 += l2
    else:
        Q, R = x1-x2, y1-y2
        P = Q/R
        if abs(P) < l1/h1:
            if R < 0: 
                y1 += h1 
                x1 += h1*P
            else: 
                y1 -= h1
                x1 -= h1*P
        else:
            if Q < 0:
                x1 += l1
                y1 += l1/P
            else:
                x1 -= l1
                y1 -= l1/P
        if abs(P) < l2/h2:
            if R > 0: 
                y2 += h2 
                x2 += h2*P
            else: 
                y2 -= h2
                x2 -= h2*P
        else:
            if Q > 0:
                x2 += l2
                y2 += l2/P
            else:
                x2 -= l2
                y2 -= l2/P
    return 'M%s,%sL%s,%s' % (x1, y1, x2, y2)

# (1) Doc generation 

class latex:
    "% This is generated, do not edit by hands!\n"
    def __init__(self):
        r"\begin{document}"
        self.src = os.path.basename(sys.argv[0])
        self.tex = latex.__doc__ + '\n'
        self.digest = re.sub('_', '\_', r'\texttt{%s}' % __digest__.decode('utf-8'))
        
    def head(self, lpkg, hcmd, title, author, email, beam=False):
        "_"
        for p in ('draftwatermark', 'listings', 'embedfile', 'graphicx', 'tikz', 'url') + lpkg:
            a = p.split('|')
            if len(a) > 1:
                self.tex += r'\usepackage[%s]{%s}' % (a[1], a[0]) + '\n'
            else:
                self.tex += r'\usepackage{%s}' % a[0] + '\n'
        base = {'pyt':r'\emph{Python}', 'pdf':r'\textsc{pdf}'}
        base.update(hcmd)
        for c in base:
            self.tex += r'\newcommand{\%s}{%s}' % (c, base[c]) + '\n'
        if beam:
            self.tex += r'\title[%s]{%s}' % (self.digest, title) + '\n'
            self.tex += r'\author{%s\inst{*}}\institute{*%s}' % (author, email) + '\n'
            self.tex += r'\pgfdeclareimage[height=.6cm]{logo}{%s/rcf.png}\logo{\pgfuseimage{logo}}' % os.path.dirname(os.path.abspath(__file__)) + '\n'
        else:
            self.tex += r'\title{\bf %s}' % title + '\n'
            self.tex += r'\author{%s -- \url{%s} \\' % (author, email) + '\n'
            self.tex += r'\tiny{[%s]}\footnote{the first five characters of the base64 encoding of the \textsc{sha1} digest of the attached source file.}}}' % self.digest
            self.tex += r'\pagestyle{myheadings} \markright{\tiny{%s}\hfill}' % self.digest + '\n'
        self.tex += latex.__init__.__doc__ + '\n'
        self.tex += r'\lstset{language=Python, breaklines=true}'
        self.tex += r'\embedfile[filespec=%s]{%s}' % (self.src, os.path.abspath(self.src)) + '\n'
        if beam:
            self.tex += r"\frame{\titlepage}" + '\n'
        else:
            self.tex += r'\maketitle' + '\n'
        
    def gen_pdf(self, name):
        r"""\end{document}"""
        self.tex += latex.gen_pdf.__doc__ + '\n'
        open('%s.tex' % name, 'w').write(self.tex)
        here = os.path.dirname(os.path.abspath(__file__))
        subprocess.Popen(('cd /tmp; pdflatex -interaction=batchmode %s/%s.tex 1>/dev/null' % (here, name)), shell=True).communicate()
        shutil.move('/tmp/%s.pdf' % name, '%s/%s.pdf' % (here, name))

class article (latex):
    r"\documentclass[a4paper,10pt]{article}"
    def __init__(self, title, author, email):
        "_"
        latex.__init__(self)
        self.tex += article.__doc__ + '\n'
        self.head(('geometry|margin=2cm', 'inputenc|utf8', 'lmodern', 'color', 'longtable', 'array'), {}, title, author, email)
        self.tex += r'\begin{abstract}' + __doc__ + r'\end{abstract}' + '\n'
        
    def section(self, title, content):
        "_"
        self.tex += r'\section{%s}' % title + '\n'
        self.tex += content + '\n'

    def insert_code(self, pat):
        " ....in LaTeX "
        o, d = r'\lstset{basicstyle=\ttfamily, numbers=left, numberstyle=\tiny, stepnumber=5, numbersep=5pt}', False
        o += r'\begin{lstlisting}[texcl]' + '\n'
        for l in open(__file__).readlines():
            if re.match(r'(if|\s*def|class|__)', l): d = False
            if re.match(pat, l): d = True
            if d: o += l
        return o + r'\end{lstlisting}' + '\n'

class beamer (latex):
    r"\documentclass{beamer}"
    def __init__(self, title, author, email):
        "_"
        latex.__init__(self)
        self.tex += beamer.__doc__ + '\n'
        self.head(('beamerthemeshadow', ), {}, title, author, email, True)
        self.tex += r'\note{%s}' % __doc__ + '\n'

    def frame(self, title, content):
        "frame"
        self.tex += r'\frame{\frametitle{%s}' % title + '\n'
        self.tex += content + '\n'
        self.tex += '}\n'
    
def gen_doc():
    "article and beamer"
    art, sli = article(__title__, __author__, __email__), beamer(__title__, __author__, __email__)
    #
    art.section('chapitre', 'blabla')
    art.tex += art.insert_code('__RE_U__') 
    art.section('Parser', 'blabla')
    art.tex += art.insert_code(r'\s*def\s+parse\(') 
    #
    sli.frame(r'What $\sqcup$ is?', r""" 
The $\sqcup$ language is a {\bf Universal Graph Language};\\
\begin{itemize}
\item Symbol: $\bigsqcup$ \\
\end{itemize} 
""")
    name = os.path.basename(__file__)
    art.gen_pdf(name)
    sli.gen_pdf('beamer_' + name)

# (2) Tests

def get_random_set():
    "_"
    c, aset = ('A', 'B', 'C', '{', '}', '->', ' '), []
    for i in range(999):
        x, l = '', 0
        for j in range(random.randint(10, 100)):
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
        #x = functools.reduce(lambda x, i:x+random.choice(c), range(100), '')
        aset.append(('Rand_case_{}'.format(i), x))
    return aset

def gen_test():
    "gen_test"
    c, uobj = ('A', 'B', 'C', '{', '}', '->', ' '), u()
    refname, cmpname, o = 'ref.txt', 'cmp.txt', '' 
    for x in __AST_SET__:
        o += '{:03d}: {} {}\n\n'.format(__AST_SET__.index(x), x, uobj.parse(x[1]))
    open(cmpname, 'w').write(o)
    if os.path.isfile(refname):
        r = subprocess.Popen(('diff', refname, cmpname), stdout=subprocess.PIPE).communicate()[0].strip().decode('utf-8')
        if not re.match('^\s*$', r): 
            print ('Test fails:\nReference<\nComputed>\n', r)
    else:
        shutil.move(cmpname, refname)

# (3) Git storage

def register(content=''):
    """ If the same content is requested, then id does not change """
    base = __git_base__
    if not os.path.isdir(base):
        os.mkdir(base)
    rev = dbhash.open('%s/rev.db' % base, 'c')
    if rev.has_key(content):
        gid = rev[content]
    else:
        gid = create_id(rev)
        rev[content] = gid
    rev.close()    
    return gid

def print_db(db):
    "_"
    d = dbhash.open(db, 'r')
    for item in d.keys():
        print ('[%s] %s -> %s' % (db, item, d[item]))
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
        e['GIT_AUTHOR_NAME'], e['GIT_AUTHOR_EMAIL'] = user, ip
        e['GIT_COMMITTER_NAME'], e['GIT_COMMITTER_EMAIL'] = __author__, __email__
        e['GIT_DIR'] = '%s/.git' % __git_base__
        self.e = e
        if not os.path.isdir(e['GIT_DIR']):
            subprocess.Popen(('git', 'init', '-q'), env=e, close_fds=True).communicate()
            p = subprocess.Popen(('git', 'hash-object', '-w', '--stdin'), env=e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
            li = '100644 blob %s\tstart\n' % p.communicate(' \n')[0].strip()
            q = subprocess.Popen(('git', 'mktree'), env=e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
            r = subprocess.Popen(('git', 'commit-tree', q.communicate(li)[0].strip()), env=e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
            subprocess.Popen(('git', 'update-ref', 'refs/heads/master', r.communicate('start')[0].strip()), env=e, stdout=subprocess.PIPE).communicate()

    def save(self, key, c, state=''):
        "_"
        p = subprocess.Popen(('git', 'show', 'master^{tree}:'+key), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate('')
        p = subprocess.Popen(('git', 'ls-tree', 'master^{tree}'), env=self.e, stdout=subprocess.PIPE)
        liste = p.communicate()[0].strip()
        if err:
            liste += '\n100644 blob %s\t%s' % (self.sha(c), key) 
            self.commit (liste, key)
        else:
            if out[:-1] != c:
                self.commit(re.sub(r'(100644 blob) [0-9a-f]{40}(\t%s)' % key, '\\1 %s\\2' % self.sha(c), liste), key+'\n'+state)
        p = subprocess.Popen(('git', 'log', '--pretty=format:%H', '-1'), env=self.e, stdout=subprocess.PIPE)
        return p.communicate()[0][:15]

    def sha(self, content):
        "_"
        p = subprocess.Popen(('git', 'hash-object', '-w', '--stdin'), env=self.e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        return p.communicate(content+'\n')[0].strip()

    def commit(self, li, msg):
        "_"
        p = subprocess.Popen(('git', 'mktree'), env=self.e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        sha = p.communicate(li)[0].strip()
        p = subprocess.Popen(('git', 'show-ref', '--hash', 'refs/heads/master'), env=self.e, stdout=subprocess.PIPE)
        parent = p.communicate()[0].strip()
        p = subprocess.Popen(('git', 'commit-tree', sha, '-p', parent), env=self.e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        mm = p.communicate(msg)[0].strip()
        p = subprocess.Popen(('git', 'update-ref', 'refs/heads/master', mm), env=self.e, stdout=subprocess.PIPE)

    def list(self):
        "_"
        p = subprocess.Popen(('git', 'ls-tree', 'master^{tree}'), env=self.e, stdout=subprocess.PIPE)
        liste = p.communicate()[0].strip()
        return liste.split('\n')

    def history(self, key=''):
        "_"
        if key:
            p = subprocess.Popen(('git', 'log', '--pretty=format:%H:%an:%ar:%s', '--', key), env=self.e, stdout=subprocess.PIPE)
        else:
            p = subprocess.Popen(('git', 'log', '--pretty=format:%H:%an:%ar:%s'), env=self.e, stdout=subprocess.PIPE)
        liste = p.communicate()[0].strip()
        return liste.split('\n')

    def gethead(self, key):
        "_"
        p = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%H:%an:%ar:%at', '--', key), env=self.e, stdout=subprocess.PIPE) # ar
        return p.communicate()[0].strip()
    
    def revision(self, key):
        "_"
        c = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%H', '--', key), env=self.e, stdout=subprocess.PIPE)
        return c.communicate()[0][:15]

    def date(self, key):
        "_"
        c = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%ci', '--', key), env=self.e, stdout=subprocess.PIPE)
        return c.communicate()[0][:-5]

    def cat(self, key):
        "_"
        p = subprocess.Popen(('git', 'show', 'master^{tree}:'+key), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate()
        return '"Not Found!"' if err else out[:-1]

    def cat_blob(self, key):
        "_"
        p = subprocess.Popen(('git', 'show', key), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate()
        return '' if err else out[:-1]

    def cat_revision(self, gid):
        "_"
        p = subprocess.Popen(('git', 'show', 'master^{tree}:%s' % gid), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        content, err = p.communicate()
        c = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%H', '--', gid), env=self.e, stdout=subprocess.PIPE)
        rev = c.communicate()[0]
        return ('', '[Diagram Not Found cat_revision!]') if err else (rev[:15], content[:-1])

    def cat_getrev(self, rev):
        "_"
        c = subprocess.Popen(('git', 'log', '--pretty=oneline', '-1', rev), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = c.communicate()
        idd, cont = ['', ''], '[Diagram Not Found!]'
        if not err:
            if out != '':
                idd = out.strip().split()
                p = subprocess.Popen(('git', 'show', '%s:%s' % (rev, idd[1])), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                cont = p.communicate()[0][:-1]
        return idd[0][:15], idd[1], cont

    def cat_full(self, key, arev):
        "_"
        c = subprocess.Popen(('git', 'log', '--pretty=format:%H', '-1', arev), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = c.communicate()
        rev, cont = '', '[Diagram Not Found!]'
        if not err:
            if out != '':
                rev = out.strip()
                p = subprocess.Popen(('git', 'show', '%s:%s' % (rev, key)), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                cont = p.communicate()[0][:-1]
        return rev[:15], cont

    def cat_simple(self, key, rev):
        "_"
        p = subprocess.Popen(('git', 'show', '%s:%s' % (rev, key)), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        return p.communicate()[0][:-1]

    def test(self, key, rev):
        "_"
        c = subprocess.Popen(('git', 'log', '%s:@%s' % (rev, key)), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        o, e = c.communicate()
        return False if e else True 
    
    def tag(self, name, rev):
        "_"
        c = subprocess.Popen(('git', 'tag', name, rev), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        o, e = c.communicate()
        return e if e else o

    def tag_list(self):
        "_"
        o, e = subprocess.Popen(('git', 'tag'), env=self.e, stdout=subprocess.PIPE).communicate()
        return o

# (4) Web application 

def reg(value):
    " function attribute is a way to access matching group in one line test "
    reg.v = value
    return value

def favicon():
    "_"
    code = '<svg %s n="%s"><path stroke-width="4" fill="none" stroke="Dodgerblue" d="M3,1L3,14L13,14L13,1"/></svg>' % (_SVGNS, datetime.datetime.now())
    tmp = base64.b64encode(code.encode('utf-8'))
    return '<link %s rel="shortcut icon" type="image/svg+xml" href="data:image/svg+xml;base64,%s"/>\n' % (_XHTMLNS, tmp.decode('utf-8'))

def logo(opac=1):
    "_"
    return '<path id="logo" stroke-width="8" fill="none" stroke="Dodgerblue" onclick="window.open(\'http://%s\');" title="on Github: [http://%s]" opacity="%s" d="M10,10L10,35L30,35L30,10"/>' % (__url__, __url__, opac)

def style():
    """h1,h3,h6,p,li,b,a,td,th{font-family:helvetica neue,helvetica,arial,sans-serif;} a{text-decoration:none;} 
table {border: 1px solid #666;width:100%;border-collapse:collapse;} td,th {border: 1px solid #666;} 
h1{position:absolute;top:-8;left:60;} h6{position:absolute;top:0;right:10;}"""
    return '<style>{}</style>\n'.format(style.__doc__)

def table_test(par, title, tset):
    "_"
    o, uobj = '<h1>%s Test set</h1>\n<table>' % title, u()
    if par:
        o += '<tr><th>#</th><th>Description</th><th>⊔ input</th><th>Nodes</th><th>Arcs</th></tr>\n'
        for x in tset: 
            res, d0, d1 = uobj.parse(x[1]), '', ''
            for i in res[0]:
                tmp = '{}'.format(res[0][i])
                d0 += '{}: {}<br/>'.format(html.escape(i), html.escape(tmp))
            for e in res[1]:
                tmp = '{}'.format(e)
                d1 += '{}<br/>'.format(html.escape(tmp))
            o += '<tr><td><small>{:03d}</small></td><td>{}</td><td>{}</td><td>{}</td><td>{}</td></tr>\n'.format(tset.index(x) + 1, x[0], html.escape(x[1]), d0, d1)
    else:
        o += '<tr><th>#</th><th>Description</th><th>⊔ input</th><th>unparsed string</th></tr>\n'
        for x in tset: 
            res, d0 = uobj.unparse(uobj.parse(x[1])), ''
            d0 = re.sub('\n', '<br/>', html.escape(res))
            o += '<tr><td><small>{:03d}</small></td><td>{}</td><td>{}</td><td>{}</td></tr>\n'.format(tset.index(x) + 1, x[0], html.escape(x[1]), d0)
    return o + '</table>'

def table_about(host):
    "_"
    o = '<h1>Help</h1>\n<table>'
    o += '<tr><th>#</th><th>Action</th><th>Example</th></tr>\n'
    n = 0
    for x in __ACTIONS__:
        n += 1
        o += '<tr><td><small>{:03d}</small></td><td>Keyword: \'{}\'</td><td><a href="u3?{}">http://{}/u3?{}</a></td></tr>\n'.format(n, x, x, host, x)
    for x in __OUT_LANG__:
        n += 1
        o += '<tr><td><small>{:03d}</small></td><td>Output Language: {}</td><td><a href="u3?{}&A-&gt;B">http://{}/u3?{}&A-&gt;B</a></td></tr>\n'.format(n, x, x, host, x)
    for x in __IN_MODEL__:
        n, d = n + 1, html.escape(__IN_MODEL__[x])
        o += '<tr><td><small>{:03d}</small></td><td>Input Model Type: {}</td><td><a target="_blank" href="u3?_svg&{}">http://{}/u3?_svg&{}</a></td></tr>\n'.format(n, x, d, host, d)
    return o + '</table>'

def hhead():
    "_"
    return '<html>' + favicon() + style() + '\n<svg %s height="64">%s</svg>\n' % (_SVGNS, logo())

def htail():
    "_"
    return '<h6 title="Base64 encoded short sha1 digest">%s</h6></html>' % __digest__.decode('utf-8')

def application(environ, start_response):
    """ WSGI Web application """
    s, mime, o, myu, host = urllib.parse.unquote(environ['QUERY_STRING']), 'text/plain; charset=utf-8', 'Error!', u(), environ['SERVER_NAME']
    lang, mod, gid, arg, act, fname = None, None, None, None, None, 'u.py'
    if reg(re.match(r'\s*(%s$|)(_?)(%s|)&?((\w{10})|(.*))\s*$' % ('$|'.join(__ACTIONS__), '|'.join(__OUT_LANG__)), s, re.I)):
        act, mod, lang, gid, arg = reg.v.group(1), reg.v.group(2), reg.v.group(3), reg.v.group(5), reg.v.group(6) 
    if act:
        if act.lower() in ('pdf', 'paper', 'beamer'):
            mime, name = 'application/pdf', 'beamer_u3' if act == 'beamer' else 'u3'
            fname = '{}.pdf'.format(name)
            f = '%s/%s.pdf' % (os.path.dirname(environ['SCRIPT_FILENAME']), name)
            o = open(f, 'rb').read()
        else:
            mime, fname, o = 'text/html; charset=utf-8', act, hhead()
            if act.lower() in ('about', 'help', 'usage'): 
                o += table_about(host)
            elif act.lower() in ('update',): 
                o += 'update'
            elif act.lower() in ('edit', 'ace', 'git'): 
                o += 'edit' 
            elif act.lower() in ('log',): 
                o += 'log'
            elif act.lower() in ('test', 'parse'): 
                o += table_test(True, 'Parsing', __AST_SET__)
            elif act.lower() in ('unparse',): 
                o += table_test(False, 'Unparsing', __AST_SET__)
            elif act.lower() in ('random',): 
                o += table_test(False, 'Random', get_random_set())
            elif act.lower() in ('download', 'source'): 
                mime = 'text/plain; charset=utf-8'
                o = open(__file__, 'r', encoding='utf-8').read()
            o += htail()
            o = o.encode('utf-8')
    else:
        if gid:
            arg = gitu().cat(gid)
        elif environ['REQUEST_METHOD'].lower() == 'put':
            arg = environ['wsgi.input'].read().decode('utf-8')
        if lang:
            if lang in ('xml','svg') and mod: mime = 'application/xhtml+xml; charset=utf-8'
            if arg:
                if lang == 'svg' and environ['REQUEST_METHOD'].lower() == 'post': 
                    raw = environ['wsgi.input'].read().decode('utf-8')
                    o = myu.gen_svg(myu.parse(arg), eval(urllib.parse.unquote(raw[2:])))
                    #o += '<!-- %s -->'%urllib.parse.unquote(raw[2:])
                else:
                    o = eval('myu.headfoot(myu.gen_{}, lang, host)(myu.parse(arg))'.format(lang))
            else:
                if environ['REQUEST_METHOD'].lower() == 'post':
                    arg = '\n'.join(environ['wsgi.input'].read().decode('utf-8').split('\n')[4:-2])
                    o = eval('myu.headfoot(myu.gen_{}, lang, host)(myu.parse(arg))'.format(lang))
                else:
                    mime, fname = 'text/html; charset=utf-8', lang
                    o = hhead() + '<form method=post enctype=multipart/form-data><input type=file name=a onchange="submit();"/>' + htail()
        else:
            if arg:
                o = myu.headfoot(myu.gen_ast, 'python', host)(myu.parse(arg))
            else:
                o = open(__file__, 'r', encoding='utf-8').read() 
        o = o.encode('utf-8')
    start_response('200 OK', [('Content-type', mime), ('Content-Disposition', 'filename={}'.format(fname))])
    return [o] 

# (5) Command line

def put(server, service, content):
    "_"
    h = http.client.HTTPConnection(server)
    h.request('PUT', service, content)
    return h.getresponse().read().decode('utf-8')

def command_line():
    " Command line"
    import getopt, http.client
    opts, args = getopt.getopt(sys.argv[1:], 'h:f:s', ['host=', 'format=', 'stdin'])
    lang, host = '', '127.0.0.1' # use '193.84.73.209 for testing'

    if not opts and not args:
        gen_test()
        gen_doc()
        print(__digest__.decode('utf-8'))
    
    for r in opts:
        if r[0] in ('-h', '--host'):
            host = r[1]
        elif r[0] in ('-f', '--format'):
            lang = r[1]
        elif r[0] in ('-s', '--stdin'):
            args.append(sys.stdin.read())
        else:
            print (help('u'))
 
    for arg in args:
        print ('ici'+arg)
        data = str(open(arg).read()) if os.path.isfile(arg) else arg
        print (data)
        if host in ('localhost', '127.0.0.1'):
            myu = u()
            if lang:
                o = eval('myu.headfoot(myu.gen_{}, lang, host)(myu.parse(arg))'.format(lang))
            else:
                if arg:
                    o = myu.headfoot(myu.gen_ast, 'python', host)(myu.parse(data))
                else:
                    o = open(__file__, 'r', encoding='utf-8').read()
        else:
            o = put(host, '/u3?%s' % o, data)
        print (o)

def iter_child_nodes(node):                                                    
    "Yield all direct child nodes of *node*, that is, all fields that are nodes and all items of fields that are lists of nodes."                           
    for name, field in iter_fields(node):                                      
        if isinstance(field, AST):                                             
            yield field                                                        
        elif isinstance(field, list):                                          
            for item in field:                                                 
                if isinstance(item, AST):                                      
                    yield item                                                 

if __name__ == '__main__':
    " Command line"
    command_line()
    prg = "def foo():return 'hello'"
    p = ast.parse(open(__file__).read())
    #print(p.body[0].body[0].value.s[0])
    #print (ast.dump(p))
    #a = 2; b= 3; print (a)

    # define user's class
    class useu(u):  
        def gen_ada(self, ast):
            return 'My ADA code generator'
    myu = useu()
    #print (myu.gen_ada(myu.parse('A')))
    #print (myu.gen_c(myu.parse('A')))


# end    
    
