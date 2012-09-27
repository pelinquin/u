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

# REMINDER TODO LIST:
# - svg with proportional police size
# - types data to move in a Berkeley database
# - add block operator
# - display refresh for Webkit
# - fix bug on Firefow PDF plugin (request sent twice)
# - cycle detection for feedback models
# - Add Sphinx doc 
# - Uddate paper and beamer

r"""
The $\sqcup$ language is a universal typed sparse graph language. 
It serves {\em Model Driven Engineering}.
"""

__author__   = 'Laurent Fournier'
__email__    = 'lfournie@rockwellcollins.com'
__title__    = 'The Universal Short Graph Language'
__subtitle__ = 'Introducing the $\sqcup$ language'
__version__  = '0.3'
__license__  = 'GPLv3'
__url__      = 'github.com/pelinquin/u'
__git_base__ = '/u'

import sys, os, re, hashlib, subprocess, urllib.parse, base64, random, functools, datetime, shutil, html, ast, http.client, dbm, tempfile, time

__digest__ = base64.urlsafe_b64encode(hashlib.sha1(open(__file__, 'r', encoding='utf-8').read().encode('utf-8')).digest())[:5]

def introduction():
    r"""Blabla intro with diagram: %(lp_diag)s blabla"""
    return introduction.__doc__ % {'lp_diag': tikz('M"Literate program"S M-"weave">doc"Documentation"S M-"tangle">Code"Source Code"S', 1.7)}

_XHTMLNS  = 'xmlns="http://www.w3.org/1999/xhtml"'
_SVGNS    = 'xmlns="http://www.w3.org/2000/svg"'
_XLINKNS  = 'xmlns:xlink="http://www.w3.org/1999/xlink"'

__separator__ = r'[\|\'`";,!~^@*+/$]' # 14 chars
__delimiter__ = r'%s(?:%s%s)?' % (__separator__, __separator__, __separator__)

__RE_U__ = r'''     # RegExp 
   (?:              # Three basic tokens:
    ([{}])          # (1) Block 
   |
    (\#[^\n]*)      # or (2) Line comment
   |                # or (3) NODE:
    (?=[^\s<\-=>])  # Not empty token 
    (?:(\w{1,10})|) # Name      
    (?::(\w)|)      # Type pre  
    ((%s)(.+?)\6|\[([^\]]+)\]|\(([^)]+)\)|) # Content
    (\w|)           # Type post 
    (?:\.(\w{1,20}|\*)|) # Port      
   |                # or (4) ARC:  
    ([<\-=>])       # Head      
    (?:([^\W\d_])|) # Type pre  
    ((%s)(.+?)\15|\[([^\]]+)\]|\(([^)]+)\)|) # Content
    (?:([^\W\d_])|) # Type post  
    ([<\-=>])       # Tail
)''' % (__delimiter__, __delimiter__)

__ARC_T__  = ('--', '->', '-<', '-=', '=-', '=>', '=<', '==', '<-', '<>', '<<', '<=', '>-', '>>', '><', '>=')
__NODE_T__ = ('|',  '\'', '`',  '"',  ';',  ',',  '!',  '~',  '^',  '@',  '*',  '+',  '/',  '$',  '(',  '[' )
 
__ACTIONS__ = ('download', 'source', 'update', 'about', 'help', 'usage', 'pdf', 'list', 'history', 'paper', 'beamer', 'log', 'test', 'parse', 'unparse', 'random', 'signup', 'change', 'database', 'bug', 'pi')

__OUT_LANG__ = { # language Name, Abreviation, comment begin, comment end, header, version, is interpreted online with "_" prefix
    'c'          :['c',    ('/*', '*/', ''), 'gcc ansi pedantic', False],
    'objectivec' :['m',    ('/*', '*/', ''), '', False],
    'python'     :['py',   ('#', '', '#!/usr/bin/python3\n# -*- coding: utf-8 -*-\n'), ' python 3.2 ', True],
    'ada'        :['adb',  ('--', '', ''), 'Ada95 Ravenscar', False],
    'scala'      :['scl',  ('--', '', ''), '', False],
    'java'       :['java', ('//', '', ''), '', False],
    'ruby'       :['rb',   ('#', '', ''), '', False],
    'ocaml'      :['ml',   ('(*', '*)', ''), '', False],
    'haskell'    :['hs',   ('{-', '-}', ''), '', False],
    'lua'        :['lua',  ('--', '', ''), '', False],
    'tikz'       :['tex',  ('%', '', ''), 'for LaTeX', True],
    'svg'        :['svg',  ('<!--', '-->', '<?xml version="1.0" encoding="UTF-8"?>\n'), 'Mozilla  Webkit', True],
    'aadl'       :['adl',  ('--', '', ''), 'AADL v2', False],
    'sdl'        :['sdl',  ('--', '', ''), '', False],
    'lustre'     :['lst',  ('--', '', ''), '', False],
    'vhdl'       :['hdl',  ('--', '', ''), '', False],
    'systemc'    :['sc',   ('//', '', ''), '', False],
    'xml'        :['xml',  ('<!--', '-->', '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'), '⊔ serialization', True],
    'simu'       :['simu', ('<!--', '-->', '<?xml version="1.0" encoding="UTF-8"?>\n'), 'Mozilla  Webkit', True],
}
 
__DATA_ports__ = {
    None:('p1', 'p2', 'p3', 'p4'),
    'P': ('p1', 'p2', 'p3', 'p4'),
    'T': ('i', 'o'),
    'O': ('in1', 'in2', 'out1', 'out2'),
    'b': ('in1', 'in2', 'out1', 'out2'),
    'V': ('in1', 'in2', 'out1', 'out2'),
    'a': ('p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10', 'p11', 'p12', 'p13', 'p14', 'p15', 'p16'),
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
        'c': ('class','fill:blue;','class|0,4,4','fill-opacity:.1;stroke:gray;stroke-width:.5;', 2, 0),
        'D': ('node','fill:blue;','rect','filter:url(#.shadow);fill-opacity:.1;', 10, 50),
        'x': ('node','fill:blue;','rect','fill:blue;fill-opacity:.2;', 10, 50),
        'd': ('node','fill:blue;','rect','filter:url(#.shadow);fill-opacity:.1;', 10, 50),
        'z': ('place','fill:black;','circle','fill:green;fill-opacity:.3;', 10, 2),
        't': ('transition','fill:white;','rect','fill:black;fill-opacity:.8;', 2, 40),
        'p': ('node','fill:black;','rect|-10,0,0','fill:gray;fill-opacity:.1;stroke:black;stroke-width:.5;stroke-dasharray:9,5;', 20, 20),
        }, {
        None:'stroke:red;   stroke-width:1;   fill:none; marker-end:url(#.arrow);',
        '1'  : 'stroke:green; stroke-width:2;   fill:none; marker-end:url(#.arrow);',
        '1None': 'stroke:green; stroke-width:2;   fill:none; marker-end:url(#.arrow);',
        '0J': 'stroke:brown; stroke-width:3;   fill:none; marker-end:url(#.arrow);',
        'r' : 'stroke:black; stroke-width:1.5; fill:none; marker-start:url(#.r_arrow);',
        '4I': 'stroke:green; stroke-width:2;   fill:none; marker-end:url(#.arrow);',
        'L' : 'stroke:blue;  stroke-width:3;   fill:none; marker-end:url(#.arrow);',
        })

__DATA_tikz__ = ({
        None:('rectangle,rounded corners=2pt,draw=gray!40,fill=blue!20,align=left', ''),
        'w': ('rectangle,rounded corners=2pt,draw=gray!40,fill=blue!20', ''),
        'T': ('circle,drop shadow,draw=green!40,fill=gray!20', ''), 
        'O': ('rectangle,drop shadow,rounded corners=3pt,draw=red!40,fill=blue!25', ''),
        't': ('rectangle,rounded corners=5pt,drop shadow,draw=gray!40,fill=red!30,align=center', ''),
        'n': ('rectangle,rounded corners=3pt,drop shadow,draw=gray!40,fill=gray!20,align=left', ''),
        'm': ('rectangle,rounded corners=2pt,drop shadow,draw=gray!40,fill=brown!30,align=left', ''),
        'g': ('ellipse,drop shadow,draw=gray!40,fill=green!20', ''),
        'l': ('ellipse,drop shadow,draw=gray!40,fill=blue!20', ''),
        'a': ('rectangle,drop shadow,draw=gray!40,fill=blue!20,minimum width=2cm, minimum height=2cm', ''),
        'b': ('rectangle,drop shadow,draw=gray!40,fill=blue!20', ''),
        'c': ('diamond,drop shadow,draw=gray!40,fill=blue!20', ''),
        'd': ('regular polygon,regular polygon sides=5,drop shadow,draw=gray!40,fill=blue!20', ''),
        'e': ('regular polygon,regular polygon sides=6,drop shadow,draw=gray!40,fill=blue!20', ''),
        'f': ('regular polygon,regular polygon sides=8,drop shadow,draw=gray!40,fill=blue!20', ''),
        'S': ('rectangle,drop shadow,rounded corners=3pt,draw=gray!20,fill=blue!20', ''),
        'E': ('ellipse,drop shadow,draw=gray!20,fill=green!20', ''),
        'p': ('trapezium,dashed,trapezium left angle=70,trapezium right angle=-70, minimum width=.5cm, minimum height=1cm,draw=black,fill=gray!20', ''),
        }, {
        None:'--',
        'I':  '->,>=open diamond',
        'L':  '->,>=triangle 60',
        'r':  '--',
        's':  '->,>=latex',
        '1None': '->,>=latex',
        'S':  '->,>=latex',
        '1e':  '->,>=latex',
        '1l':  '->,>=latex,dashed',
        '1v': '->,very thick, line width=5pt, draw=red',
        '1d': '->>,dotted',
        '0x': '>->,>=triangle 90, thick, draw=gray, fill=gray',
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
        'p': ('plus', ),
        'g': ('generator', ),
        'd': ('display', ),
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
__DATA_simu__       = ({None: (), }, {None:(), })

__MACRO__ = {
    'u_nested' : 'process{A->B->C->A}',
    'u_process': 'A"u concrete\nsyntax\nstring"n B"u abstract\nsyntax\nPython\nstructure"n  A -e(u parser)> B B -e(Web)> "SVG"g B -e(doc.gen.)> "Tikz"g B -e(code gen.)> "Ada"l B -e> "Ocaml"l B -e(model gen.)> "AADL"l e"AADL\nUML\nSimulink\nKAOS"m -d(use)> A "u type\nchecker"t -l> B "optimizer"t -l> A'
}

__IN_MODEL__ = {
    'UML':                   """myclass:C"hello" 
""", 
    'SysML':                 'A->B', 
    'AADL-Graph':            'A->B', 
    'Marte':                 'A->B', 
    'PSL':                   'A->B', 
    'Xcos':                  'P->S.0 I->S.1 D->S.2 S.3->Y.0 Y.1->E.1 E.0->P E.0->I E.O->D #PID controler', 
    'Kaos':                  'A->B', 
    'Entity-Relation-Graph': 'A->B', 
    'Tree-Diagram':          'A->B',
    'Network-Graph':         'A->B', 
    'Flowchart':             'A->B--C D--C',
    'Petri-net':             'A->B', 
    'State-Machine':         'A->B', 
    'Markov-Chain':          ':M{A B} A-[.6]>A B-[.3]>B A-[.4]>B-[.7]>A}', 
    'Behavior-Tree':         'A->B',
} 

__AST_SET__ = [
    ('TypePropagation',        'A:T {B C:X}'),
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
    ('RecurseComposition',     'A{B} B{A}'),
    ('MultiParents',           'A{C} B{C}'),
    ('MultiChild',             'A{B} A{C}'),
    ('Optimize1',              'A A"hh" A:T'),
    ('Optimize2',              'A->B B->C'),
    ('Optimize3',              'A{B} A{C}'),
]

# (0) Parser

def gettypes(uast):
    "_"
    nl, el = {None:True}, {None:True}
    nodes, arcs = uast
    for n in nodes:
        nl[nodes[n][1]] = True 
    for a in arcs:
        el['{}{}'.format(a[2], a[3])] = True 
    return nl, el

class u:
    """ This is the base class for ⊔ 
    One can customize that class by adding/modifying __DADA_xxx__ structure and by overloading a gen_xxx() method
    """
  
    def __init__(self):
        "Load types mapping"
        self.m = {}
        def gen_x(self, uast):
            #(sc, ec, head) = __OUT_LANG__[l][1]
            #return '{} TODO! {}'.format(sc, ec)
            return 'TODO!'
        for l in __OUT_LANG__:
            self.m[l] = eval('__DATA_%s__' % l)
            if not (hasattr(u, 'gen_%s' % l) and callable(getattr(u, 'gen_%s' % l))):
                gen_x.__name__ = 'gen_%s' % l
                setattr(u, gen_x.__name__, gen_x)

    def addarc(self, c, i, cli):
        "utils"
        arcs = []
        for a in c:
            if type(a).__name__ == 'list':
                for x in range(-int(a[1])) if a[1] and int(a[1]) < 0 else [a[1]]:
                    for b in i:
                        if type(b).__name__ == 'list':
                            for y in range(-int(b[1])) if b[1] and int(b[1]) < 0 else [b[1]]:
                                arcs.append(((a[0], x), (b[0], y)) + cli)
        return arcs

    def typeLabel(self, g, arc=True):
        "_"
        if arc:
            lab = g[17] if g[17] else g[16] if g[16] else g[15]
            typ = g[18] if g[18] else g[12]
            sep = g[14] if g[14] else g[13][0] + g[13][-1] if g[13] else None 
            res = (__ARC_T__.index(g[11]+g[19]), typ, sep, lab)
        else:
            typ = g[9] if g[9] else g[3]
            lab = g[8] if g[8] else g[7] if g[7] else g[6]
            sep = g[5] if g[5] else g[4][0] + g[4][-1] if g[4] else None 
            nid = g[2] if g[2] else re.sub(r'\W', '', '_%s' % lab.lower())[:9] if lab else '__%s' % typ
            res = (nid, typ, sep, lab)
        return res

    def merge_attr(self, nid, nodes, prt, typ, sep, lab):
        "_"
        if prt and typ == None:
            typ = nodes[prt][1]
        if nid in nodes:
            tmp, i = list(nodes[nid]), 0
            for elt in (prt, typ, sep, lab):
                if elt != None:
                    tmp[i] = elt
                i += 1
            nodes[nid] = tuple(tmp)
        else:
            nodes[nid] = (prt, typ, sep, lab)    

    def gen_alltypes_old(self, lang):
        "_"
        x = ''
        for i in eval('__DATA_%s__' % lang)[0]:
            x += ' type_None' if i == None else ' type_%s:%s' % (i, i)
        return x

    def gen_alltypes(self, lang):
        "_"
        return functools.reduce(lambda y, i: y + ' ' + ('None"Type None"' if i == None else '%s"Type %s"%s' % (i, i, i)), eval('__DATA_%s__' % lang)[0], '') 

    def parse(self, x):
        r"""
The \u{} kernel parser code\\
Specification:\\
A {\em token} is a Name with possible attributes (type, label)\\
A {\em link} is an arrow string with possible attributes (type, arrow, label)\\
A {\em group} is a sequence of groups, links, token delimited by curly braces\\
An {\em element} is either a group or a token
A token has a {\em parent} iff there is a token (parent) just before the open brace of the group it belongs to\\
A link makes a relation between the two near elements. 
A link to or from a group is equivalent as a set of links to or from each member of the group.\\
Output 
A dictionnary with tokens as key and parent+attributes as value
A list of all links between two tokens+port and link attributes
"""
        for p in __MACRO__: x = re.sub(r'\b%s\b'%p, __MACRO__[p], x) # library
        nodes, arcs, stack = {}, [], [[[]]]
        if reg(re.match(r'types_(\w+)$', x)):
            if reg.v.group(1) in list(__OUT_LANG__):
                x = self.gen_alltypes(reg.v.group(1))
        index, grp, sak = 0, 0, [None]
        for m in re.compile(__RE_U__, re.X|re.S).finditer(x):
            if m.group(2): continue # comments
            if m.group(1) == '{': # open block
                index += 1
                if len(stack) <= index: stack.append([])
                grp = len(stack[index])
                stack[index].append([])
                sak.append(None)
            elif m.group(1) == '}': # close block
                if index:
                    sak.pop()
                    if sak: sak[-1] = None
                    index -= 1 
                    old_grp = grp
                    grp = len(stack[index])-1
                    stack[index][grp].append(old_grp)
            elif m.group(12): # link
                sak[-1] = None
                if stack[index][grp]: stack[index][grp].append(self.typeLabel(m.groups()))
            else: # node
                (nid, typ, sep, lab) = self.typeLabel(m.groups(), False) # Compact
                prt = sak[-2] if len(sak)>1 else None                    # Compact
                self.merge_attr(nid, nodes, prt, typ, sep, lab)          # Compact
                stack[index][grp].append([nid, self.getport(typ, m.group(11))])
                sak[-1] = nid
        for lvl in stack: 
            for grp in lvl: 
                while grp and type(grp[-1]).__name__ == 'tuple': del grp[-1] # remove orphan links
        link, crt  = None, None
        for lvl in stack:
            for grp in lvl:
                for elt in grp: 
                    if (type(elt).__name__) == 'tuple': link = elt
                    elif link != None:
                        arcs += self.addarc(stack[stack.index(lvl) + 1][crt] if type(crt).__name__ == 'int' else [crt], 
                                            stack[stack.index(lvl) + 1][elt] if type(elt).__name__ == 'int' else [elt], 
                                            link) 
                        link = None
                    if elt != link: crt = elt
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

    def unparse(self, uast):
        " returns an optimized u string from an AST"
        nodes, arcs = uast
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
                    tree.setdefault(prnt, []).append(n)
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
        def app(uast):
            (sc, ec, head) = __OUT_LANG__[lang][1]
            nodes, arcs = uast
            d = '{}'.format(datetime.datetime.now())
            o = '{}{} ⊔ Generated Code [{}] {}\n'.format(head, sc, d[:19], ec)
            o += '{} ******** Do not edit by hand! ******** {}\n'.format(sc, ec)
            o += '{} Base64 short sha1 digest: {: >12} {}\n'.format(sc, __digest__.decode('utf-8'), ec)
            o += '{} Host: {: >32} {}\n'.format(sc, host, ec) 
            o += '{} Forge:  https://{} {}\n'.format(sc, __url__, ec)
            o += '{} © Copyright 2012 Rockwell Collins, Inc {}\n'.format(sc, ec)
            o += '{} ** GNU General Public License  (v3) ** {}\n'.format(sc, ec) 
            o += '{} Output language: {} [{}] {}\n'.format(sc, lang, __OUT_LANG__[lang][2], ec)
            o += '\n%s AST = %s %s\n\n' % (sc, re.sub(r'\-\-', '__', '%s %s' % uast), ec)
            nt, at = gettypes(uast)
            for n in nt:
                if n in __DATA_ports__:
                    o += '%s Node type:"%s" Ports: %s %s\n' % (sc, n, __DATA_ports__[n], ec)
            o += '\n'
            #for e in at:
            #    o += '' # language dependent!
                #o += '%s Arc type:"%s" %s\n' % (sc, 'tmp', ec)
            o += '%s Topologic sort/cycles:%s %s\n' % (sc, self.toposort(arcs), ec)
            for n in nodes:
                o += '%s \'%s\': %s %s\n' % (sc, n, ast.dump(self.gast(n, nodes[n])), ec)
            return o + appli(uast) + '\n{} {} Nodes {} Arcs {: >30} {}'.format(sc, len(nodes), len(arcs), 'the end of file.', ec)
        return app

    def gast(self, n, nod):
        "get node Python ast"
        [st1, st2] = nod[2] if nod[2] and len(nod[2]) == 2 else [nod[2], nod[2]]
        lab = '%s%s%s' % (st1, nod[3], st2) if nod[3] else ''
        a = ast.parse(n)
        if lab:
            try:
                a = ast.parse(nod[3])
            except:
                try:
                    a = ast.parse(lab)
                except:
                    try:
                        a = ast.parse('{%s}' % nod[3])
                    except:
                        try:
                            l = re.sub('\n', r'\\\\n', nod[3])
                            a = ast.parse('"%s"' % l)
                        except:
                            pass
        return a

    def gen_ast(self, uast):
        " uast "
        nodes, arcs = uast
        o = '# ⊔ Python Abstract Syntax Structure:\n\n'
        o += 'Nodes = {\n'
        for n in nodes:
            o += ' \'{}\': {},\n'.format(n, nodes[n])
        o += '}\n\nArcs = [\n' 
        for e in arcs:
            o += ' {},\n'.format(e)
        return o + ']\n'

    def getchild(self, nodes):
        "_"
        child = {}
        for n in nodes:
            if nodes[n][0]:
                child.setdefault(nodes[n][0], []).append(n)
        return child
 
    def layout_simple(self, uast, rankdir='LR'):
        "_"
        nodes, arcs = uast
        bbx, bby, pos, d = None, None, {}, 'digraph G { rankdir=%s ' % rankdir 
        for n in nodes:
            d += ' %s[label="%s"];' % (n, nodeCodeGen(self.gast(n, nodes[n])).out)
        for e in arcs:
            label = re.sub(r' ', '_', '' if e[5] == None else '[label="%s"];'%e[5])
            d += ' %s->%s %s' % (e[0][0], e[1][0], label) 
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

    def layout(self, uast, rankdir='LR'):
        "Computes 2D automatic layout for graphics (tikz and svg) generation"
        #log('dot')
        nodes, arcs = uast
        bbx, bby, pos, d = None, None, {}, 'digraph G { rankdir=%s ' % rankdir 
        child = self.getchild(nodes)
        for n in nodes:
            if n not in child:
                d += ' %s[label="%s"];' % (n, nodeCodeGen(self.gast(n, nodes[n])).out)
        for e in arcs:
            if (e[0][0] not in child) and (e[1][0] not in child):
                label = re.sub(r' ', '_', '' if e[5] == None else '[label="%s"];'%e[5])
                d += ' %s->%s %s' % (e[0][0], e[1][0], label) 
        d += '}' 
        #print (d) # for debug
        p = subprocess.Popen(('dot'), stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        for l in p.communicate(input=d.encode('utf-8'))[0].decode('utf-8').split('\n'):
            #print (l) for debug
            if reg(re.search('bb="0,0,([\.\d]+),([\.\d]+)"', l)):
                bbx, bby = float(reg.v.group(1)), float(reg.v.group(2))
            elif reg(re.search('^\s*(\w+)\s*\[label=[^,]*, pos="([\.\d]+),([\.\d]+)"', l)) and bbx and bby:
                pos[reg.v.group(1)] = (int(float(reg.v.group(2))*100/bbx), int(float(reg.v.group(3))*100/bby))
        for item in child:
            x, y, n = 0, 0, 0
            for c in child[item]:
                if c in pos:
                    n += 1
                    x += pos[c][0]
                    y += pos[c][1]
            if n:
                pos[item] = (int(x/n), int(y/n))
        return pos

    def gen_tikz_header(self, ln, le):
        r'\tikzset{'
        o = self.gen_tikz_header.__doc__ + '\n'
        for n in ln:
            if n in __DATA_tikz__[0]:
                o += '  %s/.style={%s},\n' % (n, __DATA_tikz__[0][n][0])
        for e in le: 
            if e in __DATA_tikz__[1]:
                o += '  arc%s/.style={%s},\n' % (e, __DATA_tikz__[1][e])
        return o + '}\n'

    def gen_tikz(self, uast, standalone=True, rx=2, ry=2):
        "TikZ picture for LaTeX"
        nt, at = gettypes(uast)
        o = ''
        if standalone:
            o += r'\documentclass[landscape,a4paper,11pt]{article}' + '\n \\usepackage{tikz}\n\\begin{document}\\begin{center}\n'
            #o += r'\documentclass[class=minimal,border=10pt]{standalone}' + '\n \\usepackage{tikz}\n\\begin{document}\n'
            o += r'\usetikzlibrary{shapes,fit,arrows,shadows,backgrounds,svg.path}'+ '\n'
            #o += r'\tikzset{oz/.style={path picture={ %s } } }' % rclogo() + '\n'
            #o += r'\tikzset{oz/.style={path picture={ \draw[fill=red,draw=black] svg "M-10,-10L59,-10L59,59L-10,59Z";} } }' + '\n'
            #o += r'\tikzset{oz/.style={ path picture={ \draw[fill=red,draw=blue] svg "M0,0L10,10L20,0L10,-10Z";} } }' + '\n'
        o += self.gen_tikz_header(nt, at)  
        o += r'\begin{tikzpicture}[auto,node distance=15mm,semithick]'+ '\n'
        #o += r'\begin{tikzpicture}'+ '\n'
        nodes, arcs = uast 
        pos, ratio = self.layout(uast, 'LR'), .04
        prt = self.getchild(nodes)
        box = {}
        for n in prt:
            mx, my = 0, 0
            for c in prt[n]:
                if c in pos:
                    if abs(pos[c][0]-pos[n][0]) > mx: mx = abs(pos[c][0]-pos[n][0])
                    if abs(pos[c][1]-pos[n][1]) > my: my = abs(pos[c][1]-pos[n][1])
            box[n] = (2*mx+20,2*my+20)
        ports = {}
        for n in pos:
            x, y = pos[n][0]*ratio*rx, pos[n][1]*ratio*ry
            t = nodes[n][1]
            styl = t if t in __DATA_tikz__[0] else 'None'
            label = nodeCodeGen(self.gast(n, nodes[n]), 'tikz').out
            label = re.sub('\n', r'\\\\', re.sub(r'\\n', r'\\\\', label))
            label = re.sub('([A-Z][A-Z]+)', lambda p: r'{\sc %s}' % p.group(1).lower(), label)
            label = re.sub(r'\bu\b', r'$\sqcup$', label)
            size = ',minimum width=%s,minimum height=%s,fill=none' % box[n] if n in box else ''
            o += r'\node[%s%s](%s) at (%0.3f,%0.3f){%s};' % (styl, size, n, x, y, label) +'\n'
            #o += r'\draw(%s.north east) node[oz];' % n 
            tt = __DATA_ports__[t] if t in __DATA_ports__ else [] 
            if tt:
                ports[n], delta, p = tt, 360/len(tt), -180
                for i in tt:
                    o += r'\draw[gray] (%s.%s) node[rectangle,draw=none,fill=green!10,scale=.4]{\tiny{%s}};' % (n, p, i) + '\n'
                    p += int(delta)
        for a in arcs:
            boucle = '[loop right]' if a[0][0] == a[1][0] else ''  
            label = '' if a[5] == None else r'node[sloped,above,font=\scriptsize]{%s}' % a[5] 
            typ = '{}{}'.format(a[2], a[3]) 
            styl = typ if typ in __DATA_tikz__[1] else 'None'
            n0, n1 = a[0][0], a[1][0]
            if a[0][1] != None:
                #n0 += '.0'
                n0 += '.%d' % int(a[0][1]*360/len(ports[n0]))
            if a[1][1] != None:
                n1 += '.%d' % int(a[1][1]*360/len(ports[n1]))
            o += r'\draw[arc%s] -- (%s) to%s %s(%s);' % (styl, n0, boucle, label, n1) +'\n'   
        o += r'\end{tikzpicture}' + '\n'
        if standalone:
            o +=  r'\end{center} \end{document}'
        return o 

    def include_js(self):
        r"""function submitURL(data) { var f = document.createElement('form');f.setAttribute('method','post');h = document.createElement('input');h.setAttribute('type','hidden');h.setAttribute('name','a');h.setAttribute('value',JSON.stringify(data));f.appendChild(h);document.documentElement.appendChild(f);f.submit();} window.onload = function () { var box = {};var t = document.documentElement.childNodes;for (var n = 0; n < t.length; n++) {if (t[n].nodeName == 'text') { var b = t[n].getBBox();var tx = parseInt(t[n].getAttribute('x')); var ty = parseInt(t[n].getAttribute('y'));box[t[n].id] = [b.x, b.y, b.width, b.height, tx, ty]; }}if (Object.keys(box).length != 0) { submitURL(box); }}"""
        return script(self.include_js.__doc__, True)

    def include_js1(self):
        r"""function submitURL(data) { var f = document.createElement('form');f.setAttribute('method','post');h = document.createElement('input');h.setAttribute('type','hidden');h.setAttribute('name','a');h.setAttribute('value',JSON.stringify(data));f.appendChild(h);document.documentElement.appendChild(f);f.submit();} window.onload = function () {var box = {}; box['size']=[window.innerWidth, window.innerHeight]; if (Object.keys(box).length != 0) { submitURL(box); }}"""
        return script(self.include_js1.__doc__, True)

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
        return script(self.include_js_zoom_pan.__doc__, True)

    def user_interface(self):
        "_"
        o = '<text id="zoom"  x="99%" y="12" title="zoom factor">100%</text>\n'
        o += '<svg id="zoomin" title="zoom in" onclick="zoom(-10);" viewBox="0 0 20 20" y="18" width="99%" height="20" preserveAspectRatio="xMaxYMin meet"><rect height="100%" width="20" fill="lightgray" stroke-width="0"/><rect x="3" y="8" height="20%" width="14" fill="white"/><rect x="8" y="3" height="70%" width="4" fill="white"/></svg>'
        o += '<svg id="zoomout" title="zoom out" onclick="zoom(10);" viewBox="0 0 20 20" y="40" width="99%" height="20" preserveAspectRatio="xMaxYMin meet"><rect height="100%" width="20" fill="lightgray" stroke-width="0"/><rect x="3" y="8" height="20%" width="14" fill="white"/></svg>\n'
        o += '<g id="target" transform="translate(0,0)" fill="none" stroke-width="1" stroke="red"><path display="none" d="M-50,-40L-50,-50L-40,-50M40,-50L50,-50L50,-40M50,40L50,50L40,50M-40,50L-50,50L-50,40"/><path display="none" d="M-60,-50L-50,-50L-50,-60M50,-60L50,-50L60,-50M60,50L50,50L50,60M-50,60L-50,50L-60,50"/></g>\n'
        return o

    def node_text(self, n, nod, x, y):
        "_"
        o = ' <text class="node{}" id="{}" x="{}" y="{}">'.format(nod[1], n, x, y)
        clas, attr, meth = '', {}, {}
        for e in self.gast(n, nod).body:
            if type(e).__name__ == 'ClassDef':
                clas = e.name
                for i in e.body:
                    if type(i).__name__ == 'Expr':
                        if type(i.value).__name__ == 'Name': attr[i.value.id] = True
                    elif type(i).__name__ == 'Assign':
                        if type(i.targets[0]).__name__ == 'Name': attr[i.targets[0].id] = True 
                        if type(i.value).__name__ == 'Num': attr[i.targets[0].id] = i.value.n
                    elif type(i).__name__ == 'FunctionDef': meth[i.name] = True
        if clas:
            o += '<tspan font-size=".3em" opacity=".5" x="{}">Class:</tspan>'.format(x)
            o += '<tspan dx="10">{}</tspan>'.format(clas)
            if attr:
                o += '<tspan font-size=".3em" opacity=".5" dy="1.6em" x="{}">Attributes:</tspan>'.format(x)
            for i in attr:
                o += '<tspan x="{}" dy="1em" font-size=".6em">{}</tspan>'.format(x, i)
                if attr[i] is not True:
                    o += '<tspan dx="6" font-size=".6em"> : {}</tspan>'.format(attr[i])
            if meth:
                o += '<tspan font-size=".3em" opacity=".5" dy="1.6em" x="{}">Methods:</tspan>'.format(x)
            for m in meth:
                o += '<tspan x="{}" dy="1em" font-size=".6em">+{}()</tspan>'.format(x, m)
        else:
            txt = nod[3] if nod[3] != None else n
            o += '<tspan x="{}">{}</tspan>'.format(x, txt)
        return o + '</text>'

    def node_ports(self, n, b , tab):
        "_"
        o = ''
        for p in tab:
            d = 200*(.5 + tab.index(p))/len(tab) - 100
            x, y, anchor = (b[0]+1, b[1] + (d+100)*b[3]/100, 'start') if d<0 else (b[0] + b[2]-1, b[1] + (100-d)*b[3]/100, 'end')
            o += ' <text class="port" x="{}" y="{}" dominant-baseline="middle" text-anchor="{}">{}</text>'.format(x, y, anchor, p)
        return o 

    def setbox_svg(self, prt, box, nodes):
        "set box"
        for n in box:
            t = nodes[n][1]
            mx, my = __DATA_svg__[0][t][4] if t in __DATA_svg__[0] else 0, __DATA_svg__[0][t][5] if t in __DATA_svg__[0] else 0
            if n in prt:
                x, y, w, h = 10000, 10000, 0, 0
                for i in prt[n]:
                    if box[i][0] < x: x = box[i][0]
                    if box[i][1] < y: y = box[i][1]
                for i in prt[n]:
                    if box[i][0] + box[i][2] - x > w: w = box[i][0] + box[i][2] - x
                    if box[i][1] + box[i][3] - y > h: h = box[i][1] + box[i][3] - y
                box[n][:4] = [x-mx, y-my, w+2*mx, h+2*my]
            box[n][:4] = [sum(p) for p in zip(box[n][:4],(-mx, -my, 2*mx, 2*my))]

    def shape(self, t, a):
        "_"
        sty = __DATA_svg__[0][t][2] if t in __DATA_svg__[0] else ''
        args = sty.split('|')
        (skewx, rx, ry) = args[1].split(',') if len(args)>1 else (0, 0, 0)
        o = ' <g class="node%s" transform="translate(%s,%s) skewX(%s)">' % (t, a[0]+a[2]/2, a[1]+a[3]/2, skewx)
        if re.match('rect', sty):
            o += '<rect  x="%s" y="%s" width="%s" height="%s" rx="%s" ry="%s"/>' % (-a[2]/2, -a[3]/2, a[2], a[3], rx, ry)
        elif re.match('class', sty):
            o += '<rect x="%s" y="%s" width="%s" height="%s" rx="%s" ry="%s"/>' % (-a[2]/2, -a[3]/2, a[2], a[3], rx, ry)
        elif re.match('ellipse', sty):
            o += '<ellipse rx="%s" ry="%s"/>' % (a[2]/2, a[3]/2)
        elif re.match('circle', sty):
            o += '<circle r="%s"/>' % (a[2]/2)
        else:
            o += '<rect x="%s" y="%s" width="%s" height="%s"/>' % (-a[2]/2, -a[3]/2, a[2], a[3])
        return o + '</g>\n'

#function post(url, data, cb) {var req = new XMLHttpRequest();req.onreadystatechange = processRequest;function processRequest () {if (req.readyState == 4) {if (req.status == 200) {if (cb) { cb(req.responseText); }} else {alert('Error Post status:'+ req.status);}}} this.doPost = function() {req.open('POST', url, true);req.send(data);} };
#function init() {
#  var fD = new FormData(); fD.append('w',window.innerWidth); fD.append('h',window.innerHeight);
#  var s = new String(document.location);
#  var ai = new post(s, fD, function(res) {window.location.reload(true);});
#  ai.doPost();
#}

    def gen_simu(self, uast, box={}):
        "simulation"
        o = '<svg id="win" %s>\n' % _SVGNS 
        o += style('text{font-family:helvetica neue,helvetica,arial,sans-serif;font-size:8pt;}path,rect,circle{stroke-width:1;fill:none;stroke:black;}path.axe{marker-end:url(#.arrow);marker-mid:url(#.tic);stroke:;stroke-width:2;}path.curve:hover{stroke-width:2;stroke:red;cursor:crosshair;}')
        if box:
            o += script(r"""
function cotip(evt) {
var tip = document.getElementById('coord');var c = document.getElementById('circle');
var t = parseInt((evt.clientX-20)*parseInt(tip.getAttribute('sx'))/(window.innerWidth-130));
//var v = parseInt((evt.clientY-20)*parseInt(tip.getAttribute('m'))/(window.innerHeigth-20));
tip.setAttribute('x',evt.clientX+5); tip.setAttribute('y',evt.clientY);c.setAttribute('cx',evt.clientX); c.setAttribute('cy',evt.clientY);
tip.firstChild.nodeValue = 't:' + t + ' ' + evt.target.id +  ':' + evt.clientY;
}
""")
            o += self.svg_defs()
            x = self.gen_python(uast)
            r, e = subprocess.Popen(('python3'), stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE).communicate(input=x.encode('utf-8'))
            if r:
                w, h = box['size']
                sx, sy = w-130, h-40 
                h = eval(r.decode('utf-8'))
                n, Ma, mi = 0, [], []
                for c in h: 
                    n = len(h[c])
                    Ma.append(max(h[c]))
                    mi.append(min(h[c]))
                M, m, i = max(Ma), min(mi), 0
                if M == m: M += 1
                rx, ry = 'h30'*(sx//30+1), 'v-30'*(sy//30)
                X, Xy, Yy = 20, 20 + sy*M/(M-m), sy+20
                o += '<rect style="stroke:#DDD;stroke-width:2;" x="10" y="10" width="%s" height="%s"/>\n' % (sx+110, sy+20)
                o += '<path class="axe" d="M%s,%s%s"/><path class="axe" d="M%s,%s%s"/><text x="%s" y="%s">t</text><text x="%s" y="%s">0</text>\n'%(X, Xy, rx, X, Yy, ry, sx+50, Xy, 11, Xy+3) 
                o += '<text id="coord" sx="%s" m="%s" M="%s">_</text><circle id="circle" r="5" fill="none"/>\n' % (n, m, M)
                random.seed(10)
                for c in h:
                    color, pos, i = '%06x' % random.randrange(0xffffff), 60 + i*20, i+1
                    o += '<path style="stroke:#%s;" d="M%s,%sh30"/><text style="fill:#%s;stroke-width:1;" x="%s" y="%s">%s</text>\n' % (color, sx+20, pos, color, sx+60, pos, c)
                    path = 'M' + ' '.join(['%.2f,%.2f' % (X+sx*i//n, Xy-h[c][i]*sy/(M-m)) for i in range(n)])
                    #path = 'M' + ' '.join(['%.2f %.2f S %.2f %.2f' % (X+sx*i//n, Xy-h[c][i]*sy/(M-m), X+sx*i//n, Xy-h[c][i]*sy/(M-m)) for i in range(n)])
                    o += '<path class="curve" id="%s" onmousemove="cotip(evt);" style="stroke:#%s;" d="%s"/>\n' % (c, color, path)  
            else:
                o += '<text x="100" y="20">Error!</text>'
        else:
            o += self.include_js1() 
        o += '</svg>\n'
        return o

    def gen_svg(self, uast, box={}):
        "svg"
        nt, at = gettypes(uast)
        o = '<svg %s>\n' % _SVGNS + self.gen_svg_header(nt, at, True if box else False) 
        if box: 
            nodes, arcs = uast
            #o += '<!--g %s -->\n' % self.getchild(nodes)
            prt = self.getchild(nodes)
            self.setbox_svg(prt, box, nodes)
            cycle, seq = None, self.toposort(arcs)
            if type(seq).__name__ == 'dict': cycle, seq = seq, None
            o += '<title id=".title">%s</title>\n' % __title__ + favicon() + logo(.02) + '\n' + self.include_js_zoom_pan() + self.user_interface()
            o += '<g id=".graph">\n'
            for n in box:
                t = nodes[n][1] if nodes[n][1] in __DATA_svg__[0] else 'None'
                o += self.shape(t, box[n])
                o += self.node_ports(n, box[n], __DATA_ports__[nodes[n][1]] if nodes[n][1] in __DATA_ports__ else ())
                o += self.node_text(n, nodes[n], box[n][4], box[n][5])
            o += ' <g id=".arcs" >\n'
            ne = 0
            for e in arcs:
                ne += 1
                l0 = (e[0][1], len(__DATA_ports__[nodes[e[0][0]][1]])) if e[0][1] != None else None
                l1 = (e[1][1], len(__DATA_ports__[nodes[e[1][0]][1]])) if e[1][1] != None else None
                cla = ' class="arc{}{}"'.format(e[2], e[3])
                o += '  <path id="e_{}"{} d="{}"/>\n'.format(ne, cla, npath(box[e[0][0]], box[e[1][0]], l0, l1))
                if e[5] != None:
                    o += '<text><textPath {} xlink:href="#e_{}" startOffset="50%">{}</textPath></text>'.format(_XLINKNS, ne, e[5]) 
            o += ' </g> <!--arcs -->\n'
            o += '</g> <!-- graph -->\n' 
        else:
            nodes, arcs = uast # temporary use pos instead!
            o += self.include_js()
            pos, ratio = self.layout(uast, 'LR'), 4
            for n in pos:
                x, y = pos[n][0]*ratio, pos[n][1]*ratio
                o += self.node_text(n, nodes[n], x, y) 
        return o + '</svg>\n'

    def toposort(self, data):
        "returns topologic sort or cycle"
        def tsort(arcs):
            d = {}
            for a in arcs: d.setdefault(a[1][0], set()).update(set(a[0][0])),d.setdefault(a[0][0], set())
            while True:
                ordered = set(item for item, dep in d.items() if not dep)
                if not ordered: break
                yield ','.join(sorted(ordered))
                d = { item: (dep - ordered) for item,dep in d.items() if item not in ordered }
            if d: yield '[%s]'%d
        a = '|'.join(tsort(data))
        m = re.search (r'\[([^\]]+)\]', a)
        return eval(m.group(1)) if m else a.split('|')

    def findcycle(self, arcs):
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

    def svg_defs(self):
        "_"
        o = '<defs>'
        o += '<marker id=".arrow" viewBox="0 0 500 500" refX="80" refY="50" markerUnits="strokeWidth" orient="auto" markerWidth="40" markerHeight="30"><polyline points="0,0 100,50 0,100 20,50" fill="#555"/></marker>'
        o += '<marker id=".r_arrow" viewBox="0 0 500 500" refX="70" refY="50" markerUnits="strokeWidth" orient="auto" markerWidth="40" markerHeight="30"><polyline points="150,0 50,50 150,100 130,50" fill="#555"/></marker>'
        o += '<marker id=".tic" refX="0" refY="2" orient="auto" markerHeight="4"><path stroke="red" d="M0,0 0,10"/></marker>'
        o += '<radialGradient id=".grad" cx="0%" cy="0%" r="90%"><stop offset="0%" stop-color="#FFF"/><stop offset="100%" stop-color="#DDD" class="end"/></radialGradient>'
        o += '<filter id=".shadow" filterUnits="userSpaceOnUse"><feGaussianBlur in="SourceAlpha" result="blur" stdDeviation="2"/><feOffset dy="3" dx="2" in="blur" result="offsetBlur"/><feMerge><feMergeNode in="offsetBlur"/><feMergeNode in="SourceGraphic"/></feMerge></filter>'
        return o + '</defs>\n'
    
    def gen_svg_header(self, ln, le, full=False):
        "_"
        o = '<style type="text/css">\n'
        o += '@font-face{font-family:Graublau; src: url(\'./fonts/GraublauWeb.otf\') format("opentype");}\n'
        o += '@font-face{font-family:vag; src: url(\'./fonts/VAG-HandWritten.otf\') format("opentype");}\n'
        o += 'text,tspan{font-family:helvetica neue,helvetica,arial,sans-serif;}\n'
        o += 'text,tspan{stroke:none; stroke-width:0;}\n'
        o += 'text.tiny,tspan.tiny{font-family:helvetica neue,helvetica,arial,sans-serif;font-size: 4px;fill:DarkSlateGray;}\n'
        o += 'text.body{font-family:helvetica neue,helvetica,arial,sans-serif;font-size: .5em;fill:DarkSlateGray;}\n'
        o += '\ntext#zoom{font-size: .8em;fill:lightgray;text-anchor:end;}\n'
        if full:
            o += 'svg#zoomin,svg#zoomout{cursor:pointer;}\n'
            o += 'path{stroke:black;fill:none;}\n'
            o += 'textPath{font-size: .6em; dominant-baseline:text-after-edge;}\n'
            o += 'g#target path{stroke:red;}\n' 
            o += 'text.port{font-size: .3em;}\n'
            for n in ln:
                if n in __DATA_svg__[0]:
                    o += 'g.node%s{ %s }\n' % (n, __DATA_svg__[0][n][3]) 
                    o += 'text.node%s tspan{ %s }\n' % (n, __DATA_svg__[0][n][1]) 
            found = {None:True}
            for e in le:
                if e in __DATA_svg__[1]:
                    found[e] = True
                #else:
                #    a, b = e.split('_')
                #    if a in __DATA_svg__[1]:
                #        found[a] = True
                #    elif b in __DATA_svg__[1]:
                #        found[b] = True
            for x in found:
                o += 'path.arc%s { %s }\n' % (x, __DATA_svg__[1][x]) 
        o += '</style>\n'
        if full:
            o += self.svg_defs()
        return o + '\n'

    def gen_c(self, uast):
        "c"
        nodes, arcs = uast
        o = ''
        #for n in nodes: o += ' \'{}\': {},\n'.format(n, nodes[n])
        #for e in arcs: o += ' {},\n'.format(e)
        cycle, seq = None, self.toposort(arcs)
        if type(seq).__name__ == 'dict': cycle, seq = seq, [] 
        if seq:  
            o += 'int main(void) {\n'  
            for e in arcs: 
                o += ' {},\n'.format(e) 
            for j in seq:
                for i in j.split(','):
                    typ, ports = nodes[i][1], []
                    if typ in __DATA_ports__:
                        ports = __DATA_ports__[typ]
                    o += nodeCodeGen(self.gast(i, nodes[i]), 'c').out
            o += '  return(0)\n}\n'
        return o

    def linked(self, arcs):
        pr, nx = {}, {}
        for a in arcs:
            pr.setdefault(a[1][0], []).append(a)
            nx.setdefault(a[0][0], []).append(a)
        return pr, nx

    def gen_python(self, uast):
        "python" 
        o = '\nimport sys, os, re, functools, random\n\n'
        nodes, arcs = uast 
        pr, nx = self.linked(arcs)
        cycle, seq = None, self.toposort(arcs)
        if type(seq).__name__ == 'dict': cycle, seq = seq, []
        seen = {}
        for j in seq:
            for i in j.split(','):
                if i:
                    typ = nodes[i][1]
                    if typ not in seen:
                        seen[typ] = True
                        (op, init) = ('*', 1) if typ == 'M' else ('+', 0)
                        if typ not in ('m','p'):
                            o += 'def op_%s(*a): return functools.reduce(lambda y, i: y%si, a, %s)\n\n' % (typ, op, init)
        o += 'if __name__ == \'__main__\':\n'
        for j in seq:
            for i in j.split(','):
                if i:
                    typ, disp = nodes[i][1], False
                    op = '+' if typ =='p' else '*' if typ =='m' else ','
                    (prefix, suffix) = ('op_%s(' % typ,')') if op == ',' else ('', '')
                    value = nodes[i][3]
                    if nodes[i][3] and re.search(r'\*$', value):
                        value, disp = value[:-1], True
                    try:
                        value = ['%s' % int(value)]
                    except:
                        value = []
                    p = pr[i] if i in pr else []
                    li = ['-' + k[0][0] if k[2]==0 else k[0][0] for k in p]
                    if li == []: li = ['0']
                    o += '  %s = %s%s%s\n' % (i, prefix, (' %s '%op).join(li + value), suffix)
                    if disp:
                        o += '  print(%s)\n' % (i)
        o += '  h = {'
        for n in nodes:
            d = random.randrange(-100,100)
            o += '\'%s\':[ random.randrange(-200+%s,800+%s) for i in range(50)],' % (n, d, d)
        o += '}\n  print(h)\n'
        return o 

    def gen_xml(self, uast):
        """u,node,arc{display:block;} node{color:blue;} arc{color:green;}
u:before{display:block;text-align:center;font-size:20pt;content: '⊔ XML serialization';}
u:after{display:block;font-size:8pt;text-align:right;content: '[' attr(digest) ']';}
node{display:block;font-size:10pt;} 
node:before{display:block;font-size:12pt;content: 'Node: 'attr(id)' Parent: ('attr(parent)') Type: ['attr(type)'] Separator:'attr(separator)} 
arc{display:block;font-size:10pt;} 
arc:before{display:block;font-size:12pt;content: 'Arc: 'attr(src)' port:('attr(src_port)') -> 'attr(dst)' port:('attr(dst_port)') Type: [' attr(type)']'}"""
        nodes, arcs = uast
        o = '<?xml-stylesheet type="text/css" href="data:text/css,{}"?>\n\n'.format(self.gen_xml.__doc__)
        o += '<u digest="{}">\n <nodes nb="{}">\n'.format(__digest__.decode('utf-8'), len(nodes))
        for n in nodes: 
            par = ' parent="{}"'.format(nodes[n][0]) if nodes[n][0] else ''
            typ = ' type="{}{}"'.format(nodes[n][1], html.escape(nodes[n][2]) if nodes[n][2] else '') if nodes[n][1] or nodes[n][2] else ''
            o += '  <node id="{}"{}{}'.format(n, par, typ)
            o += '>\n   {}\n  </node'.format(nodes[n][3]) if nodes[n][3] else '/'
            o += '>\n'
        o += ' </nodes>\n' 
        o += '<arcs nb="%s">\n' % len(arcs)
        for e in arcs: 
            typ = ' type="{}{}{}"'.format(e[3] if e[3] else '', e[2], html.escape(e[4]) if e[4] else '') 
            psrc = ' src_port="{}"'.format(__DATA_ports__[e[3]][e[0][1]]) if e[0][1] != None else ''
            pdst = ' dst_port="{}"'.format(__DATA_ports__[e[3]][e[1][1]]) if e[1][1] != None else ''       
            o += '  <arc src="{}"{} dst="{}"{}{}'.format(e[0][0], psrc, e[1][0], pdst, typ)
            o += '>\n   {}\n  </arc'.format(html.escape(e[5])) if e[5] else '/' 
            o += '>\n'
        return o + ' </arcs>\n</u>\n' 

# (1) utilities

def npath(b1, b2, p1=None, p2=None):
    "computes svg path for linking two boxes arcs"
    x1, y1, x2, y2 = b1[0] + b1[2]/2, b1[1] + b1[3]/2, b2[0] + b2[2]/2, b2[1] + b2[3]/2
    h1, l1, h2, l2 = 1 + b1[3]/2, 1 + b1[2]/2, 1 + b2[3]/2, 1 + b2[2]/2
    xo1, yo1, xo2, yo2 = x1, y1, x2, y2
    if x1 == x2:
        (y1, y2) = (y1 + h1, y2 - h2) if y1 < y2 else (y1 - h1, y2 + h2)
    elif y1 == y2:
        (x1, x2) = (x1 + l1, x2 - l2) if x1 < x2 else (x1 - l1, x2 + l2)
    else:
        Q, R = x1-x2, y1-y2
        P = Q/R
        if abs(P) < l1/h1:
            (x1, y1) = (x1 + h1*P, y1 + h1) if R < 0 else (x1 - h1*P, y1 - h1) 
        else:
            (x1, y1) = (x1 + l1, y1 + l1/P) if Q < 0 else (x1 - l1, y1 - l1/P)
        if abs(P) < l2/h2:
            (x2, y2) = (x2 + h2*P, y2 + h2) if R > 0 else (x2 - h2*P, y2 - h2)
        else:
            (x2, y2) = (x2 + l2, y2 + l2/P) if Q > 0 else (x2 - l2, y2 - l2/P)
    if p1:
        d = 200*(.5 + p1[0])/p1[1] - 100
        (x1, y1) = (b1[0]+1, b1[1] + (d+100)*b1[3]/100) if d<0 else (b1[0] + b1[2]-1, b1[1] + (100-d)*b1[3]/100)
    if p2:
        d = 200*(.5 + p2[0])/p2[1] - 100
        (x2, y2) = (b2[0]+1, b2[1] + (d+100)*b2[3]/100) if d<0 else (b2[0] + b2[2]-1, b2[1] + (100-d)*b2[3]/100)
    r = abs(x1-x2)/50
    cx1, cy1 = x1 + r*(x1-xo1), y1 + r*(y1-yo1)
    cx2, cy2 = x2 + r*(x2-xo2), y2 + r*(y2-yo2)
    if (y1-yo1)*(y1-y2) > 0: cy1 = y1 
    if (y2-yo2)*(y2-y1) > 0: cy2 = y2 
    if b1[0] > b2[0] and b1[1] > b2[0] and b1[0]+b1[2] < b2[0]+b2[2] and b1[1]+b1[3] < b2[1]+b2[3]: cx2, cy2 = x1, y1
    if b2[0] > b1[0] and b2[1] > b1[0] and b2[0]+b2[2] < b1[0]+b1[2] and b2[1]+b2[3] < b1[1]+b1[3]: cx1, cy1 = x2, y2
    return 'M%s,%sC%s,%s %s,%s %s,%s' % (x1, y1, cx1, cy1, cx2, cy2, x2, y2)

# (2) Node Code generation

class nodeCodeGen(ast.NodeVisitor):
    "_"
    BOOLOP_SYM = { ast.And:'and', ast.Or:'or'}
    BINOP_SYM = { ast.Add:'+', ast.Sub:'-'}
    CMPOP_SYM = {ast.Eq:'==', ast.Gt:'>', ast.GtE:'>=', ast.Lt:'<', ast.LtE:'<=', ast.NotEq:'!=',}
    UNARYOP_SYM = { ast.Invert: '~', ast.Not: 'not', ast.UAdd: '+', ast.USub: '-'} 
    out, indent, nlines = '', 0, 0

    def __init__(self, node_ast, lang=''):
        "_"
        self.lang = lang
        ast.NodeVisitor.visit(self, node_ast)

    def w(self, x):
        "_"
        cr = '\n' if self.lang else r'\n'
        indent_with = ' '*4 if self.lang else ''
        if self.nlines:
            if self.out:
                self.out += cr * self.nlines
            self.out += indent_with * self.indent
            self.nlines = 0
        self.out += x
 
    def newline(self, node=None, extra=0):
        "_"
        self.nlines = max(self.nlines, 1 + extra)
 
    def body(self, statements):
        "_"
        self.new_line = True
        self.indent += 1
        for stmt in statements: self.visit(stmt)
        self.indent -= 1
 
    def signature(self, node):
        "_"
        want_comma = []
        def write_comma():
            if want_comma:
                self.w(', ')
            else:
                want_comma.append(True)
        padding = [None] * (len(node.args) - len(node.defaults))
        for arg, default in zip(node.args, padding + node.defaults):
            write_comma()
            self.visit(arg)
            if default is not None:
                self.w('=')
                self.visit(default)

    def visit_Assign(self, node):
        "_"
        self.newline(node)
        for idx, target in enumerate(node.targets):
            if idx:
                self.w(', ')
            self.visit(target)
        self.w(' = ')
        self.visit(node.value)
        if self.lang == 'c':
            self.w(';')
 
    def visit_AugAssign(self, node):
        "_"
        self.newline(node)
        self.visit(node.target)
        self.w(self.BINOP_SYM[type(node.op)] + '=')
        self.visit(node.value)
 
    def visit_Expr(self, node):
        "_"
        self.newline(node)
        self.generic_visit(node)
 
    def visit_FunctionDef(self, node):
        "_"
        #self.newline(extra=1)
        self.newline(node)
        if self.lang == 'python':
            self.w('def %s(' % node.name)
        elif self.lang == 'c':
            self.w('void %s(' % node.name)
        else:
            self.w('%s(' % node.name)
        self.signature(node.args)
        
        if self.lang == 'python':
            self.w('):')
        elif self.lang == 'c':
            self.w(');')
        else:
            self.w(')')
        self.body(node.body)
 
    def visit_ClassDef(self, node):
        "_"
        have_args = []
        def paren_or_comma():
            if have_args:
                self.w(', ')
            else:
                have_args.append(True)
                self.w('(')
        #self.newline(extra=2)
        self.newline(node)
        if self.lang == 'c':
            self.w('typdef struct %s' % node.name)
        else:
            self.w('class %s' % node.name)
        for base in node.bases:
            paren_or_comma()
            self.visit(base)
        if self.lang == 'c':
            self.w(have_args and '):' or ' {')
        else:
            self.w(have_args and '):' or ':')
        self.body(node.body)
        if self.lang == 'c':
            self.newline(node)
            self.w('};')
 
    def visit_Pass(self, node):
        "_"
        if self.lang == 'python':
            self.newline(node)
            self.w('pass')
 
    def visit_Attribute(self, node):
        "_"
        self.visit(node.value)
        self.w('.' + node.attr)
 
    def visit_Call(self, node):
        "_"
        want_comma = []
        def write_comma():
            if want_comma:
                self.w(', ')
            else:
                want_comma.append(True)
        self.visit(node.func)
        self.w('(')
        for arg in node.args:
            write_comma()
            self.visit(arg)
        for keyword in node.keywords:
            write_comma()
            self.w(keyword.arg + '=')
            self.visit(keyword.value)
        if node.starargs is not None:
            write_comma()
            self.w('*')
            self.visit(node.starargs)
        if node.kwargs is not None:
            write_comma()
            self.w('**')
            self.visit(node.kwargs)
        self.w(')')
 
    def visit_Name(self, node):
        "_"
        self.w(node.id)
 
    def visit_Str(self, node):
        "_"
        if self.lang == 'c':
            self.w(repr(node.s))
        else:
            self.w(node.s)
 
    def visit_Bytes(self, node):
        "_"
        self.w(repr(node.s))
 
    def visit_Num(self, node):
        "_"
        self.w(repr(node.n))
 
    def visit_Tuple(self, node):
        "_"
        self.w('(')
        idx = -1
        for idx, item in enumerate(node.elts):
            if idx:
                self.w(', ')
            self.visit(item)
        self.w(idx and ')' or ',)')
 
    def sequence_visit(left, right):
        def visit(self, node):
            self.w(left)
            for idx, item in enumerate(node.elts):
                if idx:
                    self.w(', ')
                self.visit(item)
            self.w(right)
        return visit
 
    visit_List = sequence_visit('[', ']')
    visit_Set = sequence_visit('{', '}')
    del sequence_visit
 
    def visit_Dict(self, node):
        "_"
        self.w('{')
        for idx, (key, value) in enumerate(zip(node.keys, node.values)):
            if idx:
                self.w(', ')
            self.visit(key)
            self.w(': ')
            self.visit(value)
        self.w('}')
 
    def visit_BinOp(self, node):
        "_"
        self.visit(node.left)
        self.w(' %s ' % self.BINOP_SYM[type(node.op)])
        self.visit(node.right)
 
    def visit_BoolOp(self, node):
        "_"
        self.w('(')
        for idx, value in enumerate(node.values):
            if idx:
                self.w(' %s ' % self.BOOLOP_SYM[type(node.op)])
            self.visit(value)
        self.w(')')
 
    def visit_Compare(self, node):
        "_"
        self.w('(')
        self.w(node.left)
        for op, right in zip(node.ops, node.comparators):
            self.w(' %s %%' % self.CMPOP_SYM[type(op)])
            self.visit(right)
        self.w(')')
 
    def visit_UnaryOp(self, node):
        "_"
        self.w('(')
        op = self.UNARYOP_SYM[type(node.op)]
        self.w(op)
        if op == 'not':
            self.w(' ')
        self.visit(node.operand)
        self.w(')')
 
    def visit_Subscript(self, node):
        "_"
        self.visit(node.value)
        self.w('[')
        self.visit(node.slice)
        self.w(']')
 
    def visit_ExtSlice(self, node):
        "_"
        for idx, item in node.dims:
            if idx:
                self.w(', ')
            self.visit(item)
 
    def generator_visit(left, right):
        "_"
        def visit(self, node):
            self.w(left)
            self.visit(node.elt)
            for comprehension in node.generators:
                self.visit(comprehension)
            self.w(right)
        return visit
 
    visit_ListComp = generator_visit('[', ']')
    visit_GeneratorExp = generator_visit('(', ')')
    visit_SetComp = generator_visit('{', '}')
    del generator_visit
 
    def visit_DictComp(self, node):
        "_"
        self.w('{')
        self.visit(node.key)
        self.w(': ')
        self.visit(node.value)
        for comprehension in node.generators:
            self.visit(comprehension)
        self.w('}')
 
    def visit_IfExp(self, node):
        "_"
        self.visit(node.body)
        self.w(' if ')
        self.visit(node.test)
        self.w(' else ')
        self.visit(node.orelse)
 
    def visit_Starred(self, node):
        "_"
        self.w('*')
        self.visit(node.value)
 
    def visit_comprehension(self, node):
        "_"
        self.w(' for ')
        self.visit(node.target)
        self.w(' in ')
        self.visit(node.iter)
        if node.ifs:
            for if_ in node.ifs:
                self.w(' if ')
                self.visit(if_)
 
# (3) Doc generation 

class latex:
    "% This is generated, do not edit by hands!\n"
    def __init__(self, userfile='__file__', today=''):
        r"\begin{document}"
        self.src = os.path.basename(sys.argv[0])
        self.tex = latex.__doc__ + '\n'
        self.today = r'\date{%s}' % today if today else ''
        digest = base64.urlsafe_b64encode(hashlib.sha1(open(userfile, 'r', encoding='utf-8').read().encode('utf-8')+__digest__).digest())[:5]
        self.digest = re.sub('_', '\_', r'\texttt{%s}' % digest.decode('utf-8'))
        self.embeds = ['u.py']
        for l in open(userfile, 'r', encoding='utf-8').readlines(): #2
            m = re.search(r'/(\w+\.png)', l) 
            if m:
                self.embeds.append(m.group(1))

    def head(self, lpkg, hcmd, title, subtitle, author, email, beam=False):
        "_"
        for p in ('listings', 'embedfile', 'graphicx', 'tikz', 'hyperref') + lpkg:
            a = p.split('|')
            if len(a) > 1:
                self.tex += r'\usepackage[%s]{%s}' % (a[1], a[0]) + '\n'
            else:
                self.tex += r'\usepackage{%s}' % a[0] + '\n'
        base = {'pyt':r'\emph{Python}', 
                'pdf':r'\textsc{pdf}', 
                'wsg':r'\textsc{wysiwyg}', 
                'hmi':r'\textsc{hmi}',
                'mde':r'\textsc{mde}'}
        base.update(hcmd)
        for c in base:
            self.tex += r'\newcommand{\%s}{%s}' % (c, base[c]) + '\n' 
        self.tex += r'\renewcommand{\u}{$\sqcup$}' + '\n'


        self.tex += r'\newcommand{\prereq}{Default}'

        #LAB!
        #self.tex += r'\newcommand{\req}[3]{{\fbox{\parbox{.95\linewidth}{ {\sc Req.} {\tiny \textcolor{blue}{$\langle \prereq{}#1 \rangle$} } }  \em #2 $\circ$   \hfill {\tiny $\wordsl{#3}$ } } } }' + '\n'
        #self.tex += r'\newcommand{\req}[3]{\paragraph{{\sc Req.} {\tiny \textcolor{blue}{$\langle \prereq{}#1 \rangle$} } }  {\em #2 $\circ$ } \hfill {\tiny $\wordsl{#3}$ } }' + '\n'
        #self.tex += r'\newcommand{\req}[3]{\paragraph{\marginpar{\sc \textcolor{green}{Req.}}{\tiny \textcolor{blue}{$\langle \prereq{}#1 \rangle$} } }  {\em #2 $\circ$ } \hfill {\tiny $\wordsl{#3}$ } }' + '\n'


        self.tex += r'\def\wordsl#1{\wordsloopiter#1 \nil} \def\wordsloopiter#1 #2\nil{ \textcolor{brown}{\langle #1 \rangle} \ifx&#2& \let\next\relax \else \def\next{\wordsloopiter#2\nil} \fi \next}' + '\n'
        self.tex += r'\newcommand{\req}[3]{\paragraph{{\sc Req.} {\tiny \textcolor{blue}{$\langle \prereq{}#1 \rangle$} } } {\em #2 $\circ$ } \hfill {\tiny $\wordsl{#3}$ } \index{Req[\prereq{}#1]}}' + '\n'
    

        if beam: 
            self.tex += r'\title[%s]{%s} %s' % (self.digest, title, self.today) + '\n'
            if subtitle:
                self.tex += r'\subtitle{%s}' % (subtitle) + '\n'
            self.tex += r'\author{%s\inst{*}}\institute{*%s}' % (author, email) + '\n'
        else:
            ti = r'%s \\ {\large %s}' % (title, subtitle) if subtitle else title
            #self.tex += r'\thispagestyle{fancy}'+ '\n'
            self.tex += r'\title{\bf %s}' % ti + '\n'
            self.tex += r'\author{%s -- \url{%s} \\' % (author, email) + '\n'
            self.tex += r'{\tiny \tt [%s]}\footnote{the first five characters of the base64 encoding of the \textsc{sha1} digest of the attached source files.}}' % self.digest + '\n'
            self.tex += self.today + '\n'
            #self.tex += r'\pagestyle{myheadings} \markright{{\tiny \tt [%s]}\hfill}' % self.digest + '\n'
            #self.tex += r'\usetikzlibrary{svg.path}'+ '\n'
            #self.tex += r'\pagestyle{fancy}' + '\n'
            #self.tex += r'\lhead{\begin{tikzpicture} %s \end{tikzpicture}}' % rclogo(.4, 0, -2) + '\n'
            #self.tex += r'\lfoot{{\tiny \tt [%s]} }' % self.digest + '\n'
            self.tex += r'\makeindex' + '\n'
        self.tex += latex.__init__.__doc__ + '\n'
        self.tex += r'\lstset{language=Python, breaklines=true}' + '\n'
        self.tex += r'\hypersetup{colorlinks,linkcolor=Red, urlcolor=Blue}' + '\n'
        self.tex += r'\usetikzlibrary{shapes,fit,arrows,shadows,backgrounds,svg.path}'+ '\n'
        if self.src != 'u.py':
            self.tex += r'\embedfile[filespec=%s]{%s}' % (self.src, os.path.abspath(self.src)) + '\n'
        for x in self.embeds:
            self.tex += r'\embedfile[filespec=%s]{%s}' % (x, os.path.abspath(x)) + '\n'
        if beam:
            self.tex += r"\begin{frame} \titlepage" + '\n'
            #self.tex += r"\setbeamertemplate{footline}{\insertframenumber}" + '\n'
            #self.tex += r'\setbeamertemplate{footline}[frame number]' + '\n'
            self.tex += r'\usetikzlibrary{svg.path} \begin{tikzpicture}[remember picture,overlay,shift={(current page.north east)}] %s \end{tikzpicture}' % rclogo(.4) + '\n'
            self.tex += r'\begin{tikzpicture}[remember picture,overlay,shift={(current page.north west)}] %s \end{tikzpicture}' % ulogo() + '\n'
            self.tex += r"\end{frame}" + '\n'
        else: 
            self.tex += r'\maketitle' + '\n'
            self.tex += r'\usetikzlibrary{svg.path} \begin{tikzpicture}[remember picture,overlay,shift={(current page.north east)}] %s \end{tikzpicture}' % rclogo() + '\n' 

    def itemize(self, tab):
        "_"
        return functools.reduce(lambda y, k: y+r'\item %s' % k+ '\n', tab, r'\begin{itemize}' + '\n') + r'\end{itemize}' + '\n'

    def gen(self, name):
        r"""\end{document}"""
        self.tex += latex.gen.__doc__ + '\n'
        open('%s.tex' % name, 'w').write(self.tex)
        #here = os.path.dirname(os.path.abspath(__file__))
        here = os.path.dirname(os.path.abspath(self.src))
        if here == '/tmp':
            subprocess.call(('pdflatex','-interaction=batchmode', '-output-directory', '/tmp', '%s.tex'%name, '1>/dev/null'))
        else:
            subprocess.call(('pdflatex','-interaction=batchmode', '-output-directory', '/tmp', '%s/%s.tex' % (here, name), '1>/dev/null'))
            shutil.move('/tmp/%s.pdf' % name, '%s/%s.pdf' % (here, name))

class article (latex):
    r"\documentclass[a4paper,11pt]{article}"
    def __init__(self, userfile=__file__, title='title', author='author', email='email', subtitle='', today=''):
        "_"
        self.userfile = userfile
        latex.__init__(self, userfile, today)
        self.tex += article.__doc__ + '\n'
        self.head(('geometry|margin=2cm', 'inputenc|utf8', 'lmodern', 'color', 'longtable', 'array', 'makeidx'), {}, title, subtitle, author, email)
        #self.tex += r'\makeindex' + '\n'
        
    def abstract(self, content, kw=()):
        "_"
        self.tex += r'\begin{abstract}' + content + '\n'
        if kw:
            self.tex += r'\vspace{.4cm}\par\indent {\small {\bf Keywords\/}: ' + ', '.join(kw) + '.}'
        self.tex += r'\end{abstract}' + '\n'

    def index(self):
        "_"
        self.tex += r'\printindex' + '\n'

    def end(self, note=False):
        "_"
        if note:
            self.tex += r'\begin{flushright}{\tiny The end of the document}\end{flushright}' +'\n' 
        self.tex += '\n' 

    def section(self, title, content):
        "_"
        self.tex += r'\section{%s}' % title + '\n'
        self.tex += content + '\n'

    def subsection(self, title, content):
        "_"
        self.tex += r'\subsection{%s}' % title + '\n'
        self.tex += content + '\n'

    def biblio(self, hbib):
        "_"
        self.tex += r'\begin{thebibliography}{99}' 
        for i in hbib: self.tex += r'\bibitem{%s} %s.' % (i, hbib[i]) + '\n'
        self.tex += r'\end{thebibliography}'

    def gen(self, note=True):
        "_"
        self.end(note)
        latex.gen(self, os.path.basename(self.userfile)[:-3])

class article12 (article):
    r"\documentclass[a4paper,12pt]{article}"
    def __init__(self, userfile, title='title', author='author', email='email', subtitle=''):
        "_"
        self.userfile = userfile
        latex.__init__(self, userfile)
        self.tex += article12.__doc__ + '\n'
        latex.head(self, ('geometry|margin=2cm', 'inputenc|utf8', 'lmodern', 'color', 'longtable', 'array'), {}, title, subtitle, author, email)

class report (article):
    r"\documentclass[a4paper,11pt]{report}"
    def __init__(self, userfile, title='title', author='author', email='email', subtitle=''):
        "_"
        self.userfile = userfile
        latex.__init__(self, userfile)
        self.tex += report.__doc__ + '\n'
        latex.head(self, ('geometry|margin=2cm', 'inputenc|utf8', 'lmodern', 'color', 'longtable', 'array', 'fancyhdr'), {}, title, subtitle, author, email)
        self.tex += r'\tableofcontents' + '\n' + r'\listoffigures' + '\n' + r'\listoftables' + '\n'

    def chapter(self, title, content):
        "_"
        self.tex += r'\chapter{%s}' % title + '\n'
        #self.tex += r'\thispagestyle{fancy}' + '\n'
        self.tex += content + '\n'

class draft (article):
    r"\documentclass[a4paper,12pt]{article}"
    def __init__(self, userfile, title='title', author='author', email='email', subtitle=''):
        "_"
        self.userfile = userfile
        latex.__init__(self, userfile)
        self.tex += draft.__doc__ + '\n'
        latex.head(self, ('draftwatermark', 'geometry|margin=2cm', 'inputenc|utf8', 'lmodern', 'color', 'longtable', 'array'), {}, title, subtitle, author, email)
 
def tikz(ustr, rx=1, ry=1):
    "_"
    myu = u()
    return myu.gen_tikz(myu.parse(ustr), False, rx, ry)

def insert_code(userfile, pat, sli=None):
    " ....in LaTeX "
    #o, x, d = r'\lstset{basicstyle=\small\ttfamily, numbers=left, numberstyle=\tiny, stepnumber=5, numbersep=5pt}', [], False
    o, x, d = r'\lstset{basicstyle=\small\ttfamily, numbers=left, numberstyle=\tiny}', [], False
    o += r'\begin{lstlisting}[texcl]' + '\n'
    for l in open(userfile, 'r', encoding='utf-8').readlines(): #1
        if re.match(r'(if|\s*def|class|\s*$)', l): d = False
        if re.match(pat, l): d = True
        if d: x.append(l)
    z = eval('x[%s]'%sli) if sli else x
    return o + ''.join(z) + r'\end{lstlisting}' + '\n'

class beamer (latex):
    r"\documentclass{beamer}"
    def __init__(self, userfile, title='title', author='author', email='email', subtitle='', today=''):
        "_"
        self.userfile = userfile
        latex.__init__(self, userfile, today)
        self.tex += beamer.__doc__ + '\n'
        self.head(('beamerthemeshadow', ), {}, title, subtitle, author, email, True)
        self.tex += r'\note{%s}' % __doc__ + '\n'
        self.header = r'\begin{frame}[fragile] ' + '\n' + r'\frametitle{%s \hfill {\tiny \insertframenumber/\inserttotalframenumber } }'

    def frame(self, title, content):
        "basic frame"
        self.tex += self.header % title + '\n'
        self.tex += content + '\n'
        self.tex += r'\end{frame}' + '\n'

    def frame2(self, title, c1, c2):
        "2 columns"
        self.tex += self.header % title + '\n'
        self.tex += r'\begin{columns}[c]' + '\n'
        self.tex += r'\column{2.1in}' + '\n' + c1 + '\n' + r'\column{2.1in}' + '\n' + c2 + '\n'
        self.tex += r'\end{columns}' + '\n'
        self.tex += r'\end{frame}' + '\n'

    def itemize(self, title, head, tab, tail=''):
        "_"
        self.tex += self.header % title + '\n' + head
        self.tex += functools.reduce(lambda y, k: y+r'\item %s.'%k+ '\n', tab, r'\begin{itemize}' + '\n') + r'\end{itemize}' + r'%s' % tail + '\n' + r'\end{frame}' + '\n'
    
    def itemize2(self, title, tab1, tab2, tail=''):
        "_"
        self.tex += r'\begin{frame}'
        self.tex += r'\frametitle{%s}' % title + '\n'
        self.tex += r'\begin{columns}[l]' + '\n'
        self.tex += r'\column{1.5in}' + '\n'
        self.tex += functools.reduce(lambda y, k: y+r'\item %s'%k+ '\n', tab1, r'\begin{itemize}') + r'\end{itemize}'
        self.tex += r'\column{1.5in}' + '\n' 
        self.tex += functools.reduce(lambda y, k: y+r'\item %s'%k+ '\n', tab2, r'\begin{itemize}') + r'\end{itemize}'
        self.tex += r'\end{columns}' + '\n'
        self.tex += r'\par ' + tail
        self.tex += r'\end{frame}' + '\n'

    def enum(self, title, head, tab, tail=''):
        "_"
        self.tex += self.header % title + '\n' + head
        self.tex += functools.reduce(lambda y, k: y+r'\item %s.'%k+ '\n', tab, r'\begin{enumerate}' + '\n') + r'\end{enumerate}' + '%s' % tail + '\n' + r'\end{frame}' + '\n'
        
    #def gen_pdf(self):
    #    latex.gen_pdf(self, 'beamer_' + os.path.basename(self.userfile)[:-3])

    def gen(self, note=True):
        "_"
        latex.gen(self, os.path.basename(self.userfile)[:-3])

def rclogo(r=.75, x=-7, y=-4):
    "RockwellCollins TikZ logo (SVG source)"
    return r'\begin{scope}[draw=none,scale=%f,shift={(%d,%d)}] \fill[black] svg "M10.0,86.6L01.8,86.6L-5.8,69.5L-.4,69.5L02.3,75.7C02.3,75.7 03.4,75.8 04.3,75.8C05.3,75.8 05.8,75.6 05.8,75.6C07.4,75.1 06.9,73.6 06.9,73.6C06.8,73.2 06.6,72.7 06.6,72.7L05.2,69.5L10.5,69.5L12.4,73.6C13.3,76.0 11.5,76.8 11.5,76.8C10.9,77.1 09.9,77.2 09.9,77.2L09.9,77.3C10.6,77.3 11.4,77.5 11.4,77.5C13.9,77.9 15.1,79.7 15.1,79.7C16.5,81.5 16.1,83.4 16.1,83.4C15.8,85.1 14.3,85.9 14.3,85.9C13.4,86.4 11.9,86.6 11.9,86.6C11.2,86.6 10.0,86.6 10.0,86.6zM52.4,86.6L44.9,69.5L50.2,69.5L52.8,75.4L54.6,69.5L60.1,69.5L58.1,74.3C57.6,75.5 57.3,76.0 57.3,76.0C58.3,76.6 59.6,77.6 59.6,77.6L64.9,81.8L62.9,69.5L68.9,69.5L76.1,78.4C76.1,78.4 76.2,78.5 76.3,78.6L76.3,78.6C76.2,78.4 76.1,77.9 76.1,77.9L74.7,69.5L80.9,69.5L91.7,82.2L85.8,82.2L79.1,73.7C79.1,73.7 79.0,73.5 79,73.4L78.9,73.5C79.0,73.7 79.0,74.0 79.0,74.0L80.4,82.2L75.1,82.2L68.4,73.7C68.4,73.7 68.3,73.6 68.2,73.4L68.2,73.5C68.3,73.6 68.3,74.0 68.3,74.0L69.7,82.2L59.6,82.2L53.1,76.6L53.1,76.6C53.6,77.4 54.2,78.8 54.2,78.8L57.7,86.6L52.4,86.6zM111.8,86.6L104.2,69.5L109.6,69.5L117.1,86.6L111.8,86.6zM120.3,86.6L112.8,69.5L118.1,69.5L125.7,86.6L120.3,86.6zM07.5,83.9C07.5,83.9 08.5,83.9 09.0,83.8C09.0,83.8 09.8,83.7 10.3,83.1C10.3,83.1 11.1,82.2 10.5,80.8C10.5,80.8 09.9,79.3 08.4,78.8C08.4,78.8 07.7,78.6 06.5,78.6L03.6,78.6L05.9,83.8L07.5,83.9zM99.4,82.6C98.7,82.6 98.2,82.6 98.2,82.6C94.4,82.2 91.8,79.7 91.8,79.7C89.5,77.8 89,75.6 89,75.6C88.5,74.4 88.7,73.2 88.7,73.2C89.2,70.2 92.5,69.5 92.5,69.5C93.8,69.2 95.3,69.2 95.3,69.2C95.3,69.2 97.7,69.0 100.7,69.5L101.3,69.6L102.6,72.4C102.3,72.3 101.9,72.2 101.4,72.1C101.3,72.1 99.1,71.7 97.4,71.8C97.4,71.8 96.4,71.8 95.6,72.1C95.6,72.1 94.7,72.3 94.3,72.8C94.3,72.8 93.8,73.3 93.8,73.9C93.8,73.9 93.7,74.3 93.9,74.8L103.9,75.0L104.0,75.1C104.8,76.1 105.2,77.3 105.2,77.3C106.0,79.4 104.7,80.9 104.7,80.9C103.7,82.1 101.9,82.4 101.9,82.4C101.1,82.6 100.1,82.6 99.4,82.6zM25.0,82.5C22.8,82.5 20.7,81.8 18.9,80.5C17.1,79.3 15.8,77.6 15.1,75.9L15.1,75.8C15.0,75.4 14.9,75.1 14.8,74.7C14.8,74.4 14.8,74.1 14.8,73.8C14.8,72.7 15.0,71.9 15.6,71.1C16.7,69.9 18.5,69.2 21.0,69.2C23.6,69.1 26.0,69.7 28.1,71.2C29.9,72.4 31.2,74.1 31.9,75.8L31.9,75.9C32.0,76.3 32.1,76.6 32.1,77.0C32.2,77.3 32.2,77.6 32.2,77.9C32.2,79.0 31.9,79.8 31.3,80.6C30.3,81.8 28.4,82.5 26,82.5C25.6,82.5 25.3,82.5 25.0,82.5zM44.0,82.5C43.8,82.5 43.6,82.5 43.6,82.5C41.9,82.4 39.3,81.9 37.2,80.5C35.0,79.0 33.4,76.8 33.1,74.6C32.8,73.2 32.9,70.7 36.2,69.6C36.8,69.4 37.8,69.2 39.5,69.2C39.6,69.2 40.8,69.2 42.4,69.5L43.7,72.4C42.5,72.2 41.6,72.2 41.6,72.2C40.3,72.2 39.4,72.7 39.4,72.7C38.0,73.6 38.5,75.2 38.5,75.3C39.3,78.4 42.6,79.0 43.1,79.1C44.5,79.3 45.8,79.2 46.6,79.0L48.0,82.1C46.4,82.5 44.7,82.5 44.0,82.5zM98.7,80.0C98.8,80.0 98.9,79.9 99.0,79.9C99.1,79.9 99.6,79.9 100.0,79.7C100.0,79.7 100.6,79.4 100.8,78.7C101.0,77.9 100.5,76.9 100.5,76.9L95.2,76.8C95.2,76.8 94.8,76.8 94.6,76.8L94.6,76.8C94.6,76.9 94.7,77.0 94.7,77.0C95.0,77.7 95.5,78.3 96.0,78.8C96.7,79.4 97.5,79.8 98.3,79.9C98.4,79.9 98.5,80.0 98.7,80.0zM24.4,79.3C24.6,79.3 24.6,79.3 24.6,79.3C27.0,79.4 27.0,77.4 27.0,77.4C27.0,76.7 26.8,76.1 26.7,75.9L26.7,75.8C26.0,74.1 24.7,73.2 24.7,73.2C23.5,72.4 22.3,72.4 22.3,72.4C19.9,72.3 20,74.4 20,74.4C19.9,75.0 20.2,75.6 20.2,75.8L20.3,75.9C21.0,77.6 22.2,78.5 22.2,78.5C23.1,79.1 24.0,79.3 24.4,79.3zM80.4,66.6C75.1,66.6 71.8,64.7 71.8,64.7C67.2,62.4 65.7,58.8 65.7,58.8C64.4,56.1 65.0,53.7 65.0,53.7C65.5,51.3 67.7,50.2 67.7,50.2C69.1,49.4 71.1,49.1 71.1,49.1C72.4,49.0 73.6,49.0 73.6,49.0C74.9,49.0 76.3,49.2 76.3,49.2L77.8,52.7C75.1,52.4 74.0,52.5 74.0,52.5C71.7,52.7 71.0,54.5 71.0,54.5C70.5,55.7 70.8,56.9 70.8,56.9C71.3,59.1 73.2,60.6 73.2,60.6C75.6,62.7 79.1,62.7 79.1,62.7C80.4,62.8 82.0,62.6 82.0,62.6L84,66.2L83.7,66.3C82.4,66.5 81.4,66.5 81.4,66.5C81.1,66.5 80.7,66.5 80.4,66.6zM102.8,66.2L95.3,49.0L100.6,49.0L108.1,66.2L102.8,66.2zM111.4,66.2L103.9,49.0L109.2,49.0L116.7,66.2L111.4,66.2zM89.0,62.1C86.8,62.1 84.8,61.4 82.9,60.1C81.1,58.9 79.8,57.3 79.2,55.5L79.1,55.5C79.0,55.1 78.9,54.7 78.9,54.3C78.8,54.0 78.8,53.7 78.8,53.4C78.8,52.4 79.1,51.5 79.7,50.8C80.7,49.5 82.6,48.9 85.1,48.8C87.7,48.7 90.1,49.3 92.2,50.8C94.0,52.0 95.3,53.7 95.9,55.4L96,55.5C96.1,55.9 96.1,56.3 96.2,56.6C96.2,57.0 96.3,57.3 96.3,57.6C96.3,58.6 96.0,59.5 95.4,60.2C94.4,61.4 92.5,62.1 90.0,62.1C89.7,62.2 89.4,62.2 89.0,62.1zM149.1,62.1C144.2,61.9 142.9,59.2 142.9,59.1L142.8,59.0C142.4,58.1 142.6,57.4 142.6,57.3C142.7,56.3 143.5,55.8 143.5,55.7C144.0,55.3 144.9,54.9 144.9,54.9L146.0,54.4C146.8,54.0 147.1,53.7 147.1,53.7C147.3,53.5 147.3,53.2 147.3,53.0C147.3,52.6 147.2,52.4 147.0,52.2C146.6,51.7 145.9,51.7 145.9,51.6C145.2,51.5 144.5,51.6 144.5,51.6C143.1,51.6 141.3,52.1 140.7,52.3C140.4,51.7 139.8,50.5 139.4,49.4C140.7,49.1 144.1,48.7 146.1,48.7C146.1,48.7 146.2,48.7 146.2,48.7C151.1,48.8 152.5,51.5 152.6,51.7L152.6,51.8C153.0,52.7 152.8,53.4 152.8,53.4C152.7,54.5 151.9,55.0 151.9,55.1C151.4,55.5 150.6,55.9 150.5,55.9L149.4,56.4C148.6,56.8 148.4,57.1 148.3,57.1C148.1,57.4 148.1,57.6 148.1,57.8C148.1,58.2 148.2,58.4 148.4,58.6C148.8,59.0 149.5,59.1 149.5,59.1C150.2,59.3 150.9,59.2 151,59.2C152.3,59.2 153.8,58.7 154.5,58.5C154.7,59.0 155.4,60.7 155.7,61.2C154.8,61.5 152.6,62.0 150.2,62.1L150.0,62.1C150.0,62.1 149.1,62.1 149.1,62.1zM136.7,62.0C135.0,62.0 133.6,61.4 133.6,61.4C131.2,60.6 130.0,59.0 130.0,59.0L130.0,59.0L131.4,61.8L126.5,61.7L120.9,49.0L126.1,49.0L128.9,55.1C129.2,55.8 129.5,56.3 129.5,56.3C130.2,57.4 131.3,57.9 131.3,57.9C132.4,58.4 133.6,58.2 133.6,58.2C134.6,58.1 134.8,57.4 134.8,57.4C134.9,57.1 134.8,56.7 134.8,56.7C134.7,56.2 134.5,55.8 134.5,55.8L131.5,49.0L136.7,49.0L139.8,55.9C140.1,56.7 140.4,57.4 140.4,57.4C140.7,58.2 140.7,58.9 140.7,58.9C140.7,60.2 139.8,61.0 139.8,61.0C138.6,62.1 136.7,62.0 136.7,62.0zM118.0,61.8L112.4,49.0L117.7,49.0L123.3,61.7L118.0,61.8zM88.7,59.0C91.1,59.0 91.0,57.0 91.0,57.0C91.1,56.4 90.8,55.7 90.8,55.6L90.7,55.5C90.0,53.8 88.8,52.9 88.8,52.9C87.6,52.0 86.4,52.0 86.4,52.0C84.0,52.0 84.0,54.0 84.0,54.0C84.0,54.7 84.2,55.3 84.3,55.5L84.3,55.6C85.0,57.3 86.3,58.1 86.3,58.1C87.5,59.0 88.7,59.0 88.7,59.0z"; \fill[red] svg "M121.3,69.5L119.8,66.1L125.2,66.1L126.7,69.5L121.3,69.5zM112.8,69.5L111.4,66.2L116.7,66.2L118.1,69.5L112.8,69.5zM104.2,69.5L102.8,66.2L108.1,66.2L109.6,69.5L104.2,69.5z"; \end{scope}' % (r, x, y)

def rpilogo():
    """provision!
    color green:#75a928
    color red:#bc1142
"""
    return r"""\definecolor{mygreen}{RGB}{117,169,40}  \begin{scope}[draw=none,scale=.1,shift={(-100,-100)}] \fill[mygreen] svg "M 158.37,1.65c-3.61,0.11 -7.51,1.44 -11.93,4.93C135.61,2.41 125.11,0.96 115.71,9.46 101.22,7.58 96.50,11.46 92.93,16 89.75,15.93 69.11,12.72 59.65,26.84 35.87,24.03 28.35,40.83 36.87,56.5c-4.85,7.51 -9.88,14.94 1.46,29.28 -4.01,7.98 -1.52,16.64 7.93,27.12 -2.49,11.22 2.41,19.14 11.21,25.31 -1.64,15.35 14.08,24.28 18.78,27.46 1.80,8.94 5.56,17.39 23.53,22.06 2.96,13.33 13.76,15.63 24.21,18.43 -34.56,20.08 -64.20,46.52 -64,111.37l-5.06,9.03C15.33,350.69 -20.31,428.16 35.43,491.12c3.64,19.70 9.74,33.86 15.18,49.53 8.13,63.13 61.21,92.69 75.21,96.18 20.51,15.62 42.36,30.45 71.93,40.84 27.87,28.74 58.07,39.70 88.43,39.68 0.44,0 0.89,0.00 1.34,0 30.36,0.01 60.56,-10.93 88.43,-39.68 29.56,-10.38 51.42,-25.21 71.93,-40.84 14.00,-3.49 67.08,-33.05 75.21,-96.18 5.43,-15.66 11.54,-29.82 15.18,-49.53 55.75,-62.96 20.09,-140.42 -19.53,-164.53L513.75,317.56c0.20,-64.85 -29.43,-91.28 -64,-111.37 10.45,-2.79 21.25,-5.10 24.21,-18.43 17.96,-4.66 21.72,-13.11 23.53,-22.06 4.69,-3.18 20.42,-12.11 18.78,-27.46 8.80,-6.17 13.71,-14.08 11.21,-25.31 9.46,-10.48 11.95,-19.14 7.93,-27.12C546.79,71.44 541.76,64.01 536.90,56.5 545.42,40.83 537.90,24.03 514.12,26.84 504.66,12.72 484.02,15.93 480.84,16 477.27,11.46 472.55,7.58 458.06,9.46 448.67,0.962 438.17,2.41 427.34,6.59 414.48,-3.55 405.97,4.58 396.25,7.65 380.67,2.56 377.11,9.53 369.46,12.37 352.49,8.78 347.33,16.59 339.18,24.84l-9.46,-0.18c-25.61,15.09 -38.33,45.82 -42.84,61.62 -4.51,-15.80 -17.20,-46.53 -42.81,-61.62l-9.46,0.18C226.44,16.59 221.28,8.78 204.31,12.37 196.66,9.53 193.10,2.56 177.53,7.65c-6.37,-2.01 -12.24,-6.21 -19.15,-6z"; \end{scope}"""

def rpilogo():
    """color green:#75a928 red:#bc1142"""
    return r"""\definecolor{pigreen}{RGB}{117,169,40} \definecolor{pired}{RGB}{188,17,66} \begin{scope}[draw=none,xscale=0.1,yscale=-0.1,shift={(-30,5)}]\fill[black] svg "M 158.37,1.65c-3.61,0.11 -7.51,1.44 -11.93,4.93C135.61,2.41 125.11,0.96 115.71,9.46 101.22,7.58 96.50,11.46 92.93,16 89.75,15.93 69.11,12.72 59.65,26.84 35.87,24.03 28.35,40.83 36.87,56.5c-4.85,7.51 -9.88,14.94 1.46,29.28 -4.01,7.98 -1.52,16.64 7.93,27.12 -2.49,11.22 2.41,19.14 11.21,25.31 -1.64,15.35 14.08,24.28 18.78,27.46 1.80,8.94 5.56,17.39 23.53,22.06 2.96,13.33 13.76,15.63 24.21,18.43 -34.56,20.08 -64.20,46.52 -64,111.37l-5.06,9.03C15.33,350.69 -20.31,428.16 35.43,491.12c3.64,19.70 9.74,33.86 15.18,49.53 8.13,63.13 61.21,92.69 75.21,96.18 20.51,15.62 42.36,30.45 71.93,40.84 27.87,28.74 58.07,39.70 88.43,39.68 0.44,0 0.89,0.00 1.34,0 30.36,0.01 60.56,-10.93 88.43,-39.68 29.56,-10.38 51.42,-25.21 71.93,-40.84 14.00,-3.49 67.08,-33.05 75.21,-96.18 5.43,-15.66 11.54,-29.82 15.18,-49.53 55.75,-62.96 20.09,-140.42 -19.53,-164.53L513.75,317.56c0.20,-64.85 -29.43,-91.28 -64,-111.37 10.45,-2.79 21.25,-5.10 24.21,-18.43 17.96,-4.66 21.72,-13.11 23.53,-22.06 4.69,-3.18 20.42,-12.11 18.78,-27.46 8.80,-6.17 13.71,-14.08 11.21,-25.31 9.46,-10.48 11.95,-19.14 7.93,-27.12C546.79,71.44 541.76,64.01 536.90,56.5 545.42,40.83 537.90,24.03 514.12,26.84 504.66,12.72 484.02,15.93 480.84,16 477.27,11.46 472.55,7.58 458.06,9.46 448.67,0.962 438.17,2.41 427.34,6.59 414.48,-3.55 405.97,4.58 396.25,7.65 380.67,2.56 377.11,9.53 369.46,12.37 352.49,8.78 347.33,16.59 339.18,24.84l-9.46,-0.18c-25.61,15.09 -38.33,45.82 -42.84,61.62 -4.51,-15.80 -17.20,-46.53 -42.81,-61.62l-9.46,0.18C226.44,16.59 221.28,8.78 204.31,12.37 196.66,9.53 193.10,2.56 177.53,7.65c-6.37,-2.01 -12.24,-6.21 -19.15,-6z";\fill[pigreen] svg "M 107.39,68.05c67.94,35.03 107.44,63.36 129.08,87.50 -11.08,44.41 -68.89,46.44 -90.03,45.19 4.32,-2.01 7.93,-4.42 9.22,-8.13 -5.30,-3.76 -24.11,-0.39 -37.24,-7.77 5.04,-1.04 7.40,-2.06 9.76,-5.78 -12.40,-3.95 -25.76,-7.36 -33.62,-13.92 4.24,0.05 8.20,0.94 13.74,-2.89 -11.11,-5.98 -22.96,-10.73 -32.18,-19.88 5.74,-0.14 11.93,-0.05 13.74,-2.16 -10.17,-6.30 -18.75,-13.30 -25.85,-20.97 8.03,0.97 11.43,0.13 13.37,-1.26 -7.68,-7.87 -17.41,-14.52 -22.05,-24.22 5.96,2.05 11.43,2.84 15.36,-0.18 -2.61,-5.89 -13.80,-9.36 -20.24,-23.14 6.28,0.60 12.94,1.37 14.28,0C61.80,58.51 56.79,51.83 51.88,44.91 65.33,44.71 85.71,44.96 84.79,43.82l-8.31,-8.49c13.13,-3.53 26.58,0.56 36.33,3.61 4.38,-3.45 -0.07,-7.82 -5.42,-12.29 11.16,1.49 21.25,4.05 30.37,7.59 4.87,-4.39 -3.16,-8.79 -7.05,-13.19 17.24,3.27 24.55,7.87 31.81,12.47 5.26,-5.05 0.30,-9.34 -3.25,-13.74 13.00,4.81 19.70,11.03 26.75,17.17 2.39,-3.22 6.07,-5.59 1.62,-13.37 9.23,5.32 16.18,11.59 21.33,18.62 5.71,-3.63 3.40,-8.61 3.43,-13.19 9.59,7.80 15.68,16.11 23.14,24.22 1.50,-1.09 2.81,-4.80 3.97,-10.66 22.89,22.21 55.24,78.15 8.31,100.34C207.95,109.95 160.25,86.01 107.39,68.05zM 467.92,68.05C399.97,103.08 360.47,131.42 338.83,155.56c11.08,44.41 68.89,46.44 90.03,45.19 -4.32,-2.01 -7.93,-4.42 -9.22,-8.13 5.30,-3.76 24.11,-0.39 37.24,-7.77 -5.04,-1.04 -7.40,-2.06 -9.76,-5.78 12.40,-3.95 25.76,-7.36 33.62,-13.92 -4.24,0.05 -8.20,0.94 -13.74,-2.89 11.11,-5.98 22.96,-10.73 32.18,-19.88 -5.74,-0.14 -11.93,-0.05 -13.74,-2.16 10.17,-6.30 18.75,-13.30 25.85,-20.97 -8.03,0.97 -11.43,0.13 -13.37,-1.26 7.68,-7.87 17.41,-14.52 22.05,-24.22 -5.96,2.05 -11.43,2.84 -15.36,-0.18 2.61,-5.89 13.80,-9.36 20.24,-23.14 -6.28,0.60 -12.94,1.37 -14.28,0 2.92,-11.88 7.92,-18.57 12.83,-25.49 -13.45,-0.19 -33.82,0.05 -32.90,-1.08l8.31,-8.49c-13.13,-3.53 -26.58,0.56 -36.33,3.61 -4.38,-3.45 0.07,-7.82 5.42,-12.29 -11.16,1.49 -21.25,4.05 -30.37,7.59 -4.87,-4.39 3.16,-8.79 7.05,-13.19 -17.24,3.27 -24.55,7.87 -31.81,12.47 -5.26,-5.05 -0.30,-9.34 3.25,-13.74 -13.00,4.81 -19.70,11.03 -26.75,17.17 -2.39,-3.22 -6.07,-5.59 -1.62,-13.37 -9.23,5.32 -16.18,11.59 -21.33,18.62 -5.71,-3.63 -3.40,-8.61 -3.43,-13.19 -9.59,7.80 -15.68,16.11 -23.14,24.22 -1.50,-1.09 -2.81,-4.80 -3.97,-10.66 -22.89,22.21 -55.24,78.15 -8.31,100.34 39.91,-32.94 87.61,-56.88 140.47,-74.84z";\fill[pired] svg "M 369.94,520.36c0.88,52.75 -60.11,88.24 -107.70,71.02 -47.41,-11.85 -72.65,-74.37 -40.86,-113.39 30.26,-42.28 100.43,-43.46 132.37,-2.53 10.26,12.58 16.23,28.65 16.18,44.91zM 241.45,305.96c21.06,15.08 33.52,41.43 29.26,67.33 -2.75,25.76 -16.19,50.39 -35.61,67.44 -20.28,16.78 -49.20,22.53 -74.48,15.96 -27.65,-9.94 -47.47,-39.97 -43.36,-69.50 1.12,-35.31 22.02,-70.38 55.62,-83.55 21.97,-9.78 47.87,-11.88 68.58,2.31zM 332.48,301.96c-50.42,44.57 -26.96,87.00 -12.16,113.94 19.23,22.30 48.98,57.51 107.13,29.76 50.42,-44.57 26.96,-87.00 12.16,-113.94 -18.60,-21.30 -50.01,-58.40 -107.13,-29.76zM 72.91,342.08C109.32,332.33 85.20,492.72 55.57,479.56 22.99,453.35 12.49,376.58 72.91,342.08zM 493.67,340.08c-36.41,-9.75 -12.29,150.63 17.33,137.47 32.58,-26.21 43.08,-102.97 -17.33,-137.47zM 369.97,220.65c62.83,-10.61 115.11,26.72 113.01,94.85 -2.06,26.12 -136.15,-90.96 -113.01,-94.85zM 196.35,218.65C133.52,208.04 81.24,245.37 83.34,313.51 85.41,339.63 219.50,222.54 196.35,218.65zM 286.61,202.75c-37.50,-0.97 -73.49,27.83 -73.58,44.54 -0.10,20.30 29.65,41.09 73.83,41.62 45.12,0.32 73.91,-16.64 74.06,-37.59 0.16,-23.73 -41.03,-48.93 -74.31,-48.57zM 288.90,619.11c32.69,-1.42 76.57,10.53 76.65,26.39 0.54,15.40 -39.78,50.21 -78.82,49.53 -40.42,1.74 -80.06,-33.11 -79.54,-45.19 -0.60,-17.71 49.22,-31.54 81.71,-30.73zM 168.13,525.10c23.27,28.04 33.89,77.31 14.46,91.84 -18.37,11.08 -63.01,6.52 -94.73,-39.05 -21.39,-38.24 -18.63,-77.15 -3.61,-88.58 22.46,-13.68 57.17,4.79 83.88,35.79zM 405.02,516.21c-25.18,29.50 -39.21,83.30 -20.83,100.64 17.56,13.46 64.72,11.58 99.56,-36.75 25.29,-32.46 16.82,-86.68 2.37,-101.07 -21.46,-16.60 -52.27,4.64 -81.09,37.18z";\end{scope}"""

def gen_doc():
    "weave article and beamer"
    art, sli = article(__file__, __title__, __author__, __email__, __subtitle__), beamer(__file__, __title__, __author__, __email__, __subtitle__)
    #
    art.section('Introduction', introduction())
    art.section('Parser', insert_code(__file__, '__RE_U__') )
    #
    sli.itemize(r'What $\sqcup$ is?', r'The $\sqcup$ language is a {\bf Universal Graph Language};', 
                (r'Symbol: $\bigsqcup$',
                 r'Name: "square cup"',
                 r'Universality (close to "u")',
                 r'Unicode character $\sqcup$: (U+2294)',
                 r'License: \textsc{gpl} v3'))

    sli.itemize(r'Introducing the $\sqcup$ language...', 
                r"""...as {\em Extented Literate Programming} 
\par $\sqcup$ is a sparse graph language
\par In $\sqcup$, { \verb?"A->B"? } string is simply rendered as: """ + '\n' + tikz('A->B') + '(TikZ output)',
                (r'{\bf T}-yped',
                 r'{\bf H}-ierachical',
                 r'{\bf O}-nline',
                 r'{\bf N}-eutral',
                 r'{\bf U}-nicode',
                 r'{\bf S}-hort'),
                '...inspired from the Dot (Graphviz) and yUML language to fix some XML pitfalls')
    sli.frame(r'The big picture', tikz('u_process', 2.5, 1.3))
            
    sli.itemize(r'$\sqcup$ Semantics', r'', 
                (r'Model concept: model ({\sc ast}), diagram, string',
                 r'Object concept: object, node, token',
                 r'Relation concept: relation, arc, link',
                 r'Port concept: model port, port, dot port',
                 r'Attribute concept: property, content, label',
                 r'Type concept: type, category, class'),
                'N-ary relation, composition, Concept of {\em role} defined by the couple: ({\em port type, node type})')
                 
    #art.gen()
    #sli.gen()

# (4) Tests

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

# (5) Git storage

class gitu:
    """ All git methods share the same env """

    def __init__(self, user='anybody', ip='0.0.0.0'):
        """ create the GIT repository if needed"""
        if not os.path.isdir(__git_base__): os.mkdir(__git_base__)
        e = os.environ.copy()
        e['GIT_AUTHOR_NAME'], e['GIT_AUTHOR_EMAIL'] = user, ip
        e['GIT_COMMITTER_NAME'], e['GIT_COMMITTER_EMAIL'] = __author__, __email__
        e['GIT_DIR'] = '%s/.git' % __git_base__
        self.e = e
        if not os.path.isdir(e['GIT_DIR']):
            subprocess.Popen(('git', 'init', '-q'), env=e, close_fds=True).communicate()
            p = subprocess.Popen(('git', 'hash-object', '-w', '--stdin'), env=e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
            li = '100644 blob %s\tstart\n' % p.communicate(' \n'.encode('utf-8'))[0].strip().decode('utf-8')
            q = subprocess.Popen(('git', 'mktree'), env=e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
            li = li.encode('utf-8')
            tutu = q.communicate(li)[0].strip().decode('utf-8')
            r = subprocess.Popen(('git', 'commit-tree', tutu), env=e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
            subprocess.Popen(('git', 'update-ref', 'refs/heads/master', r.communicate('start'.encode('utf-8'))[0].strip().decode('utf-8')), env=e, stdout=subprocess.PIPE).communicate()

    def save(self, key, c, state='NON'):
        "_"
        p = subprocess.Popen(('git', 'show', 'master^{tree}:'+key), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate('')
        out = out.decode('utf-8')
        p = subprocess.Popen(('git', 'ls-tree', 'master^{tree}'), env=self.e, stdout=subprocess.PIPE)
        liste = p.communicate()[0].decode('utf-8')
        if err:
            liste += '100644 blob %s\t%s' % (self.sha(c.encode('utf-8')), key)
            self.commit (liste, key)
        else:
            if out != c:
                h = self.sha(c.encode('utf-8'))
                self.commit(re.sub(r'(100644 blob) [0-9a-f]{40}(\t%s)\n' % key, '\\1 %s\\2\n' % h, liste), state) 
        p = subprocess.Popen(('git', 'log', '--pretty=format:%H', '-1'), env=self.e, stdout=subprocess.PIPE)
        return p.communicate()[0][:15]

    def sha(self, content):
        "_"
        p = subprocess.Popen(('git', 'hash-object', '-w', '--stdin'), env=self.e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        return p.communicate(content)[0].strip().decode('utf-8')
    
    def commit(self, li, msg):
        "_"
        # remove bad blobs!
        #for f in ('1', 'tata~b54d625aa203ea55cc14a4b77bb942d818d45738~57c1518fe1'):
        #    li = re.sub(r'100644 blob\s[0-9a-f]{40}\t%s\n' % f, '', li)
        li = li.encode('utf-8')
        msg = msg.encode('utf-8')
        p = subprocess.Popen(('git', 'mktree'), env=self.e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        sha = p.communicate(li)[0].strip().decode('utf-8')
        p = subprocess.Popen(('git', 'show-ref', '--hash', 'refs/heads/master'), env=self.e, stdout=subprocess.PIPE)
        parent = p.communicate()[0].strip().decode('utf-8')
        p = subprocess.Popen(('git', 'commit-tree', sha, '-p', parent), env=self.e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        mm = p.communicate(msg)[0].strip().decode('utf-8')
        p = subprocess.Popen(('git', 'update-ref', 'refs/heads/master', mm), env=self.e, stdout=subprocess.PIPE)

    def list(self):
        "_"
        liste = subprocess.Popen(('git', 'ls-tree', 'master^{tree}'), env=self.e, stdout=subprocess.PIPE).communicate()[0].strip()
        return liste.decode('utf-8').split('\n')

    def history(self, key=''):
        "_"
        if key:
            p = subprocess.Popen(('git', 'log', '--pretty=format:%H:%an:%ar:%s', '--', key), env=self.e, stdout=subprocess.PIPE)
        else:
            p = subprocess.Popen(('git', 'log', '-50', '--pretty=format:%H:%an:%ar:%s'), env=self.e, stdout=subprocess.PIPE)
        liste = p.communicate()[0].strip()
        return liste.decode('utf-8').split('\n')

    def gethead(self, key):
        "_"
        p = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%H:%an:%ar:%at', '--', key), env=self.e, stdout=subprocess.PIPE) 
        return p.communicate()[0].strip().decode('utf-8')

    def gethead2(self, key):
        "_"
        p = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%H:%an:%ar:%s', '--', key), env=self.e, stdout=subprocess.PIPE) 
        return p.communicate()[0].strip().decode('utf-8')
    
    def revision(self, key):
        "_"
        c = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%H', '--', key), env=self.e, stdout=subprocess.PIPE)
        return c.communicate()[0][:15]

    def date(self, key):
        "_"
        c = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%ci', '--', key), env=self.e, stdout=subprocess.PIPE)
        return c.communicate()[0][:-5]

    def rm(self, key):
        "_"
        p = subprocess.Popen(('git', 'reset', '--hard'), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate()
        #p = subprocess.Popen(('git', 'filter-branch', '--tree-filter', '\'rm %s\'' % key, 'HEAD'), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        #out, err = p.communicate()
        o = err if err else out
        return o.decode('utf-8')

    def cat(self, key):
        "_"
        p = subprocess.Popen(('git', 'show', 'master^{tree}:'+key), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate()
        s = 'print ("New file!")' if re.search('\.py$', key) else 'New->File'
        return s if err else out.decode('utf-8')

    def cat_rev(self, key, rev):
        "_"
        p = subprocess.Popen(('git', 'show', '%s:%s' % (rev, key)), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate()
        s = 'print ("New revision!")' if re.search('\.py$', key) else 'New->Revision'
        return s if err else out.decode('utf-8')

    def cat_simple(self, key, rev):
        "_"
        p = subprocess.Popen(('git', 'show', '%s:%s' % (rev, key)), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        return p.communicate()[0][:-1]

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

# (6) Web application 

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

def include_ulogo():
    "_"
    return  r'\begin{tikzpicture} %s \end{tikzpicture}' % ulogo() + '\n'

def ulogo():
    "_"
    return r"""\draw[draw=none,fill=blue,scale=1,shift={(.6,-1)}] svg "M0,0L0,18L4,18L4,4L14,4L14,18L18,18L18,0Z";"""

def style_old():
    """h1,h2,h3,h6,p,li,b,a,td,th{font-family:helvetica neue,helvetica,arial,sans-serif;} a{text-decoration:none;text-align:right;} 
table {border: 1px solid #666;width:100%;border-collapse:collapse;} td,th {border: 1px solid #666;padding:2pt;}
h1{position:absolute;top:-8;left:60;} h2{position:absolute;top:0;left:50%;color:#DDD} h6#digest{position:absolute;top:0;right:10;} 
textarea.editor{resize:none;width:100%; color:white;background-color:#444;}"""
    return '<style>{}</style>\n'.format(style_old.__doc__)

def table_test(par, tset):
    "_"
    o, uobj = '<table>', u()
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
    o = '<table>'
    o += '<tr><th>#</th><th>Action</th><th>Example</th></tr>\n'
    n = 0
    for x in __ACTIONS__:
        n += 1
        o += '<tr><td><small>{:03d}</small></td><td>Keyword: \'{}\'</td><td><a href="u?{}">http://{}/u?{}</a></td></tr>\n'.format(n, x, x, host, x)
    for x in __OUT_LANG__:
        n += 1
        o += '<tr><td><small>{:03d}</small></td><td>Output Language: {}</td><td><a href="u?{}&A-&gt;B">http://{}/u?{}&A-&gt;B</a></td></tr>\n'.format(n, x, x, host, x)
    for x in __IN_MODEL__:
        n, d = n + 1, html.escape(__IN_MODEL__[x])
        o += '<tr><td><small>{:03d}</small></td><td>Input Model Type: {}</td><td><a target="_blank" href="u?_svg&{}">http://{}/u?_svg&{}</a></td></tr>\n'.format(n, x, d, host, d)
    return o + '</table>'

def hhead(key, host):
    "_"
    return '<html>' + favicon() + style_old() + '\n<svg %s height="60">%s</svg><br/>\n<h1>%s</h1>\n<h2 title="server">[%s]</h2>\n<a href="/u?help">Help</a>\n' % (_SVGNS, logo(), key.title(), host)

def htail():
    "_"
    return '<h6 id="digest" title="base64 encoded short sha1 digest">%s</h6></html>' % __digest__.decode('utf-8')

def tex2pdf(txt):
    "TeX to PDF"
    src = 'tikzfile' # better use tempfile module
    dir = tempfile.mkdtemp()
    open('/tmp/%s.tex' % src, 'w', encoding='utf-8').write(txt)
    subprocess.call(('pdflatex','-interaction=batchmode', '-output-directory', '/tmp', '%s.tex' % src, '1>/dev/null'))
    return open('/tmp/%s.pdf' % src, 'rb').read()

def script(s, svg=False):
    "_"
    ns = _XLINKNS if svg else ''
    return '<script %s type="text/ecmascript">\n/*<![CDATA[*//*---->*/\n%s\n/*--*//*]]>*/</script>\n' % (ns, s) 

def style(s):
    "_"
    return '<style type="text/css">' + s + '</style>\n' 

def save(environ, start_response, gid='start'):
    "_"
    start_response('200 OK', [('Content-type', 'text/plain; charset=utf-8'),])
    if environ['REQUEST_METHOD'].lower() == 'post':  
        l = environ['wsgi.input'].read().decode('utf-8').split('\r\n')
        user = 'anonymous'
        sid = parse_sid(environ)
        if sid:
            d = dbm.open('%s/rev' % __git_base__)
            user = d[sid].decode('utf-8')
            d.close()
        g = gitu(user)
        g.save(gid, '\n'.join(l[4:-2]), l[3])
    return ['ok']

def ide(environ, start_response, gid='start', rev=None):
    "_"
    is_python = bool(re.search('\.py$', gid))
    content = gitu().cat_rev(gid, rev) if rev else gitu().cat(gid)
    o = '<html>\n'
    o += '<meta http-equiv="Content-Type" content="text/html; charset=utf-8">\n'
    o += '<title id="title">%s</title>\n' % gid + favicon()
    o += style('h6,input,a{font-family:helvetica neue,helvetica,arial,sans-serif;color:Dodgerblue;}a,input{font-size:.7em;}html,body,textarea,embed,object,input,div,a{margin:0;padding:0;}textarea#editor{position:absolute;left:0;top:0;resize:none;width:50%;height:100%;padding-top:20;}object#reader,embed#reader{position:absolute;right:0;top:0;width:50%;height:100%;background-color:#F1F4FF;}select#lang{position:absolute;right:50%;top:22;z-index:11;}input#message{position:absolute;right:50%;top:0;}a#list{position:absolute;left:0;top:0;}a#history{position:absolute;left:18;top:0;}h6#sid{position:absolute;right:50%;bottom:0;z-index:11;}input#login{position:absolute;left:36;top:0;}input#pw{position:absolute;left:100;top:0;}input#send{position:absolute;left:170;top:0;padding:0;border:none;background:Dodgerblue;color:white}a#msg{position:absolute;left:260;top:0;color:red}a#up{position:absolute;left:210;top:0;}a#altreader{position:absolute;right:210;top:100;display:none;}')
    o += script("""function post(url, data, cb) {var req = new XMLHttpRequest();req.onreadystatechange = processRequest;function processRequest () {if (req.readyState == 4) {if (req.status == 200) {if (cb) { cb(req.responseText); }} else {alert('Error Post status:'+ req.status);}}} this.doPost = function() {req.open('POST', url, true);req.send(data);} };
window.onload = run;
function save() {
//alert ('save');
var name = document.getElementById("title").firstChild.nodeValue;
document.getElementById("title").firstChild.nodeValue = name.replace(/^\*(.*)$/,'$1');
if (typeof ed != 'undefined') {var v=ed.getValue();} else { var v=document.getElementById("editor").value; }
var fD = new FormData();
fD.append('content', document.getElementById("message").value  + '\\n' + v);
var ai = new post('u?^%s', fD, function(res) {run(true);});ai.doPost();
}
//function load1(evt) {
//var a = document.getElementById("reader").getAttribute('data');
//var b = evt.target.getAttribute('data');
//alert('load ' + b + " " + a);
//}
function run(arg) {
//alert ('run');
var py = '%s'; var rev='%s'; var gid='%s'; if ((rev == 'None') || (arg == true)) { rev = ''; } else { rev = '~' + rev;}
var url = 'u?';
if (py == 'True') { url += '$' + gid + rev} else { url += document.getElementById("lang").value + '@' + gid + rev; }
// triks for Webkit refresh [BUG 123536]
document.getElementById("reader").style.webkitTransform = 'scale(1)'; document.getElementById("reader").setAttribute('data', ''); // Webkit BUG 123536
//document.getElementById("reader").setAttribute('data', '');
//document.getElementById("reader").onchange();
document.getElementById("reader").setAttribute('data', url);
//document.getElementById("reader").setAttribute('src', url);
//alert (url);
//document.getElementById("message").value = '';
//document.getElementById("message").blur();
}
""" % (gid, is_python, rev, gid))
    if not is_python:
        runable = ['_%s'%e for e in __OUT_LANG__ if __OUT_LANG__[e][3]]
        o += '<select id="lang" onchange="run();" title="Refresh:\'Alt R\'">' + ''.join(['<option>{}</option>'.format(i) for i in runable + list(__OUT_LANG__) ]) + '</select>\n'
    # begin authentication
    user, sid = 'anonymous', parse_sid(environ)
    if sid:
        d = dbm.open('%s/rev' % __git_base__)
        user = d[sid].decode('utf-8')
        d.close()
    else:
        d = dbm.open('%s/rev' % __git_base__, 'c')
        #d['_'] = '%d' % (int(d['_'])+1) if '_' in d else '0'
        #sid = hashf(d['_'])
        sid = hashf('%s' % time.clock())
        d[sid] = 'anonymous'
        d.close()
    authen, msg = False, ''
    if environ['REQUEST_METHOD'].lower() == 'post': 
        d, x = dbm.open('%s/rev' % __git_base__, 'c'), urllib.parse.unquote(environ['wsgi.input'].read().decode('utf-8'))
        if reg(re.match(r'login=([^&]+)&pw=([^&]+)$', x)):
            user, pw = reg.v.group(1), reg.v.group(2)
            if (user in d) and (d[user].decode('utf-8') == hashf(pw.encode('utf-8'))):
                d[sid], authen = user, True
            else:
                msg = 'wrong login!' 
        else: # logout
            d[sid] = 'anonymous'
        d.close()
    else:
        authen = False if user == 'anonymous' else True
    o += '<form method="post">' 
    if authen:
        o += '<input id="login" title="user" style="border:none" value="%s" size="10"/>' % user
        o += '<input id="send" type="submit" title="logout" name="logout" value="Logout"/>\n'
    else:
        o += '<input id="login" name="login" title="log in with existing account" placeholder="Username" size="8" value=""/>'
        o += '<input id="pw"    name="pw" type="password" title="password" placeholder="Password" size="8" value=""/>' 
        o += '<input id="send"  type="submit" title="submit login/password" value="Login"/>' 
        o += '<a id="up" href="u?signup" title="create a new account">Signup<a/>\n'
    o += '</form><a id="msg" title="error message">%s</a>'% msg
    # end authentication
    o += '<a id="list" href="u?list" title="list"><svg height="16" width="16"><path d="M0,0L16,0L16,16L0,16Z" stroke-width="0" stroke="black" fill="Dodgerblue"/><path d="M4,4L12,4M4,8L12,8M4,12L12,12" stroke-width="2" stroke="white" fill="none"/></svg></a>\n'
    o += '<a id="history" title="history" href="u?history"><svg height="16" width="16"><path d="M0,0L16,0L16,16L0,16Z" stroke-width="0" stroke="black" fill="Dodgerblue"/><path d="M4,4L4,12M4,8L12,8M12,4L12,12" stroke-width="2" stroke="white" fill="none"/></svg></a>\n'
    o += '<input type="text" id="message" placeholder="...enter a commit message" onchange="save();" size="21"/>\n'
    o += '<textarea id="editor">{}</textarea>\n'.format(content)
    here = os.path.dirname(os.path.abspath(__file__))
    AREA = False # config
    if os.path.isfile('%s/cm.css' % here) and os.path.isfile('%s/cm.js' % here) and not AREA:
        o += style(open('%s/cm.css' % here, 'r', encoding='utf-8').read()) + script(open('%s/cm.js' % here, 'r', encoding='utf-8').read())
    o += '<a id="altreader">PDF generation...</a>\n'
    o += '<object id="reader" data=""/>\n'
    #o += '<object id="reader" data=""><param name="autoplay" value="true"/></object>\n'
    #o += '<embed id="reader" src=""></embed>\n'
    start_response('200 OK', [('Content-type', 'text/html; charset=utf-8'), ('set-cookie', 'sid=%s' % sid) ])
    o += '</html>\n'
    return [o.encode('utf-8')]

def parse_sid(environ):
    ""
    if 'HTTP_COOKIE' in environ:
        if reg(re.match(r'sid=([^;]+)', environ['HTTP_COOKIE'])):
            return reg.v.group(1)
    return None

def hashf(s):
    "_"
    return base64.urlsafe_b64encode(hashlib.sha1(s).digest())[:-18].decode('utf-8')

def signup(environ, ch=False):
    msg, passed = 'Change your password' if ch else 'Create a new account!', False
    o = '| <a href="/u?change">Change password</a> | <a href="/u?signup">Signup</a> | <a href="/u?list">List</a> | <a href="/u?history">History</a><br/>\n'
    if environ['REQUEST_METHOD'].lower() == 'post': 
        msg = '...error, try again!'
        x = urllib.parse.unquote(environ['wsgi.input'].read().decode('utf-8'))
        #msg = x
        if reg(re.match(r'login=([^&]{4,20})&pw=([^&]{4,20})&pw2=([^&]{4,20})$', x)):
            user, pw, pw2 = reg.v.group(1), reg.v.group(2), reg.v.group(3)
            d = dbm.open('%s/rev' % __git_base__, 'c')
            if ch:
                if pw != pw2:
                    d[user] = hashf(pw2.encode('utf-8'))
                    passed, msg = True, 'Password for \'%s\' changed!' % user 
            else:
                if pw == pw2 and not re.search(user, pw) and user not in d:
                    sid = parse_sid(environ)
                    if sid == None:
                        #d['_'] = '%d' % (int(d['_'])+1) if '_' in d else '0'
                        #sid = hashf(d['_'])
                        sid = hashf('%s'%time.clock())
                    d[sid], d[user] = user, hashf(pw.encode('utf-8'))
                    passed, msg = True, 'New account for \'%s\' created!' % user
            d.close()
    o += '<a id="msg" title="error message">%s</a>\n' % msg
    if not passed:
        o += '<form method="post">\n' 
        #o += '<input id="cb" type="checkbox" name="cb" onclick="submit();"/><br/>\n'
        o += '<input id="login" name="login" title="select a user name" placeholder="Username" size="20" value=""/><br/>\n'
        l1, l2 = 'old password' if ch else 'password', 'new password' if ch else 'password again'
        o += '<input id="pw"    name="pw" type="password" title="...%s" placeholder="%s" size="20" value=""/><br/>\n' % (l1, l1.title())
        o += '<input id="pw2"   name="pw2" type="password" title="...%s" placeholder="%s" size="20" value=""/><br/>\n' % (l2, l2.title())
        o += '<input id="user"  type="submit" title="submit" value="Send"/>\n'
        o += '</form>\n'
    o += '<h6>Login name shall be [4,20] length<br/>Password shall be [4,20] length<br/>The two passwords shall equal<br/>Login shall not be already in the database<br/>Maximum ten users for the same ip address<br/>Password shall not contain user name</h6>\n'
    return o

def action(environ, start_response, key, host):
    "_"
    if key == 'database':
        mime, fname = 'text/plain', 'db'
        d, o = dbm.open('%s/rev' % __git_base__, 'r'), ''
        for item in d.keys(): o +=  ('%s -> %s\n' % (item.decode('utf-8'), d[item].decode('utf-8')))
        d.close() 
    elif key == 'log':
        mime, fname = 'text/plain', 'db'
        o = open('/tmp/log', 'r').read()
    elif key in ('pdf', 'paper', 'beamer'):
        mime, name = 'application/pdf; charset=utf-8', 'beamer_u' if key == 'beamer' else 'u'
        fname = '{}.pdf'.format(name)
        f = '%s/%s.pdf' % (os.path.dirname(environ['SCRIPT_FILENAME']), name)
        o = open(f, 'rb').read()
    else:
        mime, fname, o = 'text/html; charset=utf-8', key, hhead(key, host)
        ficon = '<svg height="20" width="20"><path d="M0,0L14,0L20,6L20,20L0,20Z" stroke-width="2" stroke="gray" fill="#EEE"/><path d="M4,4L12,4M4,8L16,8M4,12L16,12M4,16L16,16" stroke-width="1" stroke="gray" fill="none"/></svg>'
        pyicon = '<svg height="20" width="20"><path d="M0,0L14,0L20,6L20,20L0,20Z" stroke-width="2" stroke="gray" fill="#FFF"/><path d="M4,4L12,4M4,8L16,8M4,12L16,12" stroke-width="1" stroke="gray" fill="none"/><g transform="scale(.15),translate(-40,-60)"><path style="fill:#366994;" d="M 99.75,67.46C71.71,67.46 73.46,79.62 73.46,79.62L73.5,92.21L100.25,92.21L100.25,96L62.87,96C62.87,96 44.93,93.96 44.93,122.25 C44.93,150.53 60.59,149.53 60.59,149.53L69.93,149.53L69.93,136.40C69.93,136.40 69.43,120.75 85.34,120.75C101.25,120.75 111.87,120.75 111.87,120.75C111.87,120.75 126.78,120.99 126.78,106.34C126.78,91.69 126.78,82.12 126.78,82.12C126.78,82.12 129.04,67.46 99.75,67.46z M85,75.93 C87.66,75.93 89.81,78.08 89.81,80.75C89.81,83.41 87.66,85.56 85,85.56C82.33,85.56 80.18,83.41 80.18,80.75 C 80.18,78.08 82.33,75.93 85,75.93z"/><path style="fill:#ffc331;" d="M100.54,177.31C128.57,177.31 126.82,165.15 126.82,165.15L126.79,152.56L100.04,152.56L100.04,148.78L137.42,148.78C137.42,148.78 155.35,150.81 155.35,122.53C155.35,94.24 139.70,95.25 139.70,95.25L130.35,95.25L130.35,108.37C130.35,108.37 130.86,124.03 114.95,124.03C99.04,124.03 88.42,124.03 88.421,124.03C88.42,124.03 73.51,123.79 73.51,138.43C73.51,153.08 73.51,162.65 73.51,162.65C73.51,162.65 71.25,177.31 100.54,177.31zM115.29,168.84C112.63,168.84 110.48,166.69 110.48,164.03C110.48,161.37 112.63,159.22 115.29,159.22C117.95,159.22 120.10,161.37 120.10,164.03C120.10,166.69 117.95,168.84 115.29,168.84z"/></g></svg>'
        if key in ('about', 'help', 'usage'): 
            o += table_about(host)
        elif key == 'signup': 
            o += signup(environ)
        elif key == 'change': 
            o += signup(environ, True)
        elif key in ('test', 'parse'): 
            o += table_test(True, __AST_SET__)
        elif key in ('unparse',): 
            o += table_test(False, __AST_SET__)
        elif key in ('random',): 
            o += table_test(False, get_random_set())
        elif key == 'list':
            i = 0
            o += '| <a href="/u?history">History</a>'
            o += '<table><tr><th width="2.5cm">#</th><th>blob</th><th width="2cm">commit</th><th width="1cm">author</th><th>age</th><th>message</th></tr>'
            for l in gitu().list():
                i += 1
                t = l.split('\t')
                if len(t) == 2:
                    icon = pyicon if re.search('\.py$', t[1]) else ficon
                    h = gitu().gethead2(t[1])
                    r = h.split(':')
                    o += '<tr><td>%03d</td><td>%s <a href="u?@%s">%s</a></td><td style="font-family:courier;">%s</td><td>%s</td><td>%s</td><td>%s</td></tr>' % (i, icon, t[1], t[1], r[0][:10], r[1], r[2], r[3]) 
            o += '</table>'
        elif key == 'history':
            i = 0
            o += '| <a href="/u?list">List</a>'
            o += '<table><tr><th width="2.5cm">#</th><th>name</th><th width="2cm">commit</th><th width="1cm">author</th><th>age</th><th>message</th></tr>'
            aa = gitu()
            for l in aa.history():
                li = l.split(':')
                i += 1
                p = subprocess.Popen(('git', 'diff-tree', '--name-only', li[0]), env=aa.e, stdout=subprocess.PIPE)
                liste = p.communicate()[0].strip()
                li2 = liste.split()
                if len(li2)>1:
                    name = li2[1].decode('utf-8')[:30]
                    o += '<tr><td>%03d</td><td><a href="u?@%s~%s">%s</a></td><td style="font-family:courier;">%s</td><td>%s</td><td>%s</td><td>%s</td></tr>' % (i, li2[1].decode('utf-8'), li[0][:10], name, li[0][:10], li[1], li[2], li[3])
            o += '</table>'                
        elif key in ('update',):
            here = os.path.dirname(os.path.abspath(__file__))
            # add security
            if host == 'pelinquin':
                cmd = 'cd %s; ls' % here
            else:
                cmd = 'cd %s/..; rm -rf u; git clone https://github.com/pelinquin/u.git' % here # Usualy git://github...
            out, err = subprocess.Popen((cmd), shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
            o += '<pre>Application Update...\n'
            o += 'Error:%s\n' % err if err else 'Message:%s\nUpdate OK\n' % out.decode('utf-8')
            o += '</pre><br/><a href="u?help">Reload the new version</a>'
        elif key in ('source', 'download'):
            o += '<br/><a href="u">Python3 Source code</a>'
        else:
            o += '<pre>Keywork:%s</pre>' % key
        o += htail()
        o = o.encode('utf-8')
    start_response('200 OK', [('Content-type', mime), ('Content-Disposition', 'filename={}'.format(fname))])
    return [o]

def rm(environ, start_response, gid):
    "_"
    o = '%s removed!' % gid
    o += '\n%s' % gitu().rm(gid)
    start_response('200 OK', [('Content-type', 'text/plain; charset=utf-8')])
    return [o]

def display(environ, start_response, gid, rev= None):
    "_"
    raw = gitu().cat_rev(gid, rev) if rev else gitu().cat(gid)
    here = os.path.dirname(os.path.abspath(__file__))
    head = '#!/usr/bin/python3\n# -*- coding: utf-8 -*-\nimport os,sys\np="%s"\nif p not in sys.path: sys.path.append(p)\nimport u\n' % here
    src = '/tmp/%s' % gid
    pdf = src[:-2]+'pdf'
    pdfname = 'toto.pdf'
    open(src, 'w', encoding='utf-8').write(head + raw)
    # FIX HERE !
    out, err = subprocess.Popen(('cd /tmp; rm -rf %s; python3 %s' % (pdf, src)), shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
    #out, err = subprocess.Popen(('python3', src), stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
    #out, err = subprocess.Popen(('cat', src), stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
    #open('/tmp/logpy', 'w', encoding='utf-8').write(out.decode('utf-8'))
    #out = out.encode('utf-8')
    if err:
        pdf = 'error'
        #o = err
        o = 'Python error!\n\n'.encode('utf-8') + err 
        mime = 'text/plain; charset=utf-8'
    else:
        if os.path.isfile(pdf): 
            mime = 'application/pdf'
            o = open(pdf, 'rb').read()
        else:
            pdf = 'no pdf generated!'
            o = out
            mime = 'text/plain; charset=utf-8'
    start_response('200 OK', [('Content-type', mime), ('Content-Disposition', 'filename={}'.format(pdfname))])
    return [o]

def log(s, ip=''):
    "Append log"
    now = '%s' % datetime.datetime.now()
    open('/tmp/log', 'a', encoding='utf-8').write('%s|%s|%s\n' % (now[:-7], ip, s))

def bug(environ, start_response):
    "not fixed!"
    o = '<html><h1>Bug</h1><p>the data attribute value after onload event call is not the new one!</p><object style="background:red;" onchange="alert(event.target.data);" alt="none" data="/u?pdf" width="500" height="500"></object></html>\n'
    start_response('200 OK', [('Content-type', 'text/html; charset=utf-8') ])
    return [o.encode('utf-8')]

def pi(environ, start_response):
    "_"
    start_response('200 OK', [('Content-type', 'image/jpeg') ])
    return [open('%s/pi_sfrbox.jpg' % os.path.dirname(os.path.abspath(__file__)), 'rb').read()]

def p2p_host(environ, start_response, host):
    "_"
    if host:
        o = 'OK host:\'%s\' added to pear list!' % host
    else:
        d = dbm.open('%s/rev' % __git_base__)
        o = '%s' % eval('{' + ', '.join(['%s:%s' % (x, d[x])  for x in d.keys()]) + '}')
        d.close()
    start_response('200 OK', [('Content-type', 'text/plain') ])
    return [o]

def application(environ, start_response):
    """ WSGI Web application """
    s, mime, o, myu, host, fname = urllib.parse.unquote(environ['QUERY_STRING']), 'text/plain; charset=utf-8', 'Error!', u(), environ['SERVER_NAME'], 'u.py'
    act, mod, lng, way, gid, arg, rev = None, None, None, None, None, None, None
    log(s, environ['REMOTE_ADDR'])
    if reg(re.match(r'\s*(%s$|=.*$|)(_?)(%s|)(([@\^\$!])(.+)|&?(.+|))\s*$' % ('$|'.join(__ACTIONS__), '|'.join(__OUT_LANG__)), s, re.I)):
        act, mod, lng, way, gid, arg = reg.v.group(1), reg.v.group(2), reg.v.group(3), reg.v.group(5), reg.v.group(6), reg.v.group(7)
    if gid and reg(re.match(r'([^~]+)~([a-fA-F\d]{6,20})$', gid)):
        gid, rev = reg.v.group(1), reg.v.group(2)
    o = 'act:"%s" mod:"%s" lng:"%s" way:"%s" gid:"%s" arg:"%s"\n' % (act, mod, lng, way, gid, arg)
    if act == 'bug': return bug(environ, start_response)
    if act == 'pi':  return pi(environ, start_response)
    if act and act[0] == '=':  return p2p_host(environ, start_response, act[1:])
    if act:
        return action(environ, start_response, act.lower(), host)
    elif gid:
        if way == '^':
            return save(environ, start_response, gid)
        elif way == '$':
            return display(environ, start_response, gid, rev)
        elif way == '@':
            if lng:
                arg = gitu().cat_rev(gid, rev) if rev else gitu().cat(gid)
            else:
                #log('ide')
                return ide(environ, start_response, gid, rev)
        elif way == '!':
            return rm(environ, start_response, gid)
        else:
            pass
    if arg:
        if lng:
            if lng in ('xml', 'svg', 'simu') and mod: mime = 'application/xhtml+xml; charset=utf-8'
            elif lng == 'tikz' and mod: mime, fname = 'application/pdf', 'u.pdf'
            elif lng == 'python' and mod: mime = 'text/plain; charset=utf-8'
            if lng in ('svg', 'simu') and environ['REQUEST_METHOD'].lower() == 'post': 
                raw = environ['wsgi.input'].read().decode('utf-8')
                o = eval('myu.gen_{}(myu.parse(arg), {})'.format(lng, urllib.parse.unquote(raw[2:])))
            else:
                o = eval('myu.headfoot(myu.gen_{}, lng, host)(myu.parse(arg))'.format(lng))
            if lng == 'tikz' and mod: o = tex2pdf(o)
            if lng == 'python' and mod: 
                r, e = subprocess.Popen(('python3'), stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE).communicate(input=o.encode('utf-8'))
                o = e.decode('utf-8') if e else r.decode('utf-8')
        else:
            o = myu.headfoot(myu.gen_ast, 'python', host)(myu.parse(arg))
        if lng != 'tikz' or not mod:
            o = o.encode('utf-8')
    else:
        o = open(__file__, 'r', encoding='utf-8').read().encode('utf-8')
        start_response('200 OK', [('Content-type', 'text/plain')])
        #start_response('206 Partial Content', [('Content-type', 'text/plain'), ('Accept-Ranges','bytes'), ('Content-Length','20'), ('Content-Range','bytes 5-25')])
        #return ['toto tonton\ntata %s' % environ['QUERY_STRING']] 
        return ['toto tonton\ntata %s' % environ] 
    start_response('200 OK', [('Content-type', mime), ('Content-Disposition', 'filename={}'.format(fname))])
    return [o] 

# (7) Command line

def put(server, service, content):
    "_"
    hh = http.client.HTTPConnection(server)
    hh.request('PUT', service, content)
    return hh.getresponse().read().decode('utf-8')

def command_line():
    " Command line"
    import getopt
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
        data = str(open(arg).read()) if os.path.isfile(arg) else arg
        if host in ('localhost', '127.0.0.1'):
            myu = u()
            if lang:
                o = eval('myu.headfoot(myu.gen_{}, lang, host)(myu.parse(data))'.format(lang))
            else:
                if arg:
                    o = myu.headfoot(myu.gen_ast, 'python', host)(myu.parse(data))
                else:
                    o = open(__file__, 'r', encoding='utf-8').read()
        else:
            o = put(host, '/u?%s' % lang, data)
        print (o)

def do_pdf_4test(f, s1, s2='', s3='', s4=''):
    """%PDF-1.7\n1 0 obj<</F1 2 0 R>>endobj\n2 0 obj<</BaseFont/Helvetica/Encoding/WinAnsiEncoding/Name/F1/Subtype/Type1/Type/Font>>endobj\n3 0 obj<</Contents 6 0 R/MediaBox[0 0 421 298]/Parent 5 0 R/Resources<</Font 1 0 R/ProcSet[]>>/Type/Page>>endobj\n4 0 obj<</PageMode/UseNone/Pages 5 0 R>>endobj\n5 0 obj<</Count 1/Kids[3 0 R]/Type/Pages>>endobj\n6 0 obj<</Filter[/FlateDecode]"""
    cod = zlib.compress('BT /F1 16 Tf ET\r\nBT 300 270 Td (%s) Tj ET\r\nBT /F1 42 Tf ET\r\nBT 5 180 Td (%18s) Tj ET\r\nBT /F1 12 Tf ET\r\nBT 10 50 Td (%s) Tj ET\nBT /F1 20 Tf ET\r\nBT 100 150 Td (%16s) Tj ET'%(s3,s1,s2,s4))
    o = do_pdf_4test.__doc__ + '/Length %d>>\nstream\n'%len(cod) + cod + 'endstream endobj\n'
    xref = 'xref\r\n0 7\r\n0000000000 65535 f\r\n0000000009 00000 n\r\n0000000036 00000 n\r\n0000000130 00000 n\r\n0000000243 00000 n\r\n0000000290 00000 n\r\n0000000339 00000 n\r\n'
    trailer = 'trailer<</Root 4 0 R/Size 7>>startxref %d\n'%len(o)
    open(f, 'w').write(o + xref + trailer + '%%EOF')    

if __name__ == '__main__':
    command_line()

    

# end

