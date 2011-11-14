#!/usr/bin/python
# -*- coding: utf-8 -*-

# Welcome to ⊓ &#2293; (SquareCup) 
# see https://github/pelinquin/u

#-----------------------------------------------------------------------------
#  © Copyright 2011 Rockwell Collins, Inc 
#    This file is part of U (SquareCup).
#
#    U (SquareCap) is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    U (SquareCap) is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with U (SquareCup).  If not, see <http://www.gnu.org/licenses/>.
#-----------------------------------------------------------------------------

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
__version__ = '0.1'
__license__ = 'GPLv3'

import os,sys,re,unicodedata,hashlib,shutil,subprocess,urllib

__RE_U__ = r'''                # RegExp with 10 groups
   (?:                            # Token is NODE:
    (?=(?:[^\W\d_]|:[^\W\d_]|[\"\'])) #  check not empty 
    (?:([^\W\d_]\w*)|)            #  Name      G1
    (?:[\"\']([^\"]*)[\"\'])?             #  Label     G2
    (?:\.(\w+))?                  #  Port      G3
    (?::([^\W\d_]\w*)|)           #  Type      G4
    (?:\(([^\)]*)\))?             #  Arguments G5
   )|(?:                          # Or EDGE:
    ([\-<>])                      #  Head      G6
    (?:\"([^\"]*)\")?             #  Label     G7
    (?:([^\W\d_]\w*)|)            #  Type      G8
    (?:\(([^\)]*)\))?             #  Arguments G9
    ([\-<>])                      #  Tail      G10
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

_XHTMLNS  = 'xmlns="http://www.w3.org/1999/xhtml"'
_SVGNS    = 'xmlns="http://www.w3.org/2000/svg"'
_XLINKNS  = 'xmlns:xlink="http://www.w3.org/1999/xlink"'

__OUT_LANG__ = {'c'     :'c',
                'python':'py',
                'ada'   :'ada',
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

__CODE_GEN_SET__ = {
    ('Simple','#simple dot diagram\nA->B'):
        ({'A': (), 'B': ()}, [('A', '->', 'B')]),
    ('Class','A"my_classA":class -I> B"my_classB":class'):    
        ({'A': ('my_classA', 'class'), 'B': ('my_classB', 'class')}, [('A', '->', 'B', None, 'I')]),
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

def parse(x,r=False):
    r""" Here is a non-regression test set:
    >>> parse('A -> B')    
    ({'A': (), 'B': ()}, [('A', '->', 'B')])
    >>> parse('A"my_classA":class -I> B"my_classB":class')    
    ({'A': ('my_classA', 'class'), 'B': ('my_classB', 'class')}, [('A', '->', 'B', None, 'I')])
    """
    Nodes,Edges,kids,nid,oid,npo,opo,nid,moid,c = {},[],{},None,None,'','',[],[],[]
    if type(x).__name__ == 'str': 
        x = eval(reduce(lambda y,k: re.sub(k[0],k[1],y),__RE_FILTER__,x))
    #print '|%s|'%x
    for s in x:
        if type(s).__name__ == 'list':
            n,k,e = parse(s,True)
            mnid = k.keys()
            if nid:
                mnid = [nid]
            if moid and c:
                for i in moid:
                    for j in mnid:
                        Edges.append(strip3((i,c[0]+c[4],j,c[1],c[2],c[3])))
                c = []
            moid = mnid
            if oid and nodes.has_key(oid):
                t = list(Nodes[oid])
                t.append(k.keys())
                Nodes[oid] = tuple(t)
            oid = nid = None
            Nodes.update(n)
            Edges += e
        else:
            nodes,edges = {},[]
            for m in re.compile(__RE_U__,re.U|re.X).finditer(s):
                a = map(lambda k:m.group(k),range(1,11))
                if a[5] and a[9]: # this is an edge
                    c = a[5:10]
                    c1 = a[5:10]
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
    return (Nodes,kids,Edges) if r else (Nodes,Edges)

def gen_readme():
    """
    Welcome to U project, do not hesitate to share your viewpoint and contribute if you wish.\n\n
    This is a 'just one file' project; the full documentation and the source code are a unique Python file; 'u.py'.
    This file includes all you need to understand and play with the project.\n\n
    To have formated documentation, run the 'u.py' file to build 'u.tex' (LaTeX) file, compile and print it.\n
    For your convenience, the 'u.pdf' PDF file is commited.\n\nEnjoy!"""
    digest = hashlib.sha1(open(__file__).read()).hexdigest()
    o = 'Head SHA1 digest: %s\n\n'%digest[:10]
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
        o += r'\begin{lstlisting}[texcl] ' + '\n' + l[1].encode('utf-8') + '\n' + r'\end{lstlisting}' + ' & \n'
        o += gen_tikz(h[l],[],False) + r'\\ \hline' + '\n'
    return o + r'\end{longtable}' + '\n'

def reg(value):
    " function attribute is a way to access matching group in one line test "
    reg.v = value
    return value

######### WEB APPLICATION ###########

def application(environ,start_response):
    """ This is a WSGI Web service for the U language\n
It is recommended to use it in SSL mode (https) 
Any argument is interpreted as a U string and the default output is the Abstract Syntax Tree.
If a language name is given first, then the generated code for this language is in output
If a language name is given but no other arguments (no &), then a file browser is provided to select an input U file.
If the '_' character is given before the language name, then the interpretation of the generated code is in output;
 for 'svg' the graphics is rendered within the browser
 for 'tikz', the pdf reader is called for rendering the graphics
 for 'c', gcc is called to compile the generated code and execute the binary\n
Special keywords:
 'pdf' or 'paper' returns the generated paper on U in pdf format
 'update' is used to update the web application with the last release from Github
 'help','about' or 'usage' display this message.\n
If no argument is given, the Python source code is given for reading or downloading. 
"""
    s,mime,o = urllib.unquote(environ['QUERY_STRING']),'text/plain','Error!'
    if reg(re.match(r'\s*(update$|about$|help$|usage$|pdf$|paper$|)(?:(_?)(%s|raw|ast)(?:&(.*)|)|(.*))\s*$'%'|'.join(__OUT_LANG__),s,re.I)):
        form,action,under,lang,args = False,reg.v.group(1),reg.v.group(2),reg.v.group(3),reg.v.group(5) if reg.v.group(5) else reg.v.group(4)
        if lang: lang = lang.lower()
        if (action,under,lang,args) == ('',None,None,None):
            start_response('200 OK',[('Content-type',mime)])
            return [(open(environ['SCRIPT_FILENAME']).read())] 
        elif action and action.lower() in ('about','help','usage'):
            o = application.__doc__
        elif action and action.lower() in ('paper','pdf'):
            pwd = os.path.dirname(environ['SCRIPT_FILENAME'])
            o = open('%s/u.pdf'%pwd).read()
            mime = 'application/pdf'
        elif action and action.lower() == 'update':
            pwd = os.path.dirname(environ['SCRIPT_FILENAME'])
            #cmd = 'cd %s/..; mv u old_u; git clone git://github.com/pelinquin/u.git; cd u'%pwd
            cmd = 'ls %s'%__file__
            o = subprocess.Popen((cmd), shell=True,stdout=subprocess.PIPE).communicate()[0]
        elif args == None:
            if environ['REQUEST_METHOD'].lower() == 'post':
                raw = environ['wsgi.input'].read(int(environ.get('CONTENT_LENGTH','0')))
                ast = parse('\n'.join(raw.split('\n')[4:-2]))
                if lang in ('ast','raw'):
                    o = '%s %s'%ast
                else: 
                    o = eval('gen_%s(ast)'%lang).encode('utf-8')
            else:
                mime,form,o = 'text/html',True, '<form method=post enctype=multipart/form-data><input type=file name=a onchange="submit();"/></form>'
        elif lang in (None, 'ast','raw'):
            o = '%s %s'%parse(args)
        else:
            ast = parse(args)
            o = eval('gen_%s(ast)'%lang).encode('utf-8')
        if (under == '_') and not form:
            if (lang == 'tikz'):
                o = tex2pdf(o)
                mime = 'application/pdf'
            elif lang == 'svg':
                mime = 'application/xhtml+xml'
    start_response('200 OK',[('Content-type',mime)])
    return [(o)] 

def strip3(z):
    return z[:-3] if not (z[-1] or z[-2] or z[-3]) else z[:-2] if not (z[-1] or z[-2]) else z[:-1] if not z[-1] else z
    
######### CODE GENERATION ###########
def gen_c(ast,m=[]):
    "/* Generated from U AST: */\n"
    o = '/* %s */\n/* %s */\n'%ast
    Nodes = ast[0]
    for x in Nodes:
        if Nodes[x]:
            if len(Nodes[x]) == 2 and Nodes[x][1] == 'class':
                o += '\n/* Class: %s */\n'%Nodes[x][0]
                o += 'typedef struct %s {\n'%x
                o += '  int a;\n'
                o += '} %s;\n'%x
    return gen_c.__doc__ + o + '\n/* end file */\n'

def gen_python(ast,m=[]):
    """#!/usr/bin/python\n# -*- coding: utf-8 -*-\n# Generated from U AST:\n"""
    o = '# %s\n# %s\n'%ast
    return gen_python.__doc__ + o + '\n# end file\n'

def gen_ada(ast,m=[]):
    "-- Generated from U AST:\n"
    o = '-- %s\n-- %s\n'%ast
    return gen_ada.__doc__ + o + '\n-- end file\n'

def gen_scala(ast,m=[]):
    "// Generated from U AST:\n"
    o = '// %s\n// %s\n'%ast
    return gen_scala.__doc__ + o + '\n// end file\n'

def gen_java(ast,m=[]):
    "// Generated from U AST:\n"
    o = '// %s\n// %s\n'%ast
    return gen_java.__doc__ + o + '\n// end file\n'

def gen_ruby(ast,m=[]):
    "# Generated from U AST:\n"
    o = '# %s\n# %s\n'%ast
    return gen_ruby.__doc__ + o + '\n# end file\n'

def gen_ocaml(ast,m=[]):
    "(* Generated from U AST: *)\n"
    o = '(* %s *)\n(* %s *)\n'%ast
    return gen_ocaml.__doc__ + o + '\n(* end file *)\n'

def gen_lua(ast,m=[]):
    "-- Generated from U AST:\n"
    o = '-- %s\n-- %s\n'%ast
    return gen_lua.__doc__ + o + '\n-- end file\n'

def layout(nodes,edges,rankdir='TB'):
    "computes layout for graphics (tikz and svg) generation"
    bbx,bby,pos,d = None,None,{},'digraph G { rankdir=%s '%rankdir
    for n in nodes:
        label = n.encode('utf-8') if not n[0] else n[0]
        d+= ' %s[label="%s"];'%(n.encode('utf-8'),label)
    for e in edges:
        d+= ' %s->%s'%(e[0].encode('utf-8'),e[2].encode('utf-8'))
    p = subprocess.Popen(['dot'], stdout=subprocess.PIPE, stdin=subprocess.PIPE)
    for l in p.communicate(input=d + '}')[0].split('\n'):
        if reg(re.search('bb="0,0,(\d+),(\d+)"',l)):
            bbx,bby = float(reg.v.group(1)),float(reg.v.group(2))
        elif reg(re.search('^\s*(\w+)\s*\[label=[^,]*, pos="(\d+),(\d+)"',l)) and bbx and bby:
            pos[reg.v.group(1)] = (int(reg.v.group(2))*100/bbx,int(reg.v.group(3))*100/bby)
    return pos

def gen_tikz_header():
    r'''\usetikzlibrary{shapes,fit,arrows,shadows,backgrounds}
    \tikzstyle{inherit}=[->,>=triangle 60 reversed]
    \tikzset{oplus/.style={draw,circle,inner sep=10pt,path pp={ \draw[black] (path pp bounding box.south) -- (path pp bounding box.north) (path pp bounding box.west) -- (path pp bounding box.east);}}} 
    \tikzstyle{rect}=[rectangle,drop shadow,draw=green!40,fill=gray!20]
    \tikzstyle{block}=[rectangle,drop shadow,rounded corners=3pt,draw=red!40,fill=blue!25]'''
    return gen_tikz_header.__doc__ + '\n'

def gen_tikz(ast,m=[],standalone=True):
    "% Generated from U AST:\n"
    o = ''
    if standalone:
        o += r'\documentclass[a4paper]{article} \usepackage{tikz}' + '\n'
        o += r'\begin{document}' + '\n'
    pos,ratio = layout(ast[0],ast[1]),4
    o += '%% %s\n%% %s\n'%ast 
    o += gen_tikz_header()
    o += r'\begin{tikzpicture}[auto,node distance=15mm,semithick]'+ '\n'
    for n in pos:
        name = n.encode('utf-8')
        label = name 
        shape = 'rect'
        (x,y) = (pos[n][0]/25,pos[n][1]/25)
        o += r'\node[%s](%s) at (%0.3f,%0.3f) {%s};'%(shape,name,x,y,label) + '\n' 
    Edges = ast[1]
    for e in Edges:
        typ,boucle = 'inherit','[bend left]'
        label = '' if len(e)<4 else 'node{%s}'%e[3]
        o += r'\draw[%s](%s) to%s %s(%s);'%(typ,e[0],boucle,label,e[2]) + '\n'
    o += r'\end{tikzpicture}'+ '\n'
    if standalone:
        o +=  r'\end{document}'
    return gen_tikz.__doc__  + o + '\n% end file\n'

def include_js():
    r"""
    if (typeof($)=='undefined') { function $(id) { return document.getElementById(id.replace(/^#/,'')); } }
    var nodeBox  = [];
    window.onload = function () { 
      var t = $('.nodes').childNodes;
      for (var n = 0; n < t.length; n++) {
        if (t[n].nodeName == 'g') { nodeBox[t[n].id] = t[n].firstChild.getBBox(); } 
      }
      for (var n = 0; n < t.length; n++) {
        if (t[n].nodeName == 'g') {
          var m = 8;
          var b = nodeBox[t[n].id];
          var shape = t[n].firstChild.nextSibling;
          shape.setAttribute('x',b.x-m);
          shape.setAttribute('y',b.y-m);
          shape.setAttribute('width',b.width+2*m);
          shape.setAttribute('height',b.height+2*m);
        }
      }
      var t = $('.connectors').childNodes;
      for ( var n = 0; n < t.length; n++ ) {
        if (t[n].nodeName == 'g') {
          var b = nodeBox[t[n].getAttribute('n1')];
          var c = nodeBox[t[n].getAttribute('n2')];
          var d = 'M ' + (b.x+b.width/2) + ' ' + (b.y+b.height/2) + ' L ' + (c.x+c.width/2) + ' ' + (c.y+c.height/2);
          t[n].firstChild.setAttribute('d',d);
        }
      }
    }"""
    o = '<script %s type="text/ecmascript">\n/*<![CDATA[*//*---->*/\n'%_XLINKNS
    return o + include_js.__doc__  + '\n/*--*//*]]>*/</script>\n'

def gen_svg(ast,m=[]):
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!-- Generated from U AST: -->\n"
    pos,ratio = layout(ast[0],ast[1]),4
    o = '<!-- dash replaced by underscore\n' + re.sub(r'\-','_','%s\n%s'%ast) + '\n-->\n'
    o += '<svg %s>\n'%_SVGNS
    o += '<title id=".title">%s</title>\n'%__title__
    #o += '<link %s rel="shortcut icon" href="./logo16.png"/>\n'%(_XHTMLNS,pfx)
    #o += '<script %s type="text/ecmascript" xlink:href="%s/%s"></script>\n'%(_XLINKNS,pfx,__JS__)
    o += include_js()
    o += '<g id=".nodes">\n'
    for x in pos:
        label = x.encode('utf-8')
        if ast[0][x]:
            if ast[0][x][0]:
                label = ast[0][x][0].encode('utf-8')
        o += '<g id="%s">'%x
        o += '<text fill="red" x="%s" y="%s">%s</text>'%(pos[x][0]*ratio,pos[x][1]*ratio,label)
        o += '<rect style="fill-opacity:0.2" rx="5"/>'
        o += '</g>\n'
    o += '</g>\n'
    Edges = ast[1]
    o += '<g id=".connectors" >\n'
    for e in Edges:
        o += '<g n1="%s" n2="%s"><path fill="none" stroke="green"/></g>\n'%(e[0],e[2])
    o += '</g>\n'
    return gen_svg.__doc__ + o + '\n</svg>\n<!-- end file -->\n'

def gen_aadl(ast,m=[]):
    "-- Generated from U AST:\n"
    o = '-- %s\n-- %s\n'%ast
    return gen_aadl.__doc__ + o + '\n-- end file\n'

def gen_sdl(ast,m=[]):
    "# Generated from U AST:\n"
    o = '# %s\n# %s\n'%ast
    return gen_sdl.__doc__ + o + '\n# end file\n'

def gen_lustre(ast,m=[]):
    "-- Generated from U AST:\n"
    o = '-- %s\n-- %s\n'%ast
    return gen_lustre.__doc__ + o + '\n-- end file\n'

################# MAIN ################

def code_gen_test(ref=False):
    """ These test are published in the PDF paper and generates coding files for all output languages
    For non regression on AST, better see doctest in the header of the parse function
    ref=True  -> build the reference files
    ref=False -> compare computed files with reference files
    """
    for case in __CODE_GEN_SET__:
        ast = parse(case[1])
        if ast != __CODE_GEN_SET__[case]: print '|%s|'%case[0],ast
        assert ast == __CODE_GEN_SET__[case]   
        for l in __OUT_LANG__:
            if not os.path.isdir(l): os.mkdir(l)   
            refname,cmpname = '%s/%s_ref.%s'%(l,case[0],__OUT_LANG__[l]),'%s/%s.%s'%(l,case[0],__OUT_LANG__[l])
            if ref:
                open(refname,'w').write(eval('gen_%s(ast)'%l).encode('utf-8'))
            else:
                open(cmpname,'w').write(eval('gen_%s(ast)'%l).encode('utf-8'))    
                r = subprocess.Popen(('diff',refname,cmpname),stdout=subprocess.PIPE).communicate()[0].strip()
                if not re.match('^\s*$',r): print refname,r
                assert re.match('^\s*$',r)

def ast_test(ref=False):
    """ """
    n,h = 0,' {\n'
    for i in __AST_SET__:
        n +=1
        i = re.sub(r'\n','\\\\n',i)
        h += '\t# %02d\n\t\'%s\':\n\t\t%s,\n'%(n,i,parse(i))
    h += '\n}'
    ast_hash = eval(h)
    if ref:
        open('test_ref.txt','w').write(h)   
    else:
        if os.path.isfile('test_ref.txt'):
            content = open('test_ref.txt').read()
            assert content == h

if __name__ == '__main__':
    "Run the module or use it as WSGI application with an Apache server"
    import doctest
    doctest.testmod()
    code_gen_test(True)
    ast_test(True)
    gen_apache_conf()
    gen_doc()
    gen_readme()

    # debug!
    #print parse('A->B')
    #import antigravity
    print u"\u2293\u2294"
# the end
