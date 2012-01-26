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

"""
This is a module to manage Git version management tool
There is no user defined file tree, only a Git database.
Blob ids are generated and managed by a small berkeley database
"""

import os,re,hashlib,base64,dbhash,subprocess

__version__='0.1'
__BASE__='/u'
__digest__ = base64.urlsafe_b64encode(hashlib.sha1(open(__file__).read()).digest())[:5]

def register(content=''):
    """ If the same content is requested, then id does not change """
    base = __BASE__
    if not os.path.isdir(base):
        os.mkdir(base)
    rev = dbhash.open('%s/rev.db'%base,'c')
    if rev.has_key(content):
        gid = rev[content]
    else:
        gid = create_id(rev)
        rev[content] = gid
    rev.close()    
    return gid

def print_db(db):
    """
    """
    d = dbhash.open(db, 'r')
    for item in d.keys():
        print '[%s] %s -> %s' % (db,item,d[item])
    d.close()

def create_id(rev):
    """ Create a new diagram id"""
    rev['_'] = '%d'%(long(rev['_'])+1) if rev.has_key('_') else '0'
    return base64.urlsafe_b64encode(hashlib.sha1(rev['_']).digest())[:-18]

class gitu:
    """ All git methods share the same env """
    def __init__(self,user='anybody',ip='0.0.0.0'):
        """ create the GIT repository if needed"""
        if not os.path.isdir(__BASE__):
            os.mkdir(__BASE__)
        e = os.environ.copy()
        e['GIT_AUTHOR_NAME'],e['GIT_AUTHOR_EMAIL'] = user,ip
        e['GIT_COMMITTER_NAME'],e['GIT_COMMITTER_EMAIL'] = 'laurent','pelinquin@gmail.com'
        e['GIT_DIR'] = '%s/.git'%__BASE__
        self.e = e
        if not os.path.isdir(e['GIT_DIR']):
            subprocess.Popen(('git', 'init','-q'), env=e,close_fds=True).communicate()
            p = subprocess.Popen(('git', 'hash-object','-w','--stdin'), env=e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
            li = '100644 blob %s\tstart\n'%p.communicate(' \n')[0].strip()
            q = subprocess.Popen(('git', 'mktree'), env=e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
            r = subprocess.Popen(('git', 'commit-tree', q.communicate(li)[0].strip()), env=e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
            subprocess.Popen(('git', 'update-ref', 'refs/heads/master',r.communicate('start')[0].strip()), env=e, stdout=subprocess.PIPE).communicate()

    def save(self,key,c,state=''):
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

    def sha(self,content):
        """ """
        p = subprocess.Popen(('git', 'hash-object','-w','--stdin'), env=self.e, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        return p.communicate(content+'\n')[0].strip()

    def commit(self,li,msg):
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

    def history(self,key=''):
        """ """
        if key:
            p = subprocess.Popen(('git', 'log', '--pretty=format:%H:%an:%ar:%s','--',key), env=self.e, stdout=subprocess.PIPE)
        else:
            p = subprocess.Popen(('git', 'log', '--pretty=format:%H:%an:%ar:%s'), env=self.e, stdout=subprocess.PIPE)
        liste = p.communicate()[0].strip()
        return liste.split('\n')

    def gethead(self,key):
        """ """
        p = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%H:%an:%ar:%at','--',key), env=self.e, stdout=subprocess.PIPE) # ar
        return p.communicate()[0].strip()
    
    def revision(self,key):
        """ """
        c = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%H','--', key), env=self.e, stdout=subprocess.PIPE)
        return c.communicate()[0][:15]

    def date(self,key):
        """ """
        c = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%ci','--', key), env=self.e, stdout=subprocess.PIPE)
        return c.communicate()[0][:-5]

    def cat(self,key):
        """ """
        p = subprocess.Popen(('git', 'show', 'master^{tree}:'+key), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate()
        return '[Not Found!]' if err else out[:-1]

    def cat_blob(self,key):
        """ """
        p = subprocess.Popen(('git', 'show', key), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate()
        return '' if err else out[:-1]

    def cat_revision(self,gid):
        """ """
        p = subprocess.Popen(('git', 'show', 'master^{tree}:%s'%gid), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        content, err = p.communicate()
        c = subprocess.Popen(('git', 'log', '-1', '--pretty=format:%H', '--', gid), env=self.e, stdout=subprocess.PIPE)
        rev = c.communicate()[0]
        return ('','[Diagram Not Found cat_revision!]') if err else (rev[:15],content[:-1])

    def cat_getrev(self,rev):
        """ """
        c = subprocess.Popen(('git', 'log', '--pretty=oneline','-1',rev), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out,err = c.communicate()
        idd,cont = ['',''],'[Diagram Not Found!]'
        if not err:
            if out != '':
                idd = out.strip().split()
                p = subprocess.Popen(('git', 'show','%s:%s'%(rev,idd[1])), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                cont = p.communicate()[0][:-1]
        return idd[0][:15],idd[1],cont

    def cat_full(self,key,arev):
        """ """
        c = subprocess.Popen(('git', 'log', '--pretty=format:%H','-1',arev), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out,err = c.communicate()
        rev,cont = '','[Diagram Not Found!]'
        if not err:
            if out != '':
                rev = out.strip()
                p = subprocess.Popen(('git', 'show','%s:%s'%(rev,key)), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                cont = p.communicate()[0][:-1]
        return rev[:15],cont

    def cat_simple(self,key,rev):
        """ """
        p = subprocess.Popen(('git', 'show','%s:%s'%(rev,key)), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        return p.communicate()[0][:-1]

    def test(self,key,rev):
        """ """
        c = subprocess.Popen(('git', 'log', '%s:@%s'%(rev,key)), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        o,e = c.communicate()
        return False if e else True 
    
    def tag(self,name,rev):
        """ """
        c = subprocess.Popen(('git', 'tag', name, rev), env=self.e, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        o,e = c.communicate()
        return e if e else o

    def tag_list(self):
        """ """
        o,e = subprocess.Popen(('git', 'tag'), env=self.e, stdout=subprocess.PIPE).communicate()
        return o

 
if __name__ == '__main__': 
    import sys
    content = 'A->C'
    gid = register(content)
    print_db('%s/rev.db'%__BASE__)
    mygit = gitu()
    rev = mygit.save(gid,content,'message') 
    print 'Saved %s'%rev
    print mygit.cat_simple(gid,rev)
    print mygit.cat(gid)
    print mygit.history(gid)
    print mygit.list()

