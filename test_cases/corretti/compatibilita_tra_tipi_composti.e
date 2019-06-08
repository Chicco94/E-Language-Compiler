var i : int := 0;
var j : float := 0;
var p : int* := &i;
var q : float* := &j;
var pp : int** := &p;

def f(ref i: int) : void {
  i := p;
}

def g(ref i: int*) : void {
  i := pp;
}

def h(val i: int*, const j: int) : void {}
f(i);
f(*p);
g(p);
g(*pp);
g(&i);
h(&i, 1);
h(p, 1);

var arr : [2,2] int* := [[p,p],[p,*pp]];
#var mat : <2,2> int* := [[q,p],[p,*pp]]; #q float* != int*