# meaningless example
def f (val h :int):float {
  h+=1;
  h+=1;
  return (h + 2);
}

def PP :int := 1;
def KK :int := 3+PP;
var w:int := 5;
var u :[2]int := [w*(KK+1-r),-w];
var r :int := 2;
var a :int := u[r-1];
var mat :[2,2]int :=[[1,2+KK],[3,4]];
var pi :int* := &w;
var x :float := 5+f(r);

def main():void {
  def g (val k :int):int {
    k+=1;
    k+=1;
    return (k); # noi non gestiamo aritmetica tra interi e char
  }
  
  var c :char := 'r';
  
  u[w] := r+g(r);
  mat[1,1] := 0;
  
  if (c>PP) {mat[r,a]+=1;}
  
  # noi non abbiamo fatto il pre e post incremento
  r +=1;
  u[r]+=1;
  r := u[r];
  
  r+=1;
  u[r] += u[r];
  r +=1;

  # noi non abbiamo gestito l'aritmetica dei puntatori
  # r += (*pi)++;
  
  # 'a' == 68 in ascii
  r +=1;
  for w in 68..r {
    r +=1;
    # noi non abbiamo gestito l'aritmetica dei puntatori
    # u[r] += w + r * pi;
    r += u[w];
  }
  
  w += w+1;
}

