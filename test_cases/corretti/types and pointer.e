var a :int           := 0+1;
var b :bool          := true;
var c :char          := '0';
var f :float         := 0.0;
var s :string        := "0.0";

var pa :int*         := &a;
var pb :bool*        := &b;
var pc :char*        := &c;
var pf :float*       := &f;
var ps :string*      := &s;

var aa :[2,2]int       := [[a,a],[a,a]];
var ab :[2,2]bool      := [[b,b],[b,b]];
var ac :[2,2]char      := [[c,c],[c,c]];
var af :[2,2]float     := [[f,f],[f,f]];
var as :[2,2]string    := [[s,s],[s,s]];

var ppa :int * *       := &pa;
var ppb :bool * *      := &pb;
var ppc :char * *      := &pc;
var ppf :float * *     := &pf;
var pps :string * *    := &ps;

var paa :[2,2]int*     := &aa;
var pab :[2,2]bool*    := &ab;
var pac :[2,2]char*    := &ac;
var paf :[2,2]float*   := &af;
var pas :[2,2]string*  := &as;

var ppaa :[2,2]int * *   := &paa;
var ppab :[2,2]bool * *  := &pab;
var ppac :[2,2]char * *  := &pac;
var ppaf :[2,2]float * * := &paf;
var ppas :[2,2]string * *:= &pas;

var pppa :int * * *     := &ppa;
var pppb :bool * * *    := &ppb;
var pppc :char * * *    := &ppc;
var pppf :float * * *   := &ppf;
var ppps :string * * *  := &pps;

var pppa :[2,2]int * * *     := &ppaa;
var pppb :[2,2]bool * * *    := &ppab;
var pppc :[2,2]char * * *    := &ppac;
var pppf :[2,2]float * * *   := &ppaf;
var ppps :[2,2]string * * *  := &ppas;