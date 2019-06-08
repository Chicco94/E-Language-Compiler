var start :int := 0;
var end :int := 20;

def main():void{
    var i :int :=0;
    var j :int :=5;
    for i in start..end{
        switch (i+1){
            match 4 {
                j-=1;
            }
            match 8{
                j*=3;
            }
            match _{
                j%%=10;
            }
        } 
    }
}