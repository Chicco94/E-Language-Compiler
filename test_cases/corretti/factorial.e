def factorial(n :int ) :int{
    if (n == 1) {
        return (1);
    } else { if (n > 0) {
            return (n * factorial(n-1));
        } else {
            writeString("invalid argument to factorial"); 
            return (-1);      
        }
    }
}

def main() :void{
    def start_test :int := 0;
    def end_test :int := 10;
    def test_values :<10>int := [1,22,41,91,56,87,28,15,37,60];
    var i :int := 0;
    var result :int := 0;
    for i in start_test..end_test{
        result := factorial(test_values[i]);
        if (result == -1){
            break;
        }
    }
}
