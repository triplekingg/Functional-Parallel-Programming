object Roman {
     def toRoman(n: Int): String = {
       val numbers = List(1000,900,500,400,100,90,50,40,10,9,5,4,1)
       val roman_nums = List("M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I")
       def get(num: Int, index: Int): String = {
         if(index + 1 > numbers.length){""
         }
         else{
           if(num>=numbers(index)) {roman_nums(index) + get(num-numbers(index),index)}
           else {get(num,index+1)}
         }
       }
       get(n,0)
     }
    def main(args: Array[String]): Unit = {
		println(toRoman(1029))
    }
}
