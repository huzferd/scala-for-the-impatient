package chap

object Chap02 {
  // 1. The signum of a number is 1 if the number is positive, –1 if it is negative, and 0 if it is zero.
  //Write a function that computes this value.
  
  def sgn(x: Int): Int = { if (x < 0) -1 else if(x > 0) 1 else 0 } // sgn: (x: Int)Int
  sgn(3)      // res0: Int = 1
  sgn(0)      // res1: Int = 0
  sgn(-2)     // res1: Int = 0

  // 2. What is the value of an empty block expression {}? What is its type?
  val value = {} // value: Unit = ()

  // 3. Come up with one situation where the assignment x = y = 1 is valid in Scala. 
  // (Hint: Pick a suitable type for x.)
  var y: Int = 0      // y: Int = 0
  val x: Unit = y = 1 // x: Unit = ()
  
  // 4. Write a Scala equivalent for the Java loop
  // for (int i = 10; i >= 0; i--) System.out.println(i);
  for(i <- 10 to (0, -1)) println(i)
  
  // 5. Write a procedure countdown(n: Int) that prints the numbers from n to 0.
  def countdown(n: Int) { 
    for(i <- n to (0, -1)) println(i) 
  }
  countdown(10)
  
  // 6. Write a for loop for computing the product of the Unicode codes of all letters in a string. 
  // For example, the product of the characters in "Hello" is 9415087488L.
  def toUnicode(s: String): BigInt = { 
    var ch: BigInt = 1;
    for(i <- s) ch *= i;
    ch
  }
  toUnicode("Hello") // res0: BigInt = 9415087488

  // 7. Solve the preceding exercise without writing a loop. (Hint: Look at the StringOps Scaladoc.)
  "Hello".foldLeft(1: BigInt)((x, y) => x * y); // res1: BigInt = 9415087488

  // 8. Write a function product(s : String) that computes the product, as described in the preceding exercises.
  def product(s: String) {
    s.foldLeft(1: BigInt)((x, y) => x * y);
  }  
  product("Hello") // res1: BigInt = 9415087488

  // 9. Make the function of the preceding exercise a recursive function.
  def productRecursive(s: String): BigInt = { 
    if(s.length == 0) 1 
    else s.head * productRecursive(s.tail) 
  }

  // 10. Write a function that computes xn, where n is an integer. Use the following recursive
  // definition:
  // • x^n = y · y if n is even and positive, where y = xn / 2.
  // • x^n = x · x^n – 1 if n is odd and positive.
  // • x^0 = 1.
  // • x^n = 1 / x–n if n is negative.
  // Don’t use a return statement.

  def pow(x: Double, n: Int): Double = { 
    if(n == 0) 1 
    else if(x < 0) 1 / pow(x, -n) 
    else if(n % 2 == 0) {
      val i = pow(x, n / 2); 
      i * i;
    } 
    else { 
      x * pow(x, n - 1) 
    }
  }
  pow(5, 10) // res4: Double = 9765625.0
}