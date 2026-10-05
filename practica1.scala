object practica1 {
    def main (args: Array[String]): Unit = {
        println(decimalBinario(7))
        println(mayor(123456789))
        println(pares(123456789))
        println(pares2(123456789))
        println(contarMayusculas("Hola Mundo"))
        println(contarCambioParidad(13579))
        println(segundoMayor(123456789))
        println(cadenaCaracteresMasLarga("aaaaabbbccddeeefffggg"))
    }

    def decimalBinario(num: Int, acc: String = ""): String = {
        if num<= 0 then acc
        else decimalBinario(num/2, num%2 + acc)
    }
    
    def mayor(num: Int, acc: Int = 0): Int = {
        if num <= 0 then acc
        else mayor(num/10, if (num%10 > acc) num%10 else acc)
    }

    def pares(num: Int, acc: Int = 0): Int = {
        if num <= 0 then acc
        else if (num%10)%2 == 0 then pares(num/10, acc + 1)
        else pares(num/10, acc)
    }

    def pares2(num: Int, acc: String = ""): String = {
        if num <= 0 then acc
        else if (num%10)%2 == 0 then pares2(num/10, (num%10).toString + acc)
        else pares2(num/10, acc)
    }

    def contarMayusculas(palabra: String, acc: Int = 0): Int = {
        if palabra.isEmpty then acc
        else if palabra.head.isUpper then contarMayusculas(palabra.tail, acc + 1)
        else contarMayusculas(palabra.tail, acc)
    }

    def contarCambioParidad(num: Int, acc: Int = 0): Int = {
        if num == 0 then acc - 1
        else contarCambioParidad(num/10, if (num%10)%2 != (num/10)%10%2 then acc + 1 else acc)
    }

    def segundoMayor(num: Int, acc: Int = 0, mayor: Int = 0): Int = {
        if num <= 0 then acc
        else if (num%10 > mayor) then segundoMayor(num/10, mayor, num%10)
        else if (num%10 > acc && num%10 < mayor) then segundoMayor(num/10, num%10, mayor)
        else segundoMayor(num/10, acc, mayor)
    }

    def cadenaCaracteresMasLarga(cadena: String, acc: Int = 0): Int = {
        if cadena.isEmpty then acc
        else if cadena.length == 1 then acc + 1
        else if cadena.head == cadena.tail.head then cadenaCaracteresMasLarga(cadena.tail, acc + 1)
        else cadenaCaracteresMasLarga(cadena.tail, 0)
    }
}