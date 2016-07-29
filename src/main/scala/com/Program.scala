package com

object Program {
    case class Element(name: String, symbol: String)
    case class SplurthianTestCase(element: Element, result: Boolean)

    def main(args: Array[String]): Unit = {
        val examples: List[SplurthianTestCase] =
            List(
                SplurthianTestCase(Element("Spenglerium", "Ee"), result=true),
                SplurthianTestCase(Element("Zeddemorium", "Zr"), result=true),
                SplurthianTestCase(Element("Venkmine", "Kn"), result=true),
                SplurthianTestCase(Element("Stantzon", "Zt"), result=false),
                SplurthianTestCase(Element("Melintzum", "Nn"), result=false),
                SplurthianTestCase(Element("Tullium", "Ty"), result=false))

        println("Does it check out?")

        val result = checkExamples(examples)
        println(s"The answer is: $result")
    }

    def isValidElementName(element: Element): Boolean = {
        val name = element.name.toLowerCase
        if (element.symbol.length != 2) false
        else {
            val (first, second) = element.symbol.toLowerCase().splitAt(1)
            val indexFirst = name.indexOf(first)
            val indexSecond = name.indexOf(second, indexFirst+1)
            if (indexFirst < indexSecond && indexFirst >=0 && indexSecond >= 1) true
            else false
        }
    }

    def checkExample(example: SplurthianTestCase): Boolean = {
        isValidElementName(example.element) == example.result
    }

    def checkExamples(examples: List[SplurthianTestCase]): Boolean = {
        examples.forall(example => checkExample(example))
    }
}

