package vc.ksk.elevator
import java.io._
import scala.collection.mutable.ArrayBuffer
import vc.ksk.util.Loan._
import scala.annotation.tailrec


/*
 * request reader
 * request is described by ArrayBuffer[Person]
 */
object RequestReader {
    def read() : ArrayBuffer[Person] = {
        read(readLine)
    }

    def read(reader : BufferedReader) : ArrayBuffer[Person] = {
        read(reader.readLine)
    }

    @tailrec
    private def read(readLine : => String, buf: ArrayBuffer[Person] = ArrayBuffer[Person]()) : ArrayBuffer[Person] = {
        readLine match {
            case null => buf
            case line => { buf += parseLine(line) ; read(readLine, buf)}
        }
    }

    private def parseLine(line : String) : Person = {
        val array = line.split(",");
        Person(array(0).toInt, array(2).toInt, array(3).toInt, array(1).toInt)
    }
}