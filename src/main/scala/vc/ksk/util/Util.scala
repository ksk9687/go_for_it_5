package vc.ksk.util

object Loan {
    def using[C <: { def close() }, T](h : C)(work : C => T) : T = {
        try {
            work(h);
        } finally {
            h.close();
        }
    }
    implicit def d2c(d : { def dispose() }) : { def close() } = {
        new { def close() = d.dispose() }
    }
}

object Reader {
    def readAll(reader : { def read() : Int }) : String = {
        val s = new StringBuilder();
        var c:Int = 0;
        while ({c = reader.read(); c != -1}) {
            s.append(c.toChar)
        }
        s.toString()
    }
}
