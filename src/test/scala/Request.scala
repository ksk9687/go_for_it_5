import org.scalatest.FunSuite
import vc.ksk.elevator.RequestReader
import vc.ksk.util.Loan._
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.StringReader
import org.apache.commons.io._
import vc.ksk.util.Reader
import scala.collection.mutable.ArrayBuffer
import vc.ksk.elevator.Person

class Request extends FunSuite {

    test("Request reader should read request from BufferedReader") {
        val sr = new StringReader(
                "1,10,1,2\n" + 
                "2,20,6,8\n");
        val request = RequestReader.read(new BufferedReader(sr));
        assert(request.length == 2);
        assert(request(0).id == 1);
        assert(request(0).at == 10);
        assert(request(0).from == 1);
        assert(request(0).to == 2);
        assert(request(1).id == 2);
        assert(request(1).at == 20);
        assert(request(1).from == 6);
        assert(request(1).to == 8);
    }
    test("Request reader should read request from csv file") {
        var request = ArrayBuffer[Person]();
        using(getClass().getClassLoader().getResourceAsStream("input_i.csv")) { is =>
            using(new BufferedReader(new InputStreamReader(is))) { br =>
                request = RequestReader.read(br);
            }
        }
        
        using(getClass().getClassLoader().getResourceAsStream("input_i.csv")) { is =>
            using(new BufferedReader(new InputStreamReader(is))) { br =>
               val request2 = RequestReader.read(new BufferedReader(new StringReader(Reader.readAll(br))));
               assert(request == request2)
            }
        }
    }

    test("Request reader should read request from stdin") {
        using(getClass().getClassLoader().getResourceAsStream("input_i.csv")) { is =>
            Console.withIn(is) {
                val request = RequestReader.read();
                assert(request.length == 10);
            }
        }
    }
}