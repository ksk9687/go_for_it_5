import org.scalatest.FunSuite
import vc.ksk.elevator._
import vc.ksk.util.Loan._
import scala.collection.mutable.ArrayBuffer
import java.io.StringReader
import java.io.BufferedReader


class SchedulerTest extends FunSuite {

    test("SimpleScheduler should schedule based on first-come-first-served") {
        val logger : SimpleMoveLogger = new SimpleMoveLogger()
        val elevator = new Elevator(new DefaultSpec(), logger);
        val sr = new StringReader(
                "1,10,1,2\n" + 
                "2,20,6,8\n");
        val request = RequestReader.read(new BufferedReader(sr));
        val scheduler = new SimpleScheduler(Vector(elevator), request);
        scheduler.schedule();
        val t = MoveValidator.validate(request, logger.logq, Map((elevator.spec.id, elevator.spec)));
        assert(t == 2 + 14)
    }

    test("SimpleScheduler should valid schedule") {
        val logger : SimpleMoveLogger = new SimpleMoveLogger()
        val elevator = new Elevator(new DefaultSpec(), logger);
        var request = ArrayBuffer[Person]();
        using(getClass().getClassLoader().getResourceAsStream("input_i.csv")) { is =>
            Console.withIn(is) {
                request = RequestReader.read();
            }
        }

        val scheduler = new SimpleScheduler(Vector(elevator), request);
        scheduler.schedule();
        val t = MoveValidator.validate(request, logger.logq, Map((elevator.spec.id, elevator.spec)));
    }
    
    test("LOCKScheduler shuld schedule elevator"){
        val logger : SimpleMoveLogger = new SimpleMoveLogger()
        val elevator = new Elevator(new DefaultSpec{override val capacity = 5}, logger);
        var request = ArrayBuffer[Person]();
        using(getClass().getClassLoader().getResourceAsStream("input_iii_iv.csv")) { is =>
            Console.withIn(is) {
                request = RequestReader.read();
            }
        }

        val scheduler = new LOCKScheduler(Vector(elevator), request);
        scheduler.schedule();
        val t = MoveValidator.validate(request, logger.logq, Map((elevator.spec.id, elevator.spec)));
    }
    

}