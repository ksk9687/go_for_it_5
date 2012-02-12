package vc.ksk.elevator
import scala.collection.mutable.ArrayBuffer
import java.io.FileReader
import vc.ksk.util.Loan._
import java.io.BufferedReader

object Main {
    def one() {
        val logger : SimpleMoveLogger = new SimpleMoveLogger()
        val elevator = new Elevator(new DefaultSpec(), logger);
        var request = RequestReader.read();
        val scheduler = new SimpleScheduler(Vector(elevator), request);
        scheduler.schedule();
        val t = MoveValidator.validate(request, logger.logq, Map((elevator.spec.id, elevator.spec)));
        logger.print();
    }

    def two(logFile : String) {
        val logger : SimpleMoveLogger = new SimpleMoveLogger();
        var request = RequestReader.read();
        using(new BufferedReader(new FileReader(logFile))) { br =>
            logger.read(br.readLine());
        }
        val t = MoveValidator.validate(request,logger.logq,Map(1 -> new DefaultSpec { override val capacity = 5 }));
        println(t);
    }

    def three() {
        val logger : SimpleMoveLogger = new SimpleMoveLogger()
        val elevator = new Elevator(new DefaultSpec { override val capacity = 5 }, logger);
        var request = RequestReader.read();
        val scheduler = new LOCKScheduler(Vector(elevator), request);
        scheduler.schedule();
        val t = MoveValidator.validate(request, logger.logq, Map((elevator.spec.id, elevator.spec)));
        logger.print()
    }

    def main(args : Array[String]) : Unit = {
        if (args.length <= 0) {
        	sys.exit();
        }else if(args(0) == "1"){
            one();
        }else if(args(0) == "2"){
            if(args.length <= 1){
                println("2 outFileName")
                sys.exit();
            }
            two(args(1));
        }else if(args(0) == "3"){
            three();
        }else if(args(0) == "4"){
            println("not implemented yet");
        }

    }

}