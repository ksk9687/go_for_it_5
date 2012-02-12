package vc.ksk.elevator
import scala.collection.mutable.Queue
import java.io.BufferedWriter
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

case class LogEntry(id : Int, time : EType.Time, floor : EType.Floor, move : EState.Value, people : Set[Int]) {
    private val psn = 5;
    private val dummyId = 0;

    override def toString() : String = {
        var ids = people.toList;
        if (ids.size > psn) {
            ids = ids.drop(ids.size - psn);
        } else if (ids.size < psn) {
            for (i <- 1 to psn - (people.size)) {
                ids = dummyId +: ids;
            }
        }

        id + "," + time + "," + floor + "," +
            (move match {
                case EState.Close => "E"
                case EState.Open  => "B"
                case _            => "error"
            }) +
            ids.reverse.foldLeft("") { (x, y) =>
                x + "," + y
            }
    }
}

trait MoveLogger {
    def log(e : Elevator, p : Set[Person]);
}

class SimpleMoveLogger extends MoveLogger {
    val logq = Queue[LogEntry]();

    def init() {
        logq.clear();
    }

    def log(e : Elevator, p : Set[Person]) {
        logq += LogEntry(e.spec.id, e.time, e.at, e.doorState, p.map(_.id));
    }
    def print() {
        write(new { def write(s : String) = println(s); def flush() = Console.flush() });
    }

    def write[T <: { def write(s : String); def flush() }](out : T) {
        for (log <- logq) {
            out.write(log.toString());
        }
        out.flush();
    }

    @tailrec
    final def read(readLine : => String) : Unit = {
        readLine match {
            case null =>
            case line => {
                val array = line.split(",");
                var people = Set[Int]();
                for (i <- 4 to 8) {
                    if (array(i).toInt != 0) {
                        people += array(i).toInt;
                    }
                }
                logq += LogEntry(array(0).toInt, array(1).toInt, array(2).toInt, if (array(3) == "B") EState.Open else EState.Close, people); ;
                read(readLine)
            }
        }
    }
}

class InvalidScheduleException(msg : String) extends Exception(msg);

/*
 * validator
 */
object MoveValidator {
    import collection.mutable.Map;
    object rState extends Enumeration {
        val Wait, Ride, Served = Value
    }

    def validate(request : ArrayBuffer[Person], log : Queue[LogEntry], specs : collection.immutable.Map[Int, ElevatorSpec]) : Int = {
        var time = 0;
        //state for each request
        val reqState = {
            var m = Map[Int, rState.Value]();
            for (r <- request) {
                m(r.id) = rState.Wait;
            }
            m
        }
        //request time
        val reqTime = {
            var m = Map[Int, Int]();
            for (r <- request) {
                m(r.id) = r.at;
            }
            m
        }

        //state of elevators
        val elevState = {
            specs.foldLeft(Map[Int, LogEntry]()) {
                (k, s) =>
                    k + (s._1 -> LogEntry(s._2.id, s._2.initTime, s._2.initFloor, s._2.initState, Set[Int]()))
            }
        }
        //people in elevators' cage
        var cage = {
            specs.map(s => (s._1 -> Set[Int]()));
        }

        //check elevators' move
        for (move <- log) {
            if (move.move == elevState(move.id).move) {
                throw new InvalidScheduleException("have to Close after Open the door");
            }
            if (elevState(move.id).move == EState.Open) {
                //Close
                if (elevState(move.id).floor != move.floor) {
                    throw new InvalidScheduleException("have to Close the door before move");
                }
                if (elevState(move.id).time + specs(move.id).openToClose > move.time) {
                    throw new InvalidScheduleException("have to wait " + specs(move.id).openToClose + " before Close the door");
                }
                if (cage(move.id).size + move.people.size > specs(move.id).capacity) {
                    throw new InvalidScheduleException("over capacity");
                }
                for (p <- move.people) {
                    reqState(p) = rState.Ride;
                }
                cage = cage.updated(move.id, cage(move.id) ++ move.people);
                elevState.update(move.id, move);
            } else {
                //Open
                if (elevState(move.id).floor < move.floor) {
                    if ((move.floor - elevState(move.id).floor) * specs(move.id).up + elevState(move.id).time > move.time) {
                        throw new InvalidScheduleException("illegal move speed");
                    }
                }
                if (elevState(move.id).floor > move.floor) {
                    if ((elevState(move.id).floor - move.floor) * specs(move.id).down + elevState(move.id).time > move.time) {
                        throw new InvalidScheduleException("illegal move speed");
                    }
                }
                if (!move.people.subsetOf(cage(move.id))) {
                    throw new InvalidScheduleException("get out not ridden people:" + (move.people -- cage(move.id)));
                }
                for (p <- move.people) {
                    //change state
                    reqState(p) = rState.Served;
                    time += move.time - reqTime(p);
                }
                cage = cage.updated(move.id, cage(move.id) -- move.people);
                elevState.update(move.id, move);
            }
        }
        //check whether all requests are served
        for (r <- reqState) {
            if (r._2 != rState.Served) {
                throw new InvalidScheduleException("Unserved request:" + r._1);
            }
        }
        return time;
    }
}