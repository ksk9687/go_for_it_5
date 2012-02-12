package vc.ksk.elevator
import java.util.ArrayList

//State of Elevator
object EState extends Enumeration {
    val Close, Open = Value
}

object EType {
    type Time = Int;
    type Floor = Int;
}

//Specification of a Elevator
abstract class ElevatorSpec {
    val id : Int;

    val capacity : Int;

    val up : EType.Time;
    val down : EType.Time;
    val openToClose : EType.Time;

    val maxFloor : EType.Floor;
    val minFloor : EType.Floor;

    val initState : EState.Value;
    val initFloor : EType.Floor;
    val initTime : EType.Time;
}
//specification about very simple elevator
class DefaultSpec extends ElevatorSpec {
    val id : Int = 1;

    val capacity : Int = 1;

    val up : EType.Time = 2;
    val down : EType.Time = 2;
    val openToClose : EType.Time = 5;

    val maxFloor : EType.Floor = 10;
    val minFloor : EType.Floor = 1;

    val initState : EState.Value = EState.Close;
    val initFloor : EType.Floor = minFloor;
    val initTime : EType.Time = 0;
}

class IllegalMoveException(msg : String) extends Exception(msg);

//Implementation of Elevator
class Elevator(val spec : ElevatorSpec, val logger : MoveLogger = new SimpleMoveLogger()) {
    //door state
    private var state = spec.initState;
    def doorState = state;

    //elevator is at floor
    private var floor = spec.initFloor;
    def at = floor;

    //elapsed time from initial state
    private var elapsedTime = spec.initTime;
    def time = elapsedTime;

    //door opened time
    private var opendTime = 0;
    private var rider = Set[Person]();

    //Reset state
    def init() {
        state = spec.initState;
        floor = spec.initFloor;
        elapsedTime = spec.initTime;
        opendTime = 0;
        for (i <- spec.minFloor to spec.maxFloor) {
            cage(i) = Set();
        }
    }

    //elevater's cage
    val cage : collection.mutable.Map[Int, Set[Person]] = {
        val m = collection.mutable.Map[Int, Set[Person]]();
        for (i <- spec.minFloor to spec.maxFloor) {
            m(i) = Set();
        }
        m
    }

    def num : Int = cage.foldLeft(0) { (x, y) => x + y._2.size };
    def max : Boolean = num >= spec.capacity

    //go to floor "to"
    def goTo(to : EType.Floor) = {
        if (to != floor && state != EState.Close) {
            throw new IllegalMoveException("You have to close the door before move.");
        }

        if (to > spec.maxFloor || to < spec.minFloor) {
            throw new IllegalMoveException("Illegal floor: " + to);
        }

        while (to != floor) {
            if (to > floor) {
                up();
            } else {
                down();
            }
        }
    }

    //go up one floor
    def up() = {
        if (state == EState.Open) {
            throw new IllegalMoveException("You have to close the door before move.");
        }
        if (floor == spec.maxFloor) {
            throw new IllegalMoveException("Elevator is already at the highest floor: " + floor);
        }
        floor += 1;
        elapsedTime += spec.up;
    }

    //go down one floor
    def down() = {
        if (state == EState.Open) {
            throw new IllegalMoveException("You have to close the door before move.");
        }
        if (floor == spec.minFloor) {
            throw new IllegalMoveException("Elevator is already at the lowest floor: " + floor);
        }
        floor -= 1;
        elapsedTime += spec.down;
    }

    //open the door
    def open() = {
        if (state == EState.Open) {
            throw new IllegalMoveException("You have to close the door before open it.");
        }
        opendTime = elapsedTime;
        state = EState.Open;
        logger.log(this, cage(floor));
        cage(floor) = Set();
    }

    //close the door and ride people ride on elevator
    def close(ride : Set[Person] = Set()) = {
        if (state == EState.Close) {
            throw new IllegalMoveException("You have to open the door before close it.");
        }
        if (elapsedTime < opendTime + spec.openToClose) {
            throw new IllegalMoveException("You have to wait " + spec.openToClose + " to close after open the door.");
        }
        if (ride.size + num > spec.capacity) {
            throw new IllegalMoveException("Over capacity.");
        }
        state = EState.Close;
        for (r <- ride) {
            cage(r.to) += r;
        }
        logger.log(this, ride ++ rider);
        rider = Set();
    }
    //wait openToClose and Close door
    def waitAndClose(ride : Set[Person] = Set()) {
        if (state == EState.Close) {
            throw new IllegalMoveException("You have to open the door before close it.");
        }
        if (ride.size + cage(floor).size > spec.capacity) {
            throw new IllegalMoveException("Over capacity.");
        }
        if (elapsedTime < opendTime + spec.openToClose) {
            stayTo(opendTime + spec.openToClose);
        }
        close(ride);
    }
    //ride people to this elevator
    def ride(people : Set[Person]) {
        if (people.size + num > spec.capacity) {
            throw new IllegalMoveException("Over capacity.");
        }
        rider ++= people;
        for (r <- rider) {
            cage(r.to) += r;
        }
    }
    //stay as it is
    def stay(t : EType.Time) {
        elapsedTime += t;
    }

    //stay until t
    def stayTo(t : EType.Time) {
        if (elapsedTime < t) {
            elapsedTime = t;
        }
    }
}