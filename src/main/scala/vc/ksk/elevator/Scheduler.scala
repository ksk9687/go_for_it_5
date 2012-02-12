package vc.ksk.elevator
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue

trait Scheduler {
    protected val elevators : Vector[Elevator];
    protected val request : ArrayBuffer[Person];

    protected val minFloor = elevators.foldLeft(elevators.head.spec.minFloor) { (x, e) => math.min(x, e.spec.minFloor) }
    protected val maxFloor = elevators.foldLeft(elevators.head.spec.maxFloor) { (x, e) => math.max(x, e.spec.maxFloor) }
    protected val waiting = {
        var w = Map[Int, ArrayBuffer[Person]]();
        for (e <- elevators) {
            for (f <- e.spec.minFloor to e.spec.maxFloor) {
                w += f -> ArrayBuffer[Person]();
            }
        }
        for (r <- request) {
            w(r.from) += r
        }
        w
    }
    
    //served all request or not
    protected def end() : Boolean = waiting.forall(_._2.size == 0) && elevators.forall(_.num == 0);

    //nearest request in time
    protected def recent() : Person = {
        var p = Person(0, 0, 0, Int.MaxValue);
        for (w <- waiting) {
            if (w._2.size != 0) {
                if (p.at > w._2(0).at) {
                    p = w._2(0);
                }
            }
        }
        p
    }
    def schedule();
}

/**
 * scheduler for 1-preson elevator
 */
class SimpleScheduler(
    protected val elevators : Vector[Elevator],
    protected val request : ArrayBuffer[Person]) extends Scheduler {

    def schedule() {
        val elevator = elevators(0);
        
        //for each request
        for (r <- request) {
            //go to from floor
            if (elevator.at != r.from) {
                if (elevator.doorState == EState.Open) {
                    elevator.waitAndClose();
                }
                elevator.goTo(r.from);
            }
            //open door
            if (elevator.doorState != EState.Open) {
                elevator.open();
            }
            //wait to ride
            if (elevator.time < r.at) {
                elevator.stayTo(r.at);
            }
            //close door
            elevator.waitAndClose(Set(r));
            //go to to floor
            elevator.goTo(r.to);
            //open door
            elevator.open();
        }
    }
}

/*
 * Lock scheduling
 * http://en.wikipedia.org/wiki/LOOK_algorithm
 */
class LOCKScheduler(
    protected val elevators : Vector[Elevator],
    protected val request : ArrayBuffer[Person]) extends Scheduler {


    def close(e : Elevator) {
        if (e.doorState == EState.Open) {
            e.waitAndClose();
        }
    }
    //pick up p
    def in(e : Elevator, p : Person) {
        if (e.at != p.from && e.doorState == EState.Open) {
            close(e);
        }
        e.goTo(p.from);
        if (e.doorState != EState.Open) {
            e.open();
        }
        e.stayTo(p.at);
        e.ride(Set(p));
    }
    //drop p
    def out(e : Elevator, p : Person) {
        if (e.at != p.to && e.doorState == EState.Open) {
            close(e);
        }
        e.goTo(p.to);
        if (e.doorState != EState.Open) {
            e.open();
        }
    }

    def schedule() {
        val elevator = elevators(0);
        var up : Boolean = true;
        var noMove = 0;

        def checkFloor(i : Int, goUp : Boolean) = {
            var r = Set[Person]()
            waiting(i).foreach { x =>
                if (!elevator.max && x.at <= elevator.time + elevator.spec.openToClose + math.abs(i - elevator.at) * (if (goUp) elevator.spec.up else elevator.spec.down) && ((x.to > x.from) == goUp)) {
                    in(elevator, x);
                    r += x;
                    noMove = 0;
                }
            }
            waiting(i) --= r;
            elevator.cage(i).foreach { x =>
                out(elevator, x);
                noMove = 0;
            }
        }
        while (!end()) {
            val currentFloor = elevator.at;
            if (up) {
                for (i <- currentFloor to elevator.spec.maxFloor) {
                    checkFloor(i, up);
                }
                up = false;
                for (i <- elevator.spec.maxFloor to currentFloor by -1) {
                    checkFloor(i, up);
                }
                noMove += 1;
            } else {
                for (i <- currentFloor to elevator.spec.minFloor by -1) {
                    checkFloor(i, up);
                }
                up = true;
                for (i <- elevator.spec.minFloor to currentFloor) {
                    checkFloor(i, up);
                }
                noMove += 1;
            }
            if (noMove >= 3) {
                in(elevator, recent());
                noMove = 0;
            }
        }
    }
}
