import org.scalatest.FunSuite
import vc.ksk.elevator._

class Elevatortest extends FunSuite {
    test("Elevator should be initialy at foor 1 and closed.") {
        val e = new Elevator(new DefaultSpec());
        assert(e.doorState == EState.Close);
        assert(e.at == 1);
        assert(e.time == 0);
    }

    test("Spec should be able to define Elevator's spec") {
        val e = new Elevator(new DefaultSpec {
            override val initFloor = 4;
            override val initState = EState.Open;
        });
        assert(e.doorState == EState.Open);
        assert(e.at == 4);
    }

    test("Elevator should move up to maxFloor.") {
        val e = new Elevator(new DefaultSpec());
        assert(e.at == 1);
        e.up();
        assert(e.at == 2);
        e.up();
        assert(e.at == 3);
        e.goTo(9);
        assert(e.at == 9);
        e.up();
        assert(e.at == 10);
        intercept[IllegalMoveException] {
            e.up();
        }
        assert(e.at == 10);
    }

    test("Elevator should move down to minFloor.") {
        val e = new Elevator(new DefaultSpec());
        assert(e.at == 1);
        intercept[IllegalMoveException] {
            e.down();
        }
        assert(e.at == 1);
        e.goTo(2);
        assert(e.at == 2);
        e.down();
        assert(e.at == 1);
        intercept[IllegalMoveException] {
            e.down();
        }
        assert(e.at == 1);
    }

    test("Elevator door should be able to open and close.") {
        val e = new Elevator(new DefaultSpec {
            override val openToClose = 5;
        });
        assert(e.doorState == EState.Close);
        e.open();
        assert(e.doorState == EState.Open);
        intercept[IllegalMoveException] {
            e.close();
        }
        assert(e.doorState == EState.Open);
        e.stay(3);
        assert(e.doorState == EState.Open);
        intercept[IllegalMoveException] {
            e.close();
        }
        assert(e.doorState == EState.Open);
        e.stay(2);
        assert(e.doorState == EState.Open);
        e.close();
        assert(e.doorState == EState.Close);
    }

    test("Elevator should not move when its door is open.") {
        val e = new Elevator(new DefaultSpec());
        e.open();
        intercept[IllegalMoveException] {
            e.up();
        }
        e.stay(5);
        e.close();
        e.up();
    }

    test("Elevator should take defined time to move.") {
        val time = List(2, 5, 1, 0);
        for (u <- time; d <- time; c <- time) {
            var t = 0;
            val e = new Elevator(new DefaultSpec {
            	override val initTime = t;
            	override val up = u;
            	override val down = d;
            	override val openToClose = c;
            });
            
            assert(e.time == t);
            e.up();
            t += u;
            assert(e.time == t);
            e.up();
            t += u;
            e.up();
            t += u;
            
            e.down();
            t += d;
            e.down();
            t += d;
            assert(e.time == t);
            
            e.goTo(8);
            t += u*6;
            assert(e.time == t);
            e.open();
            e.stay(c);
            t += c;
            e.close();
            assert(e.time == t);
        }
    }
    
}