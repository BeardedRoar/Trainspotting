import TSim.*;

public class Lab1 {
// temporary comment, switch 1: 17,7 2: 15,9 3: 3,9 4: 3,11
  public Lab1(Integer speed1, Integer speed2) {
      TSimInterface tsi = TSimInterface.getInstance();

    try {
        tsi.setSpeed(1, speed1);
        tsi.setSpeed(2, speed2);

        tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
    }
    catch (CommandException e) {
        e.printStackTrace();    // or only e.getMessage() for the error
        System.exit(1);
    }
  }
}
