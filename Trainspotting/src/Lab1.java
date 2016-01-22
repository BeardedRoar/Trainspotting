import TSim.*;

public class Lab1 {

    private TSimInterface tsi = TSimInterface.getInstance();

// temporary comment, switch 1: 17,7 2: 15,9 3: 3,9 4: 3,11
  public Lab1(Integer speed1, Integer speed2) {

    try {
        //tsi.setSpeed(1, speed1);
        tsi.setSpeed(2, speed2);

        tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
        tsi.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
        new Thread(new TrainRunnable(2)).start();
    }
    catch (CommandException e) {
        e.printStackTrace();    // or only e.getMessage() for the error
        System.exit(1);
    }
  }

    private class TrainRunnable implements Runnable {

        private final int id;

        public TrainRunnable(int id){
            this.id = id;
        }

        @Override
        public void run() {
            try {
                SensorEvent se = Lab1.this.tsi.getSensor(id);
                tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
                tsi.setSpeed(se.getTrainId(), 0);
            } catch (CommandException e) {
                e.printStackTrace();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
