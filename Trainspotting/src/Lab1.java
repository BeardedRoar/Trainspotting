import TSim.*;

import java.util.concurrent.Semaphore;

public class Lab1 {

    private final Semaphore[] tracks;
    private TSimInterface tsi = TSimInterface.getInstance();

    // temporary comment, switch 1: 17,7 2: 15,9 3: 3,9 4: 3,11
    // short path: 2,3,4,6,7
    // overtaking-paths: 1,5,8
  public Lab1(Integer speed1, Integer speed2) {

      this.tracks = new Semaphore[8];

    try {
        tsi.setSpeed(1, speed1);
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
