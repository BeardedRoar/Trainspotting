import TSim.*;

import java.util.concurrent.Semaphore;

public class Lab1 {

    private final Semaphore[] tracks;
    private final Thread[] trainThreads;
    private TSimInterface tsi = TSimInterface.getInstance();

    // temporary comment, switch 1: 17,7 2: 15,9 3: 4,9 4: 3,11
    // short path: 2,3,4,6,7
    // overtaking-paths: 1,5,8
  public Lab1(Integer speed1, Integer speed2) {

      this.tracks = new Semaphore[5];
      this.trainThreads = new Thread[2];

      for (int i = 0; i < 5; i++) {
          tracks[i] = new Semaphore(1);
      }

    try {
        tsi.setSpeed(1, speed1);
        tsi.setSpeed(2, speed2);

        trainThreads[0] = new Thread(new TrainRunnable(1, speed1));
        trainThreads[1] = new Thread(new TrainRunnable(2, speed2));
        for (int i = 0; i < 2; i++) {
            trainThreads[i].start();
        }
    }
    catch (CommandException e) {
        e.printStackTrace();    // or only e.getMessage() for the error
        System.exit(1);
    }
  }

    private class TrainRunnable implements Runnable {

        private final int id, speed;
        private boolean downwards;

        public TrainRunnable(int id, int speed){
            this.id = id;
            this.speed = speed;
            this.downwards = id == 1;
        }

        @Override
        public void run() {
            try {
                if (id == 2){
                    tracks[4].acquire();
                }
                while (true) {
                    SensorEvent se = Lab1.this.tsi.getSensor(id);
                    handleSensorEvent(se);
                }
            } catch (CommandException e) {
                e.printStackTrace();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        private void handleSensorEvent(SensorEvent se) throws InterruptedException {
            if (se.getStatus() == SensorEvent.ACTIVE) {
                int x = se.getXpos(); int y = se.getYpos();
                try {
                    if (x == 14 && (y == 7 || y == 8)) {
                        if (downwards) {
                            tryEnterSharedTarck(1);
                            Lab1.this.tsi.setSwitch(17, 7,
                                    y == 7 ? TSimInterface.SWITCH_RIGHT: TSimInterface.SWITCH_LEFT);
                        } else {
                            tracks[1].release();
                            stopTrain();
                            waitAtStop();
                            resumeTrain();
                        }
                    } else if (x == 18 && y == 7) {
                        if (downwards) {
                            tracks[0].release();
                            Lab1.this.tsi.setSwitch(15, 9,
                                    tracks[2].tryAcquire() ? TSimInterface.SWITCH_RIGHT : TSimInterface.SWITCH_LEFT);
                        } else {
                            tracks[2].release();
                            Lab1.this.tsi.setSwitch(17, 7,
                                    tracks[0].tryAcquire() ? TSimInterface.SWITCH_LEFT : TSimInterface.SWITCH_RIGHT);
                        }
                    } else if (x == 9 && (y == 9 || y == 10)) {
                        if (downwards) {
                            tracks[1].release();
                            tryEnterSharedTarck(3);
                            Lab1.this.tsi.setSwitch(4, 9,
                                    y == 9 ? TSimInterface.SWITCH_LEFT : TSimInterface.SWITCH_RIGHT);
                        } else {
                            tracks[3].release();
                            tryEnterSharedTarck(1);
                            Lab1.this.tsi.setSwitch(15, 9,
                                    y == 10 ? TSimInterface.SWITCH_LEFT : TSimInterface.SWITCH_RIGHT);
                        }
                    } else if (x == 1 && y == 10) {
                        if (downwards) {
                            tracks[2].release();
                            Lab1.this.tsi.setSwitch(3, 11,
                                    tracks[4].tryAcquire() ? TSimInterface.SWITCH_LEFT : TSimInterface.SWITCH_RIGHT);
                        } else {
                            tracks[4].release();
                            Lab1.this.tsi.setSwitch(4, 9,
                                    tracks[2].tryAcquire() ? TSimInterface.SWITCH_LEFT : TSimInterface.SWITCH_RIGHT);
                        }
                    } else if (x == 10 && (y == 13 || y == 11)) {
                        if (downwards) {
                            tracks[3].release();
                            stopTrain();
                            waitAtStop();
                            resumeTrain();
                        } else {
                            tryEnterSharedTarck(3);
                            Lab1.this.tsi.setSwitch(3, 11,
                                    y == 13 ? TSimInterface.SWITCH_RIGHT : TSimInterface.SWITCH_LEFT);
                        }
                    }
                } catch (CommandException e){
                    e.printStackTrace();
                }
            }
        }

        private void tryEnterSharedTarck(int trackID) throws CommandException, InterruptedException {
            if (!tracks[trackID].tryAcquire()) {
                stopTrain();
                tracks[trackID].acquire();
                resumeTrain();
            }
        }

        private void stopTrain() throws InterruptedException, CommandException {
            Lab1.this.tsi.setSpeed(id, 0);
            Thread.sleep(2* 100 * speed);
        }

        private void resumeTrain() throws CommandException {
            Lab1.this.tsi.setSpeed(id, ((downwards == (id == 1)) ? speed : -speed));
        }

        private void waitAtStop() throws InterruptedException, CommandException {
            Thread.sleep(1000);
            downwards = !downwards;
        }

    }
}
