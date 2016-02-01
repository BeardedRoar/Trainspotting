import TSim.*;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;

public class Lab1 {

    private final Semaphore[] tracks;
    private final Thread[] trainThreads;
    private TSimInterface tsi = TSimInterface.getInstance();

    public Lab1(Integer speed1, Integer speed2) {

        this.tracks = new Semaphore[6];
        this.trainThreads = new Thread[2];

        for (int i = 0; i < 6; i++) {
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
        private List<Integer> heldSemaphores;

        public TrainRunnable(int id, int speed){
            this.id = id;
            this.speed = speed;
            this.downwards = id == 1;
            heldSemaphores = new ArrayList<Integer>();

        }

        @Override
        public void run() {
            try {
                if (id == 2){
                    tracks[4].acquire();
                    heldSemaphores.add(4);
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
                            tryEnterSharedTrack(1, 17, 7,
                                    y == 7 ? TSimInterface.SWITCH_RIGHT: TSimInterface.SWITCH_LEFT);
                        } else {
                            releaseIfPossible(1);
                        }
                    } else if (x == 11 && (y == 7 || y == 8)) {
                        if (downwards) {
                            releaseIfPossible(5);
                        } else {
                            tryEnterSharedTrack(5);
                        }
                    } else if (x == 14 && (y == 3 || y == 5)) {
                        if (!downwards) {
                            stopTrain();
                            waitAtStop();
                            resumeTrain();
                        }
                    } else if (x == 18 && y == 7) {
                        if (downwards) {
                            releaseIfPossible(0);
                            tryEnterFastTrack(2,15,9,TSimInterface.SWITCH_RIGHT,TSimInterface.SWITCH_LEFT);
                        } else {
                            releaseIfPossible(2);
                            tryEnterFastTrack(0,17,7,TSimInterface.SWITCH_LEFT,TSimInterface.SWITCH_RIGHT);
                        }
                    } else if (x == 12 && (y == 9 || y == 10)) {
                        if (downwards) {
                            releaseIfPossible(1);
                        } else {
                            tryEnterSharedTrack(1, 15, 9,
                                    y == 10 ? TSimInterface.SWITCH_LEFT : TSimInterface.SWITCH_RIGHT);
                        }
                    } else if (x == 7 && (y == 9 || y == 10)) {
                        if (downwards) {
                            tryEnterSharedTrack(3, 4, 9,
                                    y == 9 ? TSimInterface.SWITCH_LEFT : TSimInterface.SWITCH_RIGHT);
                        } else {
                            releaseIfPossible(3);
                        }
                    } else if (y == 5 && (x == 9 || x == 6)) {
                        if (downwards) {
                            tryEnterSharedTrack(5);
                        } else {
                            releaseIfPossible(5);
                        }
                    } else if (x == 1 && y == 10) {
                        if (downwards) {
                            releaseIfPossible(2);
                            tryEnterFastTrack(4,3,11,TSimInterface.SWITCH_LEFT,TSimInterface.SWITCH_RIGHT);
                        } else {
                            releaseIfPossible(4);
                            tryEnterFastTrack(2,4,9,TSimInterface.SWITCH_LEFT,TSimInterface.SWITCH_RIGHT);
                        }
                    } else if (x == 14 && (y == 13 || y == 11)) {
                        if (downwards) {
                            stopTrain();
                            waitAtStop();
                            resumeTrain();
                        }

                    } else if (x == 6 && (y == 13 || y == 11)) {
                        if (downwards) {
                            releaseIfPossible(3);
                        } else {
                            tryEnterSharedTrack(3, 3, 11,
                                    y == 13 ? TSimInterface.SWITCH_RIGHT : TSimInterface.SWITCH_LEFT);
                        }
                    }

                } catch (CommandException e){
                    e.printStackTrace();
                }
            }
        }

        /**
         * The calling Thread tries to acquire the given Semaphore and if it is successful the train is allowed
         * to enter the shared track. Otherwise it is blocked until the Semaphore is released and the train stops
         * until then.
         *
         * @param trackID The number of the Semaphore corresponding to the shared track.
         * @throws CommandException if the train cannot be stopped or resumed.
         * @throws InterruptedException if the Thread is interrupted.
         */
        private void tryEnterSharedTrack(int trackID) throws CommandException, InterruptedException {
            if (!tracks[trackID].tryAcquire()) {
                stopTrain();
                tracks[trackID].acquire();
                heldSemaphores.add(trackID);
                resumeTrain();
            } else {
                heldSemaphores.add(trackID);
            }
        }

        /**
         * The calling Thread tries to acquire the given Semaphore and if it is successful the train is allowed
         * to enter the shared track. To enter it flips the given switch to the given direction.
         * Otherwise it is blocked until the Semaphore is released and the train stops until then.
         *
         * @param trackID The number of the Semaphore corresponding to the shared track.
         * @param switchX The X-coordinate of the Switch corresponding to the shared track.
         * @param switchY The Y-coordinate of the Switch corresponding to the shared track.
         * @param dir The direction to flip the Switch in order to enter the shared track.
         * @throws CommandException
         * @throws InterruptedException
         */
        private void tryEnterSharedTrack(int trackID, int switchX, int switchY, int dir)
                throws CommandException, InterruptedException {
            if (!tracks[trackID].tryAcquire()) {
                stopTrain();
                tracks[trackID].acquire();
                heldSemaphores.add(trackID);
                Lab1.this.tsi.setSwitch(switchX, switchY, dir);
                resumeTrain();
            } else {
                heldSemaphores.add(trackID);
                Lab1.this.tsi.setSwitch(switchX, switchY, dir);
            }
        }

        /**
         * Sets the speed of the train to 0 and puts the thread to sleep for a period of time depending on the train's
         * speed.
         *
         * @throws InterruptedException if the Thread is interrupted.
         * @throws CommandException if speed cannot be set due to illegal id, illegal speed or a crash.
         */
        private void stopTrain() throws InterruptedException, CommandException {
            Lab1.this.tsi.setSpeed(id, 0);
        }

        /**
         * Sets the speed of the train to either speed or -speed depending on which stop it currently is at.
         *
         * @throws CommandException if speed cannot be set due to illegal id, illegal speed or a crash.
         */
        private void resumeTrain() throws CommandException {
            Lab1.this.tsi.setSpeed(id, ((downwards == (id == 1)) ? speed : -speed));
        }

        /**
         * Called at a Stop, puts the calling Thread to sleep for 1s + the time it takes for the train to stop
         * and changes direction of the train.
         *
         * @throws InterruptedException if the Thread is interrupted
         */
        private void waitAtStop() throws InterruptedException {
            Thread.sleep(1000+ 2*20*speed);
            downwards = !downwards;
        }

        /**
         * Checks if the thread is holding the given Semaphore and if it is, releases it and removes it from the list
         * of held semaphores.
         *
         * @param semaphore The Semaphore to be released if held.
         */
        private void releaseIfPossible(int semaphore){
            if(heldSemaphores.contains(semaphore)) {
                tracks[semaphore].release();
                heldSemaphores.remove((Object) semaphore);
            }
        }

        /**
         * The calling Thread tries to acquire the given Semaphore and if it is successful flips the given Switch so
         * the train can enter the fast track. Otherwise it flips it to enter the slow track.
         *
         * @param trackID The number of the Semaphore corresponding to the fast track.
         * @param switchX The X-coordinate of the Switch corresponding to the fast track.
         * @param switchY The Y-coordinate of the Switch corresponding to the fast track.
         * @param successDir The direction to flip the Switch to in order to enter the fast track.
         * @param failDir The direction to flip the Switch to in order to enter the slow track.
         * @throws CommandException if the Switch cannot be flipped.
         */
        private void tryEnterFastTrack(int trackID, int switchX, int switchY, int successDir, int failDir) throws CommandException {
            if(tracks[trackID].tryAcquire()){
                Lab1.this.tsi.setSwitch(switchX, switchY, successDir);
                heldSemaphores.add(trackID);
            } else {
                Lab1.this.tsi.setSwitch(switchX, switchY,failDir);
            }
        }

    }
}
