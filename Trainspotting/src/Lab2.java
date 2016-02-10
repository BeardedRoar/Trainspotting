import TSim.*;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Represents the logic needed to run the simulation of Laboration 1 for the course TDA383
 */
public class Lab2 {

    // Monitors representing the different parts of the track only one train should be able to access without
    // risk of accidents.
    private final TrackMonitor[] tracks;

    // The threads responsible for logic for the different trains
    private final Thread[] trainThreads;

    // The interface for the simulator, makes it possible to handle the trains.
    private TSimInterface tsi = TSimInterface.getInstance();

    /**
     * Creates a new instance of teh simulation-logic needed for lab2, using speeds given for the two trains.
     * @param speed1 the speed for the first train as an integer.
     * @param speed2 the speed for the second train as an integer.
     */
    public Lab2(Integer speed1, Integer speed2) {
        // Initiate the list and the semaphores.
        this.tracks = new TrackMonitor[6];
        this.trainThreads = new Thread[2];

        for (int i = 0; i < 6; i++) {
            tracks[i] = new TrackMonitor();
        }

        trainThreads[0] = new Thread(new TrainRunnable(1, speed1), "Train_0");
        trainThreads[1] = new Thread(new TrainRunnable(2, speed2), "Train_1");

        for (int i = 0; i < 2; i++) {
            trainThreads[i].start();
        }

    }

    /**
     * A runnable representing the necessary logic needed to run a train.
     * Will be able run parallel to another instance or another runnable.
     */
    private class TrainRunnable implements Runnable {

        private final int id, speed;

        // Represents the state of the train, is it going "downwards" in the map.
        // If true, train goes downwards, if false the train goes upwards.
        private boolean downwards;

        private final List<Integer> heldLocks;
        /**
         * Constructor for the runnable, needing an identifier of the train, given as a unique integer and must
         * match the id the train is given by the simulator.
         * @param id the identification of the train. Must match the id given by the simulator.
         * @param speed the speed of the train.
         */
        public TrainRunnable(int id, int speed){
            this.id = id;
            this.speed = speed;

            // The train with id 1 starts by going downwards, while train number 2 starts by going upwards.
            this.downwards = id == 1;
            heldLocks = new ArrayList<Integer>();

        }

        @Override
        public void run() {
            try {
                // Initiates the trains, the train with id 2 starts on a critical part of the tracks,
                // and must therefore be given the responding semaphore.
                if (id == 2){
                    tracks[4].enter();
                    heldLocks.add(4);
                }
                tsi.setSpeed(id, speed);

                // The logic while the trains are running.
                // First await the next sensor, then handle that sensor-event and repeat.
                while (true) {
                    SensorEvent se = Lab2.this.tsi.getSensor(id);
                    handleSensorEvent(se);
                }

                // Semaphores are not released on an exception as the train still occupy the responding track,
                // and such could make the accident worse by letting another train collide with a still-standing train.
            } catch (CommandException | InterruptedException e) {
                System.exit(1);
                e.printStackTrace();
            }
        }

        /**
         * Handles a sensor event according to the state of the train passing the sensor and the state of track
         * it tries to enter.
         * @param se the sensor-event to handle
         * @throws InterruptedException in case the thread is interrupted while the event is still being handled.
         */
        private void handleSensorEvent(SensorEvent se) throws InterruptedException, CommandException {
            // Each event is assumed to be caused by the train monitored by thread, as this should be the only event
            // it could access.

            // Only handle relevant events by not performing the same logic on entering and exiting the same sensor.
            if (se.getStatus() == SensorEvent.ACTIVE) {

                // Each block represents one sensor or two parallel sensors, and is handled different depending on the
                // direction of the train, as this dictates if and what track it has left or tries to enter.

                // Which sensor that has been passed is found out by it's x and y coordinates.
                int x = se.getXpos();
                int y = se.getYpos();

                if (x == 14 && (y == 3 || y == 5)) {
                    // Sensor 0
                    if (!downwards) {
                        stopTrain();
                        waitAtStop();
                        resumeTrain();
                    }
                } else if (y == 5 && (x == 9 || x == 6)) {
                    // Sensor 1
                    if (downwards) {
                        tryEnterCriticalSection(5);
                    } else {
                        releaseTrack(5);
                    }
                } else if (x == 11 && (y == 7 || y == 8)) {
                    // Sensor 2
                    if (downwards) {
                        releaseTrack(5);
                    } else {
                        tryEnterCriticalSection(5);
                    }
                } else if (x == 14 && (y == 7 || y == 8)) {
                    // Sensor 3
                    // Parallel sensors on tracks 0, train either trying to enter or has just left track 1.
                    if (downwards) {
                        tryEnterCriticalSection(1, 17, 7,
                                y == 7 ? TSimInterface.SWITCH_RIGHT : TSimInterface.SWITCH_LEFT);
                    } else {
                        releaseTrack(1);
                    }
                } else if (x == 18 && y == 7) {
                    // Sensor 3
                    if (downwards) {
                        releaseTrack(0);
                    } else {
                        tryEnterFastTrack(0, 17, 7, TSimInterface.SWITCH_LEFT, TSimInterface.SWITCH_RIGHT);
                    }
                } else if (x == 17 && y == 9){
                    // Sensor 4
                    if (downwards){
                        tryEnterFastTrack(2, 15, 9, TSimInterface.SWITCH_RIGHT, TSimInterface.SWITCH_LEFT);
                    } else {
                        releaseTrack(2);
                    }
                } else if (x == 12 && (y == 9 || y == 10)) {
                    // Sensor 5
                    if (downwards) {
                        releaseTrack(1);
                    } else {
                        tryEnterCriticalSection(1, 15, 9,
                                y == 10 ? TSimInterface.SWITCH_LEFT : TSimInterface.SWITCH_RIGHT);
                    }
                } else if (x == 7 && (y == 9 || y == 10)) {
                    // Sensor 6
                    if (downwards) {
                        tryEnterCriticalSection(3, 4, 9,
                                y == 9 ? TSimInterface.SWITCH_LEFT : TSimInterface.SWITCH_RIGHT);
                    } else {
                        releaseTrack(3);
                    }
                }  else if (x == 1 && y == 10) {
                    // Sensor 7
                    if (downwards) {
                        releaseTrack(2);
                        tryEnterFastTrack(4,3,11,TSimInterface.SWITCH_LEFT,TSimInterface.SWITCH_RIGHT);
                    } else {
                        releaseTrack(4);
                        tryEnterFastTrack(2,4,9,TSimInterface.SWITCH_LEFT,TSimInterface.SWITCH_RIGHT);
                    }
                } else if (x == 6 && (y == 13 || y == 11)) {
                    // Sensor 8
                    if (downwards) {
                        releaseTrack(3);
                    } else {
                        tryEnterCriticalSection(3, 3, 11,
                                y == 13 ? TSimInterface.SWITCH_RIGHT : TSimInterface.SWITCH_LEFT);
                    }
                } else if (x == 14 && (y == 13 || y == 11)) {
                    // Sensor 9
                    if (downwards) {
                        stopTrain();
                        waitAtStop();
                        resumeTrain();
                    }
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
        private void tryEnterCriticalSection(int trackID) throws CommandException, InterruptedException {
            stopTrain();
            tracks[trackID].enter();
            resumeTrain();
            heldLocks.add(trackID);
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
        private void tryEnterCriticalSection(int trackID, int switchX, int switchY, int dir)
                throws CommandException, InterruptedException {
            stopTrain();
            tracks[trackID].enter();
            Lab2.this.tsi.setSwitch(switchX, switchY, dir);
            resumeTrain();
            heldLocks.add(trackID);
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
            if(tracks[trackID].isEmpty()){
                tracks[trackID].enter();
                heldLocks.add(trackID);
                Lab2.this.tsi.setSwitch(switchX, switchY, successDir);
            } else {
                Lab2.this.tsi.setSwitch(switchX, switchY,failDir);
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
            Lab2.this.tsi.setSpeed(id, 0);
        }

        /**
         * Sets the speed of the train to either speed or -speed depending on which stop it currently is at.
         *
         * @throws CommandException if speed cannot be set due to illegal id, illegal speed or a crash.
         */
        private void resumeTrain() throws CommandException {
            Lab2.this.tsi.setSpeed(id, ((downwards == (id == 1)) ? speed : -speed));
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
         * @param trackID The Semaphore to be released if held.
         */
        private void releaseTrack(int trackID){
            if(heldLocks.contains(trackID)) {
                tracks[trackID].leave();
                heldLocks.remove((Integer)trackID);
            }
        }

    }

    private class TrackMonitor {
        final Lock lock = new ReentrantLock();
        final Condition empty = lock.newCondition();

        int count = 0;

        public void enter(){
            lock.lock();
            try {
                while (count != 0){
                    empty.await();
                }
                count++;
            } catch (InterruptedException e) {
                e.printStackTrace();
            } finally {
                lock.unlock();
            }
        }

        public void leave(){
            lock.lock();
            try {
                count = 0;
                empty.signal();
            } finally {
                lock.unlock();
            }
        }

        public boolean isEmpty(){
            return count == 0;
        }

    }
}
