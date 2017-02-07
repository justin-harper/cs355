import java.io.*;

class Game {
    private static final double noBurn = 0.0;

    private static final double fullBurn = 1.0;

    private static final double deltaTime = 0.5;

    private static void play(Rocket rocket) /* needs throws clause */{
        while (true) {
            /* read input and decide whether to burn or not */

            rocket.move(deltaTime, fullBurn); // move rocket for 0.5 second at
            // full burn
            System.out.println(rocket.getHeightString());
        }

    }

    public static void main(String[] args) {
        /* create a planet with gravity 0.3 and surface at 0.0 */
        Planet pluto = new Planet();
        /* create a rocket with engine strength 2.0, 20.0 units of fuel */
        Rocket rocket = new Rocket(2.0, 20.0, pluto);
        rocket.setHeight(50.0);
        try {
            play(rocket);
        } catch (RocketException v) {
            System.out.println(v.getMessage());
        }
    }

} // end of Game class
