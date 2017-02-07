import java.text.DecimalFormat;

class Rocket {
    static final RocketException crashed = new RocketException(
            "Crashed and Burned");
    static final RocketException landed = new RocketException("Safely Landed");
    private double velocity = 0.0; // default to 0
    private double height = 50.0; // default to 50.0
    private Planet planet;
    private static final double safeVelocity = -1.0; // Land within this
                                                       // velocity
    private void nextHeight(double deltaTime) {
        height += (velocity * deltaTime);
        if (reachedSurface()) {
            if (velocity < safeVelocity) {
                /* throw crashed exception */
            } else {
                /* throw landed exception */
            }
        }
    }

    private bool reachedSurface() {
        /* true if rocket is at or below the planet's surface */
    }

    private void nextVelocity(double burnRate, double deltaTime) {
        velocity += ((engineStrength * burnRate) - planet.getGravity())
                * deltaTime;
    }

    public void move(double dt, double burnRate) {
        /* note that dt is measured in seconds */
        if (!reachedSurface()) {
            /* update the height, velocity and fuel */
        }
    }

    public String getHeightString() {
        double maxHeight = (height > 60.0) ? height : 60.0;
        double belowGround = planet.getGround() - 10.0;
        int size = (int) (maxHeight - belowGround) + 1;
        char[] buffer = new char[size];
        DecimalFormat df = new DecimalFormat(" ###.##");

        int groundPosition = (int) (planet.getGround() - belowGround);

        for (int i = 0; i < size; i++)
            buffer[i] = ' ';
        int adjustedPosition = (int) (height - belowGround);
        adjustedPosition = (adjustedPosition <= 0) ? 0 : adjustedPosition;
        buffer[groundPosition] = '|';
        buffer[adjustedPosition] = '*';
        return (new String(buffer) + " " + df.format(velocity));
    }

}
