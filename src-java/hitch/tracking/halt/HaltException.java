package hitch.tracking.halt;

/**
 * A sentinel exception used to short-circuit execution when a selector lacks a
 * value in a tracking context.
 */
public final class HaltException extends RuntimeException {
    private static final HaltException instance = new HaltException();

    private HaltException() {
        super("HALT", null, false, false);
    }

    public static final HaltException getInstance() {
        return instance;
    }

    public static final HaltException doThrow() {
        throw instance;
    }

    public static final boolean isHalt(Object o) {
        return instance == o;
    }

    @Override
    public String toString() {
        return "#<HALT>";
    }
}
