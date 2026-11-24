package t1;

public final class ImmutableIntArray {
    private final int[] data;

    private ImmutableIntArray(int[] data) { this.data = data; }

    public int length() { return data.length; }

    public int get(int idx) { return data[idx]; }

    public ImmutableIntArray with(int idx, int value) {
        int[] copy = data.clone();
        copy[idx] = value;
        return new ImmutableIntArray(copy);
    }

    public static ImmutableIntArray of(int... elems) { return new ImmutableIntArray(elems.clone()); }

    public static ImmutableIntArray copyOf(int[] src) { return new ImmutableIntArray(src.clone()); }
}
