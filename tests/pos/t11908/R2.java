public class R2 {
    final record R(int i, String s) implements IntLike {
        public int getInt() {
            return i;
        }

	/* TODO
        // Canonical constructor
        public R(int i, java.lang.String s) {
            this.i = i;
            this.s = s.intern();
        }
	*/
    }
}
