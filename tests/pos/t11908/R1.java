// scalac: -Vprint:parser
record R1(int i, String s) {

    public String someMethod() {
        return s + "!";
    }
}
