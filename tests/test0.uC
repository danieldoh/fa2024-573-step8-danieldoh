int main() {

    int a;
    int b;
    int c;

    a = 5;
    b = 10;

    while (a < 20) {
        c = b * 2;  /* Loop-invariant */
        a = a + 1;
    }

    print(a);
}