int main() {
    
    int x;
    int y; 
    int z;

    x = 1;
    y = 6;

    while (x < 10) {
        if (x / 2 * 2 == x) {  
            z = y * 4;  /* Loop-invariant */
        } else {
            z = y * 3;  
        }
        x = x + 1;
        print(z);
    }
}
