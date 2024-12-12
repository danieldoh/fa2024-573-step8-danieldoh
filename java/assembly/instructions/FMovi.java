package assembly.instructions;

public class FMovi extends Instruction{

    public FMovi(String src, String dest){
        super();
        this.src1 = src;
        this.dest = dest;
        this.oc = OpCode.FMOVIS;
    }

    public String toString() {
        return this.oc + " " + this.dest + ", " + this.src1;
    }
}
