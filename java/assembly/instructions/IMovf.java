package assembly.instructions;

public class IMovf extends Instruction{

    public IMovf(String src, String dest){
        super();
        this.src1 = src;
        this.dest = dest;
        this.oc = OpCode.IMOVFS;
    }

    public String toString() {
        return this.oc + " " + this.dest + ", " + this.src1;
    }
    
}
