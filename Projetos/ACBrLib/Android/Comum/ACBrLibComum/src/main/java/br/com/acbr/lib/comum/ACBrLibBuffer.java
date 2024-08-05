package br.com.acbr.lib.comum;

import static java.lang.Math.min;

import com.sun.jna.ptr.IntByReference;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public class ACBrLibBuffer {
    public static final Charset UTF8 = Charset.forName("UTF8");
    public static final int TAMANHO_BUFFER_PADRAO = 1024;  // 1K

    public ByteBuffer bufferData;
    public IntByReference bufferSizeNeeded;

    public ACBrLibBuffer(){
        this(-1);
    }

    public ACBrLibBuffer(int size){
        if (size < 0) {
            size = TAMANHO_BUFFER_PADRAO;
        }

        this.bufferData = ByteBuffer.allocate(size);
        this.bufferSizeNeeded = new IntByReference(size);
    }

    public int getBufferCapacity() {
        return this.bufferData.capacity();
    }

    public int getBufferSizeNeeded() {
        return this.bufferSizeNeeded.getValue();
    }

    public String toString() {
        byte[] array = this.bufferData.array();
        String s = new String(array, 0, min(array.length, this.getBufferSizeNeeded()), UTF8);
        return s;
    }
}
