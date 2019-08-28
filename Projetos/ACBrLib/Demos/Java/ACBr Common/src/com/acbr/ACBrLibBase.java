/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr;

import com.sun.jna.ptr.IntByReference;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;

/**
 *
 * @author Rafael Dias
 */
public abstract class ACBrLibBase {
    
    protected static final Charset UTF8 = Charset.forName("UTF-8");
    protected static final int STR_BUFFER_LEN = 256;
    
    /**
     *
     * @param value
     * @return
     */
    protected static String toUTF8(String value) {
        return new String(value.getBytes(UTF8));
    }

    /**
     *
     * @param buffer
     * @param len
     * @return
     */
    protected static String fromUTF8(ByteBuffer buffer, int len) {
        return new String(buffer.array(), 0, len, UTF8);
    }

    protected static int parseInt(ByteBuffer buffer, IntByReference len) {
        return parseInt(buffer, len, 0);
    }
    
    protected static int parseInt(ByteBuffer buffer, IntByReference len, int defaultVal) {
        String s = fromUTF8(buffer, len);
        return s.matches("-?\\d+") ? Integer.parseInt(s) : defaultVal;   
    }
    
    protected static boolean parseBoolean(ByteBuffer buffer, IntByReference len) {
        String s = fromUTF8(buffer, len);
        return s.equals("1");
    }
    
    protected static String parseString(ByteBuffer buffer, IntByReference len){
        return fromUTF8(buffer, len);
    }
    
    protected static String toUTF8(Boolean value) {
        return toUTF8(value ? "1" : "0");
    }
    
    protected static String toUTF8(int value) {
        return toUTF8(Integer.toString(value));
    }
    
    protected static String toUTF8(char[] value) {
        return toUTF8(new String(value));
    }

    protected static String fromUTF8(ByteBuffer buffer, IntByReference len) {
        return new String(buffer.array(), 0, len.getValue(), UTF8);
    }

    /**
     *
     * @param result
     * @throws Exception
     */
    protected void checkResult(int result) throws Exception {
        if (result == 0) {
            return;
        }

        ByteBuffer buffer = ByteBuffer.allocate(STR_BUFFER_LEN);
        IntByReference bufferLen = new IntByReference(STR_BUFFER_LEN);

        UltimoRetorno(buffer, bufferLen);
        throw new Exception(processResult(buffer, bufferLen));
    }
    
    /**
     *
     * @param buffer
     * @param bufferLen
     * @return
     */
    protected String processResult(ByteBuffer buffer, IntByReference bufferLen){
        if (bufferLen.getValue() <= STR_BUFFER_LEN) {
            return fromUTF8(buffer, bufferLen);
        }

        if (bufferLen.getValue() > STR_BUFFER_LEN) {
            buffer = ByteBuffer.allocate(bufferLen.getValue());
            UltimoRetorno(buffer, bufferLen);
        }

        return fromUTF8(buffer, bufferLen);
    }
    
    /**
     *
     * @param buffer
     * @param bufferLen
     */
    protected abstract void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen);
}
