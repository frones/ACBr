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
     * Função para processar o resultado do retorno da biblioteca.
     * @param buffer
     * @param bufferLen
     * @return
     */
    protected String processResult(ByteBuffer buffer, IntByReference bufferLen){
        int bLen = bufferLen.getValue();
        if (bLen <= STR_BUFFER_LEN) {
            return fromUTF8(buffer, bufferLen);
        }

        if (bLen > STR_BUFFER_LEN) {
            buffer = ByteBuffer.allocate(bLen);
            bufferLen = new IntByReference(bLen);
            UltimoRetorno(buffer, bufferLen);
        }

        return fromUTF8(buffer, bufferLen);
    }
    
    /**
     * Função para pegar o ultimo retorno da biblioteca caso o retorno seja 
     * maior que o esperado, ou tenha ocorrido um erro.
     * @param buffer
     * @param bufferLen
     */
    protected abstract void UltimoRetorno(ByteBuffer buffer, IntByReference bufferLen);
}
