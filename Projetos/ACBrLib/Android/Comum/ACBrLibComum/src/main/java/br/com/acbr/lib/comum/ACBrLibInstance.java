package br.com.acbr.lib.comum;

import android.util.Log;
import com.sun.jna.Library;
import com.sun.jna.Native;

public class ACBrLibInstance {
    protected static Library acbrBridge;

    public static <T extends Library> Library getInstance(String libName, Class<T> interfaceClass){
        if ( acbrBridge == null){
            Log.i(libName,"getInstance()");
            acbrBridge =  (Library) Native.synchronizedLibrary((Library) Native.load(libName, interfaceClass));
            Log.i(libName, " Loaded: "+acbrBridge.toString());
        }
        return acbrBridge;
    }
}
