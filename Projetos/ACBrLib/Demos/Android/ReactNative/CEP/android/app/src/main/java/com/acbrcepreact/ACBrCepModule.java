package com.acbrcepreact;
import com.facebook.react.bridge.NativeModule;
import com.facebook.react.bridge.ReactApplicationContext;
import com.facebook.react.bridge.ReactContextBaseJavaModule;
import com.facebook.react.bridge.ReactMethod;
import com.facebook.react.bridge.Promise;
import br.com.acbr.lib.cep.ACBrLibCep;
import com.acbrcepreact.MainApplication;


public class  ACBrCepModule extends ReactContextBaseJavaModule {
    private ACBrLibCep acbrlibCep;

    public ACBrCepModule(ReactApplicationContext reactContext){
        super(reactContext);
        var application = (MainApplication) reactContext.getApplicationContext();
        var memoryFile =
                "[Memory]\n" +
                "[Principal]\n" +
                "CodificacaoResposta=0\n"+
                "TipoResposta=0\n" +
                "LogPath="  +  application.getAppDir().getAbsolutePath() +  "\n" +
                "LogNivel=4\n" +
                "[CEP]\nWebService=10\nUsuario=\nSenha="
                ;

        acbrlibCep = new ACBrLibCep(memoryFile, "");
    }


    @Override
    public String getName(){
        return "ACBrCep";
    }


    @ReactMethod
    public void inicializar( Promise promise ){
        try {
            int result =  acbrlibCep.inicializar();
            promise.resolve(result);
        }catch(Exception e) {
            promise.reject("Error", e);
        }
    }

    @ReactMethod
    public void buscarPorCep(String cep, Promise promise){
        try {
            String result = acbrlibCep.buscarPorCep(cep);
            promise.resolve(result);
        }catch(Exception e){
            promise.reject("Error",e);
        }
    }

}