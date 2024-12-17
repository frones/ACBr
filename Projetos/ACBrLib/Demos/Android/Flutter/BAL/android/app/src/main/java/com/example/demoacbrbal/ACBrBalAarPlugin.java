package com.example.demoacbrbal;

import androidx.annotation.NonNull;

import java.io.File;
import android.content.Context;
import android.util.Log;
import br.com.acbr.acbrlibbal.ACBrLibBAL;
import io.flutter.embedding.engine.plugins.FlutterPlugin;
import io.flutter.plugin.common.MethodCall;
import io.flutter.plugin.common.MethodChannel;

public class ACBrBalAarPlugin implements FlutterPlugin, MethodChannel.MethodCallHandler {
    private MethodChannel channel;
    private ACBrLibBAL acbrBal;
    private String arquivoConfig;
    private File appDir;

    @Override
    public void onAttachedToEngine(@NonNull FlutterPluginBinding binding) {
        Context context = binding.getApplicationContext();
        appDir = (File) context.getExternalFilesDir(null);
        arquivoConfig =  appDir.getAbsolutePath() + "/acbrlib.ini";

        channel = new MethodChannel(binding.getBinaryMessenger(), "com.example.demoacbrbal");
        channel.setMethodCallHandler(this);
        acbrBal = new ACBrLibBAL(arquivoConfig, "");
    }

    @Override
    public void onDetachedFromEngine(@NonNull FlutterPluginBinding binding) {
        channel.setMethodCallHandler(null);
        try {
            acbrBal.desativar();
            acbrBal.finalizar();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public void onMethodCall(@NonNull MethodCall call, @NonNull MethodChannel.Result result) {
        if (call.method.equals("inicializar")) {
            int status = -1;
            try {
                status = acbrBal.inicializar();
                aplicaConfiguracoesPadrao();
                result.success(status);
            } catch (Exception e) {
                result.error("Erro ao inicializar ACBrLibBAL", e.getMessage(), e);
            }
        }

        if (call.method.equals("ativar")) {
            try {
                acbrBal.ativar();
                result.success(true);
            } catch (Exception e) {
                result.error("Erro ao ativar ACBrLibBAL", e.getMessage(), e);
            }
        }

        if (call.method.equals("desativar")) {
            try {
                acbrBal.desativar();
                result.success(false);
            } catch (Exception e) {
                result.error("Erro ao desativar ACBrLibBAL", e.getMessage(), e);
            }
        }

        if ( call.method.equals("finalizar") ) {
            try {
               int status =  acbrBal.finalizar();
                result.success(status);
            } catch (Exception e) {
                result.error("Erro ao finalizar ACBrLibBAL", e.getMessage(), e);
            }
        }

        if (call.method.equals("lePeso")){
            try {
                double peso = acbrBal.lePeso(call.argument("millisecTimeOut"));
                result.success(peso);
            } catch (Exception e) {
                result.error("Erro ao ler peso", e.getMessage(), e);
            }
        }

        if (call.method.equals("lePesoStr")){
            try {
                String peso = acbrBal.lePesoStr(call.argument("millisecTimeOut"));
                Log.i("ACBrBalAarPlugin", "Peso: " + peso);
                result.success(peso);
            } catch (Exception e) {
                result.error("Erro ao ler peso", e.getMessage(), e);
            }
        }

        if (call.method.equals("ultimoPesoLido")){
            try {
                double peso = acbrBal.ultimoPesoLido();
                result.success(peso);
            } catch (Exception e) {
                result.error("Erro ao ler peso", e.getMessage(), e);
            }
        }

        if (call.method.equals("ultimoPesoLidoStr")){
            try {
                String peso = acbrBal.ultimoPesoLidoStr();
                result.success(peso);
            } catch (Exception e) {
                result.error("Erro ao ler peso", e.getMessage(), e);
            }
        }

        if ( call.method.equals("interpretarRespostaPeso")){
            try {
                double peso = acbrBal.interpretarRespostaPeso(call.argument("aResposta"));
                result.success(peso);
            } catch (Exception e) {
                result.error("Erro ao interpretar resposta de peso", e.getMessage(), e);
            }
        }

        if ( call.method.equals("interpretarRespotaPesoStr")){
            try {
                String peso = acbrBal.interpretarRespostaPesoStr(call.argument("aResposta"));
                result.success(peso);
            } catch (Exception e) {
                result.error("Erro ao interpretar resposta de peso", e.getMessage(), e);
            }
        }

        if ( call.method.equals("configLer")){
            try {
                int status = acbrBal.configLer(call.argument("eArqConfig"));
                result.success(status);
            } catch (Exception e) {
                result.error("Erro ao ler configuração", e.getMessage(), e);
            }
        }

        if ( call.method.equals("configGravar")){
            try {
                int status = acbrBal.configGravar();
                result.success(status);
            } catch (Exception e) {
                result.error("Erro ao gravar configuração", e.getMessage(), e);
            }
        }

        if ( call.method.equals("configLerValor")){
            try {
                String valor = acbrBal.configLerValor(call.argument("sessao"), call.argument("chave"));
                result.success(valor);
            } catch (Exception e) {
                result.error("Erro ao ler valor de configuração", e.getMessage(), e);
            }
        }
    }
    private void aplicaConfiguracoesPadrao() throws Exception {
        acbrBal.configGravarValor("Principal", "LogPath",appDir.getAbsolutePath());  
        acbrBal.configGravarValor("BAL", "Modelo", "2");
        acbrBal.configGravarValor("BAL", "Porta", "/dev/ttyUSER0");
        acbrBal.configGravarValor("BAL_Device", "Baud", "9600");
        acbrBal.configGravarValor("BAL_Device", "Data", "8");
        acbrBal.configGravarValor("BAL_Device", "Parity", "0");
        acbrBal.configGravarValor("BAL_Device", "Stop", "0");
        acbrBal.configGravarValor("BAL_Device", "HandShake", "0");
        acbrBal.configGravarValor("BAL_Device", "SoftFlow", "1");
        acbrBal.configGravarValor("BAL_Device", "HardFlow", "1");
        acbrBal.configGravar();
    }

}
