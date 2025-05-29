package com.example.demoacbrbal;

import androidx.annotation.NonNull;

import java.io.File;

import android.content.Context;
import android.util.Log;

import br.com.acbr.lib.bal.ACBrLibBAL;
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
        arquivoConfig = appDir.getAbsolutePath() + "/acbrlib.ini";

        channel = new MethodChannel(binding.getBinaryMessenger(), "acbrlib_bal_flutter");
        channel.setMethodCallHandler(this);
        acbrBal = new ACBrLibBAL(arquivoConfig, "");
    }

    @Override
    public void onDetachedFromEngine(@NonNull FlutterPluginBinding binding) {
        try {
            acbrBal.desativar();
            acbrBal.finalizar();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            channel.setMethodCallHandler(null);
        }
    }

    @Override
    public void onMethodCall(@NonNull MethodCall call, @NonNull MethodChannel.Result result) {
        switch (call.method) {
            case "inicializar":
                handleInicializar(result);
                break;
            case "ativar":
                handleAtivar(result);
                break;
            case "desativar":
                handleDesativar(result);
                break;
            case "finalizar":
                handleFinalizar(result);
                break;
            case "lePeso":
                handleLePeso(call, result);
                break;
            case "lePesoStr":
                handleLePesoStr(call, result);
                break;
            case "ultimoPesoLido":
                handleUltimoPesoLido(result);
                break;
            case "ultimoPesoLidoStr":
                handleUltimoPesoLidoStr(result);
                break;
            case "interpretarRespostaPeso":
                handleInterpretarRespostaPeso(call, result);
                break;
            case "interpretarRespotaPesoStr":
                handleInterpretarRespostaPesoStr(call, result);
                break;
            case "configLer":
                handleConfigLer(call, result);
                break;
            case "configGravar":
                handleConfigGravar(result);
                break;

            case "configGravarValor":
                handleConfigGravarValor(call, result);
                break;

            case "configLerValor":
                handleConfigLerValor(call, result);
                break;

            case "configExportar":
                handleConfigExportar(result);
                break;
            default:
                result.notImplemented();
                break;
        }
    }

    private void handleInicializar(MethodChannel.Result result) {
        int status = -1;
        try {
            status = acbrBal.inicializar();
            aplicaConfiguracoesPadrao();
            result.success(status);
        } catch (Exception e) {
            result.error("Erro ao inicializar ACBrLibBAL", e.getMessage(), e);
        }
    }

    private void handleAtivar(MethodChannel.Result result) {
        try {
            acbrBal.ativar();
            result.success(true);
        } catch (Exception e) {
            result.error("Erro ao ativar ACBrLibBAL", e.getMessage(), e);
        }
    }

    private void handleDesativar(MethodChannel.Result result) {
        try {
            acbrBal.desativar();
            result.success(false);
        } catch (Exception e) {
            result.error("Erro ao desativar ACBrLibBAL", e.getMessage(), e);
        }
    }

    private void handleFinalizar(MethodChannel.Result result) {
        try {
            int status = acbrBal.finalizar();
            result.success(status);
        } catch (Exception e) {
            result.error("Erro ao finalizar ACBrLibBAL", e.getMessage(), e);
        }
    }

    private void handleLePeso(MethodCall call, MethodChannel.Result result) {
        try {
            double peso = acbrBal.lePeso(call.argument("millisecTimeOut"));
            result.success(peso);
        } catch (Exception e) {
            result.error("Erro ao ler peso", e.getMessage(), e);
        }
    }

    private void handleLePesoStr(MethodCall call, MethodChannel.Result result) {
        try {
            String peso = acbrBal.lePesoStr(call.argument("millisecTimeOut"));
            Log.i("ACBrBalAarPlugin", "Peso: " + peso);
            result.success(peso);
        } catch (Exception e) {
            result.error("Erro ao ler peso", e.getMessage(), e);
        }
    }

    private void handleUltimoPesoLido(MethodChannel.Result result) {
        try {
            double peso = acbrBal.ultimoPesoLido();
            result.success(peso);
        } catch (Exception e) {
            result.error("Erro ao ler peso", e.getMessage(), e);
        }
    }

    private void handleUltimoPesoLidoStr(MethodChannel.Result result) {
        try {
            String peso = acbrBal.ultimoPesoLidoStr();
            result.success(peso);
        } catch (Exception e) {
            result.error("Erro ao ler peso", e.getMessage(), e);
        }
    }

    private void handleInterpretarRespostaPeso(MethodCall call, MethodChannel.Result result) {
        try {
            double peso = acbrBal.interpretarRespostaPeso(call.argument("aResposta"));
            result.success(peso);
        } catch (Exception e) {
            result.error("Erro ao interpretar resposta de peso", e.getMessage(), e);
        }
    }

    private void handleInterpretarRespostaPesoStr(MethodCall call, MethodChannel.Result result) {
        try {
            String peso = acbrBal.interpretarRespostaPesoStr(call.argument("aResposta"));
            result.success(peso);
        } catch (Exception e) {
            result.error("Erro ao interpretar resposta de peso", e.getMessage(), e);
        }
    }

    private void handleConfigLer(MethodCall call, MethodChannel.Result result) {
        try {
            int status = acbrBal.configLer(call.argument("eArqConfig"));
            result.success(status);
        } catch (Exception e) {
            result.error("Erro ao ler configuração", e.getMessage(), e);
        }
    }

    private void handleConfigGravar(MethodChannel.Result result) {
        try {
            int status = acbrBal.configGravar();
            result.success(status);
        } catch (Exception e) {
            result.error("Erro ao gravar configuração", e.getMessage(), e);
        }
    }

    private void handleConfigLerValor(MethodCall call, MethodChannel.Result result) {
        try {
            String valor = acbrBal.configLerValor(call.argument("sessao"), call.argument("chave"));
            result.success(valor);
        } catch (Exception e) {
            result.error("Erro ao ler valor de configuração", e.getMessage(), e);
        }
    }

    private void handleConfigGravarValor(MethodCall call, MethodChannel.Result result) {
        try {
            int status = acbrBal.configGravarValor(call.argument("sessao"), call.argument("chave"), call.argument("valor"));
            result.success(status);
        } catch (Exception e) {
            result.error("Erro ao gravar valor de configuração", e.getMessage(), e);
        }
    }


    private void aplicaConfiguracoesPadrao() throws Exception {
        acbrBal.configGravarValor("Principal", "LogPath", appDir.getAbsolutePath());
        acbrBal.configGravar();
    }

    private void handleConfigExportar(MethodChannel.Result result) {
        try {
            String resultStr = acbrBal.configExportar();
            result.success(resultStr);
        } catch (Exception e) {
            result.error("Erro ao exportar configuração", e.getMessage(), e);
        }
    }

}