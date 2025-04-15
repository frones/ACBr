package demo_acbrcep_flutter;

import androidx.annotation.NonNull;

import io.flutter.embedding.engine.plugins.FlutterPlugin;
import io.flutter.plugin.common.MethodCall;
import io.flutter.plugin.common.MethodChannel;
import br.com.acbr.lib.cep.ACBrLibCep;

import java.io.File;

import android.content.Context;
import android.util.Log;

public class ACBrCepAarPlugin implements FlutterPlugin, MethodChannel.MethodCallHandler {
    private MethodChannel channel;
    private ACBrLibCep acBrLibCep;
    private File appDir;
    private String arquivoConfig;

    @Override
    public void onAttachedToEngine(@NonNull FlutterPluginBinding binding) {
        Context context = binding.getApplicationContext();
        appDir = (File) context.getExternalFilesDir(null);
        arquivoConfig = appDir.getAbsolutePath() + "/acbrlib.ini";
        channel = new MethodChannel(binding.getBinaryMessenger(), "com.example.demo_acbrcep_flutter");
        channel.setMethodCallHandler((MethodChannel.MethodCallHandler) this);
        acBrLibCep = new ACBrLibCep(arquivoConfig, "");


    }


    @Override
    public void onDetachedFromEngine(@NonNull FlutterPluginBinding binding) {
        channel.setMethodCallHandler(null);
        acBrLibCep.finalizar();
    }


    private void handleInicializar(@NonNull MethodChannel.Result result) {
        try {
            acBrLibCep.inicializar();
            result.success("Inicializado com sucesso");
        } catch (Exception e) {
            result.error("Erro", e.getMessage(), e);
        }
    }

    private void handleFinalizar(@NonNull MethodChannel.Result result) {
        int status = acBrLibCep.finalizar();
        if (status == 0) {
            result.success("Finalizado com sucesso");
        } else {
            result.error("Erro", "Erro ao finalizar", status);
        }
    }

    private void handleBuscarPorCep(@NonNull MethodCall call, @NonNull MethodChannel.Result
            result) {
        String cep = call.argument("cep");
        try {
            String retorno = acBrLibCep.buscarPorCep(cep);
            result.success(retorno);
        } catch (Exception e) {
            result.error("Erro", e.getMessage(), e);
        }
    }

    private void handleConfigLer(MethodCall call, MethodChannel.Result result) {
        try {
            int status = acBrLibCep.configLer(call.argument("eArqConfig"));
            result.success(status);
        } catch (Exception e) {
            result.error("Erro ao ler configuração", e.getMessage(), e);
        }
    }

    private void handleConfigGravar(MethodChannel.Result result) {
        try {
            int status = acBrLibCep.configGravar();
            result.success(status);
        } catch (Exception e) {
            result.error("Erro ao gravar configuração", e.getMessage(), e);
        }
    }

    private void handleConfigLerValor(MethodCall call, MethodChannel.Result result) {
        try {
            String valor = acBrLibCep.configLerValor(call.argument("sessao"), call.argument("chave"));
            result.success(valor);
        } catch (Exception e) {
            result.error("Erro ao ler valor de configuração", e.getMessage(), e);
        }
    }

    private void handleConfigGravarValor(MethodCall call, MethodChannel.Result result) {
        try {
            int status = acBrLibCep.configGravarValor(call.argument("sessao"), call.argument("chave"), call.argument("valor"));
            result.success(status);
        } catch (Exception e) {
            result.error("Erro ao gravar valor de configuração", e.getMessage(), e);
        }
    }

    @Override
    public void onMethodCall(@NonNull MethodCall call, @NonNull MethodChannel.Result result) {
        switch (call.method) {
            case "inicializar":
                handleInicializar(result);
                break;
            case "finalizar":
                handleFinalizar(result);
                break;
            case "buscarPorCep":
                handleBuscarPorCep(call, result);
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
            default:
                result.notImplemented();
                break;
        }
    }
}
