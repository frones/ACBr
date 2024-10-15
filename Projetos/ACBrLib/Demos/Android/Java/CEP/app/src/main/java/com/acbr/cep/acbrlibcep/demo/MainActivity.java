package com.acbr.cep.acbrlibcep.demo;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.widget.Button;

import androidx.activity.EdgeToEdge;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;

import br.com.acbr.lib.cep.ACBrLibCep;

public class MainActivity extends AppCompatActivity {

    private ACBrLibCep ACBrCEP;
    private CEPApplication application;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        Button btnConfiguracoes = findViewById(R.id.btnConfiguracoes);
        Button btnComandosCEP = findViewById(R.id.btnComandosCEP);

        application = ((CEPApplication) getApplication());
        ACBrCEP = application.getAcBrLibCEP();
        configurarACBrCEP();

        btnConfiguracoes.setOnClickListener(view -> irParaTelaConfiguracoes());
        btnComandosCEP.setOnClickListener(view -> irParaTelaComandos());
    }

    private void irParaTelaConfiguracoes() {
        Intent intent = new Intent(MainActivity.this, ConfiguracoesActivity.class);
        startActivity(intent);
    }

    private void irParaTelaComandos(){
        Intent intent = new Intent(MainActivity.this, ComandosCEPActivity.class);
        startActivity(intent);
    }

    private void configurarACBrCEP() {
        try {
            ACBrCEP.inicializar();
            aplicarConfiguracoesPadrao();
        } catch (Exception e) {
            Log.e("ACBrLibConsultaCNPJ", "Erro ao inicializar ACBrLibCEP", e);
        }
    }

    private void aplicarConfiguracoesPadrao() {
        try {
            ACBrCEP.configGravarValor("Principal", "LogPath", application.getLogPath());
            ACBrCEP.configGravarValor("Principal", "LogNivel", "4");
            ACBrCEP.configGravarValor("CEP", "SSLType", "5");
            ACBrCEP.configGravar();
            ACBrCEP.configLer(application.getArqConfigPath());
        } catch (Exception e) {
            Log.e("MainActivity", e.getMessage());
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        ACBrCEP.finalizar();
    }
}