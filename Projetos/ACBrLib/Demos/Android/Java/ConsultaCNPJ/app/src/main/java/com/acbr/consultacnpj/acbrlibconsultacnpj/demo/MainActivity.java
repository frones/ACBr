package com.acbr.consultacnpj.acbrlibconsultacnpj.demo;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.widget.Button;

import androidx.activity.EdgeToEdge;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;

import br.com.acbr.lib.comum.dfe.SSLCryptLib;
import br.com.acbr.lib.comum.dfe.SSLHttpLib;
import br.com.acbr.lib.comum.dfe.SSLXmlSignLib;
import br.com.acbr.lib.consultacnpj.ACBrLibConsultaCNPJ;

public class MainActivity extends AppCompatActivity {

    private ACBrLibConsultaCNPJ ACBrConsultaCNPJ;
    private ConsultaCNPJApplication application;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        Button btnConfiguracoes = findViewById(R.id.btnConfiguracoes);
        Button btnComandosConsultaCNPJ = findViewById(R.id.btnComandosConsultaCNPJ);

        application = ((ConsultaCNPJApplication) getApplicationContext());
        ACBrConsultaCNPJ = application.getAcBrLibConsultaCNPJ();
        configurarACBrConsultaCNPJ();

        btnConfiguracoes.setOnClickListener(view -> irParaTelaConfiguracoes());
        btnComandosConsultaCNPJ.setOnClickListener(view -> irParaTelaComandos());
    }

    private void irParaTelaComandos() {
        Intent intent = new Intent(MainActivity.this, ComandosConsultaCNPJActivity.class);
        startActivity(intent);
    }

    private void irParaTelaConfiguracoes() {
        Intent intent = new Intent(MainActivity.this, ConfiguracoesActivity.class);
        startActivity(intent);
    }

    private void configurarACBrConsultaCNPJ() {
        try {
            ACBrConsultaCNPJ.inicializar();
            aplicarConfiguracoesPadrao();
        } catch (Exception e) {
            Log.e("ACBrLibConsultaCNPJ", "Erro ao inicializar ACBrLibConsultaCNPJ", e);
        }
    }

    private void aplicarConfiguracoesPadrao() {
        try {
            ACBrConsultaCNPJ.configGravarValor("Principal", "LogPath", application.getLogPath());
            ACBrConsultaCNPJ.configGravarValor("Principal", "LogNivel", "4");
            ACBrConsultaCNPJ.configGravar();
            ACBrConsultaCNPJ.configLer(application.getArqConfigPath());
        } catch (Exception e) {
            Log.e("MainActivity", e.getMessage());
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        ACBrConsultaCNPJ.finalizar();
    }
}