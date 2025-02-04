package com.acbr.pixcd.acbrlibpixcd.demo;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.widget.Button;

import androidx.appcompat.app.AppCompatActivity;

import com.acbr.pixcd.acbrlibpixcd.demo.comandos.ComandosPIXCDActivity;
import com.acbr.pixcd.acbrlibpixcd.demo.configuracoes.ConfiguracoesActivity;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class MainActivity extends AppCompatActivity {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        Button btnComandosPIXCD = findViewById(R.id.btnComandosPIXCD);
        Button btnConfiguracoes = findViewById(R.id.btnConfiguracoes);

        application = ((PIXCDApplication) getApplicationContext());
        ACBrPIXCD = application.getACBrLibPIXCD();
        configurarACBrPIXCD();

        btnComandosPIXCD.setOnClickListener(view -> IrParaTelaComandosPIXCD());
        btnConfiguracoes.setOnClickListener(view -> IrParaTelaConfiguracoes());
    }

    private void IrParaTelaComandosPIXCD(){
        Intent intent = new Intent(this, ComandosPIXCDActivity.class);
        startActivity(intent);
    }

    private void IrParaTelaConfiguracoes(){
        Intent intent = new Intent(this, ConfiguracoesActivity.class);
        startActivity(intent);
    }

    private void configurarACBrPIXCD() {
        try {
            ACBrPIXCD.inicializar();
            aplicarConfiguracoesPadrao();
        } catch (Exception e) {
            Log.e("ACBrLibPIXCD", "Erro ao inicializar ACBrLibPIXCD", e);
        }
    }

    private void aplicarConfiguracoesPadrao(){
        try{
            ACBrPIXCD.configGravarValor("Principal", "LogPath", application.getLogPath());
            ACBrPIXCD.configGravarValor("Principal", "LogNivel", "4");
            ACBrPIXCD.configGravar();
            ACBrPIXCD.configLer(application.getArqConfigPath());
        } catch (Exception e){
            Log.e("MainActivity", e.getMessage());
        }
    }

    @Override
    protected void onDestroy(){
        super.onDestroy();
        ACBrPIXCD.finalizar();
    }
}