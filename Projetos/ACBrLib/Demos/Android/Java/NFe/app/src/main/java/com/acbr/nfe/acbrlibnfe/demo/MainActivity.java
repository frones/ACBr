package com.acbr.nfe.acbrlibnfe.demo;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.widget.Button;

import androidx.appcompat.app.AppCompatActivity;

import com.acbr.nfe.acbrlibnfe.demo.comandos.ComandosNFeActivity;
import com.acbr.nfe.acbrlibnfe.demo.configuracoes.ConfiguracoesActivity;
import com.acbr.nfe.acbrlibnfe.demo.utils.NfeApplication;

import br.com.acbr.lib.comum.dfe.SSLCryptLib;
import br.com.acbr.lib.comum.dfe.SSLHttpLib;
import br.com.acbr.lib.comum.dfe.SSLXmlSignLib;
import br.com.acbr.lib.nfe.ACBrLibNFe;



public class MainActivity extends AppCompatActivity {


    private ACBrLibNFe ACBrNFe;
    private NfeApplication application;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        Button btnConfiguracoes = findViewById(R.id.btnConfiguracoes);
        Button btnComandosNFe = findViewById(R.id.btnComandosNFe);
        
        application = ((NfeApplication) getApplicationContext());
        ACBrNFe = application.getAcBrLibNFe();
        configurarACBrNFe();

        btnConfiguracoes.setOnClickListener(view -> irParaTelaConfiguracoes());
        btnComandosNFe.setOnClickListener(view -> irParaTelaComandos());
    }

    private void irParaTelaComandos() {
        Intent intent = new Intent(MainActivity.this, ComandosNFeActivity.class);
        startActivity(intent);
    }

    private void irParaTelaConfiguracoes() {
        Intent intent = new Intent(MainActivity.this, ConfiguracoesActivity.class);
        startActivity(intent);
    }

    private void configurarACBrNFe() {
        try {
            ACBrNFe.inicializar();
            aplicarConfiguracoesPadrao();
        } catch (Exception e) {
            Log.e("ACBrLibNFe", "Erro ao inicializar ACBrLibNFe", e);
        }
    }

    private void aplicarConfiguracoesPadrao() {
        try {

            ACBrNFe.configGravarValor("NFe", "PathSchemas", application.getSchemasPath());
            ACBrNFe.configGravarValor("NFe", "PathSalvar", application.getPathSalvar());
            ACBrNFe.configGravarValor("Principal", "LogPath", application.getLogPath());
            ACBrNFe.configGravarValor("Principal", "LogNivel", "4");
            ACBrNFe.configGravarValor("DFe", "ArquivoPFX", application.getPfxPath());
            ACBrNFe.configGravarValor("DFe", "SSLCryptLib", Integer.toString(SSLCryptLib.cryOpenSSL.ordinal()));
            ACBrNFe.configGravarValor("DFe", "SSLHttpLib", Integer.toString(SSLHttpLib.httpOpenSSL.ordinal()));
            ACBrNFe.configGravarValor("DFe", "SSLXmlSignLib", Integer.toString(SSLXmlSignLib.xsLibXml2.ordinal()));
            ACBrNFe.configGravar();
            ACBrNFe.configLer(application.getArqConfigPath());
        } catch (Exception e) {
            Log.e("MainActivity", e.getMessage());
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        ACBrNFe.finalizar();
    }
}