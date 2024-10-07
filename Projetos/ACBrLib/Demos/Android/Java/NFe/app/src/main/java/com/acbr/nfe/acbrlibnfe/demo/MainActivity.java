package com.acbr.nfe.acbrlibnfe.demo;

import android.Manifest;
import android.app.AlertDialog;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothManager;
import android.bluetooth.BluetoothSocket;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Set;
import java.util.UUID;

import br.com.acbr.lib.comum.dfe.SSLCryptLib;
import br.com.acbr.lib.comum.dfe.SSLHttpLib;
import br.com.acbr.lib.comum.dfe.SSLXmlSignLib;
import br.com.acbr.lib.nfe.ACBrLibNFe;
import br.com.acbr.lib.nfe.SSLType;

public class MainActivity extends AppCompatActivity {


    private ACBrLibNFe ACBrNFe;
    private NfeApplication application;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        Button btnConfiguracoes = findViewById(R.id.btnConfiguracoes);
        Button btnComandosNFe = findViewById(R.id.btnComandosNFe);

        // Verificar e solicitar permissões Bluetooth


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