package com.acbr.pixcd.acbrlibpixcd.demo.configuracoes.psp;

import android.Manifest;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ACBrLibHelper;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ConfiguracoesGerenciaNetFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtChavePIXGerenciaNet;
    private EditText txtClientIDGerenciaNet;
    private EditText txtClientSecretGerenciaNet;
    private EditText txtArquivoCertificadoGerenciaNet;
    private Button btnArquivoCertificadoGerenciaNet;
    private EditText txtScopesGerenciaNet;
    private Button btnSalvarConfiguracoesGerenciaNet;
    private Button btnCarregarConfiguracoesGerenciaNet;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_gerencianet, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtChavePIXGerenciaNet = view.findViewById(R.id.txtChavePIXGerenciaNet);
        txtClientIDGerenciaNet = view.findViewById(R.id.txtClientIDGerenciaNet);
        txtClientSecretGerenciaNet = view.findViewById(R.id.txtClientSecretGerenciaNet);
        txtArquivoCertificadoGerenciaNet = view.findViewById(R.id.txtArquivoCertificadoGerenciaNet);
        btnArquivoCertificadoGerenciaNet = view.findViewById(R.id.btnArquivoCertificadoGerenciaNet);
        txtScopesGerenciaNet = view.findViewById(R.id.txtScopesGerenciaNet);
        btnSalvarConfiguracoesGerenciaNet = view.findViewById(R.id.btnSalvarConfiguracoesGerenciaNet);
        btnCarregarConfiguracoesGerenciaNet = view.findViewById(R.id.btnCarregarConfiguracoesGerenciaNet);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnArquivoCertificadoGerenciaNet.setOnClickListener(new View.OnClickListener(){
            @Override
            public void onClick(View v){
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 2);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarCertificadoGerenciaNet();
                }
            }
        });

        btnSalvarConfiguracoesGerenciaNet.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesGerenciaNet();
            }
        });

        btnCarregarConfiguracoesGerenciaNet.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesGerenciaNet();
            }
        });

        carregarConfiguracoesGerenciaNet();

        return view;
    }

    private void carregarCertificadoGerenciaNet(){
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/x-pkcs12"};
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        startActivityForResult(Intent.createChooser(intent, "Selecione um arquivo p12"), 1);
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if (requestCode == 2) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                // Permissão concedida, abrir o seletor de arquivos
                carregarCertificadoGerenciaNet();
            } else {
                // Permissão negada, notifique o usuário
                Toast.makeText(getContext(), "Permissão necessária para acessar arquivos.", Toast.LENGTH_SHORT).show();
            }
        }
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, @Nullable Intent data){
        super.onActivityResult(requestCode, resultCode, data);

        if (requestCode == 1 && resultCode == Activity.RESULT_OK && data != null) {
            Uri uri = data.getData();
            if (uri != null) {
                try {
                    // Salvar o arquivo no diretório interno do aplicativo
                    String savedPath = saveFileFromUri(uri, "certificado.p12");
                    if (savedPath != null) {
                        txtArquivoCertificadoGerenciaNet.setText(savedPath);
                    } else {
                        Toast.makeText(getContext(), "Erro ao salvar o arquivo.", Toast.LENGTH_SHORT).show();
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                    Toast.makeText(getContext(), "Erro ao processar o arquivo: " + e.getMessage(), Toast.LENGTH_SHORT).show();
                }
            }
        }
    }

    private String saveFileFromUri(Uri uri, String fileName) throws IOException {
        // Diretório interno do aplicativo
        File directory = new File(getContext().getFilesDir(), "certificados");
        if (!directory.exists()) {
            directory.mkdirs();
        }

        File file = new File(directory, fileName);

        try (InputStream inputStream = getContext().getContentResolver().openInputStream(uri);
             FileOutputStream outputStream = new FileOutputStream(file)) {

            byte[] buffer = new byte[1024];
            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
            }
        }
        // Retorna o caminho absoluto do arquivo salvo
        return file.getAbsolutePath();
    }

    private void salvarConfiguracoesGerenciaNet(){
        try{
            ACBrPIXCD.configGravarValor("GerenciaNet", "ChavePIX", txtChavePIXGerenciaNet.getText().toString());
            ACBrPIXCD.configGravarValor("GerenciaNet", "ClientID", txtClientIDGerenciaNet.getText().toString());
            ACBrPIXCD.configGravarValor("GerenciaNet", "ClientSecret", txtClientSecretGerenciaNet.getText().toString());
            ACBrPIXCD.configGravarValor("GerenciaNet", "ArqPFX", txtArquivoCertificadoGerenciaNet.getText().toString());
            ACBrPIXCD.configGravarValor("GerenciaNet", "Scopes", txtScopesGerenciaNet.getText().toString());
            ACBrPIXCD.configGravar();
        }catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações GerenciaNet: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesGerenciaNet(){
        try{
            txtChavePIXGerenciaNet.setText(ACBrPIXCD.configLerValor("GerenciaNet", "ChavePIX"));
            txtClientIDGerenciaNet.setText(ACBrPIXCD.configLerValor("GerenciaNet", "ClientID"));
            txtClientSecretGerenciaNet.setText(ACBrPIXCD.configLerValor("GerenciaNet", "ClientSecret"));
            txtArquivoCertificadoGerenciaNet.setText(ACBrPIXCD.configLerValor("GerenciaNet", "ArqPFX"));
            txtScopesGerenciaNet.setText(ACBrPIXCD.configLerValor("GerenciaNet", "Scopes"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações GerenciaNet: " + ex.getMessage());
        }
    }
}