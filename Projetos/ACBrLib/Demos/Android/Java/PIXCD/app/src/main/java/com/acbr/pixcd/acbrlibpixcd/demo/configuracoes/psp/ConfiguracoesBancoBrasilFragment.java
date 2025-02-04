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
import android.widget.Spinner;
import android.widget.Toast;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ACBrLibHelper;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.SpinnerUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ConfiguracoesBancoBrasilFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtChavePIXBancoBrasil;
    private EditText txtClientIDBancoBrasil;
    private Spinner cmbBBAPIVersao;
    private EditText txtClientSecretBancoBrasil;
    private EditText txtDeveloperApplicationKeyBancoBrasil;
    private EditText txtArquivoChavePrivadaBancoBrasil;
    private Button btnArquivoChavePrivadaBancoBrasil;
    private EditText txtArquivoCertificadoBancoBrasil;
    private Button btnArquivoCertificadoBancoBrasil;
    private EditText txtArquivoPXFBancoBrasil;
    private Button btnArquivoPXFBancoBrasil;
    private EditText txtSenhaPFXBancoBrasil;
    private EditText txtScopesBancoBrasil;
    private Button btnSalvarConfiguracoesBancoBrasil;
    private Button btnCarregarConfiguracoesBancoBrasil;

    private String[] BBAPIVersao = {"API Versão 1", "API Versão 2"};

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_bancobrasil, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtChavePIXBancoBrasil = view.findViewById(R.id.txtChavePIXBancoBrasil);
        txtClientIDBancoBrasil = view.findViewById(R.id.txtClientIDBancoBrasil);
        cmbBBAPIVersao = view.findViewById(R.id.cmbBBAPIVersao);
        txtClientSecretBancoBrasil = view.findViewById(R.id.txtClientSecretBancoBrasil);
        txtDeveloperApplicationKeyBancoBrasil = view.findViewById(R.id.txtDeveloperApplicationKeyBancoBrasil);
        txtArquivoChavePrivadaBancoBrasil = view.findViewById(R.id.txtArquivoChavePrivadaBancoBrasil);
        btnArquivoChavePrivadaBancoBrasil = view.findViewById(R.id.btnArquivoChavePrivadaBancoBrasil);
        txtArquivoCertificadoBancoBrasil = view.findViewById(R.id.txtArquivoCertificadoBancoBrasil);
        btnArquivoCertificadoBancoBrasil = view.findViewById(R.id.btnArquivoCertificadoBancoBrasil);
        txtArquivoPXFBancoBrasil = view.findViewById(R.id.txtArquivoPXFBancoBrasil);
        btnArquivoPXFBancoBrasil = view.findViewById(R.id.btnArquivoPXFBancoBrasil);
        txtSenhaPFXBancoBrasil = view.findViewById(R.id.txtSenhaPFXBancoBrasil);
        txtScopesBancoBrasil = view.findViewById(R.id.txtScopesBancoBrasil);
        btnSalvarConfiguracoesBancoBrasil = view.findViewById(R.id.btnSalvarConfiguracoesBancoBrasil);
        btnCarregarConfiguracoesBancoBrasil = view.findViewById(R.id.btnCarregarConfiguracoesBancoBrasil);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        SpinnerUtils.preencherSpinner(getContext(), cmbBBAPIVersao, BBAPIVersao);

        btnArquivoChavePrivadaBancoBrasil.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 2);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarChavePrivadaBancoBrasil();
                }
            }
        });

        btnArquivoCertificadoBancoBrasil.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 3);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarArquivoCertificadoBancoBrasil();
                }
            }
        });

        btnArquivoPXFBancoBrasil.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 4);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarArquivoPFXBancoBrasil();
                }
            }
        });

        btnSalvarConfiguracoesBancoBrasil.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesBancoBrasil();
            }
        });

        btnCarregarConfiguracoesBancoBrasil.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesBancoBrasil();
            }
        });

        carregarConfiguracoesBancoBrasil();

        return view;
    }

    private void carregarChavePrivadaBancoBrasil(){
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/octet-stream"};  // .key files
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        startActivityForResult(Intent.createChooser(intent, "Selecione um arquivo KEY"), 2);
    }

    private void carregarArquivoCertificadoBancoBrasil(){
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/x-x509-ca-cert", "application/x-pem-file", "text/plain"};
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        startActivityForResult(Intent.createChooser(intent, "Selecione um arquivo PEM"), 3);
    }

    private void carregarArquivoPFXBancoBrasil(){
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/x-pkcs12"};  // Tipo MIME para arquivos .pfx
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        startActivityForResult(Intent.createChooser(intent, "Selecione um arquivo PFX"), 4);
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults){
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if (requestCode == 2 || requestCode == 3 || requestCode == 4) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                // Permissão concedida, abrir o seletor de arquivos
                if (requestCode == 2) {
                    carregarChavePrivadaBancoBrasil();
                } else if (requestCode == 3) {
                    carregarArquivoCertificadoBancoBrasil();
                } else {
                    carregarArquivoPFXBancoBrasil();
                }
            } else {
                // Permissão negada, notifique o usuário
                Toast.makeText(getContext(), "Permissão necessária para acessar arquivos.", Toast.LENGTH_SHORT).show();
            }
        }
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, @Nullable Intent data){
        super.onActivityResult(requestCode, resultCode, data);

        if (resultCode == Activity.RESULT_OK && data != null) {
            Uri uri = data.getData();
            if (uri != null) {
                try {
                    String savedPath = null;

                    if (requestCode == 2) {
                        // Salvar o arquivo .key
                        savedPath = saveFileFromUri(uri, "certificado.key");
                        txtArquivoChavePrivadaBancoBrasil.setText(savedPath);
                        Toast.makeText(getContext(), "Chave privada carregada com sucesso!", Toast.LENGTH_SHORT).show();
                    } else if (requestCode == 3) {
                        // Salvar o arquivo .pem
                        savedPath = saveFileFromUri(uri, "certificado.pem");
                        txtArquivoCertificadoBancoBrasil.setText(savedPath);
                        Toast.makeText(getContext(), "Certificado público carregado com sucesso!", Toast.LENGTH_SHORT).show();
                    } else if (requestCode == 4) {
                        // Salvar o arquivo .pfx
                        savedPath = saveFileFromUri(uri, "certificado.pfx");
                        txtArquivoPXFBancoBrasil.setText(savedPath);
                        Toast.makeText(getContext(), "Certificado PFX carregado com sucesso!", Toast.LENGTH_SHORT).show();
                    }

                    if (savedPath == null) {
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

    private void salvarConfiguracoesBancoBrasil(){
        try{
            ACBrPIXCD.configGravarValor("BancoBrasil", "ChavePIX", txtChavePIXBancoBrasil.getText().toString());
            ACBrPIXCD.configGravarValor("BancoBrasil", "ClientID", txtClientIDBancoBrasil.getText().toString());
            ACBrPIXCD.configGravarValor("BancoBrasil", "ClientSecret", txtClientSecretBancoBrasil.getText().toString());
            ACBrPIXCD.configGravarValor("BancoBrasil", "DeveloperApplicationKey", txtDeveloperApplicationKeyBancoBrasil.getText().toString());
            ACBrPIXCD.configGravarValor("BancoBrasil", "ArqChavePrivada", txtArquivoChavePrivadaBancoBrasil.getText().toString());
            ACBrPIXCD.configGravarValor("BancoBrasil", "ArqCertificado", txtArquivoCertificadoBancoBrasil.getText().toString());
            ACBrPIXCD.configGravarValor("BancoBrasil", "ArqPFX", txtArquivoPXFBancoBrasil.getText().toString());
            ACBrPIXCD.configGravarValor("BancoBrasil", "SenhaPFX", txtSenhaPFXBancoBrasil.getText().toString());
            ACBrPIXCD.configGravarValor("BancoBrasil", "BBAPIVersao", Integer.toString(cmbBBAPIVersao.getSelectedItemPosition()));
            ACBrPIXCD.configGravarValor("BancoBrasil", "Scopes", txtScopesBancoBrasil.getText().toString());
            ACBrPIXCD.configGravar();
        } catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações Banco do Brasil: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesBancoBrasil(){
        try {
            txtChavePIXBancoBrasil.setText(ACBrPIXCD.configLerValor("BancoBrasil", "ChavePIX"));
            txtClientIDBancoBrasil.setText(ACBrPIXCD.configLerValor("BancoBrasil", "ClientID"));
            txtClientSecretBancoBrasil.setText(ACBrPIXCD.configLerValor("BancoBrasil", "ClientSecret"));
            txtDeveloperApplicationKeyBancoBrasil.setText(ACBrPIXCD.configLerValor("BancoBrasil", "DeveloperApplicationKey"));
            txtArquivoChavePrivadaBancoBrasil.setText(ACBrPIXCD.configLerValor("BancoBrasil", "ArqChavePrivada"));
            txtArquivoCertificadoBancoBrasil.setText(ACBrPIXCD.configLerValor("BancoBrasil", "ArqCertificado"));
            txtArquivoPXFBancoBrasil.setText(ACBrPIXCD.configLerValor("BancoBrasil", "ArqPFX"));
            txtSenhaPFXBancoBrasil.setText(ACBrPIXCD.configLerValor("BancoBrasil", "SenhaPFX"));
            cmbBBAPIVersao.setSelection(Integer.parseInt(ACBrPIXCD.configLerValor("BancoBrasil", "BBAPIVersao")));
            txtScopesBancoBrasil.setText(ACBrPIXCD.configLerValor("BancoBrasil", "Scopes"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações Banco do Brasil: " + ex.getMessage());
        }
    }
}