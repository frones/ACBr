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

public class ConfiguracoesCieloFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtChavePIXCielo;
    private EditText txtClientIDCielo;
    private EditText txtClientSecretCielo;
    private EditText txtArquivoChavePrivadaCielo;
    private Button btnArquivoChavePrivadaCielo;
    private EditText txtArquivoCertificadoCielo;
    private Button btnArquivoCertificadoCielo;
    private EditText txtScopesCielo;
    private Button btnSalvarConfiguracoesCielo;
    private Button btnCarregarConfiguracoesCielo;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_cielo, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtChavePIXCielo = view.findViewById(R.id.txtChavePIXCielo);
        txtClientIDCielo = view.findViewById(R.id.txtClientIDCielo);
        txtClientSecretCielo = view.findViewById(R.id.txtClientSecretCielo);
        txtArquivoChavePrivadaCielo = view.findViewById(R.id.txtArquivoChavePrivadaCielo);
        btnArquivoChavePrivadaCielo = view.findViewById(R.id.btnArquivoChavePrivadaCielo);
        txtArquivoCertificadoCielo = view.findViewById(R.id.txtArquivoCertificadoCielo);
        btnArquivoCertificadoCielo = view.findViewById(R.id.btnArquivoCertificadoCielo);
        txtScopesCielo = view.findViewById(R.id.txtScopesCielo);
        btnSalvarConfiguracoesCielo = view.findViewById(R.id.btnSalvarConfiguracoesCielo);
        btnCarregarConfiguracoesCielo = view.findViewById(R.id.btnCarregarConfiguracoesCielo);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnArquivoChavePrivadaCielo.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 2);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarChavePrivadaCielo();
                }
            }
        });

        btnArquivoCertificadoCielo.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 3);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarArquivoCertificadoCielo();
                }
            }
        });

        btnSalvarConfiguracoesCielo.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesCielo();
            }
        });

        btnCarregarConfiguracoesCielo.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesCielo();
            }
        });

        carregarConfiguracoesCielo();

        return view;
    }

    private void carregarChavePrivadaCielo(){
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/octet-stream"};  // .key files
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        startActivityForResult(Intent.createChooser(intent, "Selecione um arquivo KEY"), 2);
    }

    private void carregarArquivoCertificadoCielo(){
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/x-x509-ca-cert"};  // .cer files
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        startActivityForResult(Intent.createChooser(intent, "Selecione um arquivo CER"), 3);
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if (requestCode == 2 || requestCode == 3) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                // Permissão concedida, abrir o seletor de arquivos
                if (requestCode == 2) {
                    carregarChavePrivadaCielo();
                } else {
                    carregarArquivoCertificadoCielo();
                }
            } else {
                // Permissão negada, notifique o usuário
                Toast.makeText(getContext(), "Permissão necessária para acessar arquivos.", Toast.LENGTH_SHORT).show();
            }
        }
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);

        if (resultCode == Activity.RESULT_OK && data != null) {
            Uri uri = data.getData();
            if (uri != null) {
                try {
                    String savedPath = null;

                    if (requestCode == 2) {
                        // Salvar o arquivo .key
                        savedPath = saveFileFromUri(uri, "certificado.key");
                        txtArquivoChavePrivadaCielo.setText(savedPath);
                        Toast.makeText(getContext(), "Chave privada carregada com sucesso!", Toast.LENGTH_SHORT).show();
                    } else if (requestCode == 3) {
                        // Salvar o arquivo .cer
                        savedPath = saveFileFromUri(uri, "certificado.cer");
                        txtArquivoCertificadoCielo.setText(savedPath);
                        Toast.makeText(getContext(), "Certificado público carregado com sucesso!", Toast.LENGTH_SHORT).show();
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

    private void salvarConfiguracoesCielo(){
        try{
            ACBrPIXCD.configGravarValor("Cielo", "ChavePIX", txtChavePIXCielo.getText().toString());
            ACBrPIXCD.configGravarValor("Cielo", "ClientID", txtClientIDCielo.getText().toString());
            ACBrPIXCD.configGravarValor("Cielo", "ClientSecret", txtClientSecretCielo.getText().toString());
            ACBrPIXCD.configGravarValor("Cielo", "ArqChavePrivada", txtArquivoChavePrivadaCielo.getText().toString());
            ACBrPIXCD.configGravarValor("Cielo", "ArqCertificado", txtArquivoCertificadoCielo.getText().toString());
            ACBrPIXCD.configGravarValor("Cielo", "Scopes", txtScopesCielo.getText().toString());
            ACBrPIXCD.configGravar();
        }catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações Cielo: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesCielo(){
        try{
            txtChavePIXCielo.setText(ACBrPIXCD.configLerValor("Cielo", "ChavePIX"));
            txtClientIDCielo.setText(ACBrPIXCD.configLerValor("Cielo", "ClientID"));
            txtClientSecretCielo.setText(ACBrPIXCD.configLerValor("Cielo", "ClientSecret"));
            txtArquivoChavePrivadaCielo.setText(ACBrPIXCD.configLerValor("Cielo", "ArqChavePrivada"));
            txtArquivoCertificadoCielo.setText(ACBrPIXCD.configLerValor("Cielo", "ArqCertificado"));
            txtScopesCielo.setText(ACBrPIXCD.configLerValor("Cielo", "Scopes"));
            ACBrPIXCD.configGravar();
        }catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações Cielo: " + ex.getMessage());
        }
    }
}