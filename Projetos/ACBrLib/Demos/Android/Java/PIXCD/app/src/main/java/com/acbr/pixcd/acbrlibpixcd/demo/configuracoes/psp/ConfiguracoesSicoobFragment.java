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

public class ConfiguracoesSicoobFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtChavePIXSicoob;
    private EditText txtClientIDSicoob;
    private EditText txtTokenSandboxSicoob;
    private EditText txtArquivoChavePrivadaSicoob;
    private Button btnArquivoChavePrivadaSicoob;
    private EditText txtArquivoCertificadoSicoob;
    private Button btnArquivoCertificadoSicoob;
    private EditText txtScopesSicoob;
    private Button btnSalvarConfiguracoesSicoob;
    private Button btnCarregarConfiguracoesSicoob;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_sicoob, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtChavePIXSicoob = view.findViewById(R.id.txtChavePIXSicoob);
        txtClientIDSicoob = view.findViewById(R.id.txtClientIDSicoob);
        txtTokenSandboxSicoob = view.findViewById(R.id.txtTokenSandboxSicoob);
        txtArquivoChavePrivadaSicoob = view.findViewById(R.id.txtArquivoChavePrivadaSicoob);
        btnArquivoChavePrivadaSicoob = view.findViewById(R.id.btnArquivoChavePrivadaSicoob);
        txtArquivoCertificadoSicoob = view.findViewById(R.id.txtArquivoCertificadoSicoob);
        btnArquivoCertificadoSicoob = view.findViewById(R.id.btnArquivoCertificadoSicoob);
        txtScopesSicoob = view.findViewById(R.id.txtScopesSicoob);
        btnSalvarConfiguracoesSicoob = view.findViewById(R.id.btnSalvarConfiguracoesSicoob);
        btnCarregarConfiguracoesSicoob = view.findViewById(R.id.btnCarregarConfiguracoesSicoob);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnArquivoChavePrivadaSicoob.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 2);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarChavePrivadaSicoob();
                }
            }
        });

        btnArquivoCertificadoSicoob.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 3);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarArquivoCertificadoSicoob();
                }
            }
        });

        btnSalvarConfiguracoesSicoob.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesSicoob();
            }
        });

        btnCarregarConfiguracoesSicoob.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesSicoob();
            }
        });

        carregarConfiguracoesSicoob();

        return view;
    }

    private void carregarChavePrivadaSicoob(){
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/octet-stream"};  // .key files
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        startActivityForResult(Intent.createChooser(intent, "Selecione um arquivo KEY"), 2);
    }

    private void carregarArquivoCertificadoSicoob(){
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/x-x509-ca-cert", "application/x-pem-file", "text/plain"};
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        startActivityForResult(Intent.createChooser(intent, "Selecione um arquivo PEM"), 3);
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults){
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if (requestCode == 2 || requestCode == 3) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                // Permissão concedida, abrir o seletor de arquivos
                if (requestCode == 2) {
                    carregarChavePrivadaSicoob();
                } else {
                    carregarArquivoCertificadoSicoob();
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
                        txtArquivoChavePrivadaSicoob.setText(savedPath);
                        Toast.makeText(getContext(), "Chave privada carregada com sucesso!", Toast.LENGTH_SHORT).show();
                    } else if (requestCode == 3) {
                        // Salvar o arquivo .cer
                        savedPath = saveFileFromUri(uri, "certificado.pem");
                        txtArquivoCertificadoSicoob.setText(savedPath);
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

    private void salvarConfiguracoesSicoob(){
        try{
            ACBrPIXCD.configGravarValor("Sicoob", "ChavePIX", txtChavePIXSicoob.getText().toString());
            ACBrPIXCD.configGravarValor("Sicoob", "ClientID", txtClientIDSicoob.getText().toString());
            ACBrPIXCD.configGravarValor("Sicoob", "TokenSandbox", txtTokenSandboxSicoob.getText().toString());
            ACBrPIXCD.configGravarValor("Sicoob", "ArqChavePrivada", txtArquivoChavePrivadaSicoob.getText().toString());
            ACBrPIXCD.configGravarValor("Sicoob", "ArqCertificado", txtArquivoCertificadoSicoob.getText().toString());
            ACBrPIXCD.configGravarValor("Sicoob", "Scopes", txtScopesSicoob.getText().toString());
            ACBrPIXCD.configGravar();
        }catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações Sicoob: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesSicoob(){
        try{
            txtChavePIXSicoob.setText(ACBrPIXCD.configLerValor("Sicoob", "ChavePIX"));
            txtClientIDSicoob.setText(ACBrPIXCD.configLerValor("Sicoob", "ClientID"));
            txtTokenSandboxSicoob.setText(ACBrPIXCD.configLerValor("Sicoob", "TokenSandbox"));
            txtArquivoChavePrivadaSicoob.setText(ACBrPIXCD.configLerValor("Sicoob", "ArqChavePrivada"));
            txtArquivoCertificadoSicoob.setText(ACBrPIXCD.configLerValor("Sicoob", "ArqCertificado"));
            txtScopesSicoob.setText(ACBrPIXCD.configLerValor("Sicoob", "Scopes"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações Sicoob: " + ex.getMessage());
        }
    }
}