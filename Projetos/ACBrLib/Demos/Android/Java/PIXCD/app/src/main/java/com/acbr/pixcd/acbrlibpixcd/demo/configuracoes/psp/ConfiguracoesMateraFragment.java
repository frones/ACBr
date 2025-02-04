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

public class ConfiguracoesMateraFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtChavePIXMatera;
    private EditText txtClientIDMatera;
    private EditText txtClientSecretMatera;
    private EditText txtSecretKeyMatera;
    private EditText txtAccountIDMatera;
    private EditText txtMediatorFeeMatera;
    private EditText txtArquivoChavePrivadaMatera;
    private Button btnArquivoChavePrivadaMatera;
    private EditText txtArquivoCertificadoMatera;
    private Button btnArquivoCertificadoMatera;
    private EditText txtScopesMatera;
    private Button btnSalvarConfiguracoesMatera;
    private Button btnCarregarConfiguracoesMatera;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_matera, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtChavePIXMatera = view.findViewById(R.id.txtChavePIXMatera);
        txtClientIDMatera = view.findViewById(R.id.txtClientIDMatera);
        txtClientSecretMatera = view.findViewById(R.id.txtClientSecretMatera);
        txtSecretKeyMatera = view.findViewById(R.id.txtSecretKeyMatera);
        txtAccountIDMatera = view.findViewById(R.id.txtAccountIDMatera);
        txtMediatorFeeMatera = view.findViewById(R.id.txtMediatorFeeMatera);
        txtArquivoChavePrivadaMatera = view.findViewById(R.id.txtArquivoChavePrivadaMatera);
        btnArquivoChavePrivadaMatera = view.findViewById(R.id.btnArquivoChavePrivadaMatera);
        txtArquivoCertificadoMatera = view.findViewById(R.id.txtArquivoCertificadoMatera);
        btnArquivoCertificadoMatera = view.findViewById(R.id.btnArquivoCertificadoMatera);
        txtScopesMatera = view.findViewById(R.id.txtScopesMatera);
        btnSalvarConfiguracoesMatera = view.findViewById(R.id.btnSalvarConfiguracoesMatera);
        btnCarregarConfiguracoesMatera = view.findViewById(R.id.btnCarregarConfiguracoesMatera);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnArquivoChavePrivadaMatera.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 2);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarChavePrivadaMatera();
                }
            }
        });

        btnArquivoCertificadoMatera.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 3);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarArquivoCertificadoMatera();
                }
            }
        });

        btnSalvarConfiguracoesMatera.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesMatera();
            }
        });

        btnCarregarConfiguracoesMatera.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesMatera();
            }
        });

        carregarConfiguracoesMatera();

        return view;
    }

    private void carregarChavePrivadaMatera(){
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/octet-stream"};  // .key files
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        startActivityForResult(Intent.createChooser(intent, "Selecione um arquivo KEY"), 2);
    }

    private void carregarArquivoCertificadoMatera(){
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
                    carregarChavePrivadaMatera();
                } else {
                    carregarArquivoCertificadoMatera();
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
                        txtArquivoChavePrivadaMatera.setText(savedPath);
                        Toast.makeText(getContext(), "Chave privada carregada com sucesso!", Toast.LENGTH_SHORT).show();
                    } else if (requestCode == 3) {
                        // Salvar o arquivo .cer
                        savedPath = saveFileFromUri(uri, "certificado.pem");
                        txtArquivoCertificadoMatera.setText(savedPath);
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

    private void salvarConfiguracoesMatera(){
        try{
            ACBrPIXCD.configGravarValor("Matera", "ChavePIX", txtChavePIXMatera.getText().toString());
            ACBrPIXCD.configGravarValor("Matera", "ClientID", txtClientIDMatera.getText().toString());
            ACBrPIXCD.configGravarValor("Matera", "SecretKey", txtSecretKeyMatera.getText().toString());
            ACBrPIXCD.configGravarValor("Matera", "ClientSecret", txtClientSecretMatera.getText().toString());
            ACBrPIXCD.configGravarValor("Matera", "ArqCertificado", txtArquivoCertificadoMatera.getText().toString());
            ACBrPIXCD.configGravarValor("Matera", "ArqChavePrivada", txtArquivoChavePrivadaMatera.getText().toString());
            ACBrPIXCD.configGravarValor("Matera", "AccountID", txtAccountIDMatera.getText().toString());
            ACBrPIXCD.configGravarValor("Matera", "MediatorFee", txtMediatorFeeMatera.getText().toString());
            ACBrPIXCD.configGravarValor("Matera", "Scopes", txtScopesMatera.getText().toString());
            ACBrPIXCD.configGravar();
        }catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações Matera: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesMatera(){
        try{
            txtChavePIXMatera.setText(ACBrPIXCD.configLerValor("Matera", "ChavePIX"));
            txtClientIDMatera.setText(ACBrPIXCD.configLerValor("Matera", "ClientID"));
            txtSecretKeyMatera.setText(ACBrPIXCD.configLerValor("Matera", "SecretKey"));
            txtClientSecretMatera.setText(ACBrPIXCD.configLerValor("Matera", "ClientSecret"));
            txtArquivoCertificadoMatera.setText(ACBrPIXCD.configLerValor("Matera", "ArqCertificado"));
            txtArquivoChavePrivadaMatera.setText(ACBrPIXCD.configLerValor("Matera", "ArqChavePrivada"));
            txtAccountIDMatera.setText(ACBrPIXCD.configLerValor("Matera", "AccountID"));
            txtMediatorFeeMatera.setText(ACBrPIXCD.configLerValor("Matera", "MediatorFee"));
            txtScopesMatera.setText(ACBrPIXCD.configLerValor("Matera", "Scopes"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações Matera: " + ex.getMessage());
        }
    }
}