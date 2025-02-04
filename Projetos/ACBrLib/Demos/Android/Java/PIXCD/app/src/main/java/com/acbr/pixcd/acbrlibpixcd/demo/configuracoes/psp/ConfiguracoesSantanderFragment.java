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

public class ConfiguracoesSantanderFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtChavePIXSantander;
    private EditText txtConsumerKeySantander;
    private EditText txtConsumerSecretSantander;
    private EditText txtArquivoCertificadoPFXSantander;
    private Button btnArquivoCertificadoPFXSantander;
    private EditText txtSenhaCertificadoPFXSantander;
    private EditText txtScopesSantander;
    private Button btnSalvarConfiguracoesSantander;
    private Button btnCarregarConfiguracoesSantander;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_santander, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtChavePIXSantander = view.findViewById(R.id.txtChavePIXSantander);
        txtConsumerKeySantander = view.findViewById(R.id.txtConsumerKeySantander);
        txtConsumerSecretSantander = view.findViewById(R.id.txtConsumerSecretSantander);
        txtArquivoCertificadoPFXSantander = view.findViewById(R.id.txtArquivoCertificadoPFXSantander);
        btnArquivoCertificadoPFXSantander = view.findViewById(R.id.btnArquivoCertificadoPFXSantander);
        txtSenhaCertificadoPFXSantander = view.findViewById(R.id.txtSenhaCertificadoPFXSantander);
        txtScopesSantander = view.findViewById(R.id.txtScopesSantander);
        btnSalvarConfiguracoesSantander = view.findViewById(R.id.btnSalvarConfiguracoesSantander);
        btnCarregarConfiguracoesSantander = view.findViewById(R.id.btnCarregarConfiguracoesSantander);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnArquivoCertificadoPFXSantander.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 2);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarCertificadoSantander();
                }
            }
        });

        btnSalvarConfiguracoesSantander.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesSantander();
            }
        });

        btnCarregarConfiguracoesSantander.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesSantander();
            }
        });

        carregarConfiguracoesSantander();

        return view;
    }

    private void carregarCertificadoSantander(){
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/x-pkcs12"};
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        startActivityForResult(Intent.createChooser(intent, "Selecione um arquivo PFX"), 1);
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if (requestCode == 2) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                // Permissão concedida, abrir o seletor de arquivos
                carregarCertificadoSantander();
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
                    String savedPath = saveFileFromUri(uri, "certificado.pfx");
                    if (savedPath != null) {
                        txtArquivoCertificadoPFXSantander.setText(savedPath);
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

    private void salvarConfiguracoesSantander(){
        try {
            ACBrPIXCD.configGravarValor("Santander", "ChavePIX", txtChavePIXSantander.getText().toString());
            ACBrPIXCD.configGravarValor("Santander", "ConsumerKey", txtConsumerKeySantander.getText().toString());
            ACBrPIXCD.configGravarValor("Santander", "ConsumerSecret", txtConsumerSecretSantander.getText().toString());
            ACBrPIXCD.configGravarValor("Santander", "ArqCertificadoPFX", txtArquivoCertificadoPFXSantander.getText().toString());
            ACBrPIXCD.configGravarValor("Santander", "SenhaCertificadoPFX", txtSenhaCertificadoPFXSantander.getText().toString());
            ACBrPIXCD.configGravarValor("Santander", "Scopes", txtScopesSantander.getText().toString());
            ACBrPIXCD.configGravar();
        }catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações Santander: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesSantander(){
        try {
            txtChavePIXSantander.setText(ACBrPIXCD.configLerValor("Santander", "ChavePIX"));
            txtConsumerKeySantander.setText(ACBrPIXCD.configLerValor("Santander", "ConsumerKey"));
            txtConsumerSecretSantander.setText(ACBrPIXCD.configLerValor("Santander", "ConsumerSecret"));
            txtArquivoCertificadoPFXSantander.setText(ACBrPIXCD.configLerValor("Santander", "ArqCertificadoPFX"));
            txtSenhaCertificadoPFXSantander.setText(ACBrPIXCD.configLerValor("Santander", "SenhaCertificadoPFX"));
            txtScopesSantander.setText(ACBrPIXCD.configLerValor("Santander", "Scopes"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações Santander: " + ex.getMessage());
        }
    }
}