package com.acbr.pixcd.acbrlibpixcd.demo.configuracoes.psp;

import android.Manifest;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.net.Uri;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;

import android.provider.MediaStore;
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

public class ConfiguracoesBradescoFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtChavePIXBradesco;
    private EditText txtClientIDBradesco;
    private EditText txtClientSecretBradesco;
    private EditText txtArquivoPFXBradesco;
    private Button btnCertificadoBradesco;
    private EditText txtSenhaCertificadoBradesco;
    private EditText txtScopesBradesco;
    private Button btnSalvarConfiguracoesBradesco;
    private Button btnCarregarConfiguracoesBradesco;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_bradesco, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtChavePIXBradesco = view.findViewById(R.id.txtChavePIXBradesco);
        txtClientIDBradesco = view.findViewById(R.id.txtClientIDBradesco);
        txtClientSecretBradesco = view.findViewById(R.id.txtClientSecretBradesco);
        txtArquivoPFXBradesco = view.findViewById(R.id.txtArquivoPFXBradesco);
        btnCertificadoBradesco = view.findViewById(R.id.btnCertificadoBradesco);
        txtSenhaCertificadoBradesco = view.findViewById(R.id.txtSenhaCertificadoBradesco);
        txtScopesBradesco = view.findViewById(R.id.txtScopesBradesco);
        btnSalvarConfiguracoesBradesco = view.findViewById(R.id.btnSalvarConfiguracoesBradesco);
        btnCarregarConfiguracoesBradesco = view.findViewById(R.id.btnCarregarConfiguracoesBradesco);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnCertificadoBradesco.setOnClickListener(new View.OnClickListener(){
            @Override
            public void onClick(View v){
                if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.READ_EXTERNAL_STORAGE)
                        != PackageManager.PERMISSION_GRANTED) {
                    // Solicitar permissão
                    ActivityCompat.requestPermissions(getActivity(),
                            new String[]{Manifest.permission.READ_EXTERNAL_STORAGE}, 2);
                } else {
                    // Permissão já concedida, abrir o seletor de arquivos
                    carregarCertificadoBradesco();
                }
            }
        });

        btnSalvarConfiguracoesBradesco.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesBradesco();
            }
        });

        btnCarregarConfiguracoesBradesco.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesBradesco();
            }
        });

        carregarConfiguracoesBradesco();

        return view;
    }

    private void carregarCertificadoBradesco(){
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
                carregarCertificadoBradesco();
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
                        txtArquivoPFXBradesco.setText(savedPath);
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

    private void salvarConfiguracoesBradesco(){
        try {
            ACBrPIXCD.configGravarValor("Bradesco", "ChavePIX", txtChavePIXBradesco.getText().toString());
            ACBrPIXCD.configGravarValor("Bradesco", "ClientID", txtClientIDBradesco.getText().toString());
            ACBrPIXCD.configGravarValor("Bradesco", "ClientSecret", txtClientSecretBradesco.getText().toString());
            ACBrPIXCD.configGravarValor("Bradesco", "ArqPFX", txtArquivoPFXBradesco.getText().toString());
            ACBrPIXCD.configGravarValor("Bradesco", "SenhaPFX", txtSenhaCertificadoBradesco.getText().toString());
            ACBrPIXCD.configGravarValor("Bradesco", "Scopes", txtScopesBradesco.getText().toString());
            ACBrPIXCD.configGravar();
        }catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações Bradesco: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesBradesco(){
        try {
            txtChavePIXBradesco.setText(ACBrPIXCD.configLerValor("Bradesco", "ChavePIX"));
            txtClientIDBradesco.setText(ACBrPIXCD.configLerValor("Bradesco", "ClientID"));
            txtClientSecretBradesco.setText(ACBrPIXCD.configLerValor("Bradesco", "ClientSecret"));
            txtArquivoPFXBradesco.setText(ACBrPIXCD.configLerValor("Bradesco", "ArqPFX"));
            txtSenhaCertificadoBradesco.setText(ACBrPIXCD.configLerValor("Bradesco", "SenhaPFX"));
            txtScopesBradesco.setText(ACBrPIXCD.configLerValor("Bradesco", "Scopes"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações Bradesco: " + ex.getMessage());
        }
    }
}