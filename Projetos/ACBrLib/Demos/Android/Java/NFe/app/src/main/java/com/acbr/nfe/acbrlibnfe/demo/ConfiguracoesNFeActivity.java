package com.acbr.nfe.acbrlibnfe.demo;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;

import com.acbr.nfe.acbrlibnfe.demo.databinding.ActivityConfiguracoesNfeBinding;
import com.google.android.material.tabs.TabLayoutMediator;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import br.com.acbr.lib.nfe.ACBrLibNFe;

public class ConfiguracoesNFeActivity extends AppCompatActivity {

    private ActivityConfiguracoesNfeBinding binding;

    private ACBrLibNFe ACBrNFe;

    private NfeApplication application;

    private static final int REQUEST_CODE_PICK_FILE = 1;
    private static final int REQUEST_CODE_READ_EXTERNAL_STORAGE = 2;
    private boolean isRequiredOpenCertificate = false;

    @SuppressLint("MissingInflatedId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = ActivityConfiguracoesNfeBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        configTabLayout();

        application = (NfeApplication)getApplicationContext();
        ACBrNFe = ACBrLibHelper.getInstance("");
        isRequiredOpenCertificate = false;

        checkAndRequestPermissions();
    }

    private void configTabLayout() {
        ViewPagerAdapter adapter = new ViewPagerAdapter(this);
        binding.viewPager.setAdapter(adapter);

        adapter.addFragment(new ConfiguracoesGeraisFragment(), "Geral");
        adapter.addFragment(new ConfiguracoesWebServicesFragment(), "WebServices");
        adapter.addFragment(new ConfiguracoesCertificadosFragment(), "Certificados");
        adapter.addFragment(new ConfiguracoesArquivosFragment(), "Arquivos");
        adapter.addFragment(new ConfiguracoesEmailFragment(), "Email");
        adapter.addFragment(new ConfiguracoesDocumentoAuxiliarFragment(), "Documento Auxiliar");

        binding.viewPager.setOffscreenPageLimit(adapter.getItemCount());

        TabLayoutMediator mediator = new TabLayoutMediator(binding.tabs, binding.viewPager, (tab, position) -> {
            tab.setText(adapter.getTitle(position));
        });

        mediator.attach();
    }

    private void checkAndRequestPermissions() {
        // Check if the READ_EXTERNAL_STORAGE permission is already granted
        if (ContextCompat.checkSelfPermission(this, android.Manifest.permission.READ_EXTERNAL_STORAGE)
                != PackageManager.PERMISSION_GRANTED) {
            // If not, request it
            ActivityCompat.requestPermissions(this,
                    new String[]{android.Manifest.permission.READ_EXTERNAL_STORAGE},
                    REQUEST_CODE_READ_EXTERNAL_STORAGE);
        } else {
            openFilePicker();
        }
    }

    // Open file picker to let the user choose a file
    protected void openFilePicker() {
        if (!isRequiredOpenCertificate) return;
        Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT);
        intent.setType("application/x-pkcs12");
        intent.addCategory(Intent.CATEGORY_OPENABLE);
        startActivityForResult(intent, REQUEST_CODE_PICK_FILE);
    }

    // Handle the file URI after the user selects a file
    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);

        if (requestCode == REQUEST_CODE_PICK_FILE && resultCode == RESULT_OK && data != null) {
            Uri uri = data.getData();

            if (uri != null) {
                // Process the file URI here (open, read, etc.)
                copyCertificateToApp(uri);
            }
        }
    }

    private void copyCertificateToApp(Uri sourceUri) {
        try {
            // Abrir InputStream do arquivo de origem
            InputStream inputStream = getContentResolver().openInputStream(sourceUri);

            // Definir o destino para o arquivo na pasta do app (getExternalFilesDir())
            File destFile = new File(application.getAppDir(), application.PFX_PADRAO);
            OutputStream outputStream = new FileOutputStream(destFile);

            // Fazer a cópia do arquivo
            byte[] buffer = new byte[1024];
            int length;
            while ((length = inputStream.read(buffer)) > 0) {
                outputStream.write(buffer, 0, length);
            }

            // Fechar os streams
            inputStream.close();
            outputStream.close();

            Toast.makeText(this, "Arquivo copiado para " + destFile.getAbsolutePath(), Toast.LENGTH_SHORT).show();
        } catch (IOException e) {
            Toast.makeText(this, "Erro ao copiar o arquivo: " + e.getMessage(), Toast.LENGTH_SHORT).show();
        }
    }

    public void getCertificate() {
        isRequiredOpenCertificate = true;
        openFilePicker();
        isRequiredOpenCertificate = false;
    }
}